%
% Copyright (C) 2026
% Author: Keith Smith <keith@santabayanian.com>
% aaan-engrave.ly
% https://github.com/KeithSBB/lilypond-AAN-converter
%
%  This program is free software; you can redistribute it and/or modify
%  it under the terms of the GNU General Public License, version 3,
%  as published by the Free Software Foundation.
%
%  WARNING: this file under GPLv3 only, not GPLv3+
%
%  This program is distributed in the hope that it will be useful,
%  but WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
%  See the GNU General Public License for more details.  It is
%  available  at
%  http://www.gnu.org/licenses/gpl-3.0.html
%
% Print converter.  \aaan-convert replaces each AAAN chord note (on or above
% the bass-clef middle line, absolute d) with a three-note accordion chord,
% the lowest inversion in which every note is on or above G3 (absolute g).
% A bass note in the same chord stays put: <e, a>^"M" becomes <e, a cis' e'>.
% Lone bass notes and everything else pass through.
%
%        \aaan-convert { ... AAAN music ... }
%        \aaanLanguage #'english     % default
%        \aaanLanguage #'russian
%
% English keeps the source chord mark.  Russian replaces a source mark
% with the Stradella row letter only (Б М 7 У) and does not print a pitch
% name.  A source chord with no mark gets no letter.  See README.md for the
% Russian cue rule.
% A source counterbass _"_" is printed unchanged in English, and as
% Cyrillic «В» (U+0412) in Russian.  The row is never inferred.

\version "2.24.4"

\include "accbasschord.ly"

% 'english or 'russian.  Read when an engraving function runs.
#(define aaan-language 'english)

aaanLanguage =
#(define-void-function (lang) (symbol?)
   (if (memq lang '(english russian))
       (set! aaan-language lang)
       (ly:warning "aaanLanguage: expected #'english or #'russian, got ~a" lang)))

#(define (aaan-canonical-quality raw)
   (cond
    ((member raw '("M" "maj")) 'major)
    ((member raw '("m" "min")) 'minor)
    ((and (string? raw) (string=? raw "7")) 'dominant)
    ((member raw '("dim" "d" "o")) 'diminished)
    (else 'unknown)))

#(define (aaan-quality-suffix canonical)
   (case canonical
     ((major) "Б")
     ((minor) "М")
     ((dominant) "7")
     ((diminished) "У")))

#(define (aaan-is-bar-check? music)
   (and (ly:music? music)
        (or (music-is-of-type? music 'bar-check)
            (eq? (ly:music-property music 'name) 'BarCheck))))

#(define (aaan-make-symbol-event text)
   (make-music 'TextScriptEvent
               'text text
               'direction 1
               'aaan-generated #t))

#(define aaan-octave-down (ly:make-pitch -1 0 0))

#(define (aaan-prop-list music sym)
   (let ((v (ly:music-property music sym)))
     (if (pair? v) (filter ly:music? v) '())))

#(define (aaan-replace-elements music new-elements)
   (let ((copy (ly:music-deep-copy music)))
     (set! (ly:music-property copy 'elements) new-elements)
     copy))

#(define (aaan-same-pitch-class? left right)
   (and (ly:pitch? left)
        (ly:pitch? right)
        (= (aaan-history-key left) (aaan-history-key right))))

#(define (aaan-counterbass-mark? event)
   (and (ly:music? event)
        (eq? (ly:music-property event 'name) 'TextScriptEvent)
        (equal? (ly:music-property event 'direction) -1)
        (equal? (ly:music-property event 'text) "_")))

#(define (aaan-has-counterbass? music)
   (any aaan-counterbass-mark?
        (append (aaan-prop-list music 'articulations)
                (aaan-prop-list music 'elements))))

#(define (aaan-ve-event event)
   (if (and (eq? aaan-language 'russian)
            (aaan-counterbass-mark? event))
       (let ((copy (ly:music-deep-copy event)))
         (set! (ly:music-property copy 'text) "В")
         copy)
       event))

% Rewrite _"_" on a bass note or a bass chord.  The mark may sit on the
% note or on the chord.  Callers read the original "_" first when they
% need to know the note is a counterbass.
#(define (aaan-with-counterbass music)
   (if (or (not (eq? aaan-language 'russian)) (not (ly:music? music)))
       music
       (let ((els (aaan-prop-list music 'elements)))
         (if (not (or (aaan-has-counterbass? music)
                      (any (lambda (e)
                             (and (music-is-of-type? e 'note-event)
                                  (aaan-has-counterbass? e)))
                           els)))
             music
             (let ((copy (ly:music-deep-copy music)))
               (set! (ly:music-property copy 'articulations)
                     (map aaan-ve-event (aaan-prop-list copy 'articulations)))
               (set! (ly:music-property copy 'elements)
                     (map (lambda (e)
                            (if (and (ly:music? e)
                                     (music-is-of-type? e 'note-event))
                                (aaan-with-counterbass e)
                                (aaan-ve-event e)))
                          (aaan-prop-list copy 'elements)))
               copy)))))

#(define (aaan-bass-entries notes chord-marked?)
   (map (lambda (n)
          (cons (ly:music-property n 'pitch)
                (or chord-marked? (aaan-has-counterbass? n))))
        notes))

#(define (aaan-root-sounded? info root)
   (and (ly:pitch? root)
        (pair? info)
        (any (lambda (item)
               (and (not (cdr item))
                    (aaan-same-pitch-class? (car item) root)))
             info)))

% Bass-button pitch for this chord root.  Staff position comes from
% aaan-on-or-above-center? against aaan-bass-center, not from semitones:
% des is on the middle line and steps down, cis is below it and stays.
#(define (aaan-bass-register pitch)
   (let loop ((p (ly:make-pitch -1
                                (ly:pitch-notename pitch)
                                (ly:pitch-alteration pitch))))
     (if (aaan-on-or-above-center? p)
         (loop (ly:pitch-transpose p aaan-octave-down))
         p)))

#(define aaan-current-voice-id (make-parameter #f))
#(define aaan-cues-here (make-parameter 0))
#(define aaan-voice-index (make-parameter #f))
#(define aaan-voice-serial 0)

#(define (aaan-next-voice-id)
   (set! aaan-voice-serial (+ aaan-voice-serial 1))
   (format #f "aaan~a" aaan-voice-serial))

#(define (aaan-in-voice id music voice-index)
   (let* ((props (and (integer? voice-index)
                      (<= 0 voice-index 3)
                      (context-spec-music (make-voice-props-set voice-index) 'Voice)))
          (seq (if (music-is-of-type? music 'sequential-music)
                   music
                   (make-music 'SequentialMusic 'elements (list music))))
          (body (if props
                    (aaan-replace-elements
                     seq
                     (cons props (aaan-prop-list seq 'elements)))
                    seq))
          (spec (context-spec-music body 'Voice id)))
     (set! (ly:music-property spec 'create-new) #t)
     spec))

% One notehead off the stem side, still under the chord.  An up stem sits
% on the right, so the cue moves left; a down stem sits on the left, so
% the cue moves right.  \\ sets the direction (even voice index up, odd
% down).  Otherwise this is LilyPond's default, down when the outer staff
% positions sum to zero or more.  Stem direction is not set here.
#(define aaan-notehead-width 1.3)

#(define (aaan-stem-direction pitches)
   (let ((idx (aaan-voice-index)))
     (if (integer? idx)
         (if (odd? idx) -1 1)
         (let ((pos (filter number?
                           (map (lambda (p)
                                  (if (ly:pitch? p)
                                      (- (aaan-diatonic-steps p)
                                         (aaan-diatonic-steps aaan-bass-center))
                                      #f))
                                pitches))))
           (if (or (null? pos)
                   (>= (+ (apply min pos) (apply max pos)) 0))
               -1
               1)))))

#(define (aaan-cue-shift pitches)
   (* -1 (aaan-stem-direction pitches) aaan-notehead-width))

#(define (aaan-cue-voice cue shift)
   (let ((nudge (cons shift 0)))
     #{
       \new Voice \with {
         \remove "Note_performer"
         \shiftOff
         \override NoteColumn.ignore-collision = ##t
         \omit Stem
         \omit Flag
         \omit Dots
         \override NoteHead.font-size = #-3
         \override NoteHead.extra-spacing-width = #'(+inf.0 . -inf.0)
         \override NoteHead.extra-offset = #nudge
         \override Parentheses.font-size = #-2
         \override Parentheses.padding = #0.2
         \override Parentheses.extra-spacing-width = #'(+inf.0 . -inf.0)
         \override Parentheses.extra-offset = #nudge
         \override Accidental.extra-spacing-width = #'(+inf.0 . -inf.0)
         \override Accidental.extra-offset = #nudge
       } { \parenthesize $cue }
     #}))

#(define (aaan-attach-cue chord root bass-notes)
   (aaan-cues-here (+ (aaan-cues-here) 1))
   (let* ((cue-pitch (aaan-bass-register root))
          (pitches (append (map (lambda (n) (ly:music-property n 'pitch))
                                (if (pair? bass-notes) bass-notes '()))
                           (map (lambda (e) (ly:music-property e 'pitch))
                                (filter (lambda (e)
                                          (and (ly:music? e)
                                               (eq? (ly:music-property e 'name)
                                                    'NoteEvent)))
                                        (aaan-prop-list chord 'elements)))))
          (shift (aaan-cue-shift pitches))
          (cue (make-music 'NoteEvent
                           'pitch cue-pitch
                           'duration (ly:make-duration 2 0 0)))
          (cue-voice (aaan-cue-voice cue shift))
          (id (aaan-current-voice-id)))
     (make-music
      'SimultaneousMusic
      'elements (list
                 (if (string? id)
                     #{ \context Voice = #id { $chord } #}
                     chord)
                 cue-voice))))

% Replace a source quality mark.  An unmarked chord keeps no letter.
#(define (aaan-apply-russian-mark scripts canonical)
   (let ((letter (aaan-quality-suffix canonical))
         (seen #f))
     (map (lambda (script)
            (if (and (not seen) (aaan-quality-mark? script))
                (begin
                  (set! seen #t)
                  (aaan-make-symbol-event letter))
                script))
          scripts)))

#(define (aaan-used-quality note raw)
   (let ((found (search-history note)))
     (if (null? found) raw found)))

#(define (aaan-convert-chord-note note extra bass-info run bass-notes)
   (let* ((root (ly:music-property note 'pitch))
          (scripts (map aaan-ve-event
                        (append (aaan-prop-list note 'articulations)
                                (if (pair? extra) extra '()))))
          (raw (aaan-quality-token scripts))
          (tones (note-event-to-chord-elements note raw))
          (quality (aaan-used-quality note raw))
          (canonical (aaan-canonical-quality quality))
          (ident (list (aaan-history-key root) canonical)))
     (if (null? tones)
         (list note bass-info run)
         (let* ((marks (if (eq? aaan-language 'russian)
                           (aaan-apply-russian-mark scripts canonical)
                           scripts))
                (chord (make-music 'EventChord
                                   'elements tones
                                   'articulations (filter ly:music? marks)))
                ;; A chord is cued only when it starts a run and its root
                ;; bass is not already sounding.  A counterbass does not
                ;; count.  A bass inside this chord does not break the run.
                (cue? (and (eq? aaan-language 'russian)
                           (not (equal? ident run))
                           (not (aaan-root-sounded? bass-info root)))))
           (list (if cue? (aaan-attach-cue chord root bass-notes) chord)
                 #f ident)))))

#(define (aaan-event-chord-of music)
   (cond
    ((not (ly:music? music)) #f)
    ((eq? (ly:music-property music 'name) 'EventChord) music)
    (else
     (or (aaan-event-chord-of (let ((el (ly:music-property music 'element)))
                                (if (ly:music? el) el #f)))
         (let loop ((els (aaan-prop-list music 'elements)))
           (if (null? els)
               #f
               (or (aaan-event-chord-of (car els))
                   (loop (cdr els)))))))))

#(define (aaan-prepend-bass music bass-notes)
   (if (null? bass-notes)
       music
       (let ((chord (aaan-event-chord-of music)))
         (set! (ly:music-property chord 'elements)
               (append (map (lambda (n)
                              (aaan-with-counterbass (ly:music-deep-copy n)))
                            bass-notes)
                       (aaan-prop-list chord 'elements)))
         music)))

#(define (aaan-convert-note music bass-info run)
   (cond
    ((is-AAAN-chord? music)
     (aaan-convert-chord-note music '() bass-info run '()))
    ((is-AAAN-bass? music)
     (list (aaan-with-counterbass music)
           (aaan-bass-entries (list music) #f)
           #f))
    (else
     (list music bass-info run))))

#(define (aaan-rhythmic-rest? music)
   (and (ly:music? music)
        (or (music-is-of-type? music 'rest-event)
            (music-is-of-type? music 'skip-event)
            (music-is-of-type? music 'multi-measure-rest))))

#(define (aaan-convert-chord music bass-info run)
   (let* ((elements (aaan-prop-list music 'elements))
          (chord-notes (filter is-AAAN-chord? elements))
          (bass-notes (filter is-AAAN-bass? elements))
          (others (filter (lambda (e)
                            (not (or (is-AAAN-chord? e)
                                     (is-AAAN-bass? e))))
                          elements))
          (chord-marked? (aaan-has-counterbass? music)))
     (cond
      ((pair? chord-notes)
       (let* ((info (append (aaan-bass-entries bass-notes chord-marked?)
                            (if (pair? bass-info) bass-info '())))
              (step (aaan-convert-chord-note (first chord-notes)
                                            (append others
                                                    (aaan-prop-list music 'articulations))
                                            info
                                            run
                                            bass-notes)))
         (if (pair? (cdr chord-notes))
             (let ((chord (aaan-event-chord-of (first step))))
               (for-each
                (lambda (extra)
                  (let ((tones (note-event-to-chord-elements
                                extra
                                (aaan-quality-token
                                 (append (aaan-prop-list extra 'articulations)
                                         others)))))
                    (if (pair? tones)
                        (set! (ly:music-property chord 'elements)
                              (append (aaan-prop-list chord 'elements) tones)))))
                (cdr chord-notes))))
         (list (aaan-prepend-bass (first step) bass-notes) #f (third step))))
      ((pair? bass-notes)
       (list (aaan-with-counterbass music)
             (aaan-bass-entries bass-notes chord-marked?)
             #f))
      ((any aaan-rhythmic-rest? elements)
       (list music #f #f))
      (else
       (list music bass-info run)))))

#(define (aaan-walk-items music bass-info run)
   (let loop ((items (aaan-prop-list music 'elements))
              (bass bass-info)
              (run run)
              (acc '()))
     (if (null? items)
         (list (aaan-replace-elements music (reverse acc)) bass run)
         (let ((step (aaan-convert-walk (car items) bass run)))
           (loop (cdr items)
                 (second step)
                 (third step)
                 (cons (first step) acc))))))

% A nested { }, tuplet, or repeat body keeps the voice it was written in.
% A new voice id is created only for the top-level sequential and for each
% \\ part.  On 2.24.4 a bare << chord \new Voice { cue } >> does not keep a
% slur that starts on the chord, so the chord is sent back to this voice.
#(define (aaan-convert-sequential music bass-info run . rest)
   (let ((voice-index (if (pair? rest) (car rest) #f)))
     (if (and (string? (aaan-current-voice-id)) (not (integer? voice-index)))
         (aaan-walk-items music bass-info run)
         (let ((voice-id (aaan-next-voice-id)))
           (parameterize ((aaan-current-voice-id voice-id)
                          (aaan-cues-here 0)
                          (aaan-voice-index voice-index))
             (let ((step (aaan-walk-items music bass-info run)))
               (list (if (or (> (aaan-cues-here) 0) (integer? voice-index))
                         (aaan-in-voice voice-id (first step) voice-index)
                         (first step))
                     (second step)
                     (third step))))))))

#(define (aaan-voice-separator? music)
   (and (ly:music? music)
        (eq? (ly:music-property music 'name) 'VoiceSeparator)))

#(define (aaan-split-voices elements)
   (let loop ((els elements) (current '()) (parts '()))
     (cond
      ((null? els) (reverse (cons (reverse current) parts)))
      ((aaan-voice-separator? (car els))
       (loop (cdr els) '() (cons (reverse current) parts)))
      (else (loop (cdr els) (cons (car els) current) parts)))))

#(define (aaan-convert-simultaneous music bass-info run)
   (let ((elements (aaan-prop-list music 'elements)))
     (if (any aaan-voice-separator? elements)
         (let ((parts (aaan-split-voices elements)))
           (list (make-music
                  'SimultaneousMusic
                  'elements (map (lambda (items index)
                                   (aaan-call-with-saved-history
                                    (lambda ()
                                      (let ((part
                                             (if (and (= (length items) 1)
                                                      (music-is-of-type? (car items)
                                                                         'sequential-music))
                                                 (car items)
                                                 (make-music 'SequentialMusic
                                                             'elements items))))
                                        (first (aaan-convert-sequential
                                                part bass-info run index))))))
                                 parts
                                 (iota (length parts))))
                 #f #f))
         (let ((walked (map (lambda (e)
                              (aaan-call-with-saved-history
                               (lambda ()
                                 (first (aaan-convert-walk e bass-info run)))))
                            elements)))
           (list (aaan-replace-elements music walked) #f #f)))))

#(define (aaan-convert-volta music bass-info run)
   (let ((body (ly:music-property music 'element))
         (alts (aaan-prop-list music 'elements)))
     (let ((body-step (if (ly:music? body)
                          (aaan-convert-walk body bass-info run)
                          (list body bass-info run))))
       (let ((alt-steps (map (lambda (alt)
                               (aaan-call-with-saved-history
                                (lambda ()
                                  (aaan-convert-walk alt
                                                    (second body-step)
                                                    (third body-step)))))
                             alts)))
         (let ((copy (ly:music-deep-copy music)))
           (if (ly:music? (first body-step))
               (set! (ly:music-property copy 'element) (first body-step)))
           (if (pair? alts)
               (set! (ly:music-property copy 'elements) (map first alt-steps)))
           (let ((tail (if (null? alt-steps) body-step (last alt-steps))))
             (list copy (second tail) (third tail))))))))

% The cue walker wraps a sequential in \new Voice so slurs stay in that
% voice.  A container must not keep that Voice as its element: the body
% is unwrapped, and the Voice is lifted around the container.  A volta
% alternative stays VoltaSpeccedMusic, so the Voice is not lifted there.
#(define (aaan-generated-voice? music)
   (and (ly:music? music)
        (eq? (ly:music-property music 'name) 'ContextSpeccedMusic)
        (eq? (ly:music-property music 'context-type) 'Voice)
        (eq? (ly:music-property music 'create-new) #t)
        (string? (ly:music-property music 'context-id))))

#(define (aaan-split-generated-voice music)
   (if (aaan-generated-voice? music)
       (values music (ly:music-property music 'element))
       (values #f music)))

#(define (aaan-convert-container music bass-info run)
   (let ((copy (ly:music-deep-copy music))
         (bass bass-info)
         (run run)
         (voice #f))
     (let ((el (ly:music-property copy 'element)))
       (if (ly:music? el)
           (let ((step (aaan-convert-walk el bass run)))
             (call-with-values
              (lambda () (aaan-split-generated-voice (first step)))
              (lambda (maybe-voice body)
                (if maybe-voice (set! voice maybe-voice))
                (set! (ly:music-property copy 'element) body)))
             (set! bass (second step))
             (set! run (third step)))))
     (let ((els (aaan-prop-list copy 'elements)))
       (if (pair? els)
           (let ((step (aaan-convert-sequential
                        (make-music 'SequentialMusic 'elements els)
                        bass run)))
             (call-with-values
              (lambda () (aaan-split-generated-voice (first step)))
              (lambda (maybe-voice body)
                (if maybe-voice (set! voice maybe-voice))
                (set! (ly:music-property copy 'elements)
                      (aaan-prop-list body 'elements))))
             (set! bass (second step))
             (set! run (third step)))))
     (if (and voice
              (not (music-is-of-type? copy 'volta-specification)))
         (begin
           (set! (ly:music-property voice 'element) copy)
           (list voice bass run))
         (list copy bass run))))

#(define (aaan-convert-walk music bass-info run)
   (cond
    ((not (ly:music? music)) (list music bass-info run))
    ((music-is-of-type? music 'note-event)
     (aaan-convert-note music bass-info run))
    ((aaan-is-bar-check? music)
     (list music bass-info run))
    ((aaan-rhythmic-rest? music)
     (list music #f #f))
    ;; EventChord is also simultaneous-music.  It must be matched first.
    ((music-is-of-type? music 'event-chord)
     (aaan-convert-chord music bass-info run))
    ((music-is-of-type? music 'sequential-music)
     (aaan-convert-sequential music bass-info run))
    ((music-is-of-type? music 'simultaneous-music)
     (aaan-convert-simultaneous music bass-info run))
    ((eq? (ly:music-property music 'name) 'VoltaRepeatedMusic)
     (aaan-convert-volta music bass-info run))
    (else
     (aaan-convert-container music bass-info run))))

aaan-convert =
#(define-music-function (music) (ly:music?)
   (clear-history)
   (first (aaan-convert-walk (ly:music-deep-copy music) #f #f)))

% ---------------------------------------------------------------------------
% \aaan-text.  One alist, one lookup.  In Russian mode a table string
% returns its Russian value.  English mode, and any string not in the
% table, pass through unchanged.
%
% Copyright is the one non-lookup case.  It translates only when the only
% words are [Copyright] Keith Smith; the year is the first 4-digit number
% not touching other digits; quotes and punctuation are ignored.
% ---------------------------------------------------------------------------

#(define aaan-text-dictionary
   '(("Bayan" . "Баян")
     ("Composed by Keith Smith" . "К. Смит")
     ("Moderately" . "Умеренно")
     ("Intro" . "Вступление")
     ("Slowly" . "Медленно")
     ("Unhurried" . "Не спеша")
     ("Leisurely" . "Неторопливо")
     ("Moderately fast" . "Умеренно скоро")
     ("Lively" . "Живо")
     ("With motion" . "Подвижно")
     ("Animated" . "Оживлённо")
     ("Fast" . "Быстро")
     ("Very fast" . "Очень быстро")
     ("Calmly" . "Спокойно")
     ("Broadly" . "Широко")
     ("Merrily" . "Весело")
     ("Cheerfully" . "Бодро")
     ("With restraint" . "Сдержанно")
     ("Singing" . "Певуче")
     ("Melodiously" . "Напевно")
     ("March tempo" . "В темпе марша")
     ("Waltz tempo" . "В темпе вальса")
     ("Chorus" . "Припев")
     ("Variation" . "Вариация")
     ("Instrumental break" . "Отыгрыш")
     ("Arranged by Keith Smith" . "Обработка К. Смита")
     ("Bass" . "Бас")))

#(define (aaan-copyright-translation text)
   ;; Keith Smith is the only rights holder with an approved Russian rendering.
   (define (first-year)
     (let ((len (string-length text)))
       (let loop ((i 0))
         (cond
          ((> (+ i 4) len) #f)
          ((and (string-every char-numeric? (substring text i (+ i 4)))
                (or (zero? i)
                    (not (char-numeric? (string-ref text (- i 1)))))
                (or (= (+ i 4) len)
                    (not (char-numeric? (string-ref text (+ i 4))))))
           (substring text i (+ i 4)))
          (else (loop (+ i 1)))))))
   (define (letter-words)
     (let loop ((chars (string->list text)) (word '()) (words '()))
       (cond
        ((and (null? chars) (null? word)) (reverse words))
        ((or (null? chars) (not (char-alphabetic? (car chars))))
         (loop (if (null? chars) '() (cdr chars))
               '()
               (if (null? word)
                   words
                   (cons (list->string (reverse word)) words))))
        (else (loop (cdr chars) (cons (car chars) word) words)))))
   (and (string? text)
        (let ((year (first-year)))
          (and year
               (member (letter-words)
                       '(("Keith" "Smith") ("Copyright" "Keith" "Smith")))
               (string-append "© Кит Смит, " year)))))

#(define (aaan-text-lookup text)
   (if (or (not (string? text))
           (not (eq? aaan-language 'russian)))
       text
       (or (assoc-ref aaan-text-dictionary text)
           (aaan-copyright-translation text)
           text)))

% Opt-in header / markup text.  Do not wrap the song title.
aaan-text =
#(define-scheme-function (str) (string?)
   (markup (aaan-text-lookup str)))
