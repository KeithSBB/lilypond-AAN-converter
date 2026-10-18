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
% inverted so every chord tone sits on or above that line.  A bass note in
% the same chord stays put: <e, a>^"M" becomes <e, a cis' e'>.  Lone bass
% notes and everything else pass through.
%
%        \aaan-convert { ... AAAN music ... }
%        \aaanLanguage #'english     % default
%        \aaanLanguage #'russian
%
% English keeps the source chord mark.  Russian replaces it with the
% Stradella row letter only (Б М 7 У) and does not print a pitch name.
% In Russian only, a zero-duration parenthesized root cue is placed with a
% chord whose root bass was not just played or sounded with it, including
% when that bass was a counterbass.  One cue covers a run of the same chord.
% A source counterbass _"_" is printed unchanged in English, and as
% Cyrillic «В» (U+0412) in Russian.  The row is never inferred.
% Italian dynamics and Italian navigation (f, fine, D.C.) are not translated.
% The song title is never translated.

\version "2.24.4"

\include "accbasschord.ly"

% 'english or 'russian.  Read when an engraving function runs.
#(define aaan-language 'english)

% Last Russian chord identity in the current measure: (notename alteration quality) or #f
#(define aaan-last-ru-identity #f)

% Exact title string.  Compared after trimming.  Not translated.
#(define aaan-song-title "")
#(define aaan-protected-titles '())
#(define aaan-untranslated-warned (make-hash-table))

aaanLanguage =
#(define-void-function (lang) (symbol?)
   (if (memq lang '(english russian))
       (set! aaan-language lang)
       (ly:warning "aaanLanguage: expected #'english or #'russian, got ~a" lang)))

aaanProtectTitle =
#(define-void-function (title) (string?)
   (set! aaan-song-title title)
   (set! aaan-protected-titles (cons title aaan-protected-titles)))

% ---------------------------------------------------------------------------
% Chord-symbol spelling.  English lead-sheet symbols, or bayan textbook
% symbols: pitch name + Б / М / 7 / У.
% ---------------------------------------------------------------------------

#(define aaan-en-pitch-names '("C" "D" "E" "F" "G" "A" "B"))
#(define aaan-ru-pitch-names '("До" "Ре" "Ми" "Фа" "Соль" "Ля" "Си"))

#(define (aaan-alteration-suffix alteration language)
   (cond
    ((= alteration 0) "")
    ((= alteration 1/2) (if (eq? language 'russian) "-диез" "#"))
    ((= alteration -1/2) (if (eq? language 'russian) "-бемоль" "b"))
    ((= alteration 1) (if (eq? language 'russian) "-дубль-диез" "##"))
    ((= alteration -1) (if (eq? language 'russian) "-дубль-бемоль" "bb"))
    (else (format #f "~a" alteration))))

#(define (aaan-format-pitch pitch language)
   (let ((names (if (eq? language 'russian) aaan-ru-pitch-names aaan-en-pitch-names))
         (notename (ly:pitch-notename pitch)))
     (string-append (list-ref names notename)
                    (aaan-alteration-suffix (ly:pitch-alteration pitch) language))))

#(define (aaan-canonical-quality raw)
   (cond
    ((member raw '("M" "maj")) 'major)
    ((member raw '("m" "min")) 'minor)
    ((and (string? raw) (string=? raw "7")) 'dominant)
    ((member raw '("dim" "d" "o")) 'diminished)
    (else 'unknown)))

#(define (aaan-quality-suffix canonical language)
   (if (eq? language 'russian)
       (case canonical
         ((major) "Б")
         ((minor) "М")
         ((dominant) "7")
         ((diminished) "У")
         (else "?"))
       (case canonical
         ((major) "")
         ((minor) "m")
         ((dominant) "7")
         ((diminished) "dim")
         (else "?"))))

#(define (aaan-is-bar-check? music)
   (and (ly:music? music)
        (or (music-is-of-type? music 'bar-check)
            (eq? (ly:music-property music 'name) 'BarCheck))))

#(define (aaan-should-emit-symbol? note-event raw-quality tied-in)
   (if (or tied-in (not (eq? aaan-language 'russian)))
       (not tied-in)
       (let ((pitch (ly:music-property note-event 'pitch)))
         (if (not (ly:pitch? pitch))
             (begin
               (log-message 'warning
                            "aaan-should-emit-symbol?: no pitch on ~a (name ~a); skipping symbol\n"
                            note-event
                            (ly:music-property note-event 'name))
               #f)
             (let* ((quality (aaan-resolve-quality note-event raw-quality))
                    (canonical (aaan-canonical-quality quality))
                    (ident (list (ly:pitch-notename pitch)
                                 (ly:pitch-alteration pitch)
                                 canonical)))
               (if (or (not aaan-last-ru-identity)
                       (not (equal? ident aaan-last-ru-identity)))
                   (begin
                     (set! aaan-last-ru-identity ident)
                     #t)
                   #f))))))

#(define (aaan-resolve-quality note-event raw)
   (if (equal? raw "unknown")
       (let ((found (search-history note-event)))
         (if (null? found) "M" found))
       raw))

#(define (aaan-format-chord-symbol note-event raw-quality)
   (let* ((quality (aaan-resolve-quality note-event raw-quality))
          (canonical (aaan-canonical-quality quality))
          (pitch (ly:music-property note-event 'pitch)))
     (if (eq? aaan-language 'russian)
         (aaan-quality-suffix canonical 'russian)
         (string-append (aaan-format-pitch pitch 'english)
                        (aaan-quality-suffix canonical 'english)))))

#(define (aaan-make-symbol-event text)
   (make-music 'TextScriptEvent
               'text text
               'direction 1
               'aaan-generated #t))

% Spelled chord tones sit on or above aaan-bass-center (absolute d).
% The Russian cue is a grace note: its main-time length is zero, so the
% bar does not move.  A main-time duration of zero is not engraved.
#(define aaan-octave-down (ly:make-pitch -1 0 0))

#(define (aaan-prop-list music sym)
   (let ((v (ly:music-property music sym)))
     (if (pair? v) (filter ly:music? v) '())))

#(define (aaan-generated-text? event)
   (and (ly:music? event)
        (eq? (ly:music-property event 'aaan-generated) #t)))

#(define (aaan-replace-elements music new-elements)
   (let ((copy (ly:music-deep-copy music)))
     (set! (ly:music-property copy 'elements) new-elements)
     copy))

#(define (aaan-pitch-class pitch)
   (list (ly:pitch-notename pitch) (ly:pitch-alteration pitch)))

#(define (aaan-same-pitch-class? left right)
   (and (ly:pitch? left)
        (ly:pitch? right)
        (equal? (aaan-pitch-class left) (aaan-pitch-class right))))

#(define (aaan-quality-script? event)
   (and (ly:music? event)
        (eq? (ly:music-property event 'name) 'TextScriptEvent)
        (equal? (ly:music-property event 'direction) 1)
        (let ((text (ly:music-property event 'text)))
          (and (string? text) (member text chord-keys)))))

#(define (aaan-counterbass-mark? event)
   (and (ly:music? event)
        (eq? (ly:music-property event 'name) 'TextScriptEvent)
        (equal? (ly:music-property event 'direction) -1)
        (equal? (ly:music-property event 'text) "_")))

#(define (aaan-has-counterbass? music)
   (any aaan-counterbass-mark?
        (append (aaan-prop-list music 'articulations)
                (aaan-prop-list music 'elements))))

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

#(define (aaan-cue-grace pitch)
   (let ((note (make-music 'NoteEvent
                           'pitch (aaan-bass-register pitch)
                           'duration (ly:make-duration 3)
                           'tweaks '((parenthesized . #t)
                                     (font-size . -3)
                                     ((Parentheses . font-size) . -2)
                                     ((Stem . transparent) . #t)
                                     ((Flag . transparent) . #t)
                                     ((Dots . transparent) . #t)))))
     (make-music 'GraceMusic
                 'element (make-music 'SequentialMusic
                                      'elements (list note)))))

#(define (aaan-attach-cue chord root)
   (let ((seq (make-music 'SequentialMusic
                          'elements (list (aaan-cue-grace root) chord))))
     (set! (ly:music-property seq 'aaan-cue-seq) #t)
     seq))

#(define (aaan-apply-russian-mark scripts canonical)
   (let ((letter (aaan-quality-suffix canonical 'russian))
         (seen #f))
     (let ((out (map (lambda (script)
                       (if (and (not seen) (aaan-quality-script? script))
                           (begin
                             (set! seen #t)
                             (aaan-make-symbol-event letter))
                           script))
                     scripts)))
       (if seen
           out
           (append out (list (aaan-make-symbol-event letter)))))))

% Quality and inversion come from note-event-to-chord-elements, which
% uses aaan-invert-onto-center.  An unmarked chord reuses that root's
% history, and the first one is major.  Low chord tones move up an
% octave until every one is on or above aaan-bass-center.
#(define (aaan-used-quality note raw)
   (let ((found (search-history note)))
     (if (string? found) found raw)))

#(define (aaan-convert-chord-note note extra bass-info run)
   (let* ((root (ly:music-property note 'pitch))
          (scripts (append (aaan-prop-list note 'articulations)
                           (if (pair? extra) extra '())))
          (raw (aaan-quality-token scripts))
          (tones (note-event-to-chord-elements note raw))
          (quality (aaan-used-quality note raw))
          (canonical (aaan-canonical-quality quality))
          (ident (list (ly:pitch-notename root)
                       (ly:pitch-alteration root)
                       canonical)))
     (if (null? tones)
         (list note bass-info run)
         (let* ((marks (if (eq? aaan-language 'russian)
                           (aaan-apply-russian-mark scripts canonical)
                           scripts))
                (chord (make-music 'EventChord
                                   'elements tones
                                   'articulations (filter ly:music? marks)))
                (cue? (and (eq? aaan-language 'russian)
                           (not (equal? ident run))
                           (not (aaan-root-sounded? bass-info root)))))
           (list (if cue? (aaan-attach-cue chord root) chord) #f ident)))))

#(define (aaan-event-chord-of music)
   (if (and (ly:music? music)
            (eq? (ly:music-property music 'aaan-cue-seq) #t))
       (last (aaan-prop-list music 'elements))
       music))

#(define (aaan-prepend-bass music bass-notes)
   (if (null? bass-notes)
       music
       (let ((chord (aaan-event-chord-of music)))
         (set! (ly:music-property chord 'elements)
               (append (map ly:music-deep-copy bass-notes)
                       (aaan-prop-list chord 'elements)))
         music)))

#(define (aaan-convert-note music bass-info run)
   (cond
    ((is-AAAN-chord? music)
     (aaan-convert-chord-note music '() bass-info run))
    ((is-AAAN-bass? music)
     (list music (aaan-bass-entries (list music) #f) #f))
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
                                            run)))
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
       (list music (aaan-bass-entries bass-notes chord-marked?) #f))
      ((any aaan-rhythmic-rest? elements)
       (list music #f #f))
      (else
       (list music bass-info run)))))

#(define (aaan-as-elements music)
   (if (and (ly:music? music)
            (eq? (ly:music-property music 'aaan-cue-seq) #t))
       (aaan-prop-list music 'elements)
       (list music)))

#(define (aaan-convert-sequential music bass-info run)
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
                 (append (reverse (aaan-as-elements (first step))) acc))))))

#(define (aaan-convert-simultaneous music bass-info run)
   (let ((walked (map (lambda (e)
                        (first (aaan-convert-walk e bass-info run)))
                      (aaan-prop-list music 'elements))))
     ;; A split is its own passage.  The music after it does not continue
     ;; a chord run or a bass from before the split.
     (list (aaan-replace-elements music walked) #f #f)))

#(define (aaan-convert-volta music bass-info run)
   (let ((body (ly:music-property music 'element))
         (alts (aaan-prop-list music 'elements)))
     (let ((body-step (if (ly:music? body)
                          (aaan-convert-walk body bass-info run)
                          (list body bass-info run))))
       (let ((alt-steps (map (lambda (alt)
                               (aaan-convert-walk alt
                                                 (second body-step)
                                                 (third body-step)))
                             alts)))
         (let ((copy (ly:music-deep-copy music)))
           (if (ly:music? (first body-step))
               (set! (ly:music-property copy 'element) (first body-step)))
           (if (pair? alts)
               (set! (ly:music-property copy 'elements) (map first alt-steps)))
           (let ((tail (if (null? alt-steps) body-step (last alt-steps))))
             (list copy (second tail) (third tail))))))))

#(define (aaan-convert-container music bass-info run)
   (let ((copy (ly:music-deep-copy music))
         (bass bass-info)
         (run run))
     (let ((el (ly:music-property copy 'element)))
       (if (ly:music? el)
           (let ((step (aaan-convert-walk el bass run)))
             (set! (ly:music-property copy 'element) (first step))
             (set! bass (second step))
             (set! run (third step)))))
     (let ((els (aaan-prop-list copy 'elements)))
       (if (pair? els)
           (let ((step (aaan-convert-sequential
                        (make-music 'SequentialMusic 'elements els)
                        bass run)))
             (set! (ly:music-property copy 'elements)
                   (ly:music-property (first step) 'elements))
             (set! bass (second step))
             (set! run (third step)))))
     (list copy bass run)))

#(define (aaan-convert-walk music bass-info run)
   (cond
    ((not (ly:music? music)) (list music bass-info run))
    ((music-is-of-type? music 'note-event)
     (aaan-convert-note music bass-info run))
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

% Source counterbass is a down-text underscore.  English prints it
% unchanged.  Russian prints Cyrillic VE (U+0412), never Latin B.
#(define (aaan-rewrite-counterbass-walk music)
   (cond
    ((not (ly:music? music)) music)
    ((aaan-counterbass-mark? music)
     (let ((copy (ly:music-deep-copy music)))
       (set! (ly:music-property copy 'text) "В")
       (set! (ly:music-property copy 'aaan-generated) #t)
       copy))
    (else
     (let ((copy (ly:music-deep-copy music)))
       (let ((el (ly:music-property copy 'element)))
         (if (ly:music? el)
             (set! (ly:music-property copy 'element)
                   (aaan-rewrite-counterbass-walk el))))
       (let ((els (ly:music-property copy 'elements)))
         (if (pair? els)
             (set! (ly:music-property copy 'elements)
                   (map aaan-rewrite-counterbass-walk els))))
       (let ((arts (ly:music-property copy 'articulations)))
         (if (pair? arts)
             (set! (ly:music-property copy 'articulations)
                   (map aaan-rewrite-counterbass-walk arts))))
       copy))))

#(define (aaan-rewrite-counterbass music)
   (if (eq? aaan-language 'russian)
       (aaan-rewrite-counterbass-walk music)
       music))

aaan-engrave-bass =
#(define-music-function (music) (ly:music?)
   (let ((previous make-staccato))
     (set! make-staccato #f)
     (let ((result (scheme-extract-bass music)))
       (set! make-staccato previous)
       (aaan-rewrite-counterbass result))))

aaan-convert =
#(define-music-function (music) (ly:music?)
   (clear-history)
   (set! aaan-last-ru-identity #f)
   (let ((step (aaan-convert-walk (ly:music-deep-copy music) #f #f)))
     (aaan-rewrite-counterbass (first step))))

% The split chord engraver is no longer a separate walker.  Print goes
% through \aaan-convert.  The name stays until the later dead-code unit.
aaan-engrave-chords =
#(define-music-function (music) (ly:music?)
   #{ \aaan-convert $music #})

% ---------------------------------------------------------------------------
% Text.  Chord symbols are already in the selected language and are marked
% aaan-generated, so this pass leaves them alone.  The title is never translated.
% Unlisted text is left in the source language and warned once.
% ---------------------------------------------------------------------------

% Prose and instrument names only.  Dynamics and Italian navigation stay
% Italian: f, p, mf, fine, D.C., D.S., rit., a tempo, coda.
#(define aaan-text-dictionary
   '(("Bayan" . "Баян")
     ("bayan" . "Баян")
     ("Accordion" . "Аккордеон")
     ("accordion" . "Аккордеон")
     ("Bass" . "Бас")
     ("Chords" . "Аккорды")
     ("Chord" . "Аккорд")
     ("Composed by Keith Smith" . "Сочинение: Кит Смит")
     ("Moderato" . "Умеренно")
     ("Allegro" . "Аллегро")
     ("Andante" . "Анданте")
     ("Adagio" . "Адажио")
     ("Largo" . "Ларго")
     ("Presto" . "Престо")
     ("Vivace" . "Виваче")
     ("Tempo" . "Темп")
     ("Introduction" . "Вступление")
     ("Verse" . "Куплет")
     ("Chorus" . "Припев")
     ("Bridge" . "Бридж")
     ("Ending" . "Концовка")
     ("Fine" . "Фине")
     ("Coda" . "Кода")
     ("Repeat" . "Повтор")
     ("D.C." . "Д. К.")
     ("D.S." . "Д. С.")
     ("al fine" . "аль фине")
     ("a tempo" . "а темпо")
     ("rit." . "рит.")
     ("ritard." . "ритард.")
     ("accel." . "акцел.")
     ("cresc." . "креш.")
     ("dim." . "дим.")
     ("sfz" . "сфц")
     ("fp" . "фп")
     ("pp" . "пп")
     ("p" . "п")
     ("mp" . "мп")
     ("mf" . "мф")
     ("f" . "ф")
     ("ff" . "фф")
     ("fff" . "ффф")))

#(define (aaan-title-exception? text)
   (or (and (string? text) (string=? text aaan-song-title))
       (any (lambda (title) (and (string? title) (string=? text title)))
            aaan-protected-titles)))

#(define (aaan-translate-string text)
   (if (or (not (eq? aaan-language 'russian))
           (not (string? text))
           (aaan-title-exception? text)
           (aaan-title-exception? (string-trim text)))
       text
       (let* ((trimmed (string-trim text))
              (hit (or (assoc text aaan-text-dictionary)
                       (assoc trimmed aaan-text-dictionary))))
         (if hit
             (cdr hit)
             (begin
               (if (not (hash-ref aaan-untranslated-warned text))
                   (begin
                     (hash-set! aaan-untranslated-warned text #t)
                     (log-message 'warning "aaan-translate: left untranslated: ~s\n" text)))
               text)))))

#(define (aaan-translate-value val)
   (cond
    ((string? val) (aaan-translate-string val))
    ((pair? val) (cons (aaan-translate-value (car val))
                       (aaan-translate-value (cdr val))))
    (else val)))

#(define (aaan-translate-event event)
   (if (or (not (ly:music? event)) (aaan-generated-text? event))
       event
       (let ((name (ly:music-property event 'name)))
         (if (memq name '(TextScriptEvent LyricEvent MarkEvent RehearsalMarkEvent))
             (let ((copy (ly:music-deep-copy event)))
               (set! (ly:music-property copy 'text)
                     (aaan-translate-value (ly:music-property copy 'text)))
               copy)
             event))))

#(define (aaan-translate-music music)
   (if (not (eq? aaan-language 'russian))
       music
       (music-map
        (lambda (event)
          (let ((translated (aaan-translate-event event)))
            (let ((els (ly:music-property translated 'elements)))
              (if (pair? els)
                  (set! (ly:music-property translated 'elements)
                        (map aaan-translate-event els))))
            (let ((arts (ly:music-property translated 'articulations)))
              (if (pair? arts)
                  (set! (ly:music-property translated 'articulations)
                        (map aaan-translate-event arts))))
            translated))
        (ly:music-deep-copy music))))

aaan-translate =
#(define-music-function (music) (ly:music?)
   (aaan-translate-music music))

% Opt-in header / markup text.  Do not wrap the song title.
aaan-text =
#(define-scheme-function (str) (string?)
   (markup (aaan-translate-string str)))

