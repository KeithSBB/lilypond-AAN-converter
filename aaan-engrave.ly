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
% Engraving converter.  American Accordion Notation (AAAN / AAA) to conventional
% bass-clef bass notes and fully spelled chords, with a chord symbol.
%
%        \aaan-engrave-bass { ... AAAN music ... }
%        \aaan-engrave-chords { ... AAAN music ... }
%        \aaan-translate { ... music ... }
%        \aaanLanguage #'english     % default
%        \aaanLanguage #'russian
%
% Articulations are taken from the source.  The MIDI staccato switch in
% accbasschord.ly is not applied here.
%
% Russian mode writes the Stradella row letter over the spelled chord (Б М 7 У)
% and does not print a pitch name.  The row symbol is written only on the first
% occurrence of a chord in a measure and on the first occurrence after a chord
% change; repeated identical chords in the same measure omit the symbol.
% English mode keeps lead-sheet symbols.
% A source counterbass is e_"_" and is engraved e_"B".  The row is never inferred.
% When the bass pitch class differs from the chord root, the chord gets _"(.)".
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
         (if (null? found) "unknown" found))
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

% Bass-clef center line is aaan-bass-center (absolute d, D3) from
% aaan-common.ly.  Bass notes and the root cue stay on or below it.
% Spelled chord notes stay above it.  The cue is a simultaneous note,
% so ledger lines appear and the chord's timing does not change.
#(define aaan-octave-down (ly:make-pitch -1 0 0))
#(define aaan-octave-up (ly:make-pitch 1 0 0))

#(define (aaan-semitones pitch)
   (ly:pitch-semitones pitch))

#(define (aaan-at-or-below-center pitch)
   (let loop ((p (ly:make-pitch -1
                                (ly:pitch-notename pitch)
                                (ly:pitch-alteration pitch))))
     (if (<= (aaan-semitones p) (aaan-semitones aaan-bass-center))
         p
         (loop (ly:pitch-transpose p aaan-octave-down)))))

#(define (aaan-with-pitch note pitch)
   (let ((copy (ly:music-deep-copy note)))
     (set! (ly:music-property copy 'pitch) pitch)
     copy))

#(define (aaan-place-bass note)
   (let ((pitch (ly:music-property note 'pitch)))
     (if (ly:pitch? pitch)
         (aaan-with-pitch note (aaan-at-or-below-center pitch))
         note)))

#(define (aaan-raise-above-center notes)
   (let loop ((notes notes))
     (let ((low (apply min (map (lambda (n)
                                  (aaan-semitones (ly:music-property n 'pitch)))
                                notes))))
       (if (> low (aaan-semitones aaan-bass-center))
           notes
           (loop (map (lambda (n)
                        (aaan-with-pitch n (ly:pitch-transpose
                                           (ly:music-property n 'pitch)
                                           aaan-octave-up)))
                      notes))))))

% Staff position relative to the bass-clef center line.  0 is d; -4 is the bottom line.
#(define (aaan-staff-pos pitch)
   (+ (* 7 (- (ly:pitch-octave pitch) -1))
      (- (ly:pitch-notename pitch) 1)))

% Stemless "(.)" attached to the chord.  A text script has no duration, so
% the bar stays as written, and the chord stem is left alone.
#(define (aaan-make-root-cue pitch)
   (let ((pos (aaan-staff-pos (aaan-at-or-below-center pitch))))
     (make-music 'TextScriptEvent
                 'text (markup #:fontsize -2 "(.)")
                 'direction 0
                 'aaan-generated #t
                 'tweaks `((outside-staff-priority . #f)
                           (staff-padding . #f)
                           (padding . 0)
                           (Y-offset . ,pos)
                           (extra-offset . (0.8 . 0))))))

#(define (aaan-pitch-class pitch)
   (list (ly:pitch-notename pitch) (ly:pitch-alteration pitch)))

#(define (aaan-same-pitch-class? left right)
   (and (ly:pitch? left)
        (ly:pitch? right)
        (equal? (aaan-pitch-class left) (aaan-pitch-class right))))

#(define (aaan-generated-text? event)
   (and (ly:music? event)
        (eq? (ly:music-property event 'aaan-generated) #t)))

#(define (aaan-drop-quality arts)
   (filter (lambda (a)
             (not (and (ly:music? a)
                       (let ((filtered (filter-for-chord-names (list a))))
                         (not (null? filtered))))))
           (if (pair? arts) arts '())))

#(define (aaan-replace-elements music new-elements)
   (let ((copy (ly:music-deep-copy music)))
     (set! (ly:music-property copy 'elements) new-elements)
     copy))

% Spelled chord above the bass-clef center line.  The root cue is not put
% in this chord: a text script is an outside-staff object and cannot sit
% on a staff line.  The cue is a note in a parallel voice.
#(define (aaan-spell-chord note-event extra-arts tied-in)
   (let* ((note-arts (ly:music-property note-event 'articulations))
          (raw (aaan-quality-token (append (if (pair? note-arts) note-arts '())
                                          (if (pair? extra-arts) extra-arts '()))))
          (elements (aaan-raise-above-center
                     (note-event-to-chord-elements note-event raw)))
          (kept (filter (lambda (a)
                          (and (ly:music? a)
                               (not (memq (ly:music-property a 'name)
                                          '(NoteEvent RestEvent SkipEvent)))))
                        (append (aaan-drop-quality note-arts)
                                (aaan-drop-quality extra-arts))))
          (emit? (aaan-should-emit-symbol? note-event raw tied-in))
          (symbol (and emit? (aaan-format-chord-symbol note-event raw)))
          (symbol-ev (if symbol (aaan-make-symbol-event symbol) #f))
          (marks (filter ly:music? (list symbol-ev))))
     (make-music 'EventChord
                 'elements (append elements marks)
                 'articulations kept)))

#(define (aaan-skip duration)
   (make-music 'SkipEvent 'duration duration))

% Small parenthesized notehead on the root staff position.  \parenthesize
% centers the notehead between the parenthesis glyphs; a text period does not.
#(define (aaan-cue-note pitch duration)
   (let ((note (make-music 'NoteEvent
                           'pitch (aaan-at-or-below-center pitch)
                           'duration duration)))
     #{ \parenthesize $note #}))

#(define (aaan-event-duration event)
   (let ((duration (ly:music-property event 'duration)))
     (if (ly:duration? duration) duration (ly:make-duration 2))))

#(define (aaan-bass-pitch event)
   (and (ly:music? event)
        (is-AAAN-bass? event)
        (ly:music-property event 'pitch)))

#(define (aaan-root-cue? bass-pitch chord-event)
   (let ((root (ly:music-property chord-event 'pitch)))
     (and (ly:pitch? bass-pitch)
          (ly:pitch? root)
          (not (aaan-same-pitch-class? bass-pitch root)))))

#(define aaan-keep-bass #f)

#(define (aaan-with-duration note duration)
   (let ((copy (ly:music-deep-copy note)))
     (set! (ly:music-property copy 'duration) duration)
     copy))

#(define (aaan-bass-only bass-here duration)
   (cond
    ((null? bass-here) (make-music 'RestEvent 'duration duration))
    ((= (length bass-here) 1)
     (aaan-with-duration (aaan-place-bass (first bass-here)) duration))
    (else
     (make-music 'EventChord
                 'elements (map (lambda (n)
                                  (aaan-with-duration (aaan-place-bass n) duration))
                                bass-here)))))

#(define (engrave-note event tied-in bass-pitch)
   (cond
    ((is-AAAN-chord? event)
     (let ((root (ly:music-property event 'pitch))
           (duration (aaan-event-duration event)))
       (list (aaan-spell-chord event '() tied-in)
             (if (aaan-root-cue? bass-pitch event)
                 (aaan-cue-note root duration)
                 (aaan-skip duration))
             (aaan-event-has-tie? event)
             bass-pitch)))
    ((is-AAAN-bass? event)
     (list (if aaan-keep-bass (aaan-place-bass event) (make-rest event))
           (aaan-skip (aaan-event-duration event))
           #f
           (ly:music-property event 'pitch)))
    (else
     (list event (aaan-skip (aaan-event-duration event)) #f bass-pitch))))

#(define (engrave-event-chord event tied-in bass-pitch)
   (let* ((elements (ly:music-property event 'elements))
          (chord-notes (filter (lambda (e) (and (ly:music? e) (is-AAAN-chord? e))) elements))
          (bass-here (filter (lambda (e) (aaan-bass-pitch e)) elements)))
     ;; \autoBeamOff is an empty chord with a beam-forbid event and no
     ;; duration.  Replacing it with a rest is what shifted the bar.
     (if (and (null? chord-notes) (null? bass-here))
         (list event (aaan-skip (get-event-chord-duration event)) #f bass-pitch)
         (let* ((local-bass (if (null? bass-here)
                                bass-pitch
                                (ly:music-property (first bass-here) 'pitch)))
                (duration (get-event-chord-duration event))
                (want-cue (and (not (null? chord-notes))
                               (aaan-root-cue? local-bass (first chord-notes))))
                (spelled (if (null? chord-notes)
                             #f
                             (aaan-spell-chord (first chord-notes)
                                              (filter (lambda (e)
                                                        (not (and (ly:music? e)
                                                                  (eq? (ly:music-property e 'name) 'NoteEvent))))
                                                      elements)
                                              tied-in)))
                (cue (if want-cue
                         (aaan-cue-note (ly:music-property (first chord-notes) 'pitch) duration)
                         (aaan-skip duration)))
                ;; Both children carry the source duration, so the
                ;; simultaneous cannot outlast the attack it replaces.
                (main (cond
                       ((and aaan-keep-bass (not (null? bass-here)) spelled)
                        (make-music 'SimultaneousMusic
                                    'elements (list (aaan-bass-only bass-here duration)
                                                    spelled)))
                       (spelled spelled)
                       (aaan-keep-bass (aaan-bass-only bass-here duration))
                       (else (make-music 'RestEvent 'duration duration)))))
           (list main cue (and spelled (aaan-event-has-tie? event)) local-bass)))))

#(define (engrave-sequential music bass-pitch)
   (let loop ((items (ly:music-property music 'elements))
              (main-acc '())
              (cue-acc '())
              (tied-in #f)
              (bass bass-pitch))
     (if (null? items)
         (list (aaan-replace-elements music (reverse main-acc))
               (aaan-replace-elements music (reverse cue-acc))
               #f
               bass)
         (let ((item (car items)))
           (if (aaan-is-bar-check? item)
               (set! aaan-last-ru-identity #f))
           (let ((step (engrave-walk item tied-in bass)))
             (loop (cdr items)
                   (cons (first step) main-acc)
                   (cons (second step) cue-acc)
                   (third step)
                   (fourth step)))))))

#(define (engrave-copy-structure music tied-in bass-pitch)
   (let ((main (ly:music-deep-copy music))
         (cue (ly:music-deep-copy music))
         (next-bass bass-pitch))
     (let ((el (ly:music-property main 'element)))
       (if (ly:music? el)
           (let ((step (engrave-walk el tied-in bass-pitch)))
             (set! (ly:music-property main 'element) (first step))
             (set! (ly:music-property cue 'element) (second step))
             (set! next-bass (fourth step)))))
     (let ((els (ly:music-property main 'elements)))
       (if (pair? els)
           (let ((steps (map (lambda (e) (engrave-walk e #f bass-pitch)) els)))
             (set! (ly:music-property main 'elements) (map first steps))
             (set! (ly:music-property cue 'elements) (map second steps)))))
     (list main cue #f next-bass)))

#(define (engrave-walk music tied-in bass-pitch)
   ;; Reset the Russian chord identity at every bar check, whatever
   ;; structure contains it.  engrave-sequential also resets, but
   ;; repeats, alternatives, and other wrappers walk their children
   ;; through engrave-copy-structure and would otherwise keep the
   ;; previous measure's identity.
   (if (aaan-is-bar-check? music)
       (set! aaan-last-ru-identity #f))
   (cond
    ((not (ly:music? music)) (list music music #f bass-pitch))
    ((music-is-of-type? music 'note-event)
     (engrave-note music tied-in bass-pitch))
    ((music-is-of-type? music 'rest-event)
     (list music (aaan-skip (aaan-event-duration music)) #f bass-pitch))
    ((music-is-of-type? music 'event-chord)
     (engrave-event-chord music tied-in bass-pitch))
    ((music-is-of-type? music 'sequential-music)
     (engrave-sequential music bass-pitch))
    ((music-is-of-type? music 'simultaneous-music)
     (engrave-copy-structure music tied-in bass-pitch))
    (else
     (engrave-copy-structure music tied-in bass-pitch))))

% Source counterbass mark is a down-text underscore.  Engrave it as B.
% Any other bass is left unmarked: the row is never inferred.
#(define (aaan-counterbass-mark? event)
   (and (ly:music? event)
        (eq? (ly:music-property event 'name) 'TextScriptEvent)
        (equal? (ly:music-property event 'direction) -1)
        (equal? (ly:music-property event 'text) "_")))

#(define (aaan-rewrite-counterbass music)
   (cond
    ((not (ly:music? music)) music)
    ((aaan-counterbass-mark? music)
     (let ((copy (ly:music-deep-copy music)))
       (set! (ly:music-property copy 'text) "B")
       (set! (ly:music-property copy 'aaan-generated) #t)
       copy))
    (else
     (let ((copy (ly:music-deep-copy music)))
       (let ((el (ly:music-property copy 'element)))
         (if (ly:music? el)
             (set! (ly:music-property copy 'element) (aaan-rewrite-counterbass el))))
       (let ((els (ly:music-property copy 'elements)))
         (if (pair? els)
             (set! (ly:music-property copy 'elements)
                   (map aaan-rewrite-counterbass els))))
       (let ((arts (ly:music-property copy 'articulations)))
         (if (pair? arts)
             (set! (ly:music-property copy 'articulations)
                   (map aaan-rewrite-counterbass arts))))
       copy))))

aaan-engrave-bass =
#(define-music-function (music) (ly:music?)
   ;; MIDI staccato is global.  Force it off so print matches the source.
   (let ((previous make-staccato))
     (set! make-staccato #f)
     (let ((result (scheme-extract-bass music)))
       (set! make-staccato previous)
       (aaan-rewrite-counterbass result))))

aaan-engrave-chords =
#(define-music-function (music) (ly:music?)
   (clear-history)
   (set! aaan-last-ru-identity #f)
   (set! aaan-keep-bass #f)
   (first (engrave-walk music #f #f)))

% Bass notes and spelled chords in one voice.  The cue is a stemless note
% in a second voice of the same length: a skip wherever there is no cue.
% A text script cannot sit on a staff line; a note head can, and it draws
% the ledger line.  The stem override stays in the cue voice.
aaan-engrave =
#(define-music-function (music) (ly:music?)
   (clear-history)
   (set! aaan-last-ru-identity #f)
   (set! aaan-keep-bass #t)
   (let* ((step (engrave-walk music #f #f))
          (main (aaan-rewrite-counterbass (first step)))
          (cue (second step)))
     (set! aaan-keep-bass #f)
     #{
       <<
         \new Voice \with { \shiftOff } { \oneVoice $main }
         \new Voice \with {
           \shiftOff
           \override NoteColumn.ignore-collision = ##t
           \override NoteColumn.force-hshift = #0
           \omit Stem
           \omit Flag
           \omit Dots
           \override NoteHead.font-size = #-3
           \override Parentheses.font-size = #-2
         } { \oneVoice $cue }
       >>
     #}))

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

