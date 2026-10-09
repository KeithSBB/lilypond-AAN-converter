%
% Copyright (C) 2026
% Author: Keith Smith <keith@santabayanian.com>
% aan-engrave.ly
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
% Engraving converter.  American Accordion Notation (AAN / AAA) to conventional
% bass-clef bass notes and fully spelled chords, with a chord symbol.
%
%        \aan-engrave-bass { ... AAN music ... }
%        \aan-engrave-chords { ... AAN music ... }
%        \aan-translate { ... music ... }
%        \aanLanguage #'english     % default
%        \aanLanguage #'russian
%
% Articulations are taken from the source.  The MIDI staccato switch in
% accbasschord.ly is not applied here.
%
% Russian mode writes the Stradella row letter over the spelled chord (Б М 7 У)
% and does not print a pitch name.  English mode keeps lead-sheet symbols.
% A source counterbass is e_"_" and is engraved e_"B".  The row is never inferred.
% When the bass pitch class differs from the chord root, the chord gets _"(.)".
% Italian dynamics and Italian navigation (f, fine, D.C.) are not translated.
% The song title is never translated.

\version "2.24.4"

\include "accbasschord.ly"

% 'english or 'russian.  Read when an engraving function runs.
#(define aan-language 'english)

% Exact title string.  Compared after trimming.  Not translated.
#(define aan-song-title "")
#(define aan-protected-titles '())
#(define aan-untranslated-warned (make-hash-table))

aanLanguage =
#(define-void-function (lang) (symbol?)
   (if (memq lang '(english russian))
       (set! aan-language lang)
       (ly:warning "aanLanguage: expected #'english or #'russian, got ~a" lang)))

aanProtectTitle =
#(define-void-function (title) (string?)
   (set! aan-song-title title)
   (set! aan-protected-titles (cons title aan-protected-titles)))

% ---------------------------------------------------------------------------
% Chord-symbol spelling.  English lead-sheet symbols, or bayan textbook
% symbols: pitch name + Б / М / 7 / У.
% ---------------------------------------------------------------------------

#(define aan-en-pitch-names '("C" "D" "E" "F" "G" "A" "B"))
#(define aan-ru-pitch-names '("До" "Ре" "Ми" "Фа" "Соль" "Ля" "Си"))

#(define (aan-alteration-suffix alteration language)
   (cond
    ((= alteration 0) "")
    ((= alteration 1/2) (if (eq? language 'russian) "-диез" "#"))
    ((= alteration -1/2) (if (eq? language 'russian) "-бемоль" "b"))
    ((= alteration 1) (if (eq? language 'russian) "-дубль-диез" "##"))
    ((= alteration -1) (if (eq? language 'russian) "-дубль-бемоль" "bb"))
    (else (format #f "~a" alteration))))

#(define (aan-format-pitch pitch language)
   (let ((names (if (eq? language 'russian) aan-ru-pitch-names aan-en-pitch-names))
         (notename (ly:pitch-notename pitch)))
     (string-append (list-ref names notename)
                    (aan-alteration-suffix (ly:pitch-alteration pitch) language))))

#(define (aan-canonical-quality raw)
   (cond
    ((member raw '("M" "maj")) 'major)
    ((member raw '("m" "min")) 'minor)
    ((and (string? raw) (string=? raw "7")) 'dominant)
    ((member raw '("dim" "d" "o")) 'diminished)
    ((and (string? raw) (string=? raw "7sus2")) 'sus2-7)
    (else 'unknown)))

#(define (aan-quality-suffix canonical language)
   (if (eq? language 'russian)
       (case canonical
         ((major) "Б")
         ((minor) "М")
         ((dominant) "7")
         ((diminished) "У")
         ((sus2-7) "7sus2")
         (else "?"))
       (case canonical
         ((major) "")
         ((minor) "m")
         ((dominant) "7")
         ((diminished) "dim")
         ((sus2-7) "7sus2")
         (else "?"))))

#(define (aan-resolve-quality note-event raw)
   (if (equal? raw "unknown")
       (let ((found (search-history note-event)))
         (if (null? found) "unknown" found))
       raw))

#(define (aan-format-chord-symbol note-event raw-quality)
   (let* ((quality (aan-resolve-quality note-event raw-quality))
          (canonical (aan-canonical-quality quality))
          (pitch (ly:music-property note-event 'pitch)))
     (if (eq? aan-language 'russian)
         (aan-quality-suffix canonical 'russian)
         (string-append (aan-format-pitch pitch 'english)
                        (aan-quality-suffix canonical 'english)))))

#(define (aan-make-symbol-event text)
   (make-music 'TextScriptEvent
               'text text
               'direction 1
               'aan-generated #t))

% Bass-clef center line is d.  Bass notes and the root cue stay on or below
% it.  Spelled chord notes stay above it.  The cue is a simultaneous note,
% so ledger lines appear and the chord's timing does not change.
#(define aan-bass-center (ly:make-pitch -1 1 0))
#(define aan-octave-down (ly:make-pitch -1 0 0))
#(define aan-octave-up (ly:make-pitch 1 0 0))

#(define (aan-semitones pitch)
   (ly:pitch-semitones pitch))

#(define (aan-at-or-below-center pitch)
   (let loop ((p (ly:make-pitch -1
                                (ly:pitch-notename pitch)
                                (ly:pitch-alteration pitch))))
     (if (<= (aan-semitones p) (aan-semitones aan-bass-center))
         p
         (loop (ly:pitch-transpose p aan-octave-down)))))

#(define (aan-with-pitch note pitch)
   (let ((copy (ly:music-deep-copy note)))
     (set! (ly:music-property copy 'pitch) pitch)
     copy))

#(define (aan-place-bass note)
   (let ((pitch (ly:music-property note 'pitch)))
     (if (ly:pitch? pitch)
         (aan-with-pitch note (aan-at-or-below-center pitch))
         note)))

#(define (aan-raise-above-center notes)
   (let loop ((notes notes))
     (let ((low (apply min (map (lambda (n)
                                  (aan-semitones (ly:music-property n 'pitch)))
                                notes))))
       (if (> low (aan-semitones aan-bass-center))
           notes
           (loop (map (lambda (n)
                        (aan-with-pitch n (ly:pitch-transpose
                                           (ly:music-property n 'pitch)
                                           aan-octave-up)))
                      notes))))))

% Stemless "(.)" on the root's staff position, in its own voice so the
% notehead override cannot reach the chord.  shiftOff keeps the column.
#(define (aan-make-root-cue pitch duration)
   (let ((cue (make-music 'NoteEvent
                          'pitch (aan-at-or-below-center pitch)
                          'duration duration)))
     #{
       \new Voice {
         \shiftOff
         \once \override Stem.stencil = ##f
         \once \override Flag.stencil = ##f
         \once \override Dots.stencil = ##f
         \once \override NoteHead.stencil = #ly:text-interface::print
         \once \override NoteHead.text = \markup { \fontsize #-2 "(.)" }
         \once \override NoteColumn.ignore-collision = ##t
         \once \override NoteColumn.force-hshift = #0
         $cue
       }
     #}))

#(define (aan-pitch-class pitch)
   (list (ly:pitch-notename pitch) (ly:pitch-alteration pitch)))

#(define (aan-same-pitch-class? left right)
   (and (ly:pitch? left)
        (ly:pitch? right)
        (equal? (aan-pitch-class left) (aan-pitch-class right))))

#(define (aan-generated-text? event)
   (and (ly:music? event)
        (eq? (ly:music-property event 'aan-generated) #t)))

#(define (aan-drop-quality arts)
   (filter (lambda (a)
             (not (and (ly:music? a)
                       (let ((filtered (filter-for-chord-names (list a))))
                         (not (null? filtered))))))
           (if (pair? arts) arts '())))

#(define (aan-replace-elements music new-elements)
   (let ((copy (ly:music-deep-copy music)))
     (set! (ly:music-property copy 'elements) new-elements)
     copy))

% Spelled chord above the bass-clef center line.  A mismatched bass adds a
% parenthesized root cue on or below that line, sharing the chord duration.
#(define (aan-spell-chord note-event extra-arts emit-symbol? root-cue?)
   (let* ((note-arts (ly:music-property note-event 'articulations))
          (raw (aan-quality-token (append (if (pair? note-arts) note-arts '())
                                          (if (pair? extra-arts) extra-arts '()))))
          (elements (aan-raise-above-center
                     (note-event-to-chord-elements note-event raw)))
          (kept (filter (lambda (a)
                          (and (ly:music? a)
                               (not (memq (ly:music-property a 'name)
                                          '(NoteEvent RestEvent SkipEvent)))))
                        (append (aan-drop-quality note-arts)
                                (aan-drop-quality extra-arts))))
          (symbol (and emit-symbol? (aan-format-chord-symbol note-event raw)))
          (symbol-ev (if symbol (aan-make-symbol-event symbol) #f))
          (marks (filter ly:music? (list symbol-ev)))
          (chord (make-music 'EventChord
                             'elements (append elements marks)
                             'articulations kept))
          (root (and (pair? elements) (ly:music-property (first elements) 'pitch)))
          (dur (and (pair? elements) (ly:music-property (first elements) 'duration))))
     (if (and root-cue? (ly:pitch? root) (ly:duration? dur))
         (make-music 'SimultaneousMusic
                     'elements (list chord (aan-make-root-cue root dur)))
         chord)))

#(define (aan-bass-pitch event)
   (and (ly:music? event)
        (is-AAN-bass? event)
        (ly:music-property event 'pitch)))

#(define (aan-root-cue? bass-pitch chord-event)
   (let ((root (ly:music-property chord-event 'pitch)))
     (and (ly:pitch? bass-pitch)
          (ly:pitch? root)
          (not (aan-same-pitch-class? bass-pitch root)))))

#(define aan-keep-bass #f)

#(define (aan-bass-only bass-here duration)
   (cond
    ((null? bass-here) (make-music 'RestEvent 'duration duration))
    ((= (length bass-here) 1) (aan-place-bass (first bass-here)))
    (else (make-music 'EventChord 'elements (map aan-place-bass bass-here)))))

#(define (engrave-note event tied-in bass-pitch)
   (cond
    ((is-AAN-chord? event)
     (cons (aan-spell-chord event '() (not tied-in) (aan-root-cue? bass-pitch event))
           (cons (aan-event-has-tie? event) bass-pitch)))
    ((is-AAN-bass? event)
     (cons (if aan-keep-bass (aan-place-bass event) (make-rest event))
           (cons #f (ly:music-property event 'pitch))))
    (else
     (cons event (cons #f bass-pitch)))))

#(define (engrave-event-chord event tied-in bass-pitch)
   (let* ((elements (ly:music-property event 'elements))
          (chord-notes (filter (lambda (e) (and (ly:music? e) (is-AAN-chord? e))) elements))
          (bass-here (filter (lambda (e) (aan-bass-pitch e)) elements))
          (local-bass (if (null? bass-here)
                          bass-pitch
                          (ly:music-property (first bass-here) 'pitch)))
          (duration (get-event-chord-duration event))
          (spelled (if (null? chord-notes)
                       #f
                       (aan-spell-chord (first chord-notes)
                                        (filter (lambda (e)
                                                  (not (and (ly:music? e)
                                                            (eq? (ly:music-property e 'name) 'NoteEvent))))
                                                elements)
                                        (not tied-in)
                                        (aan-root-cue? local-bass (first chord-notes))))))
     (cond
      ((and aan-keep-bass (not (null? bass-here)) spelled)
       (cons (make-music 'SimultaneousMusic
                         'elements (list (aan-bass-only bass-here duration) spelled))
             (cons (aan-event-has-tie? event) local-bass)))
      (spelled
       (cons spelled (cons (aan-event-has-tie? event) local-bass)))
      (else
       (cons (if aan-keep-bass
                 (aan-bass-only bass-here duration)
                 (make-music 'RestEvent 'duration duration))
             (cons #f local-bass))))))

#(define (engrave-sequential music bass-pitch)
   (let loop ((items (ly:music-property music 'elements))
              (acc '())
              (tied-in #f)
              (bass bass-pitch))
     (if (null? items)
         (cons (aan-replace-elements music (reverse acc)) (cons #f bass))
         (let ((step (engrave-walk (car items) tied-in bass)))
           (loop (cdr items)
                 (cons (car step) acc)
                 (cadr step)
                 (cddr step))))))

#(define (engrave-walk music tied-in bass-pitch)
   (cond
    ((not (ly:music? music)) (cons music (cons #f bass-pitch)))
    ((music-is-of-type? music 'note-event)
     (engrave-note music tied-in bass-pitch))
    ((music-is-of-type? music 'event-chord)
     (engrave-event-chord music tied-in bass-pitch))
    ((music-is-of-type? music 'sequential-music)
     (engrave-sequential music bass-pitch))
    ((music-is-of-type? music 'simultaneous-music)
     (cons (aan-replace-elements
            music
            (map (lambda (e) (car (engrave-walk e #f bass-pitch)))
                 (ly:music-property music 'elements)))
           (cons #f bass-pitch)))
    (else
     (let ((copy (ly:music-deep-copy music)))
       (let ((el (ly:music-property copy 'element)))
         (if (ly:music? el)
             (set! (ly:music-property copy 'element)
                   (car (engrave-walk el tied-in bass-pitch)))))
       (let ((els (ly:music-property copy 'elements)))
         (if (pair? els)
             (set! (ly:music-property copy 'elements)
                   (map (lambda (e) (car (engrave-walk e #f bass-pitch))) els))))
       (cons copy (cons #f bass-pitch))))))

% Source counterbass mark is a down-text underscore.  Engrave it as B.
% Any other bass is left unmarked: the row is never inferred.
#(define (aan-counterbass-mark? event)
   (and (ly:music? event)
        (eq? (ly:music-property event 'name) 'TextScriptEvent)
        (equal? (ly:music-property event 'direction) -1)
        (equal? (ly:music-property event 'text) "_")))

#(define (aan-rewrite-counterbass music)
   (cond
    ((not (ly:music? music)) music)
    ((aan-counterbass-mark? music)
     (let ((copy (ly:music-deep-copy music)))
       (set! (ly:music-property copy 'text) "B")
       (set! (ly:music-property copy 'aan-generated) #t)
       copy))
    (else
     (let ((copy (ly:music-deep-copy music)))
       (let ((el (ly:music-property copy 'element)))
         (if (ly:music? el)
             (set! (ly:music-property copy 'element) (aan-rewrite-counterbass el))))
       (let ((els (ly:music-property copy 'elements)))
         (if (pair? els)
             (set! (ly:music-property copy 'elements)
                   (map aan-rewrite-counterbass els))))
       (let ((arts (ly:music-property copy 'articulations)))
         (if (pair? arts)
             (set! (ly:music-property copy 'articulations)
                   (map aan-rewrite-counterbass arts))))
       copy))))

aan-engrave-bass =
#(define-music-function (music) (ly:music?)
   ;; MIDI staccato is global.  Force it off so print matches the source.
   (let ((previous make-staccato))
     (set! make-staccato #f)
     (let ((result (scheme-extract-bass music)))
       (set! make-staccato previous)
       (aan-rewrite-counterbass result))))

aan-engrave-chords =
#(define-music-function (music) (ly:music?)
   (clear-history)
   (set! aan-keep-bass #f)
   (car (engrave-walk music #f #f)))

% One bass staff: written bass notes plus spelled chords.
aan-engrave =
#(define-music-function (music) (ly:music?)
   (clear-history)
   (set! aan-keep-bass #t)
   (let ((result (car (engrave-walk music #f #f))))
     (set! aan-keep-bass #f)
     (aan-rewrite-counterbass result)))

% ---------------------------------------------------------------------------
% Text.  Chord symbols are already in the selected language and are marked
% aan-generated, so this pass leaves them alone.  The title is never translated.
% Unlisted text is left in the source language and warned once.
% ---------------------------------------------------------------------------

% Prose and instrument names only.  Dynamics and Italian navigation stay
% Italian: f, p, mf, fine, D.C., D.S., rit., a tempo, coda.
#(define aan-text-dictionary
   '(("Bayan" . "Баян")
     ("bayan" . "Баян")
     ("Accordion" . "Аккордеон")
     ("accordion" . "Аккордеон")
     ("Bass" . "Бас")
     ("Chords" . "Аккорды")
     ("Chord" . "Аккорд")
     ("Composed by Keith Smith" . "Сочинение: Кит Смит")
     ("Keith Smith" . "Кит Смит")
     ("Moderato" . "Умеренно")
     ("moderato" . "Умеренно")
     ("Copyright" . "Авторское право")))

#(define (aan-title-exception? text)
   (or (and (string? aan-song-title)
            (> (string-length aan-song-title) 0)
            (string=? text aan-song-title))
       (any (lambda (title) (and (string? title) (string=? text title)))
            aan-protected-titles)))

#(define (aan-translate-string text)
   (if (or (not (eq? aan-language 'russian))
           (not (string? text))
           (aan-title-exception? text)
           (aan-title-exception? (string-trim text)))
       text
       (let* ((trimmed (string-trim text))
              (hit (or (assoc text aan-text-dictionary)
                       (assoc trimmed aan-text-dictionary))))
         (if hit
             (cdr hit)
             (begin
               (if (not (hash-ref aan-untranslated-warned text))
                   (begin
                     (hash-set! aan-untranslated-warned text #t)
                     (log-message 'warning "aan-translate: left untranslated: ~s\n" text)))
               text)))))

#(define (aan-translate-value val)
   (cond
    ((string? val) (aan-translate-string val))
    ((pair? val) (cons (aan-translate-value (car val))
                       (aan-translate-value (cdr val))))
    (else val)))

#(define (aan-translate-event event)
   (if (or (not (ly:music? event)) (aan-generated-text? event))
       event
       (let ((name (ly:music-property event 'name)))
         (if (memq name '(TextScriptEvent LyricEvent MarkEvent RehearsalMarkEvent))
             (let ((copy (ly:music-deep-copy event)))
               (set! (ly:music-property copy 'text)
                     (aan-translate-value (ly:music-property copy 'text)))
               copy)
             event))))

#(define (aan-translate-music music)
   (if (not (eq? aan-language 'russian))
       music
       (music-map
        (lambda (event)
          (let ((translated (aan-translate-event event)))
            (let ((els (ly:music-property translated 'elements)))
              (if (pair? els)
                  (set! (ly:music-property translated 'elements)
                        (map aan-translate-event els))))
            (let ((arts (ly:music-property translated 'articulations)))
              (if (pair? arts)
                  (set! (ly:music-property translated 'articulations)
                        (map aan-translate-event arts))))
            translated))
        (ly:music-deep-copy music))))

aan-translate =
#(define-music-function (music) (ly:music?)
   (aan-translate-music music))

% Opt-in header / markup text.  Do not wrap the song title.
aan-text =
#(define-scheme-function (str) (string?)
   (markup (aan-translate-string str)))
