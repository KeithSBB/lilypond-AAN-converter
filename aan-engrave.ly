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
% Russian mode rewrites chord symbols and text passed through \aan-text or
% \aan-translate.  The song title is never translated: leave \header title
% as a literal, and set aan-song-title to the same string so a copy of the
% title inside the music is also left alone.

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
     (string-append (aan-format-pitch pitch aan-language)
                    (aan-quality-suffix canonical aan-language))))

#(define (aan-make-symbol-event text)
   (make-music 'TextScriptEvent
               'text text
               'direction 1
               'aan-generated #t))

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

% Spelled chord, source articulations except the quality token, optional symbol.
#(define (aan-spell-chord note-event extra-arts emit-symbol?)
   (let* ((note-arts (ly:music-property note-event 'articulations))
          (raw (aan-quality-token (append (if (pair? note-arts) note-arts '())
                                          (if (pair? extra-arts) extra-arts '()))))
          (elements (note-event-to-chord-elements note-event raw))
          (kept (append (aan-drop-quality note-arts) (aan-drop-quality extra-arts)))
          (symbol (and emit-symbol? (aan-format-chord-symbol note-event raw)))
          (symbol-ev (if symbol (aan-make-symbol-event symbol) #f))
          (all (if symbol-ev (append elements (list symbol-ev) kept) (append elements kept))))
     (make-music 'EventChord 'elements all)))

#(define (engrave-note event tied-in)
   (if (is-AAN-chord? event)
       (cons (aan-spell-chord event '() (not tied-in))
             (aan-event-has-tie? event))
       (cons event #f)))

#(define (engrave-event-chord event tied-in)
   (let* ((elements (ly:music-property event 'elements))
          (chord-notes (filter (lambda (e) (and (ly:music? e) (is-AAN-chord? e))) elements))
          (duration (get-event-chord-duration event)))
     (if (null? chord-notes)
         (cons (make-music 'RestEvent 'duration duration) #f)
         (cons (aan-spell-chord (first chord-notes)
                                (filter (lambda (e)
                                          (not (and (ly:music? e)
                                                    (eq? (ly:music-property e 'name) 'NoteEvent))))
                                        elements)
                                (not tied-in))
               (aan-event-has-tie? event)))))

#(define (engrave-sequential music)
   (let loop ((items (ly:music-property music 'elements))
              (acc '())
              (tied-in #f))
     (if (null? items)
         (cons (aan-replace-elements music (reverse acc)) #f)
         (let ((step (engrave-walk (car items) tied-in)))
           (loop (cdr items) (cons (car step) acc) (cdr step))))))

#(define (engrave-walk music tied-in)
   (cond
    ((not (ly:music? music)) (cons music #f))
    ((music-is-of-type? music 'note-event)
     (engrave-note music tied-in))
    ((music-is-of-type? music 'event-chord)
     (engrave-event-chord music tied-in))
    ((music-is-of-type? music 'sequential-music)
     (engrave-sequential music))
    ((music-is-of-type? music 'simultaneous-music)
     (cons (aan-replace-elements
            music
            (map (lambda (e) (car (engrave-walk e #f)))
                 (ly:music-property music 'elements)))
           #f))
    (else
     (let ((copy (ly:music-deep-copy music)))
       (let ((el (ly:music-property copy 'element)))
         (if (ly:music? el)
             (set! (ly:music-property copy 'element)
                   (car (engrave-walk el tied-in)))))
       (let ((els (ly:music-property copy 'elements)))
         (if (pair? els)
             (set! (ly:music-property copy 'elements)
                   (map (lambda (e) (car (engrave-walk e #f))) els))))
       (cons copy #f)))))

aan-engrave-bass =
#(define-music-function (music) (ly:music?)
   ;; MIDI staccato is global.  Force it off so print matches the source.
   (let ((previous make-staccato))
     (set! make-staccato #f)
     (let ((result (scheme-extract-bass music)))
       (set! make-staccato previous)
       result)))

aan-engrave-chords =
#(define-music-function (music) (ly:music?)
   (clear-history)
   (car (engrave-walk music #f)))

% ---------------------------------------------------------------------------
% Text.  Chord symbols are already in the selected language and are marked
% aan-generated, so this pass leaves them alone.  The title is never translated.
% Unlisted text is left in the source language and warned once.
% ---------------------------------------------------------------------------

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
     ("fine" . "конец")
     ("Fine" . "Конец")
     ("   fine" . "конец")
     ("pour Fine" . "к концу")
     ("D.C. al fine" . "С начала до конца")
     ("D.C al fine" . "С начала до конца")
     ("D.C." . "С начала")
     ("D.C" . "С начала")
     ("D.S. al fine" . "С знака до конца")
     ("D.S. al Coda" . "С знака до коды")
     ("D.S." . "С знака")
     ("al fine" . "до конца")
     ("Coda" . "Кода")
     ("coda" . "кода")
     ("Segno" . "Сеньо")
     ("rit." . "замедляя")
     ("ritard." . "замедляя")
     ("accel." . "ускоряя")
     ("a tempo" . "в темпе")
     ("cresc." . "усиливая")
     ("dim." . "затихая")
     ("poco" . "немного")
     ("molto" . "очень")
     ("espress." . "выразительно")
     ("dolce" . "нежно")
     ("cantabile" . "певуче")
     ("legato" . "легато")
     ("staccato" . "стаккато")
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
