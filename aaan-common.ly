%
% Copyright (C) 2025-2026
% Author: Keith Smith <keith@santabayanian.com>
% aaan-common.ly
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
% Shared American Accordion Notation (AAAN / AAA) tables and pure helpers.
% Included by accbasschord.ly (MIDI) and aaan-engrave.ly (print).
% No staccato policy lives here: MIDI may add it, engraving must not.
% Pitch accessors are guarded so a missing pitch logs a warning instead of crashing.

\version "2.24.4"

#(ly:set-option 'compile-scheme-code)

#(use-modules (ice-9 format))
#(use-modules (srfi srfi-1))
#(use-modules (srfi srfi-13))

#(define debug-level 'info)  % Options: 'none, 'debug, 'info, 'warning

#(define (log-message level format-string . args)
   (let ((levels '((debug . 1) (info . 2) (warning . 3) (error . 4))))
     (let* ((msg-level (or (assq level levels) (cons level 0)))
            (level-number (cdr msg-level))
            (level-name (symbol->string level)))
       (let ((debug-threshold (or (assq debug-level levels) (cons debug-level 0))))
         (if (and (not (eq? debug-level 'none))
                  (>= level-number (cdr debug-threshold)))
             (let ((formatted-message (apply format #f format-string args)))
               (display (string-append "[" level-name "] " formatted-message))
               (newline)))))))

% Safe pitch helpers — log and return a sentinel instead of crashing
#(define (aaan-safe-pitch music)
   (let ((p (ly:music-property music 'pitch)))
     (if (ly:pitch? p)
         p
         (begin
           (log-message 'warning
                        "aaan-safe-pitch: expected Pitch, got ~a on ~a\n  duration=~a  arts=~a  elements=~a\n"
                        p
                        music
                        (ly:music-property music 'duration)
                        (ly:music-property music 'articulations)
                        (ly:music-property music 'elements))
           #f))))

% ---------------------------------------------------------------------------
% Stradella qualities.  Keys are the AAAN text tokens accepted on a chord note.
% 7sus2 is not a Stradella row.  A quality token of exactly "7sus2" is an error.
% Parenthetical annotations such as ^"(Re7sus2)" are plain text and are left alone.
% ---------------------------------------------------------------------------

#(define aaan-chord-intvl-table (make-hash-table))
#(define aaan-major-intvls (list 4 7))
#(define aaan-minor-intvls (list 3 7))
#(define aaan-sevth-intvls (list 4 10))
#(define aaan-dim-intvls (list 3 9))

#(hash-set! aaan-chord-intvl-table "M" aaan-major-intvls)
#(hash-set! aaan-chord-intvl-table "maj" aaan-major-intvls)
#(hash-set! aaan-chord-intvl-table "m" aaan-minor-intvls)
#(hash-set! aaan-chord-intvl-table "min" aaan-minor-intvls)
#(hash-set! aaan-chord-intvl-table "7" aaan-sevth-intvls)
#(hash-set! aaan-chord-intvl-table "dim" aaan-dim-intvls)
#(hash-set! aaan-chord-intvl-table "d" aaan-dim-intvls)
#(hash-set! aaan-chord-intvl-table "o" aaan-dim-intvls)

#(define chord-intvl-table aaan-chord-intvl-table)

#(define chord-keys
   (map car (hash-map->list (lambda (key value) (cons key value)) aaan-chord-intvl-table)))

% chord-history: ((pitch-notename pitch-alteration) . chord-name)
% A omitted quality reuses the last quality for that pitch class.

#(define chord-history '())

#(define (search-history note-event)
   (log-message 'debug "search-history:  entered\n")
   (let ((pitch (aaan-safe-pitch note-event)))
     (if (not pitch)
         '()
         (let ((note-alt (list (ly:pitch-notename pitch) (ly:pitch-alteration pitch))))
           (let ((item (assoc note-alt chord-history)))
             (if item
                 (begin
                   (log-message 'debug "search-history: found ~a for (note-name , alteration) ~a\n" (cdr item) note-alt)
                   (cdr item))
                 '()))))))

#(define (save-update-history note-event chord-name)
   (log-message 'debug "save-update-history:  entered\n")
   (let ((pitch (aaan-safe-pitch note-event)))
     (if pitch
         (let ((note-alt (list (ly:pitch-notename pitch) (ly:pitch-alteration pitch))))
           (let ((item (assoc note-alt chord-history)))
             (if item
                 (begin
                   (log-message 'debug "save-update-history: Found (note-name , alteration) ~a updating to ~a\n" note-alt chord-name)
                   (set-cdr! item chord-name))
                 (begin
                   (set! chord-history (cons (cons note-alt chord-name) chord-history))
                   (log-message 'debug "save-update-history: added to Chord-history: ~a\n" chord-history))))))))

#(define (clear-history)
   (log-message 'debug "Clear-history:  Entered\n")
   (set! chord-history '()))

#(define (get-event-chord-duration eventchord)
   (let* ((elements (filter ly:music? (ly:music-property eventchord 'elements)))
          (durations (filter ly:duration?
                             (map (lambda (el) (ly:music-property el 'duration))
                                  elements))))
     (if (null? durations)
         (ly:make-duration 2 0)
         (car durations))))

% Text scripts written above the note (^"M", ^"min", ...) are quality tokens.
#(define (filter-for-chord-names articulations)
   (log-message 'debug "filter-for-chord-names: Entered \n")
   (if (null? articulations)
       (begin
         (log-message 'debug "filter-for-chord-names: articulations are empty\n")
         articulations)
       (begin
         (log-message 'debug "filter-for-chord-names: articulations are not empty\n")
         (filter (lambda (articulation)
                   (let ((type (ly:music-property articulation 'name))
                         (direction (ly:music-property articulation 'direction))
                         (text (ly:music-property articulation 'text)))
                     (and (equal? type 'TextScriptEvent)
                          (equal? direction 1)
                          (string? text)
                          (any (lambda (x) (string=? text x)) chord-keys))))
                 articulations))))

#(define (aaan-quality-token articulations)
   ;; 7sus2 is not a Stradella row.  Reject it as a quality token.
   ;; Parenthetical text such as "(Re7sus2)" is not equal to "7sus2" and is left alone.
   (for-each
    (lambda (a)
      (if (and (ly:music? a)
               (eq? (ly:music-property a 'name) 'TextScriptEvent)
               (equal? (ly:music-property a 'direction) 1)
               (equal? (ly:music-property a 'text) "7sus2"))
          (error "aaan-quality-token: \"7sus2\" is not a Stradella row. Use M, m, 7, or dim, and put any 7sus2 annotation in parentheses.")))
    (if (pair? articulations) articulations '()))
   (let ((filtered (filter-for-chord-names articulations)))
     (if (null? filtered)
         "unknown"
         (ly:music-property (first filtered) 'text))))

% Pitch (octave, notename, alteration) from a root and a semitone offset.
% Spelling follows the original converter: the next letter at or above the
% target semitone, so A7 is written <a des g>, not <a cis g>.
#(define (make-pitch-from-refpitch&semitone root-pitch semitone)
   (log-message 'debug "make-pitch-from-refpitch&semitone: Entered  root-pitch ~a, semitone ~a\n" root-pitch semitone)
   (if (not (ly:pitch? root-pitch))
       (begin
         (log-message 'warning "make-pitch-from-refpitch&semitone: non-pitch root ~a\n" root-pitch)
         (ly:make-pitch -1 0 0))
       (let* ((total-semitones (+ (ly:pitch-semitones root-pitch) semitone))
              (target-semitone (modulo total-semitones 12))
              (notename-semitone-list '(0 2 4 5 7 9 11)) ; C:0, D:2, E:4, F:5, G:7, A:9, B:11
              (closest-notename (let loop ((note-semitone notename-semitone-list) (index 0))
                                  (if (or (null? note-semitone) (>= (car note-semitone) target-semitone))
                                      index
                                      (loop (cdr note-semitone) (+ index 1)))))
              (alteration (/ (- target-semitone (list-ref notename-semitone-list closest-notename)) 2)))
         (log-message 'debug "make-pitch-from-refpitch&semitone: pitch: octave -1, notename ~a, alteration ~a\n" closest-notename alteration)
         (ly:make-pitch -1 closest-notename alteration))))

#(define (note-event-to-chord-elements note-event chord-name)
   (log-message 'debug "note-event-to-chord-elements: Entered\n")
   (log-message 'debug "note-event-to-chord-elements: chord-name:  ~a\n" chord-name)
   (if (equal? chord-name "unknown")
       (begin
         (set! chord-name (search-history note-event))
         (if (null? chord-name)
             (error "note-event-to-chord-elements: Undefined chord note and no prior useage")
             (log-message 'debug "note-event-to-chord-elements: Chord was not defined, but its history was found: ~a\n" chord-name))))
   (let ((root-pitch (aaan-safe-pitch note-event)))
     (if (not root-pitch)
         (begin
           (log-message 'warning "note-event-to-chord-elements: no pitch; returning empty\n")
           '())
         (let* ((root-notename (ly:pitch-notename root-pitch))
                (root-alteration (ly:pitch-alteration root-pitch))
                (chord-root-pitch (ly:make-pitch -1 root-notename root-alteration))
                (dur (ly:music-property note-event 'duration))
                (semitone-list (hash-ref aaan-chord-intvl-table chord-name))
                (pitches (cons chord-root-pitch
                               (map (lambda (semitone)
                                      (make-pitch-from-refpitch&semitone root-pitch semitone))
                                    (if (list? semitone-list) semitone-list '())))))
           (log-message 'debug "note-event-to-chord-elements: pitches:\n~a\n" pitches)
           (save-update-history note-event chord-name)
           (map (lambda (pitch)
                  (make-music 'NoteEvent
                              'pitch pitch
                              'duration dur))
                pitches)))))

% Middle line of the bass staff is D3.  C#3 and lower are bass-row notes.
% Db3 (same semitone count as C#3, written as a flat) stays on the chord side
% of the original test, so the alteration check is preserved.
% AAAN writes the bass in the comma octave and the chord row in the
% octave above.  Middle C is the split: a note below it is a bass button,
% a note at or above it is a chord button.  The old -11 threshold sat at
% C-sharp below the staff, so a, e, g, and d, were all spelled as chords
% and the bass attack vanished.
#(define aaan-bass-split 0)

#(define (is-AAAN-chord? note-event)
   (and (ly:music? note-event)
        (begin
          (log-message 'debug "is-AAAN-chord?: Entered ~a\n"
                       (ly:music-property note-event 'name))
          (and (equal? (ly:music-property note-event 'name) 'NoteEvent)
               (let ((pitch (aaan-safe-pitch note-event)))
                 (and pitch (>= (ly:pitch-semitones pitch) aaan-bass-split)))))))

#(define (is-AAAN-bass? note-event)
   (and (ly:music? note-event)
        (begin
          (log-message 'debug "is-AAAN-bass?: Entered \n")
          (and (equal? (ly:music-property note-event 'name) 'NoteEvent)
               (let ((pitch (aaan-safe-pitch note-event)))
                 (and pitch (< (ly:pitch-semitones pitch) aaan-bass-split)))))))

#(define (make-rest note-event)
   (log-message 'debug "make-rest: Make Rest\n")
   (let ((duration (ly:music-property note-event 'duration)))
     (make-music 'RestEvent
                 'duration (if (ly:duration? duration)
                               duration
                               (ly:make-duration 2 0)))))

#(define (aaan-event-has-tie? music)
   (let* ((arts (ly:music-property music 'articulations))
          (elems (filter ly:music? (ly:music-property music 'elements)))
          (all (append (if (pair? arts) arts '()) elems)))
     (any (lambda (a)
            (and (ly:music? a)
                 (eq? (ly:music-property a 'name) 'TieEvent)))
          all)))
