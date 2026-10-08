%
% Copyright (C) 2025-2026
% Author: Keith Smith <keith@santabayanian.com>
% aan-common.ly
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
% Shared American Accordion Notation (AAN / AAA) tables and pure helpers.
% Included by accbasschord.ly (MIDI) and aan-engrave.ly (print).
% No staccato policy lives here: MIDI may add it, engraving must not.

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

% ---------------------------------------------------------------------------
% Stradella qualities.  Keys are the AAN text tokens accepted on a chord note.
% ---------------------------------------------------------------------------

#(define aan-chord-intvl-table (make-hash-table))
#(define aan-major-intvls (list 4 7))
#(define aan-minor-intvls (list 3 7))
#(define aan-sevth-intvls (list 4 10))
#(define aan-dim-intvls (list 3 9))

#(hash-set! aan-chord-intvl-table "M" aan-major-intvls)
#(hash-set! aan-chord-intvl-table "maj" aan-major-intvls)
#(hash-set! aan-chord-intvl-table "m" aan-minor-intvls)
#(hash-set! aan-chord-intvl-table "min" aan-minor-intvls)
#(hash-set! aan-chord-intvl-table "7" aan-sevth-intvls)
#(hash-set! aan-chord-intvl-table "dim" aan-dim-intvls)
#(hash-set! aan-chord-intvl-table "d" aan-dim-intvls)
#(hash-set! aan-chord-intvl-table "o" aan-dim-intvls)
#(hash-set! aan-chord-intvl-table "7sus2" (list 2 7 10))

#(define chord-intvl-table aan-chord-intvl-table)

#(define chord-keys
   (map car (hash-map->list (lambda (key value) (cons key value)) aan-chord-intvl-table)))

% chord-history: ((pitch-notename pitch-alteration) . chord-name)
% A omitted quality reuses the last quality for that pitch class.

#(define chord-history '())

#(define (search-history note-event)
   (log-message 'debug "search-history:  entered\n")
   (let* ((pitch (ly:music-property note-event 'pitch))
          (note-alt (list (ly:pitch-notename pitch) (ly:pitch-alteration pitch))))
     (let ((item (assoc note-alt chord-history)))
       (if item
           (begin
             (log-message 'debug "search-history: found ~a for (note-name , alteration) ~a\n" (cdr item) note-alt)
             (cdr item))
           '()))))

#(define (save-update-history note-event chord-name)
   (log-message 'debug "save-update-history:  entered\n")
   (let* ((pitch (ly:music-property note-event 'pitch))
          (note-alt (list (ly:pitch-notename pitch) (ly:pitch-alteration pitch))))
     (let ((item (assoc note-alt chord-history)))
       (if item
           (begin
             (log-message 'debug "save-update-history: Found (note-name , alteration) ~a updating to ~a\n" note-alt chord-name)
             (set-cdr! item chord-name))
           (begin
             (set! chord-history (cons (cons note-alt chord-name) chord-history))
             (log-message 'debug "save-update-history: added to Chord-history: ~a\n" chord-history))))))

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

#(define (aan-quality-token articulations)
   (let ((filtered (filter-for-chord-names articulations)))
     (if (null? filtered)
         "unknown"
         (ly:music-property (first filtered) 'text))))

% Pitch (octave, notename, alteration) from a root and a semitone offset.
% Spelling follows the original converter: the next letter at or above the
% target semitone, so A7 is written <a des g>, not <a cis g>.
#(define (make-pitch-from-refpitch&semitone root-pitch semitone)
   (log-message 'debug "make-pitch-from-refpitch&semitone: Entered  root-pitch ~a, semitone ~a\n" root-pitch semitone)
   (let* ((total-semitones (+ (ly:pitch-semitones root-pitch) semitone))
          (target-semitone (modulo total-semitones 12))
          (notename-semitone-list '(0 2 4 5 7 9 11)) ; C:0, D:2, E:4, F:5, G:7, A:9, B:11
          (closest-notename (let loop ((note-semitone notename-semitone-list) (index 0))
                              (if (or (null? note-semitone) (>= (car note-semitone) target-semitone))
                                  index
                                  (loop (cdr note-semitone) (+ index 1)))))
          (alteration (/ (- target-semitone (list-ref notename-semitone-list closest-notename)) 2)))
     (log-message 'debug "make-pitch-from-refpitch&semitone: pitch: octave -1, notename ~a, alteration ~a\n" closest-notename alteration)
     (ly:make-pitch -1 closest-notename alteration)))

#(define (note-event-to-chord-elements note-event chord-name)
   (log-message 'debug "note-event-to-chord-elements: Entered\n")
   (log-message 'debug "note-event-to-chord-elements: chord-name:  ~a\n" chord-name)
   (if (equal? chord-name "unknown")
       (begin
         (set! chord-name (search-history note-event))
         (if (null? chord-name)
             (error "note-event-to-chord-elements: Undefined chord note and no prior useage")
             (log-message 'debug "note-event-to-chord-elements: Chord was not defined, but its history was found: ~a\n" chord-name))))
   (let* ((root-pitch (ly:music-property note-event 'pitch))
          (root-notename (ly:pitch-notename root-pitch))
          (root-alteration (ly:pitch-alteration root-pitch))
          (chord-root-pitch (ly:make-pitch -1 root-notename root-alteration))
          (dur (ly:music-property note-event 'duration))
          (semitone-list (hash-ref aan-chord-intvl-table chord-name))
          (pitches (cons chord-root-pitch
                         (map (lambda (semitone)
                                (make-pitch-from-refpitch&semitone root-pitch semitone))
                              semitone-list))))
     (log-message 'debug "note-event-to-chord-elements: pitches:\n~a\n" pitches)
     (save-update-history note-event chord-name)
     (map (lambda (pitch)
            (make-music 'NoteEvent
                        'pitch pitch
                        'duration dur))
          pitches)))

% Middle line of the bass staff is D3.  C#3 and lower are bass-row notes.
% Db3 (same semitone count as C#3, written as a flat) stays on the chord side
% of the original test, so the alteration check is preserved.
#(define (is-AAN-chord? note-event)
   (log-message 'debug "is-AAN-chord?: Entered ~a\n" (ly:music-property note-event 'name))
   (if (equal? (ly:music-property note-event 'name) 'NoteEvent)
       (let* ((pitch (ly:music-property note-event 'pitch))
              (note-alteration (ly:pitch-alteration pitch))
              (semitones (ly:pitch-semitones pitch)))
         (or (> semitones -11) (and (= semitones -11) (= note-alteration (/ -1 2)))))
       #f))

#(define (is-AAN-bass? note-event)
   (log-message 'debug "is-AAN-bass?: Entered \n")
   (if (equal? (ly:music-property note-event 'name) 'NoteEvent)
       (let* ((pitch (ly:music-property note-event 'pitch))
              (note-alteration (ly:pitch-alteration pitch))
              (semitones (ly:pitch-semitones pitch)))
         (not (or (> semitones -11) (and (= semitones -11) (= note-alteration (/ -1 2))))))
       #f))

#(define (make-rest note-event)
   (log-message 'debug "make-rest: Make Rest\n")
   (let ((duration (ly:music-property note-event 'duration)))
     (make-music 'RestEvent
                 'duration (if (ly:duration? duration)
                               duration
                               (ly:make-duration 2 0)))))

#(define (aan-event-has-tie? music)
   (let* ((arts (ly:music-property music 'articulations))
          (elems (filter ly:music? (ly:music-property music 'elements)))
          (all (append (if (pair? arts) arts '()) elems)))
     (any (lambda (a)
            (and (ly:music? a)
                 (eq? (ly:music-property a 'name) 'TieEvent)))
          all)))
