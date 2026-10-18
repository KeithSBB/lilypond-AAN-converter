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
% Root, minor third, diminished fifth.  (3 9) is a diminished seventh.
#(define aaan-dim-intvls (list 3 6))

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

% Semitones to diatonic steps: minor third, major third,
% diminished fifth, perfect fifth, minor seventh.
#(define aaan-interval-steps
   '((3 . 2)
     (4 . 2)
     (6 . 4)
     (7 . 4)
     (10 . 6)))

#(define (make-pitch-from-refpitch&semitone root-pitch semitone)
   (log-message 'debug "make-pitch-from-refpitch&semitone: Entered  root-pitch ~a, semitone ~a\n" root-pitch semitone)
   (if (not (ly:pitch? root-pitch))
       (begin
         (log-message 'warning "make-pitch-from-refpitch&semitone: non-pitch root ~a\n" root-pitch)
         (ly:make-pitch -1 0 0))
       (let* ((steps (assoc semitone aaan-interval-steps))
              (step (if steps (cdr steps)
                        (begin
                          (log-message 'warning "make-pitch-from-refpitch&semitone: no diatonic step for ~a semitones\n" semitone)
                          0)))
              (raw-note (+ (ly:pitch-notename root-pitch) step))
              (oct (+ (ly:pitch-octave root-pitch) (quotient raw-note 7)))
              (note (modulo raw-note 7))
              (natural (ly:make-pitch oct note 0))
              (delta (- (+ (ly:pitch-semitones root-pitch) semitone)
                        (ly:pitch-semitones natural)))
              (alteration (/ delta 2)))
         (log-message 'debug "make-pitch-from-refpitch&semitone: pitch: octave ~a, notename ~a, alteration ~a\n" oct note alteration)
         (ly:make-pitch oct note alteration))))

#(define (note-event-to-chord-elements note-event chord-name)
   (log-message 'debug "note-event-to-chord-elements: Entered\n")
   (log-message 'debug "note-event-to-chord-elements: chord-name:  ~a\n" chord-name)
   (if (equal? chord-name "unknown")
       (let ((found (search-history note-event)))
         (set! chord-name (if (null? found) "M" found))
         (log-message 'debug "note-event-to-chord-elements: quality ~a\n" chord-name)))
   (let ((root-pitch (aaan-safe-pitch note-event)))
     (if (not root-pitch)
         (begin
           (log-message 'warning "note-event-to-chord-elements: no pitch; returning empty\n")
           '())
         (let* ((root (ly:make-pitch -1
                                     (ly:pitch-notename root-pitch)
                                     (ly:pitch-alteration root-pitch)))
                (dur (ly:music-property note-event 'duration))
                (semitone-list (hash-ref aaan-chord-intvl-table chord-name))
                (pitches (aaan-invert-onto-center
                          (cons root
                                (map (lambda (semitone)
                                       (make-pitch-from-refpitch&semitone root semitone))
                                     (if (list? semitone-list) semitone-list '()))))))
           (log-message 'debug "note-event-to-chord-elements: pitches:\n~a\n" pitches)
           (save-update-history note-event chord-name)
           (map (lambda (pitch)
                  (make-music 'NoteEvent
                              'pitch pitch
                              'duration dur))
                pitches)))))

% Bass-clef middle line is absolute d (D3).  Noteheads below that line are
% bass buttons; noteheads on or above it are chord buttons.  des is on the
% line, so it is a chord; cis is in the space below, so it is a bass note.
#(define aaan-bass-center (ly:make-pitch -1 1 0))

#(define (aaan-diatonic-steps pitch)
   (+ (* 7 (ly:pitch-octave pitch))
      (ly:pitch-notename pitch)))

#(define (aaan-on-or-above-center? pitch)
   (>= (aaan-diatonic-steps pitch)
       (aaan-diatonic-steps aaan-bass-center)))

% Close voicing.  Spelled tones are built from octave -1, so a tone below
% the middle line is at most one octave below it.  One pass is enough.
% C major is <e g c'>.
#(define (aaan-invert-onto-center pitches)
   (call-with-values
    (lambda ()
      (partition aaan-on-or-above-center? pitches))
    (lambda (high low)
      (sort (append high
                    (map (lambda (p)
                           (ly:pitch-transpose p (ly:make-pitch 1 0 0)))
                         low))
            ly:pitch<?))))

#(define (is-AAAN-chord? note-event)
   (and (ly:music? note-event)
        (begin
          (log-message 'debug "is-AAAN-chord?: Entered ~a\n"
                       (ly:music-property note-event 'name))
          (and (equal? (ly:music-property note-event 'name) 'NoteEvent)
               (let ((pitch (aaan-safe-pitch note-event)))
                 (and pitch (aaan-on-or-above-center? pitch)))))))

#(define (is-AAAN-bass? note-event)
   (and (ly:music? note-event)
        (begin
          (log-message 'debug "is-AAAN-bass?: Entered \n")
          (and (equal? (ly:music-property note-event 'name) 'NoteEvent)
               (let ((pitch (aaan-safe-pitch note-event)))
                 (and pitch (not (aaan-on-or-above-center? pitch))))))))

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
