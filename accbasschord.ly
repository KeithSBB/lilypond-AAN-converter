%
% Copyright (C) 2025 
% Author: Keith Smith <keith@santabayanian.com>
% accbasschord.ly
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
% This script has two main scheme functions for converting from American 
% Accordion Notation to bass note voice and fully spelled out accordion
% chords;
%        \aan-extract-bass {... AAN music ...}  "Places rests at chords"
%        \aan-extract-chords (... AAN music ...} "spells out chords and
%						 places rests at bass"
% see section on notation: https://en.wikipedia.org/wiki/Stradella_bass_system  


\version "2.24.4"
 #(ly:set-option 'compile-scheme-code)
#(use-modules (ice-9 format))

% Define the hash table at the top level
#(define chord-intvl-table (make-hash-table))
#(define major-intvls (list 4 7))
#(define minor-intvls (list 3 7))
#(define sevth-intvls (list 4 10))
#(define dim-intvls (list 3 9))

% Populate the hash table with some key-value pairs
#(hash-set! chord-intvl-table "M" major-intvls)
#(hash-set! chord-intvl-table "maj" major-intvls)
#(hash-set! chord-intvl-table "m" minor-intvls)
#(hash-set! chord-intvl-table "min" minor-intvls)
#(hash-set! chord-intvl-table "7" sevth-intvls)
#(hash-set! chord-intvl-table "dim" dim-intvls)
#(hash-set! chord-intvl-table "d" dim-intvls)
#(hash-set! chord-intvl-table "o" dim-intvls)

#(define chord-keys (map car (hash-map->list (lambda (key value) (cons key value)) chord-intvl-table)))
#(format #f "Chord keys: ~a\n" chord-keys)

#(format #f "semitones: ~a\n" (hash-ref chord-intvl-table "M"))

% chord-history code
#(define chord-history '())

#(define (search-history note-event)
   (format #t "search-history:  entered\n")
   (let* ((pitch (ly:music-property note-event 'pitch))
          (note-alt (list (ly:pitch-notename pitch) (ly:pitch-alteration pitch))))
     (let ((item (assoc note-alt chord-history)))
       (if item
           (cdr item)
           '()))))

#(define (save-update-history note-event chord-name)
   (format #t "save-update-history:  entered\n")
   (let* ((pitch (ly:music-property note-event 'pitch))
          (note-alt (list (ly:pitch-notename pitch) (ly:pitch-alteration pitch))))
     (let ((item (assoc note-alt chord-history)))
       (if item
           (set-cdr! item chord-name)
           (if (< (length chord-history) 12)
               (begin (set! chord-history (cons (cons note-alt chord-name) chord-history))
                 (format #t "save-update-history: Chord-history: ~a\n" chord-history))
               (error "save-update-history: Maximum number of items reached"))))))

#(define (clear-history)
   (format #t "Clear-history:  Entered\n")
   (set! chord-history '()))

% End of chord-history code

#(define (get-event-chord-duration eventchord)
   (let* ((elements (ly:music-property eventchord 'elements))
          (durations (map (lambda (el) (ly:music-property el 'duration)) elements)))
     (if (null? durations)
         '()
         (car durations))))

%Gets all articulations that have ^\"..\" text from note-event or chord-event (should contain chord info)
#(define (filter-for-chord-names articulations)
   (format #t "filter-for-chord-names: Entered \n")
   (if (null? articulations)
       (begin (format #t "filter-for-chord-names: articulations are empty\n") articulations)
       (begin (format #t "filter-for-chord-names: articulations are not empty\n")
         (filter (lambda (articulation)
                   (let ((type (ly:music-property articulation 'name))
                         (direction (ly:music-property articulation 'direction))
                         (text (ly:music-property articulation 'text)))
                     (and (equal? type 'TextScriptEvent)
                          (equal? direction 1)
                          (any (lambda (x) (string=? text x)) chord-keys))))
                 articulations))))

% calculate the pitch(octave notename alteration) from root-pitch and semitone
#(define (semitone-interval root-pitch semitone)
   (format #t "semitone-interval: Entered  root-pitch ~a, semitone ~a\n" root-pitch semitone)
   (let* ((total-semitones (+ (ly:pitch-semitones root-pitch) semitone))
          (new-octave (+ (ly:pitch-octave root-pitch) (quotient total-semitones 12)))
          (target-semitone (modulo total-semitones 12))
          (notename-semitone-list '(0 2 4 5 7 9 11)) ; C:0, D:2, E:4, F:5, G:7, A:9, B:11
          (root-notename (ly:pitch-notename root-pitch))      
          (closest-notename  (let loop ((note-semitone notename-semitone-list) (index 0))
                               (if (or (null? note-semitone) (>= (car note-semitone) target-semitone))
                                   index
                                   (loop (cdr note-semitone) (+ index 1)))))
          (alteration (/ (- target-semitone (list-ref notename-semitone-list closest-notename)) 2)))
     (begin 
      (format #t "semitone-interval: root-notename ~a, target-semitone ~a\n"  root-notename target-semitone)
      (format #t "semitone-interval: pitch notename ~a, alteration ~a\n"  closest-notename alteration)
       (ly:make-pitch -1 closest-notename alteration))))

% lower level function that creates a list of notes based on a note-event and semitone-list
#(define (note-event-to-chord-elements note-event chord-name)
   (format #t "note-event-to-chord-elements: Entered\n")
   (format #t "note-event-to-chord-elements: chord-name:  ~a\n" chord-name)
   (if (equal? chord-name "unknown")
       (begin (set! chord-name (search-history note-event))
         (if (null? chord-name) 
             (error "note-event-to-chord-elements: Undefined chord note and no prior useage")
         (format #t "note-event-to-chord-elements: Chord was not defined, but its history was found: ~a\n" chord-name)
         )))
       
   (let* ((root-pitch (ly:music-property note-event 'pitch))
          (dur (ly:music-property note-event 'duration))
          (semitone-list (hash-ref chord-intvl-table chord-name))
          (pitches (cons root-pitch
                         (map (lambda (semitone)
                                (semitone-interval root-pitch semitone))
                              semitone-list))))
     (save-update-history note-event chord-name)
     (map (lambda (pitch)
            (make-music 'NoteEvent
                        'pitch pitch
                        'duration dur))
          pitches)))

% Create a chord from a note-event with chord info in articulation
#(define (create-chord-from-note event)
   (format #t "create-chord-from-note: Entered \n")
   (let* ((note-articu (ly:music-property event 'articulations))
          (duration(ly:music-property event 'duration)))
     (begin
      (format #f "create-chord-from-note:  articulation: ~a\n" note-articu)
         (let ((filtered (filter-for-chord-names note-articu)))
            (format #f "create-chord-from-note:  filtered:\n ~a\n\n" filtered)
                (begin
                 (let* ((chord-name (if (null? filtered) "unknown" (ly:music-property (first filtered) 'text)))
                        (elements (note-event-to-chord-elements event chord-name)))
                   (format #t "create-chord-from-note: Creating chord '~a' from note \n" chord-name)
                   (make-music
                    'EventChord
                    'elements elements
                    'duration duration)))))))

% replace an AAN chord-event (bass note & chord note with articulation on chord-event with chord)
#(define (create-chord-from-chord eventchord)
   (format #t "create-chord-from-chord: Entered \n")
   (let* ((chord-elements (ly:music-property eventchord 'elements))
          (duration (get-event-chord-duration eventchord))
          )
     (format #t "create-chord-from-chord: chord-elements: ~a\n" chord-elements)
     (let ((filtered (filter-for-chord-names chord-elements)))
       (format #t "create-chord-from-chord: filtered: ~a\n" filtered)
           (let* ((chord-name (if (null? filtered) "unknown" (ly:music-property (first filtered) 'text)))
                  (note-elements (filter (lambda (event)
                                           (and (ly:music? event)  (is-chord? event )))
                                         chord-elements))
                  (new-elements (apply append 
                                       (map (lambda (note-event)
                                              (note-event-to-chord-elements note-event  chord-name))
                                            note-elements))))
             (format #t "create-chord-from-chord: chordName: ~a\n" chord-name)
             (format #t "create-chord-from-chord: note-elements: ~a\n" note-elements)
             (format #t "create-chord-from-chord: new-elements: ~a\n" new-elements)
             (format #t "create-chord-from-chord: duration:  ~a\n" duration)
             (if (equal? (length new-elements) 0)
                 (make-music 'RestEvent 'duration duration)
                 (make-music 'EventChord 
                             'elements new-elements
                             'duration duration))))))

% Test if note is at or above the middle bass clef staff line (returns true or false)
#(define (is-chord? note-event)
   (format #t "is-chord?: Entered ~a\n" (ly:music-property note-event 'name))
   (if (equal? (ly:music-property note-event 'name) 'NoteEvent)
       (let* ((pitch (ly:music-property note-event 'pitch))
              (note-alteration (ly:pitch-alteration pitch))
              (semitones (ly:pitch-semitones pitch)))
         (or (> semitones -11) (and (= semitones -11) (= note-alteration (/ -1 2)))))
       #f))

#(define (is-bass? note-event)
   (format #t "is-chord?: Entered\n")
   (if (equal? (ly:music-property note-event 'name) 'NoteEvent)
       (let* ((pitch (ly:music-property note-event 'pitch))
              (note-alteration (ly:pitch-alteration pitch))
              (semitones (ly:pitch-semitones pitch)))
         (not (or (> semitones -11) (and (= semitones -11) (= note-alteration (/ -1 2))))))
       #f))


% Creates a rest with the same duration as note-event
#(define (make-rest note-event)
   (format #t "make-rest: Make Rest\n")
   (let ((duration (ly:music-property note-event 'duration)))
     (make-music 'RestEvent 'duration duration)))

% prevent descending into these music containers since other code does that
#(define (should-skip-desend_into? event)
   (format #t "should-skip-desend_into?: Entered ~a\n" (ly:music-property event 'name))
   (not  (or (equal? (ly:music-property event 'name) 'EventChord)
             (equal? (ly:music-property event 'name) 'NoteEvent))))
   

% This is the main scheme function that processes bass clef music for AAN chords
#(define (scheme-extract-chords music)
   (format #t "scheme-extract-chords: Entered\n")
   (let ((new-music (ly:music-deep-copy music)))
     (music-selective-map should-skip-desend_into?
      (lambda (event)
        (format #t "\nscheme-extract-chords: TOP LEVEL MUSIC MAP - ELEMENT:\n=====================================================\n ~a\n\n" event)
        (cond ((and (ly:music? event) (equal? (ly:music-property event 'name) 'NoteEvent))
               (if (is-chord? event)
                   (create-chord-from-note event)
                   (make-rest event)))
              ((and (ly:music? event) (equal? (ly:music-property event 'name) 'EventChord))
               (create-chord-from-chord event))
              (else event)))
      new-music)))

% Defines the main call to extract chords used in lilypond music files
aan-extract-chords = #(define-music-function (music) (ly:music?)
                        (format #t "\\aan-extract-chords: Entered\n=================================\n")
                        (clear-history)
                        (scheme-extract-chords music))

#(define (process-chords-for-bass chord)
   (format #t "process-chords-for-bass: Entered")
  (let* ((duration (get-event-chord-duration chord ))
         (elements (ly:music-property chord 'elements))
        (filtered (filter (lambda (event) (is-bass? event)) elements)))
         (cond ((equal? (length filtered) 0) (make-music 'RestEvent 'duration duration))
               ((equal? (length filtered) 1) (first filtered))
               ((> (length filtered) 1)
                  (begin
                  (format #t "process-chords-for-bass: duration is ~a\n" duration)
                  (make-music 'EventChord 
                              'elements filtered
                              'duration duration))))))
                  

#(define (scheme-extract-bass music)
   (format #t "scheme-extract-bass: Entering\n")
   (let ((new-music (ly:music-deep-copy music)))
     (music-selective-map should-skip-desend_into? 
      (lambda (event)
        (cond ((and (ly:music? event) (is-chord? event)) 
                      (let ((duration (ly:music-property event 'duration)))
                      (make-music 'RestEvent 'duration duration)))
                ((equal? (ly:music-property event 'name) 'EventChord) (process-chords-for-bass event))
                    (else event)))
             new-music)))

aan-extract-bass = #(define-music-function (music) (ly:music?)
  (format #t "\\aan-extract-bass: Entering\n====================================\n")
  (scheme-extract-bass music))