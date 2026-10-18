%
% Copyright (C) 2025-2026
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
% MIDI converter.  American Accordion Notation (AAAN, also called AAA) to a
% bass-note voice and fully spelled Stradella chords.
%
%        \aaan-extract-bass [staccato] { ... AAAN music ... }
%        \aaan-extract-chords [staccato] { ... AAAN music ... }
%
% The optional staccato switch is MIDI-only.  Accordion left-hand playing
% often defaults to staccato, and ##t applies that to the generated voices.
% Engraving must not use this switch; see aaan-engrave.ly, which keeps only
% the articulations written in the source.
%
% Shared tables and spelling live in aaan-common.ly.

\version "2.24.4"

\include "aaan-common.ly"

#(define make-staccato #f)

#(define (ly:duration-sum dur1 dur2)
   (let ((len1 (ly:duration-length dur1))
         (len2 (ly:duration-length dur2)))
     (ly:make-duration 0 0 (+ (ly:moment-main len1) (ly:moment-main len2)))))

#(define (process-simultaneous-music music mode)
   (log-message 'debug "process-simultaneous-music: Entered with mode ~a\n" mode)
   (let* ((simul-elems (ly:music-property music 'elements))
          (new-sim-elems '())
          (new-sim-elems (append
                          (map (lambda (simul-event)
                                 (if (music-is-of-type? simul-event 'sequential-music)
                                     (if (equal? mode 'bass)
                                         (scheme-extract-bass simul-event)
                                         (scheme-extract-chords simul-event))
                                     simul-event))
                               simul-elems)
                          new-sim-elems)))
     (log-message 'debug "process-simultaneous-music:  New-elements:\n~a\n" (length new-sim-elems))
     (make-music 'SimultaneousMusic
                 'elements new-sim-elems)))

% Create a chord from a note-event whose quality is an articulation.
#(define (create-chord-from-note event)
   (log-message 'debug "create-chord-from-note: Entered \n")
   (let* ((note-articu (ly:music-property event 'articulations))
          (pitch (ly:music-property event 'pitch))
          (notename (ly:pitch-notename pitch))
          (alteration (ly:pitch-alteration pitch)))
     (log-message 'debug "create-chord-from-note: articulations: ~a, pitch: notename ~a, alteration ~a\n" note-articu notename alteration)
     (let* ((chord-name (aaan-quality-token note-articu))
            (root-notename (ly:pitch-notename pitch))
            (elements (note-event-to-chord-elements event chord-name)))
       (log-message 'debug "create-chord-from-note: Creating chord '~a' with root ~a, elements: ~a\n" chord-name root-notename elements)
       (let ((newchord (make-music 'EventChord
                                   'elements elements
                                   'articulations note-articu)))
         (log-message 'debug "create-chord-from-note: result: ~a\n" newchord)
         newchord))))

% Replace an AAAN EventChord (bass note + chord note, quality on the chord)
% with a spelled chord.  A chord with no chord-row note becomes a rest.
#(define (create-chord-from-chord eventchord)
   (log-message 'debug "create-chord-from-chord: Entered\n ~a\n" eventchord)
   (let* ((chord-elements (ly:music-property eventchord 'elements))
          (duration (get-event-chord-duration eventchord)))
     (log-message 'debug "create-chord-from-chord: chord-elements:\n ~a\n" chord-elements)
     (let* ((chord-name (aaan-quality-token chord-elements))
              (note-elements (filter (lambda (event)
                                       (and (ly:music? event) (is-AAAN-chord? event)))
                                     (event-chord-notes eventchord)))
              (new-elements (apply append
                                   (map (lambda (note-event)
                                          (note-event-to-chord-elements note-event chord-name))
                                        note-elements))))
         (log-message 'debug "create-chord-from-chord: chordName: ~a\n" chord-name)
         (log-message 'debug "create-chord-from-chord: note-elements: ~a\n" note-elements)
         (log-message 'debug "create-chord-from-chord: new-elements: ~a\n" new-elements)
         (log-message 'debug "create-chord-from-chord: duration:  ~a\n" duration)
         (if (equal? (length new-elements) 0)
             (make-music 'RestEvent 'duration duration)
             (begin
               (log-message 'debug "create-chord-from-chord: eventchord:\n  ~a\n" eventchord)
               (let* ((chord-articu (filter (lambda (elem)
                                              (not (and (ly:music? elem)
                                                        (music-is-of-type? elem 'note-event))))
                                            chord-elements))
                      (newchord (make-music
                                 'EventChord
                                 'elements new-elements
                                 'articulations chord-articu)))
                 (log-message 'debug "create-chord-from-chord:  result:\n ~a\n" newchord)
                 (ly:music-deep-copy newchord)))))))

#(define (make-script x)
   (make-music 'ArticulationEvent
               'articulation-type x))

#(define (aaan-has-articulation? music articulation)
   (let ((events (append (filter ly:music? (ly:music-property music 'articulations))
                         (filter ly:music? (ly:music-property music 'elements)))))
     (any (lambda (event)
            (and (eq? (ly:music-property event 'name) 'ArticulationEvent)
                 (eq? (ly:music-property event 'articulation-type) articulation)))
          events)))

#(define (add-script m x)
   (case (ly:music-property m 'name)
     ((NoteEvent)
      (if (aaan-has-articulation? m x)
          m
          (begin
            (set! (ly:music-property m 'articulations)
                  (append (ly:music-property m 'articulations)
                          (list (make-script x))))
            m)))
     ((EventChord)
      (if (aaan-has-articulation? m x)
          m
          (begin
            (set! (ly:music-property m 'elements)
                  (append (ly:music-property m 'elements)
                          (list (make-script x))))
            m)))
     (else #f)))

#(define (add-staccato m)
   (add-script m 'staccato))

addStacc = #(define-music-function (music) (ly:music?)
              (map-some-music add-staccato music))

#(define (maybe-staccato notes)
   (if make-staccato
       (map-some-music add-staccato notes)
       notes))

% Do not descend into containers handled by the walker itself.
#(define (should-skip-descend_into? event)
   (and (ly:music? event)
        (begin
          (log-message 'debug "\nshould-skip-desend_into?: Entered ~a\n"
                       (ly:music-property event 'name))
          (not (or (music-is-of-type? event 'event-chord)
                   (music-is-of-type? event 'note-event)
                   (music-is-of-type? event 'simultaneous-music))))))

#(define (scheme-extract-chords music)
   (log-message 'debug "scheme-extract-chords: Entered\n")
   (let ((new-music (ly:music-deep-copy music)))
     (music-selective-map should-skip-descend_into?
      (lambda (event)
        (cond
         ((and (ly:music? event) (music-is-of-type? event 'note-event))
          (if (is-AAAN-chord? event)
              (create-chord-from-note event)
              (make-rest event)))
         ((music-is-of-type? event 'event-chord)
          (create-chord-from-chord event))
         ((music-is-of-type? event 'simultaneous-music)
          (process-simultaneous-music event 'chords))
         (else event)))
      new-music)))

aaan-extract-chords = #(define-music-function (staccato music) ((boolean? #f) ly:music?)
                        (log-message 'debug "\\aaan-extract-chords: Entered\n=================================\n")
                        (clear-history)
                        (if (boolean? staccato)
                            (set! make-staccato staccato)
                            (set! make-staccato #f))
                        (let ((proc-music (scheme-extract-chords music)))
                          (log-message 'debug "\n BEGINING of display-music\n")
                          (maybe-staccato proc-music)))

#(define (process-chords-for-bass chord)
   (log-message 'debug "process-chords-for-bass: Entered")
   (let* ((duration (get-event-chord-duration chord))
          (elements (ly:music-property chord 'elements))
          (filtered (filter (lambda (event) (is-AAAN-bass? event)) elements)))
     (cond ((equal? (length filtered) 0) (make-music 'RestEvent 'duration duration))
           ((equal? (length filtered) 1) (first filtered))
           (else
            (let ((marks (filter (lambda (event)
                                   (and (ly:music? event)
                                        (not (music-is-of-type? event 'note-event))))
                                 elements))
                  (arts (filter ly:music? (ly:music-property chord 'articulations))))
              (log-message 'debug "process-chords-for-bass: duration is ~a\n" duration)
              (let ((kept (make-music 'EventChord
                                      'elements (append filtered marks))))
                (if (pair? arts)
                    (set! (ly:music-property kept 'articulations) arts))
                kept))))))

#(define (scheme-extract-bass music)
   (log-message 'debug "scheme-extract-bass: Entering\n")
   (let ((new-music (ly:music-deep-copy music)))
     (music-selective-map should-skip-descend_into?
      (lambda (event)
        (cond
         ((and (ly:music? event) (is-AAAN-chord? event))
          (make-rest event))
         ((music-is-of-type? event 'note-event)
          (if (is-AAAN-bass? event)
              event
              (make-rest event)))
         ((music-is-of-type? event 'event-chord)
          (process-chords-for-bass event))
         ((music-is-of-type? event 'simultaneous-music)
          (process-simultaneous-music event 'bass))
         (else event)))
      new-music)))

aaan-extract-bass = #(define-music-function (staccato music) ((boolean? #f) ly:music?)
                      (log-message 'debug "\\aaan-extract-bass: Entering\n====================================\n")
                      (log-message 'debug "Staccato input is ~a\n" staccato)
                      (if (boolean? staccato)
                          (set! make-staccato staccato)
                          (set! make-staccato #f))
                      (maybe-staccato (scheme-extract-bass music)))
