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

#(define (aaan-extract-by-mode mode)
   (if (equal? mode 'bass)
       scheme-extract-bass
       scheme-extract-chords))

% Every child is walked, including a bare note, a tuplet, or a repeat.
% Each branch starts from the chord history outside the << >> and does
% not leave its own updates for the next branch.
#(define (process-simultaneous-music music mode)
   (let ((extract (aaan-extract-by-mode mode))
         (elements (ly:music-property music 'elements)))
     (make-music
      'SimultaneousMusic
      'elements (map (lambda (child)
                       (aaan-call-with-saved-history
                        (lambda ()
                          (if (ly:music? child)
                              (extract child)
                              child))))
                     (if (pair? elements) elements '())))))

% The body is walked first.  Each volta alternative then starts from
% that history and does not pass its updates to the next alternative.
#(define (aaan-extract-volta music mode)
   (let ((extract (aaan-extract-by-mode mode))
         (copy (ly:music-deep-copy music)))
     (let ((body (ly:music-property copy 'element)))
       (if (ly:music? body)
           (set! (ly:music-property copy 'element) (extract body))))
     (let ((alts (ly:music-property copy 'elements)))
       (if (pair? alts)
           (set! (ly:music-property copy 'elements)
                 (map (lambda (alt)
                        (aaan-call-with-saved-history
                         (lambda () (extract alt))))
                      alts))))
     copy))

% Create a chord from a note-event whose quality is an articulation.
#(define (create-chord-from-note event)
   (let* ((note-articu (ly:music-property event 'articulations))
          (chord-name (aaan-quality-token note-articu))
          (elements (note-event-to-chord-elements event chord-name)))
     (make-music 'EventChord
                 'elements elements
                 'articulations note-articu)))

#(define (aaan-event-articulations event)
   (filter ly:music?
           (let ((arts (ly:music-property event 'articulations)))
             (if (pair? arts) arts '()))))

% Replace an AAAN EventChord (bass note + chord note) with a spelled
% chord.  The quality may sit on the chord, <e, a>^"m", or on a
% chord-row note, <e, a^"m">.  A chord with no chord-row note becomes a rest.
#(define (create-chord-from-chord eventchord)
   (let* ((chord-elements (let ((els (ly:music-property eventchord 'elements)))
                            (if (pair? els) els '())))
          (duration (get-event-chord-duration eventchord))
          (note-elements (filter (lambda (event)
                                   (and (ly:music? event) (is-AAAN-chord? event)))
                                 (event-chord-notes eventchord)))
          (chord-token (aaan-quality-token chord-elements))
          (note-marks (if (null? note-elements)
                          '()
                          (filter aaan-quality-mark?
                                  (apply append
                                         (map aaan-event-articulations
                                              note-elements)))))
          (new-elements (if (null? note-elements)
                            '()
                            (apply append
                                   (map (lambda (note-event)
                                          (let ((note-token
                                                 (aaan-quality-token
                                                  (aaan-event-articulations note-event))))
                                            (note-event-to-chord-elements
                                             note-event
                                             (if (equal? note-token "unknown")
                                                 chord-token
                                                 note-token))))
                                        note-elements))))
          (kept (filter (lambda (elem)
                          (not (and (ly:music? elem)
                                    (music-is-of-type? elem 'note-event))))
                        chord-elements))
          (marks (if (or (any aaan-quality-mark? kept) (null? note-marks))
                     kept
                     (append kept (list (first note-marks))))))
     (if (null? new-elements)
         (make-music 'RestEvent 'duration duration)
         (make-music
          'EventChord
          'elements new-elements
          'articulations marks))))

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

#(define (maybe-staccato notes)
   (if make-staccato
       (map-some-music add-staccato notes)
       notes))

% music-selective-map walks children only when this is true.  Notes,
% event chords, simultaneous music, and volta repeats are handled by
% the walker, so their children are not walked again.
#(define (aaan-descend-into? event)
   (and (ly:music? event)
        (not (or (music-is-of-type? event 'event-chord)
                 (music-is-of-type? event 'note-event)
                 (music-is-of-type? event 'simultaneous-music)
                 (music-is-of-type? event 'volta-repeated-music)))))

#(define (scheme-extract-chords music)
   (let ((new-music (ly:music-deep-copy music)))
     (music-selective-map aaan-descend-into?
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
         ((music-is-of-type? event 'volta-repeated-music)
          (aaan-extract-volta event 'chords))
         (else event)))
      new-music)))

aaan-extract-chords = #(define-music-function (staccato music) ((boolean? #f) ly:music?)
                        (clear-history)
                        (if (boolean? staccato)
                            (set! make-staccato staccato)
                            (set! make-staccato #f))
                        (maybe-staccato (scheme-extract-chords music)))

% Parser chords keep post-events in elements.  Reading articulations as
% well repeats those marks.
#(define (aaan-source-marks chord)
   (filter (lambda (event)
             (and (ly:music? event)
                  (not (music-is-of-type? event 'note-event))))
           (let ((elements (ly:music-property chord 'elements)))
             (if (pair? elements) elements '()))))

#(define (aaan-bass-marks chord)
   (filter (lambda (event) (not (aaan-quality-mark? event)))
           (aaan-source-marks chord)))

#(define (process-chords-for-bass chord)
   (let* ((duration (get-event-chord-duration chord))
          (elements (ly:music-property chord 'elements))
          (filtered (filter (lambda (event) (is-AAAN-bass? event)) elements))
          (marks (aaan-bass-marks chord)))
     (cond ((equal? (length filtered) 0) (make-music 'RestEvent 'duration duration))
           ((equal? (length filtered) 1)
            ;; A one-bass chord is a note.  <a, a>4-. is a,4-. with the
            ;; staccato flag off.  The chord's type mark is not copied.
            (let ((note (ly:music-deep-copy (first filtered))))
              (if (pair? marks)
                  (set! (ly:music-property note 'articulations)
                        (append (filter ly:music?
                                        (ly:music-property note 'articulations))
                                marks)))
              note))
           (else
            (make-music 'EventChord
                        'elements (append filtered marks))))))

#(define (scheme-extract-bass music)
   (let ((new-music (ly:music-deep-copy music)))
     (music-selective-map aaan-descend-into?
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
         ((music-is-of-type? event 'volta-repeated-music)
          (aaan-extract-volta event 'bass))
         (else event)))
      new-music)))

aaan-extract-bass = #(define-music-function (staccato music) ((boolean? #f) ly:music?)
                      (if (boolean? staccato)
                          (set! make-staccato staccato)
                          (set! make-staccato #f))
                      (maybe-staccato (scheme-extract-bass music)))
