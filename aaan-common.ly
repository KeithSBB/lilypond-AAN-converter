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
% Duration-only shorthand (c,8 4) is a note with an empty pitch.  The walks
% copy the previous note's pitch onto it, the way LilyPond does.  A pitch
% that is still missing after that is a rest or a bug and stays quiet.

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

% Remembered pitch for duration-only notes.  Each walk starts by clearing it.
#(define aaan-last-pitch #f)

#(define (aaan-reset-pitch)
   (set! aaan-last-pitch #f))

% A pitched note is remembered.  A duration-only note copies that pitch.
% Nothing to copy means the pitch stays missing; callers stay quiet.
#(define (aaan-carry-pitch! note)
   (let ((p (ly:music-property note 'pitch)))
     (if (ly:pitch? p)
         (set! aaan-last-pitch p)
         (if (ly:pitch? aaan-last-pitch)
             (set! (ly:music-property note 'pitch) aaan-last-pitch)))))

#(define (aaan-carry-chord-pitches! music)
   (for-each (lambda (event)
               (if (and (ly:music? event)
                        (music-is-of-type? event 'note-event))
                   (aaan-carry-pitch! event)))
             (let ((els (ly:music-property music 'elements)))
               (if (pair? els) els '()))))

% Each branch starts from the pitch at entry and does not leak.  The first
% branch is the voice that continues, so its ending pitch is remembered.
#(define (aaan-map-branches proc elements)
   (let ((saved aaan-last-pitch)
         (first-end #f)
         (seen #f))
     (let ((out (map (lambda (element)
                       (aaan-call-with-saved-history
                        (lambda ()
                          (set! aaan-last-pitch saved)
                          (let ((result (proc element)))
                            (if (not seen)
                                (begin
                                  (set! seen #t)
                                  (set! first-end aaan-last-pitch)))
                            (set! aaan-last-pitch saved)
                            result))))
                     (if (pair? elements) elements '()))))
       (if seen
           (set! aaan-last-pitch first-end))
       out)))

% Each volta alternative starts from the pitch at entry (the end of the
% body).  The last alternative is what follows the repeat.
#(define (aaan-map-alternatives proc alts)
   (let ((entry aaan-last-pitch)
         (last-end aaan-last-pitch))
     (let ((out (map (lambda (alt)
                       (aaan-call-with-saved-history
                        (lambda ()
                          (set! aaan-last-pitch entry)
                          (let ((result (proc alt)))
                            (set! last-end aaan-last-pitch)
                            (set! aaan-last-pitch entry)
                            result))))
                     (if (pair? alts) alts '()))))
       (set! aaan-last-pitch last-end)
       out)))

% #f when the pitch is missing.  Duration-only notes are filled by the
% walk first.  Still missing after that is a rest or a bug: stay quiet.
#(define (aaan-safe-pitch music)
   (let ((p (ly:music-property music 'pitch)))
     (if (ly:pitch? p)
         p
         #f)))

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

#(define chord-keys
   (map car (hash-map->list (lambda (key value) (cons key value)) aaan-chord-intvl-table)))

% chord-history: (enharmonic pitch class . chord-name)
% C# and Db share one remembered type.

#(define chord-history '())

#(define (aaan-history-key pitch)
   (modulo (ly:pitch-semitones pitch) 12))

#(define (aaan-quality-mark? event)
   (and (ly:music? event)
        (eq? (ly:music-property event 'name) 'TextScriptEvent)
        (equal? (ly:music-property event 'direction) 1)
        (let ((text (ly:music-property event 'text)))
          (and (string? text) (member text chord-keys)))))

#(define (search-history note-event)
   (log-message 'debug "search-history:  entered\n")
   (let ((pitch (aaan-safe-pitch note-event)))
     (if (not pitch)
         '()
         (let ((key (aaan-history-key pitch)))
           (let ((item (assoc key chord-history)))
             (if item
                 (begin
                   (log-message 'debug "search-history: found ~a for pitch class ~a\n" (cdr item) key)
                   (cdr item))
                 '()))))))

#(define (save-update-history note-event chord-name)
   (log-message 'debug "save-update-history:  entered\n")
   (let ((pitch (aaan-safe-pitch note-event)))
     (if pitch
         (let ((key (aaan-history-key pitch)))
           (let ((item (assoc key chord-history)))
             (if item
                 (begin
                   (log-message 'debug "save-update-history: pitch class ~a updating to ~a\n" key chord-name)
                   (set-cdr! item chord-name))
                 (begin
                   (set! chord-history (cons (cons key chord-name) chord-history))
                   (log-message 'debug "save-update-history: added to Chord-history: ~a\n" chord-history))))))))

#(define (clear-history)
   (log-message 'debug "Clear-history:  Entered\n")
   (set! chord-history '()))

% set-cdr! updates a history pair in place, so a snapshot copies the pairs.
% << >> branches and volta alternatives each start from the history they
% were entered with, and do not leave their updates behind.
#(define (aaan-history-snapshot)
   (map (lambda (item) (cons (car item) (cdr item)))
        chord-history))

#(define (aaan-call-with-saved-history thunk)
   (let ((saved (aaan-history-snapshot)))
     (let ((result (thunk)))
       (set! chord-history saved)
       result)))

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
   (filter aaan-quality-mark?
           (if (pair? articulations) articulations '())))

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
         '()
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

% Lowest octave of each spelled tone that is still on or above G3
% (absolute g).  Tones are built from octave -1.  A tone below g is
% raised, and a tone an octave higher is lowered when that octave is
% still on g.  C major is <g c' e'>, F# major is <ais cis' fis'>,
% C7 is <bes c' e'>, A7 is <g a cis'>, Bb7 is <aes bes d'>, and
% B7 is <a b dis'>.  Button split stays at aaan-bass-center (D3).
#(define aaan-voicing-floor (ly:make-pitch -1 4 0))

#(define (aaan-on-or-above-floor? pitch)
   (>= (ly:pitch-semitones pitch)
       (ly:pitch-semitones aaan-voicing-floor)))

#(define (aaan-lowest-on-floor pitch)
   (let loop ((p pitch))
     (cond
      ((not (aaan-on-or-above-floor? p))
       (loop (ly:pitch-transpose p (ly:make-pitch 1 0 0))))
      ((aaan-on-or-above-floor?
        (ly:pitch-transpose p (ly:make-pitch -1 0 0)))
       (loop (ly:pitch-transpose p (ly:make-pitch -1 0 0))))
      (else p))))

#(define (aaan-invert-onto-center pitches)
   (sort (map aaan-lowest-on-floor pitches) ly:pitch<?))

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
