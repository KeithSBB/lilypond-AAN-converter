\version "2.24.4"
 #(ly:set-option 'compile-scheme-code)

#(use-modules (ice-9 format))

#(define (scheme-makerests-above-middle music)
   (format #t "Debug: Entering scheme-makerests-above-middle function\n")
   (let ((new-music (ly:music-deep-copy music)))
     (music-map
      (lambda (event)
        (if (and (ly:music? event)
                 (equal? (ly:music-property event 'name) 'NoteEvent))
            (let* ((pitch (ly:music-property event 'pitch))
                   (note-name (ly:pitch-notename pitch))
                   (note-alteration (ly:pitch-alteration pitch))
                   (semitones (ly:pitch-semitones pitch))
                   (octave (ly:pitch-octave pitch)))
              (format #t "Debug: Note name: ~a, Alteration ~a, Pitch ~a, Octave: ~a, Semitones ~a\n" note-name note-alteration pitch octave semitones)
              (if (or (> semitones -11) (and (= semitones -11) (= note-alteration (/ -1 2)) )) ; This condition should catch all notes above D and above in bass clef
                  (begin
                    (format #t "Debug: Note name: ~a, Octave: ~a\n" note-name octave)
                    (let ((duration (ly:music-property event 'duration)))
                      (make-music 'RestEvent 'duration duration)))
                  event))
            event))
      new-music)))

aan-extract-bass = #(define-music-function (music) (ly:music?)
  (format #t "Debug: Calling makerestsAboveMiddle\n")
  (scheme-makerests-above-middle music))