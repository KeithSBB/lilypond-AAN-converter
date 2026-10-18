\version "2.24.4"
\include "aaan-convert.ly"

% Bass-clef middle line is d.  <e, a>^"M" is bass E plus an A major button,
% not two bass notes.  The unmarked a reuses that major.  <a, a> is bass A
% plus the same A major.  d^"m", e^"7", and b^"dim" are the other rows.
% e, and c, are lone bass notes (c, keeps its written accent).  <e, a,>
% is two bass notes and must not crash.
%
% ##t adds staccato.  Omitting it leaves the accent on c, and adds nothing.
%
% extract-bass:
%   e,4 r4 a,4 r4 r4 r4 e,4 c,4-> <e, a,>4
% extract-bass ##t:
%   e,4-. r4 a,4-. r4 r4 r4 e,4-. c,4->-. <e, a,>4-.
% extract-chords:
%   <a cis' e'>4^"M" <a cis' e'>4 <a cis' e'>4 <d f a>4^"m"
%   <e gis d'>4^"7" <b d' f'>4^"dim" r4 r4 r4
% extract-chords ##t: the same, with -. on every chord and not on the rests.
sample = \absolute {
  <e, a>4^"M" a4 <a, a>4 d4^"m" e4^"7" b4^"dim" e,4 c,4-> <e, a,>4
}

#(define (pitch->absolute p)
   (let ((oct (ly:pitch-octave p)))
     (string-append
      (list-ref '("c" "d" "e" "f" "g" "a" "b") (ly:pitch-notename p))
      (cond
       ((= (ly:pitch-alteration p) 0) "")
       ((= (ly:pitch-alteration p) 1/2) "is")
       ((= (ly:pitch-alteration p) -1/2) "es")
       ((= (ly:pitch-alteration p) 1) "isis")
       ((= (ly:pitch-alteration p) -1) "eses")
       (else (format #f "(~a)" (ly:pitch-alteration p))))
      (cond
       ((= oct 0) "'")
       ((> oct 0) (make-string oct #\'))
       ((= oct -1) "")
       (else (make-string (- -1 oct) #\,))))))

#(define (staccato? music)
   (let* ((arts (ly:music-property music 'articulations))
          (elems (ly:music-property music 'elements))
          (all (append (if (pair? arts) arts '())
                       (if (pair? elems) elems '()))))
     (any (lambda (a)
            (and (ly:music? a)
                 (eq? (ly:music-property a 'name) 'ArticulationEvent)
                 (eq? (ly:music-property a 'articulation-type) 'staccato)))
          all)))

#(define (summarize music)
   (cond
    ((not (ly:music? music)) "")
    ((eq? (ly:music-property music 'name) 'NoteEvent)
     (string-append (pitch->absolute (ly:music-property music 'pitch))
                    (if (staccato? music) "-." "")))
    ((eq? (ly:music-property music 'name) 'EventChord)
     (string-append
      "<"
      (string-join
       (map summarize
            (filter (lambda (e)
                      (and (ly:music? e)
                           (eq? (ly:music-property e 'name) 'NoteEvent)))
                    (ly:music-property music 'elements)))
       " ")
      ">"
      (if (staccato? music) "-." "")))
    ((eq? (ly:music-property music 'name) 'RestEvent) "r")
    ((or (music-is-of-type? music 'sequential-music)
         (music-is-of-type? music 'simultaneous-music))
     (string-join (filter (lambda (s) (and (string? s) (not (string=? s ""))))
                          (map summarize (ly:music-property music 'elements)))
                  " "))
    (else
     (let ((e (ly:music-property music 'element)))
       (if (ly:music? e) (summarize e) "")))))

#(define (show title mus)
   (format #t "\n===== ~a =====\n" title)
   (format #t "SUMMARY: ~a\n" (summarize mus))
   (display-lily-music mus)
   (newline))

#(show "extract-bass source articulation" #{ \aaan-extract-bass \sample #})
#(show "extract-bass ##t" #{ \aaan-extract-bass ##t \sample #})
#(show "extract-chords source articulation" #{ \aaan-extract-chords \sample #})
#(show "extract-chords ##t" #{ \aaan-extract-chords ##t \sample #})

\score {
  \new PianoStaff <<
    \new Staff \with { instrumentName = "AAAN" }
    { \clef bass \sample }
    \new Staff \with { instrumentName = "Bass" }
    { \clef bass \aaan-extract-bass \sample }
    \new Staff \with { instrumentName = "Bass stacc." }
    { \clef bass \aaan-extract-bass ##t \sample }
    \new Staff \with { instrumentName = "Chords" }
    { \clef bass \aaan-extract-chords \sample }
    \new Staff \with { instrumentName = "Chords stacc." }
    { \clef bass \aaan-extract-chords ##t \sample }
  >>
  \header { title = "AAAN extract" }
  \layout { indent = 1.3\in }
}
