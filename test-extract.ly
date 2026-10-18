\version "2.24.4"
\include "aaan-convert.ly"

% Each extract is compared with the expected music.  A mismatch stops
% the compile.

sample = \absolute {
  <e, a>4^"M" a4 <a, a>4 d4^"m" e4^"7" b4^"dim" e,4 c,4-> <e, a,>4
}
cfamily = \absolute {
  c'4^"M" c'4^"m" c'4^"7" c'4^"d" <c, c'>4^"M"
  cis'4^"M" cis'4^"m" cis'4^"7" cis'4^"dim"
}
bare = \absolute { a4 d'4^"m" d'4 }
idem = \absolute { c,4-. c,4->-. <e, a,>4-. }
% A minor must not paint the following B.  The later A is still minor.
roots = \absolute { a4^"m" b4 a4 }
% cis is in the space below the middle line.  des sits on the line.
boundary = \absolute { cis4 des4^"M" }
% One bass note inside a chord keeps its written articulation when
% make-staccato is off.  The chord row keeps it too.
flagoff = \absolute { <a, a>4-. }

#(define (squash text)
   (string-join (string-tokenize text) " "))

#(define (lily-text music)
   (let ((port (open-output-string)))
     (display-lily-music music port)
     (squash (get-output-string port))))

#(define (assert-same title got want)
   (let ((got-text (lily-text got))
         (want-text (lily-text want)))
     (format #t "\n===== ~a =====\n~a\n" title got-text)
     (if (not (string=? got-text want-text))
         (ly:error "AAAN extract mismatch: ~a\n got:  ~a\n want: ~a"
                   title got-text want-text))))

#(assert-same "bass"
   #{ \aaan-extract-bass \sample #}
   #{ \absolute { e,4^"M" r4 a,4 r4 r4 r4 e,4 c,4-> <e, a,>4 } #})
#(assert-same "bass ##t"
   #{ \aaan-extract-bass ##t \sample #}
   #{ \absolute { e,4^"M"-. r4 a,4-. r4 r4 r4 e,4-. c,4->-. <e, a,>4-. } #})
#(assert-same "chords"
   #{ \aaan-extract-chords \sample #}
   #{ \absolute {
        <a cis' e'>4^"M" <a cis' e'>4 <a cis' e'>4 <d f a>4^"m"
        <e gis d'>4^"7" <b d' f'>4^"dim" r4 r4 r4
      } #})
#(assert-same "chords ##t"
   #{ \aaan-extract-chords ##t \sample #}
   #{ \absolute {
        <a cis' e'>4-.^"M" <a cis' e'>4-. <a cis' e'>4-. <d f a>4-.^"m"
        <e gis d'>4-.^"7" <b d' f'>4-.^"dim" r4 r4 r4
      } #})

#(assert-same "C and C# chords"
   #{ \aaan-extract-chords \cfamily #}
   #{ \absolute {
        <e g c'>4^"M" <ees g c'>4^"m" <e bes c'>4^"7" <ees ges c'>4^"d" <e g c'>4^"M"
        <eis gis cis'>4^"M" <e gis cis'>4^"m" <eis b cis'>4^"7" <e g cis'>4^"dim"
      } #})
#(assert-same "C bass with <c, c'>"
   #{ \aaan-extract-bass \cfamily #}
   #{ \absolute { r4 r4 r4 r4 c,4^"M" r4 r4 r4 r4 } #})

#(assert-same "first unmarked is major"
   #{ \aaan-extract-chords \bare #}
   #{ \absolute { <a cis' e'>4 <d f a>4^"m" <d f a>4 } #})

#(assert-same "written articulation, flag off"
   #{ \aaan-extract-bass \idem #}
   #{ \absolute { c,4-. c,4->-. <e, a,>4-. } #})
#(assert-same "##t does not stack staccato"
   #{ \aaan-extract-bass ##t \idem #}
   #{ \absolute { c,4-. c,4->-. <e, a,>4-. } #})

#(assert-same "two roots do not share type memory"
   #{ \aaan-extract-chords \roots #}
   #{ \absolute { <a c' e'>4^"m" <b dis' fis'>4 <a c' e'>4 } #})

#(assert-same "cis is bass, des is a chord"
   #{ \aaan-extract-bass \boundary #}
   #{ \absolute { cis4 r4 } #})
#(assert-same "des stays on the middle line"
   #{ \aaan-extract-chords \boundary #}
   #{ \absolute { r4 <des f aes>4^"M" } #})

#(assert-same "articulation kept on a chord, flag off"
   #{ \aaan-extract-bass \flagoff #}
   #{ \absolute { a,4-. } #})
#(assert-same "chord articulation kept, flag off"
   #{ \aaan-extract-chords \flagoff #}
   #{ \absolute { <a cis' e'>4-. } #})

\score {
  \new PianoStaff <<
    \new Staff \with { instrumentName = "AAAN" }
    { \clef bass \sample \cfamily \bare \idem \roots \boundary \flagoff }
    \new Staff \with { instrumentName = "Bass" }
    { \clef bass \aaan-extract-bass \sample \aaan-extract-bass \cfamily
      \aaan-extract-bass \bare \aaan-extract-bass \idem
      \aaan-extract-bass \roots \aaan-extract-bass \boundary
      \aaan-extract-bass \flagoff }
    \new Staff \with { instrumentName = "Bass stacc." }
    { \clef bass \aaan-extract-bass ##t \sample \aaan-extract-bass ##t \idem }
    \new Staff \with { instrumentName = "Chords" }
    { \clef bass \aaan-extract-chords \sample \aaan-extract-chords \cfamily
      \aaan-extract-chords \bare \aaan-extract-chords \roots
      \aaan-extract-chords \boundary \aaan-extract-chords \flagoff }
    \new Staff \with { instrumentName = "Chords stacc." }
    { \clef bass \aaan-extract-chords ##t \sample }
  >>
  \header { title = "AAAN extract" }
  \layout { indent = 1.3\in }
}
