\version "2.24.4"
\include "accbasschord.ly"

global = {
  \key c \major
  %\numericTimeSignature
  \time 4/4
  \tempo 4=120
}



testnotes = \absolute { a^"7" <cis, c''^"M">^"M"} 

%testing = \display-music {\aan-extract-chords \testnotes}


\score {
  \new Staff \with {

  } <<
  
    \new Staff = "AAN"  { \clef bass  \testnotes }
    \new Staff = "chords"  { \clef bass  \aan-extract-chords \testnotes }
    %\new Staff = "Bass"  { \clef bass  \aan-extract-bass \testnotes}
    
  >>
  \layout { }

}