\version "2.24.4"
\include "accbasschord.ly"

testnotes = \absolute { a2^"7" <cis, c'>^"M"} 

\score {

  <<
    \new Staff = "AAN"  { \clef bass  \testnotes }
    
    \new Staff = "chord" { \clef bass \aan-extract-chords \testnotes}
    
    \new Staff = "Bass"  { \clef bass \aan-extract-bass \testnotes}
  >> 
    }