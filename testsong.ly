\version "2.24.4"
\include "accbasschord.ly"

global = {
  \key c \major
  %\numericTimeSignature
  \time 4/4
  \tempo 4=120
}

voltaFine = \markup {  \text \bold {pour Fine}}



testnotes = { \repeat volta 2 { |a,4 a^"m" e, a|
                                 }\alternative {{| a,4 a^"M" e, a| }
                                                 {a,4 a^"M" e, a| }}  
\set Score.repeatCommands = #(list(list 'volta voltaFine) 'end-repeat)
                 {a,4 a^"M" <e, a>2|}
\set Score.repeatCommands = #'((volta #f)) }

\score {
  \new Staff \with {

  } <<
  
    \new Staff = "AAN"  { \clef bass \testnotes }
    \new Staff = "chords"  { \clef bass \aan-extract-chords \testnotes }
    \new Staff = "Bass"  { \clef bass  \aan-extract-bass \testnotes}
    
  >>
  \layout { }

}