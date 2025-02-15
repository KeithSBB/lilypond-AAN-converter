\version "2.24.4"
\include "accbasschord.ly"

testnotes = \absolute {c'^"7" e, g^"7" a2^"7" f,  <cis, c' >^"7sus2" } 

%Issues:
%     1. Simultaneous music is not processed correctly: bass and chord
%     2. Accordion compound chords - notation an issue in chord

\score {
  \new PianoStaff \with {

  } <<
     
    \new Staff = "AAN" \with {
        instrumentName = "AAN"
        shortInstrumentName = "AAN"
      }  { \clef bass \testnotes }
    \new Staff = "chords" \with {
        instrumentName = "Chords"
        shortInstrumentName = "Chords"
      } { \clef bass \aan-extract-chords ##t \testnotes }
    \new Staff = "Bass"  \with {
        instrumentName = "Bass"
        shortInstrumentName = "Bass"
      }{ \clef bass\aan-extract-bass ##t \testnotes}
    
  >>
  \layout { }

}