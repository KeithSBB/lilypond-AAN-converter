\version "2.24.4"
\include "accbasschord.ly"

testnotes = \absolute {<<{<a, a>4^"7" c,8 r8 ees,4 ges,4}\\{r8 c'8^"M" a2^"dim" e,8 r8}  >> } 
%testnotes = \absolute {<a, a>4^"7" c,8 r8 ees,4 ges,4} 

\score {
  \new PianoStaff \with {

  } <<
     
    \new Staff = "AAN" \with {
        instrumentName = "AAN"
        shortInstrumentName = "AAN"
      } { \clef bass \testnotes }
    \new Staff = "chords" \with {
        instrumentName = "Chords"
        shortInstrumentName = "Chords"
      } { \clef bass \aan-extract-chords ##t \testnotes } %Expected: {<a des g>8~ <a des g c e g>8 <a c ges>2 r4}
    \new Staff = "Expected chords" \with {
        instrumentName = "Exp Chords"
        shortInstrumentName = "Chords"
      } { \clef bass {<a des g>8~ <a des g c e g>8 <a c ges>2 r4}}
    \new Staff = "Bass"  \with {
        instrumentName = "Bass"
        shortInstrumentName = "Bass"
      } { \clef bass  \aan-extract-bass ##t \testnotes} %Expected: {a,4 c,8 r8 ees,4  <ges,~ e,>8 ges,8}
    \new Staff = "Bass"  \with {
        instrumentName = "Expect Bass"
        shortInstrumentName = "Exp Bass"
      } { \clef bass  {a,4 c,8 r8 ees,4  <ges,~ e,>8 ges,8}}
    
  >>
  \layout { }

}