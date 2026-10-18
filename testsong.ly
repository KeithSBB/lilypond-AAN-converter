\version "2.24.4"
\include "aaan-convert.ly"

testnotes = \absolute {<<{<a, a>4^"7" c,8 r8 ees,4 ges,4}\\{r8 c'8^"M" a2^"dim" e,8 r8}  >> } 
%testnotes = \absolute {<a, a>4^"7" c,8 r8 ees,4 ges,4} 

\score {
  \new PianoStaff \with {

  } <<
     
    \new Staff = "AAAN" \with {
        instrumentName = "AAAN"
        shortInstrumentName = "AAAN"
      } { \clef bass \testnotes }
    \new Staff = "chords" \with {
        instrumentName = "Chords"
        shortInstrumentName = "Chords"
      } { \clef bass \aaan-extract-chords ##t \testnotes }
    % Voices stay separate.  A7 is <a cis' g'>, Adim is <a c' ees'> (no fifth on 7, diminished fifth on dim).
    \new Staff = "Expected chords" \with {
        instrumentName = "Exp Chords"
        shortInstrumentName = "Chords"
      } { \clef bass << { <a cis' g'>4-.^"7" r8 r8 r4 r4 } \\ { r8 <c e g>8-.^"M" <a c' ees'>2-.^"dim" r8 r8 } >> }
    \new Staff = "Bass"  \with {
        instrumentName = "Bass"
        shortInstrumentName = "Bass"
      } { \clef bass  \aaan-extract-bass ##t \testnotes}
    % <a, a> keeps only a,.  The e, in the other voice is not merged into ges,.
    \new Staff = "Bass"  \with {
        instrumentName = "Expect Bass"
        shortInstrumentName = "Exp Bass"
      } { \clef bass << { a,4-. c,8-. r8 ees,4-. ges,4-. } \\ { r8 r8 r2 e,8-. r8 } >> }
    
  >>
  \layout { }

}