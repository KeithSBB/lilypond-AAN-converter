\version "2.24.4"
\include "testscheme.ly"

testnotes = { c'4^"M" d'^"m"  <c, c'> c'^"m" c'^"o" }

{
  \clef "bass"
  \aan-extract-chords \testnotes
  %\displayMusic  \testnotes

}
{
  \clef "bass"
  %\aan-extract-chords \testnotes

  %\displayMusic \testnotes
}