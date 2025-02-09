\version "2.24.4"
\include "accbasschord.ly"

testnotes = { a,4 a^"M" e, a }

{
  \clef "bass"
  \aan-extract-chords \testnotes
  %\displayMusic  \testnotes

}
{
  \clef "bass"
  \aan-extract-bass \testnotes

  % \testnotes
}