\version "2.24.4"
\include "aaan-convert.ly"

% Title is a literal header field and is never passed to \aaan-text.
#(set! aaan-song-title "Engrave Test")

% Two voices at once.  e,_"_" is a counterbass: English keeps the source
% underscore, Russian prints Cyrillic В.  fine stays Italian.
testnotes = \absolute {
  <<
    { <a, a>4^"7" c,8 r8 ees,4 a4^"M" }
    \\
    { r8 c'8^"M" a2^"dim" e,8_"_" r8_"fine" }
  >>
}

\score {
  \new PianoStaff <<
    \new Staff \with { instrumentName = "AAAN" }
    { \clef bass \testnotes }
    \new Staff \with { instrumentName = "Convert" }
    { \clef bass \aaan-convert \testnotes }
  >>
  \header { title = "Engrave Test" }
  \layout { }
}

\aaanLanguage #'russian

\score {
  \new PianoStaff <<
    \new Staff \with { instrumentName = \aaan-text "Bass" }
    { \clef bass \aaan-translate \aaan-convert \testnotes }
  >>
  \header {
    title = "Engrave Test"
    instrument = \aaan-text "Bayan"
    composer = \aaan-text "Composed by Keith Smith"
  }
  \layout { }
}
