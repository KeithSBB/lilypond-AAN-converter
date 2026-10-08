\version "2.24.4"
\include "aan-engrave.ly"

% Title is a literal header field and is never passed to \aan-text.
#(set! aan-song-title "Engrave Test")

% e,_"_" is an explicit counterbass mark.  It is not inferred.
% ees, then a^"M" is a bass that differs from the chord root, so the
% chord is engraved with _"(.)".
testnotes = \absolute {
  <<
    { <a, a>4^"7" c,8 r8 ees,4 a4^"M" }
    \\
    { r8 c'8^"M" a2^"dim" e,8_"_" r8_"fine" }
  >>
}

\score {
  \new PianoStaff <<
    \new Staff \with { instrumentName = "AAN" }
    { \clef bass \testnotes }
    \new Staff \with { instrumentName = "Bass" }
    { \clef bass \aan-engrave-bass \testnotes }
    \new Staff \with { instrumentName = "Chords" }
    { \clef bass \aan-engrave-chords \testnotes }
    % English symbols: A7, C, Adim, A.  Counterbass prints as _"B".
    % The A chord after ees, gets _"(.)".  fine stays Italian.
  >>
  \header {
    title = "Engrave Test"
  }
  \layout { }
}

\aanLanguage #'russian
russianBass = \aan-engrave-bass \testnotes
russianChords = \aan-engrave-chords \testnotes

\score {
  \new PianoStaff <<
    \new Staff \with { instrumentName = \aan-text "Bass" }
    { \clef bass \aan-translate \russianBass }
    \new Staff \with { instrumentName = \aan-text "Chords" }
    { \clef bass \aan-translate \russianChords }
    % Row letters only: 7, Б, У, Б.  No pitch names.  f and fine stay Italian.
  >>
  \header {
    title = "Engrave Test"
    instrument = \aan-text "Bayan"
    composer = \aan-text "Composed by Keith Smith"
  }
  \layout { }
}
