\version "2.24.4"
\include "aan-engrave.ly"

% Title is stored so a copy of it inside the music is not translated.
% The \header title below is a literal and is never passed to \aan-text.
#(set! aan-song-title "Engrave Test")

% Source stays in English AAN.  Written articulations are engraving truth.
% ^"fine" is ordinary text, not a quality token.
testnotes = \absolute {
  <<
    { <a, a>4^"7" c,8^"fine" r8 ees,4 ges,4 }
    \\
    { r8 c'8^"M" a2^"dim" e,8 r8^"D.C. al fine" }
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
    % A7, then C and Adim in the second voice.  No added staccato.
  >>
  \header {
    title = "Engrave Test"
  }
  \layout { }
}

% Language is read when the music functions run, so set it before the calls.
\aanLanguage #'russian
russianBass = \aan-engrave-bass \testnotes
russianChords = \aan-engrave-chords \testnotes

\score {
  \new PianoStaff <<
    \new Staff \with { instrumentName = \aan-text "Bass" }
    { \clef bass \aan-translate \russianBass }
    \new Staff \with { instrumentName = \aan-text "Chords" }
    { \clef bass \aan-translate \russianChords }
    % Symbols: Ля7, ДоБ, ЛяУ.  fine → конец.  D.C. al fine → С начала до конца.
  >>
  \header {
    title = "Engrave Test"
    instrument = \aan-text "Bayan"
    composer = \aan-text "Composed by Keith Smith"
  }
  \layout { }
}
