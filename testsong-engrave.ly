\version "2.24.4"
\include "aaan-convert.ly"

% Two voices at once.  Review baseline for the spelled chord tones is
% <a cis' g'>, <a cis' e'>, <g c' e'>, and <a c' ees'>.
% English keeps the source marks.  Russian row letters on those
% chords are 7, Б, Б, and У.
% e,_"_" is a counterbass: English keeps the source underscore, Russian
% prints Cyrillic В.  fine stays Italian.
testnotes = \absolute {
  <<
    { <a, a>4^"7" c,8 r8 ees,4 a4^"M" }
    \\
    { r8 c'8^"M" a2^"dim" e,8_"_" r8_"fine" }
  >>
}

englishConvert = \aaan-convert \testnotes
#(format #t "\n===== testsong-engrave english =====\n")
#(display-lily-music englishConvert)
#(newline)

\score {
  \new PianoStaff <<
    \new Staff \with { instrumentName = "AAAN" }
    { \clef bass \testnotes }
    \new Staff \with { instrumentName = "Convert" }
    { \clef bass \englishConvert }
  >>
  \header { title = "Engrave Test" }
  \layout { }
}

\aaanLanguage #'russian
russianConvert = \aaan-convert \testnotes
#(format #t "\n===== testsong-engrave russian =====\n")
#(display-lily-music russianConvert)
#(newline)

\score {
  \new PianoStaff <<
    \new Staff \with { instrumentName = \aaan-text "Bass" }
    { \clef bass \russianConvert }
  >>
  \header {
    title = "Engrave Test"
    instrument = \aaan-text "Bayan"
    composer = \aaan-text "Composed by Keith Smith"
  }
  \layout { }
}
