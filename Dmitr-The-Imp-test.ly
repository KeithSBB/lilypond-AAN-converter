\version "2.24.0"
\include "articulate.ly"
\include "accbasschord.ly"
#(use-modules (lily accreg))

% Define a variable to hold the formatted date:
date = #(strftime "%d-%m-%Y" (localtime (current-time)))

\paper {
  #(set-paper-size "letter")
 
    print-page-number = ##t
    markup-system-spacing.basic-distance = 15
    top-margin = 0.5\in
    bottom-margin = 0.5\in
    % print-first-page-number = ##t
%     oddHeaderMarkup = \markup \null
%     evenHeaderMarkup = \markup \null
%     oddFooterMarkup = \markup {
%       \fill-line {
%         \on-the-fly \print-page-number-check-first
%         \fromproperty #'page:page-number-string
%       }
 %{    }
    %} evenFooterMarkup = \oddFooterMarkup

}
\header {
  % The following fields are centered
    %dedication = "Dedication"
    title = "Dmitr The Imp"
    %subtitle = "Subtitle"
    %subsubtitle = "Subsubtitle"
      % The following fields are evenly spread on one line
      % the field "instrument" also appears on following pages
    instrument = \markup \with-color #grey "Bayan"
    %poet = "Poet"
    composer = "Composed by Keith Smith"
      % The following fields are placed at opposite ends of the same line
    %meter = "Meter"
    %arranger = "Arranger"
      % The following fields are centered at the bottom
    tagline = \markup {
       Engraved at
       \simple #(strftime "%Y-%m-%d" (localtime (current-time)))
       with \with-url #"http://lilypond.org/"
       \line { LilyPond \simple #(lilypond-version) \title }
       }
    copyright =\markup { Copyright  "2023 Keith Smith"}
}

global = {
  \key c \major
  %\numericTimeSignature
  \time 4/4
  \tempo 4=120
}

voltaFine = \markup {  \text \bold {pour Fine}}

right = \relative {

  \global

  % Music follows here.

 \discant "10"
  % Part A - ? bars
   \repeat volta 2 {

|e''8.-3 dis16-2 e8.-3 d16-4 cis8.-3 c16-2 cis8.-3 b16-4  | a8.-2 gis16-4 a8.-3 fis16-2 e2-1  | fis8.-2 f16-4 fis8.-2 gis16-4 a8.-2 gis16-4 a8.-3 fis16-2 | e8.-3 d16-4 e8.-3 fis16-2 e2-1 |\break
|e'8.-3 dis16-2  e8.-3 d16-4  cis8.-3 c16 -2 cis8.-3 b16-4  |  cis8.-3 c16-2  cis8.-3 d16-4 cis2-3  | d8.-4 cis16-3 b8.-5 b16-4 a8.-2 gis16-4 a8.-2 b16-4 | a8.-3 fis16-2 d'8.-5 b16-4 a2-2 |\break
|b4-4 gis4-1 e'2-3|d8.-4 cis16-3 d8.-4 cis16-3 b2-4 |cis8.-3 c16-2 cis8.-3 d16-4 cis8.-3 b16-4 a8.-2 gis16-4 |
 } \alternative {{|a8.-2 gis16-4 a8.-2  b16-4 a2-2|}
                 {|a8.-2 gis16-4 a8.-2  b16-4 a2-2|}} 
 
\set Score.repeatCommands = #(list(list 'volta voltaFine) 'end-repeat)
                 {|a8.-2 cis16-3 e8.-4  gis16-5 <cis,-1 a'-3>2^"   fine"|}
\set Score.repeatCommands = #'((volta #f))
\break
 \discant "20"
  % Part B - ? bars - Darker
   \repeat volta 2 {
   |fis2.-4~fis8 e8-3  |cis2.-2 b4-1 |cis2.-2 e4-3 | cis1-2 |\break
   |fis2.-4~fis8 e8-3  |cis2.-2 e4-3 |cis1-2~| cis1 |\break
   |fis2.-4~fis8 e8-3  |cis2.-2 e4-3 |b8.-1 a16-2 b8.-1 a16-2 b4-1 cis4-3 |b1-1|\break
   |fis'4-4 e4-3 fis8-4 a4.-5 |gis2-1 a4-4 gis-1 | fis1-3~ |fis1|
 } \alternative {{|  |}
                 {|   |}} \break

  % Very Dark - evil
\discant "1"
 \repeat volta 2 {
  | gis,4-1^"1:54" <a-2 d-5>4-. f4-1 <e-2 a-3>4-. | d4-1 <cis-2 f-4>4-. e2-3 |  gis4-1 <a-2 d-5>4-. f4-1 <e-2 a-3>4-. |<g-3 ais-4>4 <e-2 g-3>4 <cis-1 e-2>2|\break
  | gis'4-1 <a-2 d-5>4-. f4-1 <e-2 a-3>4-. | d4-2 <cis-2 f-4>4-. e2-3 |  gis4-1 <a-2 d-5>4-. f4-1 <e-2 a-3>4-. |ais4-4 <d-1 g-5>4 <a-2 d-\3>2|\break
  |g4-3 e-2 a-3 f-1 | gis-4 e-2 a2-3 |g4-3 e-2 a-3 f-1 |<a-2 cis-4>4 <e-1 a-2>4 <a-2 d-5>2|
 }
 \break
  |ais,2-1( d-2 | f-3 ais-4 | a4-3 g4-2 f4-1 g4-2| a2.-3) \tuplet 3/2 {r8  a-2 b-1 }| <b-1 dis-3>2.   r4^"D.C al fine" |
  

  
}

left =  {
  \global
  % Music follows here.
  \autoBeamOff
  % Music follows here.

\repeat volta 2 {
|a,4 a4^"M" e, a|a,4 a^"M" e, a|d,4 d^"M" a, d|a,4 a^"M" e, a|
|a,4 a^"M" e, a|a,4 a^"M" e, a|d,4 d^"M" a, d|d,4 d^"M" a, d|
|e,4 e^"7" b, e|e,4 e^"7" b, e|a,4 a^"M" e, a|
 }\alternative {{| a,4 a^"M" e, a| }
                 {a,4 a^"M" e, a| }}  
\set Score.repeatCommands = #(list(list 'volta voltaFine) 'end-repeat)
                 {a,4 a^"M" <e, a>2|}
\set Score.repeatCommands = #'((volta #f))

% Part B - 16 bars
 \repeat volta 2 {
 |fis,4 fis^"m" cis, fis|fis,4 fis^"m" cis, fis|fis,4 fis^"m" cis, fis|fis,4 fis^"m" cis, e,|
|fis,4 fis^"m" cis, fis|fis,4 fis^"m" cis, fis|cis, cis'^"7" gis, cis' |cis,4 cis'^"7" gis, cis |
|fis,4 fis^"m" cis, fis|fis,4 fis^"m" cis, fis|b, b^"m" fis, b| b, b^"m" fis, b|
fis,4 fis^"m" cis, fis|cis,4 cis'^"7" gis, cis |fis,4 fis^"m" cis, fis|fis,4 fis^"m" cis, e,  |
 } \alternative {{|  |}
                 {|   |}} \break

  % TRIO - 32 bars
 \repeat volta 2 {
  | d,4 d^"m" a, d| d,4 d^"m" a, d| d,4 d^"m" a, d|g, gis,_"_" a, a^"7" |
  | d,4 d^"m" a, d| d,4 d^"m" a, d| d,4 d^"m" g, g^"m"| a, a^"7" <d, d>2^"m"|
  |g,4 g^"m" d, g|e, e^"7" a, a^"7" |g,4 g^"m" d, g|a, e, <d, d>2^"m"|
  
 }
 |g,4 g^"m" d, g||g,4 g^"m" d, g|a, a^"M" e, a |<a, a>1^"M" | <b, b>2.^"7" r4 |

}

MIDI_Chords = \aan-extract-chords \left
MIDI_Bass = \aan-extract-bass \left


\score {
  \new PianoStaff \with {

  } <<
    \new Staff = "Treble"  \right
    \new Staff = "AAN" \with {
        instrumentName = "AAN"
        shortInstrumentName = "AAN"
      }  { \clef bass \left }
    \new Staff = "chords" \with {
        instrumentName = "Chords"
        shortInstrumentName = "Chords"
      } { \clef bass \MIDI_Chords }
    \new Staff = "Bass"  \with {
        instrumentName = "Bass"
        shortInstrumentName = "Bass"
      }{ \clef bass  \MIDI_Bass}
    
  >>
  \layout { }

}




\score {
  \new PianoStaff \with {

  } \articulate <<
    \unfoldRepeats 
    \new Staff = "Treble" \with {
      midiInstrument = "flute"
      midiMinimumVolume = #0.2
      midiMaximumVolume = #0.9

    } \right
    
    \unfoldRepeats
    \new Staff = "Bass" \with {
      midiInstrument = "acoustic bass"
      midiMinimumVolume = #0.2
      midiMaximumVolume = #0.9
    }  {\clef bass \MIDI_Bass }
    
    \unfoldRepeats
    \new Staff = "Chords" \with {
      midiInstrument = "acoustic grand"
      midiMinimumVolume = #0.2
      midiMaximumVolume = #0.5
    }  { \clef bass \MIDI_Chords }
    
  >>

  \midi {
    \context {
      \Score
      midiChannelMapping = #'instrument
    }
  }
}

