# lilypond-AAN-converter
## Scheme code to convert American Accordion Notation music into fully spelled out accordion chords.

American Accordion Notation (AAN, also called AAA) is a bass-clef shorthand for Stradella left hand. This repository turns that shorthand into separate bass and chord voices.

Two includes, two jobs:

| Include | Use |
| --- | --- |
| `accbasschord.ly` | MIDI. Optional staccato, because left-hand accordion playing often defaults to it. |
| `aan-engrave.ly` | Print. Spelled chords plus chord symbols. Articulations come only from the source. |

`aan-engrave.ly` includes the MIDI functions, so a print file can still call `\aan-extract-bass` and `\aan-extract-chords`.

```lilypond
\include "accbasschord.ly"

\aan-extract-bass [staccato bool] { ... AAN music ... }
\aan-extract-chords [staccato bool] { ... AAN music ... }
```

## American Accordion Notation

Notes on or below C♯3 (below the middle line of the bass staff) are bass-row notes. Notes on the middle line and above, annotated with a quality, are Stradella chord buttons.

* maj: root, major 3rd, 5th
* min: root, minor 3rd, 5th
* 7: root, major 3rd, minor 7th (the 5th is omitted)
* dim: root, minor 3rd, diminished 7th
* An augmented sonority is a 7 chord with the augmented 5th in the bass
* `7sus2` is accepted (root, 2nd, 5th, minor 7th) but is not a Stradella row

A omitted quality reuses the last quality written for that pitch class.

### Syntax examples

C major chord: `c'^"M"` or `c'^"maj"`

D minor chord: `d^"m"` or `d^"min"`

E dominant seventh: `e^"7"`

G diminished: `g^"d"`, `g^"dim"`, or `g^"o"`

### MIDI staccato

`##t` applies staccato to the extracted MIDI voice. It does not belong on an engraved staff. Omit it, or use the engraving functions, to keep the source articulation.

```lilypond
\aan-extract-bass ##t { ... AAN music ... }
\aan-extract-chords ##t { ... AAN music ... }

\aan-extract-bass { ... AAN music ... }
```

## Engraving

`\aan-engrave-bass` keeps bass-row notes and rests the chord row. `\aan-engrave-chords` spells the chord row in bass clef and writes a chord symbol above each chord attack. A tied continuation does not get a second symbol. Written staccato, accents, and other text stay; the quality token (`"M"`, `"min"`, `"7"`, `"dim"`) is replaced by the symbol.

```lilypond
\include "aan-engrave.ly"

\score {
  <<
    \new Staff { \clef bass \aan-engrave-bass \left }
    \new Staff { \clef bass \aan-engrave-chords \left }
  >>
  \layout { }
}
```

English symbols are lead-sheet names from the written root: `C`, `Cm`, `G7`, `Cdim`, `C7sus2`. Flats and sharps follow the AAN spelling (`ees^"M"` is `Eb`).

Apply these functions to absolute music, or outside `\relative`, same as the MIDI extractors.

### Russian text, title excepted

`\aanLanguage #'russian` switches chord symbols and the text pass. Bayan textbook suffixes are Б (major), М (minor), 7, and У (diminished): `ДоБ`, `РеМ`, `Соль7`, `ЛяУ`. Pitch names are До, Ре, Ми, Фа, Соль, Ля, Си, with `-диез` and `-бемоль`.

The song title is not translated. Write it as a literal `\header` field. If the same string also appears in the music, register it with `\aanProtectTitle`.

`\aan-text` is the opt-in for header and markup strings. `\aan-translate` walks text scripts, lyrics, and marks. Generated chord symbols are skipped. Text that is not in the dictionary is left unchanged and warned once.

```lilypond
\include "aan-engrave.ly"
\aanProtectTitle "Dmitr The Imp"
\aanLanguage #'russian

\header {
  title = "Dmitr The Imp"          % not translated
  instrument = \aan-text "Bayan"   % Баян
  composer = \aan-text "Composed by Keith Smith"
}

russianChords = \aan-translate \aan-engrave-chords \left
```

Set the language before the engraving call. A music variable already built in English keeps the symbols it was built with.

`testsong-engrave.ly` is the print fixture: English `A7`, `C`, `Adim`, then Russian `Ля7`, `ДоБ`, `ЛяУ`, with `fine` and `D.C. al fine` translated and the title unchanged.
