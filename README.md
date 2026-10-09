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

A counterbass is marked in the source as a down-text underscore, `e_"_"`. Engraving rewrites that mark to `e_"B"`. No other bass is treated as a counterbass.

When the bass pitch class differs from the chord root, a stemless `(.)` is attached to that chord on the root's staff position. It is a text script, not a note, so it does not add time or move the barline. Bass notes and the cue stay on or below the bass-clef center line; spelled chord notes stay above it. A cue below the staff gets ledger lines. The row is taken from the written bass, not guessed.

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

### Russian text, title excepted

`\aanLanguage #'russian` switches the chord mark to the Stradella row letter only, placed over the spelled chord: **Б** major, **М** minor, **7**, **У** diminished. Pitch names are not printed. `7sus2` stays `7sus2` because it is not a Stradella row.

Dynamics and Italian navigation stay Italian (`f`, `p`, `fine`, `D.C.`, `rit.`). Prose passed through `\aan-text` or `\aan-translate` can still change (`Bayan` → `Баян`, `Moderato` → `Умеренно`). The song title is not translated. Write it as a literal `\header` field. If the same string also appears in the music, register it with `\aanProtectTitle`.

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

`testsong-engrave.ly` is the print fixture. English symbols are `A7`, `C`, `Adim`, `A`. Russian symbols are `7`, `Б`, `У`, `Б`. The source `e,_"_"` engraves as `e_"B"`. A chord whose bass is a different pitch class has its root notehead parenthesized. `fine` stays Italian, and the title is unchanged.
