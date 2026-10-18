# lilypond-AAAN-converter
## Scheme code to convert American Accordion Association Notation music into fully spelled out accordion chords.

American Accordion Association Notation ( AAAN) is a bass-clef shorthand for Stradella left hand. This repository turns that shorthand into separate bass and chord voices.  It also provides a conversion to fully spelled out chords and optional translation into Russian

One include:

| Include | Use |
| --- | --- |
| `aaan-convert.ly` | Print. Spelled chords plus chord symbols. Articulations come only from the source. |

| Function | Description|
| --- | --- |
|\aaan-extract-bass| Extracts bass notes from **AAAN** leaving rests for chords|
|\aaan-extract-chords| Extracts **AAAN** chords, spells them out using lilypond chord (<>), and leaves rests for bass notes|
|\aaan-convert| Replaces **AAAN** chord notes with fully spelled out chords.  Bass notes are passed through.  If \aaanLanguage #'russian is set then replaces chord symbols with russian equivalent and adds cue marks.|
|\aaan-text| If \aaanLanguage #'russian then text converted to Russian, otherwise text is passed through unaltered|

**Example**
```lilypond
\include "aaan-convert.ly"
\aaanLanguage #'russian
inRussian = \aaan-convert { ... AAAN music ... }

midiBass = \aaan-extract-bass [staccato bool] { ... AAAN music ... }
midiChords = \aaan-extract-chords [staccato bool] { ... AAAN music ... }
```

## American Accordionists Association Notation

**American Accordionists Association  Notation (AAAN)** is the standard system for notating music for the piano accordion, approved in 1938.  It is designed to simplify the Stradella bass system by representing complex left-hand chords with a single note and a symbol.

### Key Notation Rules:

* **Staff Position:** Notes written below the middle line (B) of the bass clef staff represent single bass notes (fundamental or counter-bass). Notes written on or above the middle line represent chord buttons. 
* **Chord Symbols:** A letter or number above the notehead indicates the chord quality:
  
* **M** (or no letter): **Major chord**.
  
* **m**: **Minor chord.**
  
* **7**: **Dominant 7th chord**.
  
* **d or dim**: **Diminished chord**.
  
**Visual Example:** Two **C** notes, one above and one below the middle line in a chord(**<C, C'>^"M"**) with an **M** above it indicates playing the **C bass button** and the **C major chord button** simultaneously.

This notation allows musicians to read accompaniment patterns efficiently, as a single symbol triggers the full chord on the left-hand buttons. While common in the **US**, other regions may use different methods, such as writing out all chord notes or using lead sheet notation. 

### MIDI staccato

In accordion music staccato is usually implied and not written out.

`##t` applies staccato to the extracted MIDI voice. It does not belong on an engraved staff. Omit it, or use the engraving functions, to keep the source articulation.

```lilypond
\aaan-extract-bass ##t { ... AAAN music ... }
\aaan-extract-chords ##t { ... AAAN music ... }

\aaan-extract-bass { ... AAAN music ... }
```

## Conversion for Engraving
Calling \aaan-convert on **AAAN** music replaces the upper bass staff chord notes with a fully spelled out accordion chord. If the **AAAN** chord note was inside a chord already (usually with a lower staff bass note) then the fully spelled out chord notes are simply added to the existing chord with the base.   If `\aanLanguage #'english` or is omitted then any notation to the **AAAN** is added to the fully spelled out chord.  Everything else is passed through including the lower bass clef staff bass notes.  

**Accordion chords**

Accordion chords are always three notes

* **M**: <root, major third, fifth> inversion applied to keep all chord notes above middle staff line
* **m**: <root, minor third, fifth> inversion applied to keep all chord notes above middle staff line
* **7**: <root, major third>, minor seventh> inversion applied to keep all chord notes above middle staff line
* **d**: <root, minor third, diminished fifth> inversion applied to keep all chord notes above middle staff line


### Russian text

`\aanLanguage #'russian` switches the chord mark to the Stradella row letter only, placed over the spelled chord: **Б** major, **М** minor, **7**, **У** diminished. Pitch names are not printed. It also adds cue marks (`(o)`) where appropriate

In Russian mode the **AAAN** score is converted to fully spelled out chords with Russian language chord marks and a **(o)** cur mark placed under chords that are not preceded, or in  a chord with, the chord root note. 

**Note**: This is a simple replacement of the **AAAN** lilypond music similar to what \aaan-engrave-chords performs only bass notes are not replaced with rests and are included in the fully spelled out chord if the bass note and upper staff **AAAN** note(s) were composed as a chord (play bass and chord simultaneously).  The **AAAN's** chord symbol is simply replaced with the Russian equivalent.  The only additional addition is the cue marks being added.  The cue marks have no duration and thus have no impact on the other notes in the AAA Notation

**\aan-text**
This function translates certain words to Russian if `\aanLanguage #'russian` is set, otherwise it just passes through the text.
Except for the copyright, translation can be a simple text look-up:

|English|Russian|
| --- | --- |
|Bayan|Баян|
|Composed by Keith Smith|Музыка: Кит Смит|
|Moderato|Умеренно|
|Intro|Интро|
|[*any additional common terms*]|[*Russian equivalents*]|
|Copyright  2026-10-1 "Keith Smith"|Авторское право © 2026 «Keith Smith»| 

**Note:** only year used in copyright translation


Dynamics and Italian navigation stay Italian (`f`, `p`, `fine`, `D.C.`, `rit.`). Prose passed through `\aaan-text`  can still change (`Bayan` → `Баян`, `Moderato` → `Умеренно`). 
```lilypond
\include "aaan-convert.ly"

\aaanLanguage #'russian  % defaults to english if not present

\header {
  title = "Dmitr The Imp"          % not translated
  instrument = \aaan-text "Bayan"   % Баян
  composer = \aaan-text "Composed by Keith Smith"  % Музыка: Кит Смит
}

russianLeft = \aaan-convert \left
```

