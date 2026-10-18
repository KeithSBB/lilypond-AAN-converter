# lilypond-AAAN-converter
## Scheme code to convert American Accordionists Association Notation music into fully spelled out accordion chords.

American Accordionists Association Notation (AAAN) is a bass-clef shorthand for Stradella left hand. This repository turns that shorthand into separate bass and chord voices.  It also provides a conversion to fully spelled out chords and optional translation into Russian.

One include:

| Include | Use |
| --- | --- |
| `aaan-convert.ly` | Print. Spelled chords plus chord symbols. Articulations come only from the source. |

| Function | Description|
| --- | --- |
|\aaan-extract-bass| Extracts bass notes from **AAAN** leaving rests for chords|
|\aaan-extract-chords| Extracts **AAAN** chords, spells them out using lilypond chord (<>), and leaves rests for bass notes|
|\aaan-convert| Replaces **AAAN** chord notes with fully spelled out chords.  Bass notes are passed through.  If \aaanLanguage #'russian is set then replaces chord symbols with Russian equivalent and adds cue marks.|
|\aaan-text| If \aaanLanguage #'russian then words in the translation table are replaced by their Russian equivalents and other text is left as written, otherwise text is passed through unaltered|

**Example**
```lilypond
\include "aaan-convert.ly"
\aaanLanguage #'russian
inRussian = \aaan-convert { ... AAAN music ... }

midiBass = \aaan-extract-bass [staccato bool] { ... AAAN music ... }
midiChords = \aaan-extract-chords [staccato bool] { ... AAAN music ... }
```

## American Accordionists Association Notation

**American Accordionists Association Notation (AAAN)** is the standard system for notating music for the piano accordion, approved in 1938.  It is designed to simplify the Stradella bass system by representing complex left-hand chords with a single note and a symbol.

### Key Notation Rules:

* **Staff Position:** Notes written below the middle line (D) of the bass clef staff represent single bass notes (fundamental or counterbass). Notes written on or above the middle line represent chord buttons. 
* **Counterbass:** A bass note with a down-text underscore script, `_"_"`, is a counterbass. The row is not inferred.
* **Chord Symbols:** A letter or number above the notehead indicates the chord quality:
  
* **M**: **Major chord**. An unmarked chord note uses the type most recently defined for that chord root earlier in the music, and the first time it appears it is major.
  
* **m**: **Minor chord.**
  
* **7**: **Dominant 7th chord**.
  
* **d or dim**: **Diminished chord**.
  
**Visual Example:** Two **C** notes, one below the middle line (D) and one above it, written `<c, c'>^"M"`. The lower note is the bass and the upper note is the chord button: the **C bass button** and the **C major chord button** together, with **M** above the chord.

This notation allows musicians to read accompaniment patterns efficiently, as a single symbol triggers the full chord on the left-hand buttons. While common in the **US**, other regions may use different methods, such as writing out all chord notes or using lead sheet notation. 

### MIDI staccato

In accordion music staccato is usually implied and not written out.

`##t` applies staccato to the extracted MIDI voice. It does not belong on an engraved staff. Omit it, or use \aaan-convert, to keep the source articulation.

```lilypond
\aaan-extract-bass ##t { ... AAAN music ... }
\aaan-extract-chords ##t { ... AAAN music ... }

\aaan-extract-bass { ... AAAN music ... }
```

## Conversion for Engraving
Calling \aaan-convert on **AAAN** music replaces the upper bass staff chord notes with a fully spelled out accordion chord. When an AAAN chord note sounds together with a bass note, e.g. `<e, a>^"M"`, \aaan-convert keeps the bass and expands the chord note in place, giving `<e, a cis' e'>`. An unmarked chord note uses the type most recently defined for that chord root earlier in the music, and the first time it appears it is major. If `\aaanLanguage #'english` is set or omitted, then any notation on the **AAAN** is added to the fully spelled out chord. Everything else is passed through, including the lower bass clef staff bass notes.  

**Accordion chords**

Accordion chords are always three notes

* **M**: <root, major third, fifth> inversion applied so all chord notes sit on or above the middle staff line (D)
* **m**: <root, minor third, fifth> inversion applied so all chord notes sit on or above the middle staff line (D)
* **7**: <root, major third, minor seventh> (the fifth is omitted so the chord stays three notes), inversion applied so all chord notes sit on or above the middle staff line (D)
* **d**: <root, minor third, diminished fifth> inversion applied so all chord notes sit on or above the middle staff line (D)


### Russian text

- **Chord letters:** **Б** major, **М** minor, **7**, **У** diminished, placed over the spelled chord. They replace source marks only, so an unmarked chord gets no letter. Pitch names are not printed.
- **Counterbass:** `\aaan-convert` prints the source `_"_"` as the Cyrillic «В» (U+0412) under the bass note, never Latin B. English mode passes `_"_"` through unchanged.
- **Cue:** a small parenthesized root notehead under the chord, at zero duration (it takes no time, does not change the rhythm, and is silent in MIDI). Its pitch is the root at the nearest step below the middle line D, so C, C# and Cb sit at `c` and every other root at the comma octave.
- **When a cue appears:** on the first chord of a run, unless that chord's root bass was just played, meaning the root as a fundamental bass (not a counterbass) either immediately before with no rest between or inside the same `<>`.
- **Run:** consecutive chords with the same button, meaning the same root (enharmonics like C# and Db are the same) and the same type. Bar lines and ties do not break a run. A run is broken by a different chord, a separate bass note (fundamental or counterbass), a rest, a skip or full-bar rest, or the end of a simultaneous `<< >>` block. A bass note inside the same chord, like the `e,` in `<e, a>`, does not break the run. If that chord starts a run, it is cued unless the bass is its root.
- **Example:** absolute pitches, input `c,4 c'4^"M" c'4 r4 | c'4 c'4 e,4_"_" c'4`. Russian output: **Б** only on bar 1 beat 2 (the one source mark), no cue on chord 1 (its root bass was just played), no cue on chord 2 (not first in its run), a cue on bar 2 beat 1 (the rest broke the run), none on beat 2, В under `e,`, and a cue on beat 4.

**\aaan-text**
This function translates certain words to Russian if `\aaanLanguage #'russian` is set, otherwise it just passes through the text.
Except for the copyright, translation can be a simple text look-up:

|English|Russian|
| --- | --- |
|Bayan|Баян|
|Composed by Keith Smith|К. Смит|
|Moderately|Умеренно|
|Intro|Вступление|
|Slowly|Медленно|
|Unhurried|Не спеша|
|Leisurely|Неторопливо|
|Moderately fast|Умеренно скоро|
|Lively|Живо|
|With motion|Подвижно|
|Animated|Оживлённо|
|Fast|Быстро|
|Very fast|Очень быстро|
|Calmly|Спокойно|
|Broadly|Широко|
|Merrily|Весело|
|Cheerfully|Бодро|
|With restraint|Сдержанно|
|Singing|Певуче|
|Melodiously|Напевно|
|March tempo|В темпе марша|
|Waltz tempo|В темпе вальса|
|Chorus|Припев|
|Variation|Вариация|
|Instrumental break|Отыгрыш|
|Arranged by Keith Smith|Обработка К. Смита|
|Bass|Бас|
|Copyright 2026 Keith Smith|© Кит Смит, 2026|

**Note:** the words Keith Smith plus a standalone 4-digit year, optionally led by "Copyright", print © Кит Смит, <year>. Any other form (e.g. "Copyright (c) 2026 Keith Smith" or another holder) passes through unchanged.

Italian dynamics and navigation marks (`f`, `p`, `fine`, `D.C.`, `rit.`) are never translated, and Italian terms like Moderato are never transliterated and pass through unchanged (no `Аллегро`/`Модерато`). Words passed through `\aaan-text` may be replaced by a real Russian equivalent from the table (e.g. `Moderately` → `Умеренно`, `Bayan` → `Баян`). 
```lilypond
\include "aaan-convert.ly"

\aaanLanguage #'russian  % defaults to english if not present

\header {
  title = "Dmitr The Imp"          % not translated
  instrument = \aaan-text "Bayan"   % Баян
  composer = \aaan-text "Composed by Keith Smith"  % К. Смит
}

russianLeft = \aaan-convert \left
```

