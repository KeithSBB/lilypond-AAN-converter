# lilypond-AAN-converter
## Scheme code to convert American Accordion Notation music into fully spelled out accordion chords.
This script has two main scheme functions for converting from American 
Accordion Notation to bass note voice and fully spelled out accordion
chords;

       \include "accbasschord.ly"

       \aan-extract-bass {... AAN music ...}  "Places rests at chords"
       
       \aan-extract-chords (... AAN music ...} "spells out chords and places rests at bass notes"

## American Accordion Notation
AAN is a specialized notation for accordion sheet music.  Specifically the stradella bass which on a typical accordion has major, minor, seventh and diminished keys.  Notes placed below the middle staff line of the bass clef (c# and lower) are intepreted as bass notes.   Notes on the middle bass cleff staff line are annotated with a maj, min, 7 or dim text string and are interpreted as the stradella chord. 
This notation makes writing music for the accordion easier and quicker, however midi produced from lilypond will not sound the chords and if you wanted fully spelled out chords you need to re-write the bass clef.
The accbasschord.ly lilypond scheme file contains functions which convert  The AAN bass cleff music into bass notes and fully spelled out accordion chords.  The resulting music can then be used to generate midi files and/or engrave fully spelled out bass and chords.
### The stradella bass chords are triads where
* maj: root, maj-3rd, 5th
* min: root, min-3rd, 5th
* 7: root, maj-3rd, min-7th   (omits the 5th)
* dim: root, min-3rd, maj-6th (bb7)
* Note that an augmented chord can be played by playing a 7 chord and the augented 5th in th bass
* Many other chord combinations can be made to play more complex chords.

## Syntax examples
C major chord: c'^"M", or  c'^"maj"
d minor chord: d^"m", ot d^"min"
e dominate seventh chord: e^"7"
g diminished chord: g^"d", g^"dim",or g^"o"
