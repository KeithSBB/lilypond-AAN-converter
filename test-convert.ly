\version "2.24.4"
\include "aaan-convert.ly"

% An unterminated slur, tie, or hairpin must fail this file.
#(ly:set-option 'warning-as-error #t)

% <e, a>^"M" keeps the E bass and spells A major on the chord row.
% The next a is an unmarked repeat of that same chord: no second cue,
% and in Russian no letter, because the source has no mark.
% m, 7, and dim are the other rows.  d^"M" sits on the middle line.
% c'^"M" is a lone chord with no root bass before it.
% a,4 a^"M" is a chord after its root bass.
% a,_"_" a^"M" is a chord after a counterbass.
% f^"M" f f^"M" is one run.  Russian prints Б, nothing, Б.
% g is unmarked the first time, so it is major and has no letter.
% cis is below the middle line, so it stays a bass note.
% des is on that line, so it is a chord and stays on the line.
% cis and des are one button, so that bass is the Db root and des has no cue.
% b^"M" b r b: the rest breaks the run, so the last b has a cue.
% e^"M" e e, e: the root bass arrives mid-run, so the last e has no cue.
% a^"M"(\< ~ a e,)\! starts a tie, a slur, and a hairpin on a cued chord.
sample = \absolute {
  <e, a>4^"M" a4 a4^"m" e4^"7" b4^"dim" d4^"M" c'4^"M"
  a,4 a4^"M" a,4_"_" a4^"M"
  f4^"M" f4 f4^"M" g4
  cis4 des4^"M"
  c,4-> r4
  b4^"M" b4 r4 b4
  e4^"M" e4 e,4 e4
  a4^"M"(\< ~ a4 e,4)\!
}

#(define (pitch->absolute p)
   (let ((oct (ly:pitch-octave p)))
     (string-append
      (list-ref '("c" "d" "e" "f" "g" "a" "b") (ly:pitch-notename p))
      (cond
       ((= (ly:pitch-alteration p) 0) "")
       ((= (ly:pitch-alteration p) 1/2) "is")
       ((= (ly:pitch-alteration p) -1/2) "es")
       ((= (ly:pitch-alteration p) 1) "isis")
       ((= (ly:pitch-alteration p) -1) "eses")
       (else (format #f "(~a)" (ly:pitch-alteration p))))
      (cond
       ((= oct 0) "'")
       ((> oct 0) (make-string oct #\'))
       ((= oct -1) "")
       (else (make-string (- -1 oct) #\,))))))

#(define (duration->number dur)
   (if (ly:duration? dur)
       (let ((base (expt 2 (ly:duration-log dur)))
             (dots (ly:duration-dot-count dur))
             (factor (ly:duration-scale dur)))
         (string-append
          (number->string base)
          (make-string dots #\.)
          (if (= factor 1) "" (format #f "*~a" factor))))
       "?"))

#(define (script-text event)
   (let ((text (ly:music-property event 'text)))
     (cond
      ((string? text) (string-append "\"" text "\""))
      ((markup? text) "[markup]")
      (else "?"))))

#(define (scripts-of music)
   (let* ((arts (ly:music-property music 'articulations))
          (elems (ly:music-property music 'elements))
          (all (append (if (pair? arts) arts '())
                       (if (pair? elems) elems '()))))
     (filter (lambda (a)
               (and (ly:music? a)
                    (memq (ly:music-property a 'name)
                          '(TextScriptEvent ArticulationEvent
                            TieEvent SlurEvent CrescendoEvent))))
             all)))

#(define (format-scripts music)
   (string-join
    (map (lambda (a)
           (cond
            ((eq? (ly:music-property a 'name) 'TextScriptEvent)
             (string-append (if (= (ly:music-property a 'direction) -1) "_" "^")
                            (script-text a)))
            ((eq? (ly:music-property a 'name) 'TieEvent) "~")
            ((eq? (ly:music-property a 'name) 'SlurEvent)
             (if (= (ly:music-property a 'span-direction) -1) "(" ")"))
            ((eq? (ly:music-property a 'name) 'CrescendoEvent)
             (if (= (ly:music-property a 'span-direction) -1) "\\<" "\\!"))
            ((eq? (ly:music-property a 'articulation-type) 'accent) "->")
            ((eq? (ly:music-property a 'articulation-type) 'staccato) "-.")
            (else (format #f "-~a" (ly:music-property a 'articulation-type)))))
         (scripts-of music))
    ""))

#(define (find-music pred music)
   (cond
    ((not (ly:music? music)) #f)
    ((pred music) music)
    (else
     (or (find-music pred (let ((el (ly:music-property music 'element)))
                            (if (ly:music? el) el #f)))
         (let loop ((els (let ((v (ly:music-property music 'elements)))
                           (if (pair? v) v '()))))
           (if (null? els)
               #f
               (or (find-music pred (car els))
                   (loop (cdr els)))))))))

#(define (parenthesized-note? music)
   (and (ly:music? music)
        (eq? (ly:music-property music 'name) 'NoteEvent)
        (let ((tweaks (ly:music-property music 'tweaks)))
          (and (pair? tweaks) (assq 'parenthesized tweaks)))))

#(define (event-chord? music)
   (and (ly:music? music)
        (eq? (ly:music-property music 'name) 'EventChord)))

#(define (summarize music)
   (cond
    ((not (ly:music? music)) "")
    ((eq? (ly:music-property music 'name) 'GraceMusic)
     (string-append "grace(" (summarize (ly:music-property music 'element)) ")"))
    ((eq? (ly:music-property music 'name) 'NoteEvent)
     (string-append (pitch->absolute (ly:music-property music 'pitch))
                    (duration->number (ly:music-property music 'duration))
                    (format-scripts music)))
    ((eq? (ly:music-property music 'name) 'EventChord)
     (let ((notes (filter (lambda (e)
                            (and (ly:music? e)
                                 (eq? (ly:music-property e 'name) 'NoteEvent)))
                          (ly:music-property music 'elements))))
       (string-append
        "<"
        (string-join (map (lambda (e)
                            (pitch->absolute (ly:music-property e 'pitch)))
                          notes)
                     " ")
        ">"
        (duration->number (ly:music-property (first notes) 'duration))
        (format-scripts music))))
    ((eq? (ly:music-property music 'name) 'RestEvent)
     (string-append "r" (duration->number (ly:music-property music 'duration))
                    (format-scripts music)))
    ((and (eq? (ly:music-property music 'name) 'SimultaneousMusic)
          (find-music parenthesized-note? music))
     (let ((cue (find-music parenthesized-note? music))
           (chord (find-music event-chord? music)))
       (string-append "cue("
                      (pitch->absolute (ly:music-property cue 'pitch))
                      ") "
                      (if chord (summarize chord) ""))))
    ((or (music-is-of-type? music 'sequential-music)
         (music-is-of-type? music 'simultaneous-music)
         (ly:music? (ly:music-property music 'element))
         (pair? (ly:music-property music 'elements)))
     (let ((el (ly:music-property music 'element))
           (els (ly:music-property music 'elements)))
       (string-join
        (filter (lambda (s) (and (string? s) (not (string=? s ""))))
                (append (if (ly:music? el) (list (summarize el)) '())
                        (if (pair? els) (map summarize els) '())))
        " ")))
    (else "")))

#(define (codepoints text)
   (if (string? text)
       (string-join (map (lambda (c) (format #f "U+~4,'0X" (char->integer c)))
                         (string->list text))
                    " ")
       (format #f "~s" text)))

#(define latin-lookalikes (string->list "BMYbm"))

#(define (latin-lookalike? text)
   (and (string? text)
        (string-any (lambda (c) (memv c latin-lookalikes)) text)))

#(define (audit-texts music label)
   (let ((hits '()))
     (let walk ((m music))
       (if (ly:music? m)
           (begin
             (if (eq? (ly:music-property m 'name) 'TextScriptEvent)
                 (let ((text (ly:music-property m 'text)))
                   (format #t "~a TEXT ~s ~a\n" label text (codepoints text))
                   (if (latin-lookalike? text)
                       (set! hits (cons text hits)))))
             (let ((el (ly:music-property m 'element)))
               (if (ly:music? el) (walk el)))
             (for-each walk (let ((els (ly:music-property m 'elements)))
                              (if (pair? els) els '())))
             (for-each walk (let ((arts (ly:music-property m 'articulations)))
                              (if (pair? arts) arts '()))))))
     hits))

#(define expected-english
   "<e, a cis' e'>4^\"M\" <a cis' e'>4 <a c' e'>4^\"m\" <e gis d'>4^\"7\" <b d' f'>4^\"dim\" <d fis a>4^\"M\" <e g c'>4^\"M\" a,4 <a cis' e'>4^\"M\" a,4_\"_\" <a cis' e'>4^\"M\" <f a c'>4^\"M\" <f a c'>4 <f a c'>4^\"M\" <g b d'>4 cis4 <des f aes>4^\"M\" c,4-> r4 <b dis' fis'>4^\"M\" <b dis' fis'>4 r4 <b dis' fis'>4 <e gis b>4^\"M\" <e gis b>4 e,4 <e gis b>4 <a cis' e'>4^\"M\"(\\<~ <a cis' e'>4 e,4)\\!")

#(define expected-russian
   "cue(a,) <e, a cis' e'>4^\"Б\" <a cis' e'>4 cue(a,) <a c' e'>4^\"М\" cue(e,) <e gis d'>4^\"7\" cue(b,) <b d' f'>4^\"У\" cue(d,) <d fis a>4^\"Б\" cue(c) <e g c'>4^\"Б\" a,4 <a cis' e'>4^\"Б\" a,4_\"В\" cue(a,) <a cis' e'>4^\"Б\" cue(f,) <f a c'>4^\"Б\" <f a c'>4 <f a c'>4^\"Б\" cue(g,) <g b d'>4 cis4 <des f aes>4^\"Б\" c,4-> r4 cue(b,) <b dis' fis'>4^\"Б\" <b dis' fis'>4 r4 cue(b,) <b dis' fis'>4 cue(e,) <e gis b>4^\"Б\" <e gis b>4 e,4 <e gis b>4 cue(a,) <a cis' e'>4^\"Б\"(\\<~ <a cis' e'>4 e,4)\\!")

englishMusic = \aaan-convert \sample

#(define (report label music expected . rest)
   (let ((source (if (pair? rest) (car rest) sample))
         (actual (summarize music)))
     (format #t "\n===== ~a summary =====\n" label)
     (format #t "EXPECTED: ~a\n" expected)
     (format #t "ACTUAL:   ~a\n" actual)
     (format #t "LENGTH source=~a converted=~a\n"
             (ly:music-length source) (ly:music-length music))
     (format #t "\n===== ~a displayLilyMusic =====\n" label)
     (display-lily-music music)
     (newline)
     (if (not (equal? (ly:music-length source) (ly:music-length music)))
         (error "aaan-convert changed the music length"))
     (if (not (string=? actual expected))
         (error (format #f "~a summary mismatch" label)))))

#(report "english" englishMusic expected-english)

\aaanLanguage #'russian
russianMusic = \aaan-convert \sample

#(report "russian" russianMusic expected-russian)
#(let ((hits (audit-texts russianMusic "russian")))
   (if (pair? hits)
       (error (format #f "Latin look-alike in Russian marks: ~s" hits))
       (format #t "russian LATIN-LOOKALIKE none\n")))

% README run.  c, is the root bass, so the first two C chords have no cue.
% The rest starts a new run.  e,_"_" is a counterbass, not a C root bass.
readmeRun = \absolute { c,4 c'4^"M" c'4 r4 | c'4 c'4 e,4_"_" c'4 }
readmeRussian = \aaan-convert \readmeRun
#(report "readme run"
   readmeRussian
   "c,4 <e g c'>4^\"Б\" <e g c'>4 r4 cue(c) <e g c'>4 <e g c'>4 e,4_\"В\" cue(c) <e g c'>4"
   readmeRun)

% A tie and a bar line stay inside the run.  The bar is full, so a
% bar check does not warn, and the chord after the bar is not cued.
tieRun = \absolute { a4^"M" ~ a4 a4 a4 | a4 }
tieRussian = \aaan-convert \tieRun
#(report "tie across bar"
   tieRussian
   "cue(a,) <a cis' e'>4^\"Б\"~ <a cis' e'>4 <a cis' e'>4 <a cis' e'>4 <a cis' e'>4"
   tieRun)

% The end of << >> breaks a run.  The { } inside one part does not.
splitRun = \absolute { c'4^"M" << { c'4 { c'4 } } \\ { r4 } >> c'4 }
splitRussian = \aaan-convert \splitRun
#(report "end of simultaneous"
   splitRussian
   "cue(c) <e g c'>4^\"Б\" <e g c'>4 <e g c'>4 r4 cue(c) <e g c'>4"
   splitRun)

% A bass inside the same <> does not cue a chord that continues the run.
bassInside = \absolute { a4^"M" <e, a>4^"M" }
bassInsideRussian = \aaan-convert \bassInside
#(report "bass inside a run"
   bassInsideRussian
   "cue(a,) <a cis' e'>4^\"Б\" <e, a cis' e'>4^\"Б\""
   bassInside)

% cis and des are one button.
enharmonic = \absolute { cis'4^"M" des'4^"M" }
enharmonicRussian = \aaan-convert \enharmonic
#(report "enharmonic button"
   enharmonicRussian
   "cue(cis) <eis gis cis'>4^\"Б\" <des f aes>4^\"Б\""
   enharmonic)

% A counterbass is not the root bass, so the chord is cued.
counterStart = \absolute { a,4_"_" a4 }
counterRussian = \aaan-convert \counterStart
#(report "counterbass then chord"
   counterRussian
   "a,4_\"В\" cue(a,) <a cis' e'>4"
   counterStart)

#(let ((hits (audit-texts readmeRussian "readme")))
   (if (pair? hits)
       (error (format #f "Latin look-alike in readme run: ~s" hits))))

% The C cue is MIDI note 48.  This score has no sounding C3, so a cue that
% plays would show up as note 48.
midiCue = \aaan-convert \absolute { c'1^"M" }

\score {
  \new Staff \with { instrumentName = "english" }
  { \clef bass \englishMusic }
  \header { piece = "english" }
  \layout { indent = 0.6\in }
}

\score {
  \new Staff \with { instrumentName = "russian" }
  { \clef bass \russianMusic }
  \header { piece = "russian" }
  \layout { indent = 0.6\in }
}

\score {
  \new Staff { \clef bass \midiCue }
  \midi { }
  \layout { indent = 0 }
}

\score {
  \new Staff {
    \clef bass
    \readmeRussian
    \tieRussian
    \splitRussian
    \bassInsideRussian
  }
  \header { piece = "runs" }
  \layout { indent = 0.4\in }
}
