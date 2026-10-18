\version "2.24.4"
\include "aaan-convert.ly"

% <e, a>^"M" keeps the E bass and spells A major on the chord row.
% The next a is an unmarked repeat of that major, and the start of a run.
% m, 7, and dim are the other rows.  d^"M" sits on the middle line.
% c'^"M" is a lone chord with no root bass before it.
% a,4 a^"M" is a chord after its root bass.
% a,_"_" a^"M" is a chord after a counterbass.
% f^"M" f f^"M" is one run: the cue belongs only on the first.
% g is unmarked the first time, so it is major.
sample = \absolute {
  <e, a>4^"M" a4 a4^"m" e4^"7" b4^"dim" d4^"M" c'4^"M"
  a,4 a4^"M" a,4_"_" a4^"M"
  f4^"M" f4 f4^"M" g4
  c,4-> r4
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
                    (or (eq? (ly:music-property a 'name) 'TextScriptEvent)
                        (eq? (ly:music-property a 'name) 'ArticulationEvent))))
             all)))

#(define (format-scripts music)
   (string-join
    (map (lambda (a)
           (cond
            ((eq? (ly:music-property a 'name) 'TextScriptEvent)
             (string-append (if (= (ly:music-property a 'direction) -1) "_" "^")
                            (script-text a)))
            ((eq? (ly:music-property a 'articulation-type) 'accent) "->")
            ((eq? (ly:music-property a 'articulation-type) 'staccato) "-.")
            (else (format #f "-~a" (ly:music-property a 'articulation-type)))))
         (scripts-of music))
    ""))

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
   "<e, a cis' e'>4^\"M\" <a cis' e'>4 <a c' e'>4^\"m\" <e gis d'>4^\"7\" <b d' f'>4^\"dim\" <d fis a>4^\"M\" <e g c'>4^\"M\" a,4 <a cis' e'>4^\"M\" a,4_\"_\" <a cis' e'>4^\"M\" <f a c'>4^\"M\" <f a c'>4 <f a c'>4^\"M\" <g b d'>4 c,4-> r4")

#(define expected-russian
   "grace(a,8) <e, a cis' e'>4^\"Б\" <a cis' e'>4^\"Б\" grace(a,8) <a c' e'>4^\"М\" grace(e,8) <e gis d'>4^\"7\" grace(b,8) <b d' f'>4^\"У\" grace(d,8) <d fis a>4^\"Б\" grace(c8) <e g c'>4^\"Б\" a,4 <a cis' e'>4^\"Б\" a,4_\"В\" grace(a,8) <a cis' e'>4^\"Б\" grace(f,8) <f a c'>4^\"Б\" <f a c'>4^\"Б\" <f a c'>4^\"Б\" grace(g,8) <g b d'>4^\"Б\" c,4-> r4")

englishMusic = \aaan-convert \sample

#(define (report label music expected)
   (let ((actual (summarize music)))
     (format #t "\n===== ~a summary =====\n" label)
     (format #t "EXPECTED: ~a\n" expected)
     (format #t "ACTUAL:   ~a\n" actual)
     (format #t "LENGTH sample=~a converted=~a\n"
             (ly:music-length sample) (ly:music-length music))
     (format #t "\n===== ~a displayLilyMusic =====\n" label)
     (display-lily-music music)
     (newline)
     (if (not (equal? (ly:music-length sample) (ly:music-length music)))
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
