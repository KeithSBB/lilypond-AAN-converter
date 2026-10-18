\version "2.24.4"
\include "aaan-convert.ly"

% \aaan-text is one alist and one lookup.  The default language is
% English, so text passes through until \aaanLanguage #'russian.
% Two of the extra table terms stand in for the rest.  Italian tempo
% words, including Moderato, are not transliterated.

#(define (markup-strings value)
   (cond
    ((string? value) (list value))
    ((pair? value)
     (append (markup-strings (car value))
             (markup-strings (cdr value))))
    (else '())))

#(define (shown-text str)
   (string-concatenate (markup-strings (aaan-text str))))

#(define (expect-text actual expected)
   (if (equal? actual expected)
       (format #t "text OK ~s\n" expected)
       (ly:error "text mismatch: expected ~s, got ~s" expected actual)))

% No \aaanLanguage yet.
#(expect-text (shown-text "Bayan") "Bayan")

#(define text-cases
   '(("Bayan" . "Баян")
     ("Composed by Keith Smith" . "К. Смит")
     ("Moderately" . "Умеренно")
     ("Intro" . "Вступление")
     ("Chorus" . "Припев")
     ("Arranged by Keith Smith" . "Обработка К. Смита")))

\aaanLanguage #'russian
#(for-each
  (lambda (pair)
    (expect-text (shown-text (car pair)) (cdr pair)))
  text-cases)
#(expect-text (shown-text "Moderato") "Moderato")
#(expect-text (shown-text "Allegro") "Allegro")
#(expect-text (shown-text "Andante") "Andante")
#(expect-text (shown-text "Fine") "Fine")
#(expect-text (shown-text "D.C.") "D.C.")
#(expect-text (shown-text "rit.") "rit.")
#(expect-text (shown-text "f") "f")
#(expect-text (shown-text "Dmitr The Imp") "Dmitr The Imp")
#(expect-text (shown-text "Copyright 2026-10-1 \"Keith Smith\"")
              "© Кит Смит, 2026")
#(expect-text (shown-text "Copyright 2023 Keith Smith")
              "© Кит Смит, 2023")

\header {
  title = "aaan-text"
  instrument = \aaan-text "Bayan"
  composer = \aaan-text "Composed by Keith Smith"
  copyright = \aaan-text "Copyright 2026-10-1 \"Keith Smith\""
  tagline = ##f
}

\score {
  { s1 }
  \layout { }
}
