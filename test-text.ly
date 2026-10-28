\version "2.24.4"
\include "aaan-convert.ly"

% \aaan-text is one alist and one lookup.  The default language is
% English, so text passes through until \aaanLanguage #'russian.
% Italian tempo words, including Moderato, are not transliterated.
% A different rights holder, or a copyright line with no year, passes through.

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
     ("Arranged by Keith Smith" . "Обработка К. Смита")
     ("Lively" . "Живо")
     ("With motion" . "Подвижно")
     ("Bass" . "Бас")))

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
% A year range prints the first year only, as © plus the name, and does
% not print the word Copyright.  The same line without that word matches.
#(expect-text (shown-text "Copyright 2018-2024 Keith Smith")
              "© Кит Смит, 2018")
#(expect-text (shown-text "2018-2024 Keith Smith")
              "© Кит Смит, 2018")
% A literal (c) is an extra word, so the line is not the copyright form.
#(expect-text (shown-text "Copyright (c) 2026 Keith Smith")
              "Copyright (c) 2026 Keith Smith")
#(expect-text (shown-text "Copyright 2026 Ada Lovelace")
              "Copyright 2026 Ada Lovelace")
#(expect-text (shown-text "Copyright Keith Smith")
              "Copyright Keith Smith")

% Every Russian table row is built while the language is russian and
% placed in the score header below.
#(define russian-header-markups
   (map (lambda (pair) (cons (car pair) (aaan-text (car pair))))
        aaan-text-dictionary))
#(for-each
  (lambda (pair)
    (expect-text (string-concatenate (markup-strings (cdr pair)))
                 (assoc-ref aaan-text-dictionary (car pair))))
  russian-header-markups)
copyrightRange = \aaan-text "Copyright 2018-2024 Keith Smith"
#(expect-text (string-concatenate (markup-strings copyrightRange))
              "© Кит Смит, 2018")

\header {
  title = "aaan-text"
  instrument = #(assoc-ref russian-header-markups "Bayan")
  composer = #(assoc-ref russian-header-markups "Composed by Keith Smith")
  copyright = \copyrightRange
  subtitle = \markup \column #(map cdr russian-header-markups)
  tagline = ##f
}

\aaanLanguage #'english
#(expect-text (shown-text "Bayan") "Bayan")
#(expect-text (shown-text "Lively") "Lively")
#(expect-text (shown-text "Copyright 2026-10-1 \"Keith Smith\"")
              "Copyright 2026-10-1 \"Keith Smith\"")

\score {
  { s1 }
  \layout { }
}
