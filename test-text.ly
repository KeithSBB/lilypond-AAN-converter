\version "2.24.4"
\include "aaan-convert.ly"

% \aaan-text is one alist and one lookup.  Russian mode returns the
% table value.  English mode returns the input.  A string that is not
% in the table, including Italian marks, passes through.  Copyright is
% the one line that keeps the year from the input.

#(define (markup-strings value)
   (cond
    ((string? value) (list value))
    ((pair? value)
     (append (markup-strings (car value))
             (markup-strings (cdr value))))
    (else '())))

#(define (aaan-text-string str)
   (string-concatenate (markup-strings (aaan-text str))))

#(define (aaan-check actual expected)
   (if (equal? actual expected)
       (format #t "aaan-text OK ~s\n" expected)
       (ly:error "aaan-text mismatch: expected ~s, got ~s" expected actual)))

#(define aaan-text-cases
   '(("Bayan" . "Баян")
     ("Composed by Keith Smith" . "К. Смит")
     ("Moderato" . "Умеренно")
     ("Intro" . "Вступление")
     ("Slowly" . "Медленно")
     ("Unhurried" . "Не спеша")
     ("Leisurely" . "Неторопливо")
     ("Moderately fast" . "Умеренно скоро")
     ("Lively" . "Подвижно")
     ("Animated" . "Оживлённо")
     ("Brisk" . "Живо")
     ("Fast" . "Быстро")
     ("Very fast" . "Очень быстро")
     ("Calmly" . "Спокойно")
     ("Broadly" . "Широко")
     ("Merrily" . "Весело")
     ("Cheerfully" . "Бодро")
     ("With restraint" . "Сдержанно")
     ("Singing" . "Певуче")
     ("Melodiously" . "Напевно")
     ("March tempo" . "В темпе марша")
     ("Waltz tempo" . "В темпе вальса")
     ("Chorus" . "Припев")
     ("Variation" . "Вариация")
     ("Instrumental break" . "Отыгрыш")
     ("Arranged by Keith Smith" . "Обработка К. Смита")))

#(define (aaan-cyrillic-letter? char)
   (and (char>=? char #\x0400) (char<=? char #\x04FF)))

#(define (aaan-latin-letter? char)
   (or (and (char>=? char #\A) (char<=? char #\Z))
       (and (char>=? char #\a) (char<=? char #\z))))

#(define (aaan-letter? char)
   (or (aaan-cyrillic-letter? char) (aaan-latin-letter? char)))

#(define (aaan-mixed-script-word? text)
   (let loop ((chars (string->list text))
              (word '())
              (bad #f))
     (cond
      ((and (null? chars) (null? word)) bad)
      ((or (null? chars) (not (aaan-letter? (car chars))))
       (let ((cyr (any aaan-cyrillic-letter? word))
             (lat (any aaan-latin-letter? word)))
         (loop (if (null? chars) '() (cdr chars))
               '()
               (or bad (and cyr lat)))))
      (else
       (loop (cdr chars) (cons (car chars) word) bad)))))

#(if (not (= (length aaan-text-dictionary) (length aaan-text-cases)))
     (ly:error "aaan-text dictionary has ~a entries, expected ~a"
               (length aaan-text-dictionary)
               (length aaan-text-cases)))

#(for-each
  (lambda (pair)
    (let ((hit (assoc (car pair) aaan-text-dictionary)))
      (if (or (not hit) (not (string=? (cdr hit) (cdr pair))))
          (ly:error "aaan-text dictionary missing ~s -> ~s" (car pair) (cdr pair)))
      (if (aaan-mixed-script-word? (cdr pair))
          (ly:error "Latin letter inside Cyrillic word: ~s" (cdr pair)))))
  aaan-text-cases)

\aaanLanguage #'english
#(for-each
  (lambda (pair)
    (aaan-check (aaan-text-string (car pair)) (car pair)))
  aaan-text-cases)
#(aaan-check (aaan-text-string "Copyright  2026-10-1 \"Keith Smith\"")
             "Copyright  2026-10-1 \"Keith Smith\"")
#(aaan-check (aaan-text-string "Copyright 2023 Keith Smith")
             "Copyright 2023 Keith Smith")

\aaanLanguage #'russian
#(for-each
  (lambda (pair)
    (aaan-check (aaan-text-string (car pair)) (cdr pair)))
  aaan-text-cases)
#(aaan-check (aaan-text-string "Dmitr The Imp") "Dmitr The Imp")
#(aaan-check (aaan-text-string "Fine") "Fine")
#(aaan-check (aaan-text-string "D.C.") "D.C.")
#(aaan-check (aaan-text-string "rit.") "rit.")
#(aaan-check (aaan-text-string "f") "f")
#(aaan-check (aaan-text-string "Copyright  2026-10-1 \"Keith Smith\"")
             "© Кит Смит, 2026")
#(aaan-check (aaan-text-string "Copyright 2023 Keith Smith")
             "© Кит Смит, 2023")
#(aaan-check (aaan-text-string "Copyright  2023 \"Keith Smith\"")
             "© Кит Смит, 2023")
#(if (aaan-mixed-script-word? (aaan-text-string "Copyright  2026-10-1 \"Keith Smith\""))
     (ly:error "Latin letter inside the copyright translation"))

\header {
  title = "aaan-text"
  instrument = \aaan-text "Bayan"
  composer = \aaan-text "Composed by Keith Smith"
  copyright = \aaan-text "Copyright  2026-10-1 \"Keith Smith\""
  tagline = ##f
}

\score {
  { s1 }
  \layout { }
}
