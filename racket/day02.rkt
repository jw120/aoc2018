#lang racket

; Part a: Product of the number of strings that contain at least one character with exactly 2 occurrences and the number of strings that
; contain at least one charcter with exactly 3 occurreces.
(define (part-a xs)
  (let* ([counts (map string->counts xs)]
         [num-2 (length (filter (lambda (c) (member 2 c)) counts))]
         [num-3 (length (filter (lambda (c) (member 3 c)) counts))])
    (* num-2 num-3)))
(module+ test
  (require rackunit)
  (check-equal? (part-a (list "abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab")) 12))  

; Convert a string into a list of the counts of the characters in the string
(define (string->counts s)
  (hash-values (for/fold ([h (hash)])
                         ([c (in-string s)])
                 (hash-update h c (lambda (x) (+ x 1)) 0))))
(module+ test
  (check-equal? (sort (string->counts "abcaba") <) (list 1 2 3)))

; Part b: Find pair of strings that differ by one character only and return those common characters
(define (part-b xs)
  (let ([target-length (- (string-length (first xs)) 1)])
    (for*/first ([s1 xs]
                 [s2 xs]
                 #:when (eq? target-length (string-length (common-characters s1 s2))))
      (common-characters s1 s2))))
(module+ test
  (check-equal? (part-b '("abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz")) "fgij"))

; Return a string with the common characters from the given pair of strings
(define (common-characters s1 s2)
  (list->string
   (for/list ([c1 (in-string s1)]
              [c2 (in-string s2)]
              #:when (eq? c1 c2))
    c1)))
(module+ test
  (check-equal? (common-characters "fghij" "fguij") "fgij"))

(define inputs (file->lines "../input/day02.txt"))
(printf "Day 02a: ~a\n" (part-a inputs))
(printf "Day 02b: ~a\n" (part-b inputs))


