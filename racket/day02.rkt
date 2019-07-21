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

(define inputs (file->lines "../input/day02.txt"))
(printf "Day 02a: ~a\n" (part-a inputs))
(printf "Day 02b: ~a\n" "NYI")


