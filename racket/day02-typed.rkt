#lang typed/racket

;; Day 2 - Advent of Code 2018 - Typed Racket version

;;-----------
;; Part a

;; Given a list of strings returns the product of the number of strings that contain at least one character with exactly 2 occurrences
;; and the number of strings that contain at least one character with exactly 3 occurreces.
(define (part-a [xs : (Listof String)]) : Natural
  (define all-counts : (Listof (Listof Natural)) (map string->counts xs))
  (define num-2 : Natural (length (filter (lambda ([counts : (Listof Natural)]) (member 2 counts)) all-counts)))
  (define num-3 : Natural (length (filter (lambda ([counts : (Listof Natural)]) (member 3 counts)) all-counts)))
  (* num-2 num-3))
(module+ test
  (require typed/rackunit)
  (check-equal? (part-a (list "abcdef" "bababc" "abbcde" "abcccd" "aabcdd" "abcdee" "ababab")) 12))  

;; Helper function to convert a string into a list of the counts of the characters in the string;
(define (string->counts [s : String]) : (Listof Natural)
  (define (add-one [x : Natural]) : Natural (+ x 1))
  (hash-values (for/fold ([h : (Immutable-HashTable Char Natural) (hash)])
                         ([c : Char (in-string s)])
                 (hash-update h c add-one (lambda () 0)))))
(module+ test
  (check-equal? (sort (string->counts "abcaba") <) (list 1 2 3)))

;;-----------
;; Part b

;; Find first pair of strings that differ by one character only and return those common characters
;; Implemented with for*/fold as for*/first not available in typed racket and had difficulties
;; getting other for macros to type check
(define (part-b [xs : (Listof String)]) : String
  (define target-length : Integer (- (string-length (first xs)) 1))
  (define found
    (for*/fold : (U String #f)
      ([match : (U String #f) #f])
      ([s1 : String xs]
       [s2 : String xs]
       #:break (string? match))
      (let ([common : String (common-characters s1 s2)])
        (and
         (eq? target-length (string-length common))
         common))))
  (assert found string?))
(module+ test
  (check-equal? (part-b '("abcde" "fghij" "klmno" "pqrst" "fguij" "axcye" "wvxyz")) "fgij"))

;; Helper function that return a string with the common characters from the given pair of string
(define (common-characters [s1 : String] [s2 : String]) : String
  (list->string
   (for/list ([c1 (in-string s1)]
              [c2 (in-string s2)]
              #:when (eq? c1 c2))
    c1)))
(module+ test
  (check-equal? (common-characters "fghij" "fguij") "fgij"))

;;-----------
;; Main

(define inputs (file->lines "../input/day02.txt"))
(printf "Day 02a: ~a\n" (part-a inputs))
(printf "Day 02b: ~a\n" (part-b inputs))


