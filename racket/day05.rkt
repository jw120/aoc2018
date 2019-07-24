#lang racket

;; Day 5 - Advent of Code 2018 - Racket version
       
;;-----------
;; Part a

(define (part-a s)
  (string-length (react s)))

;; Make repeated passes through the list until no more reactions
(define (react s)
  (define reacted (react-pass s))
  (if (equal? reacted s)
      s
      (react reacted)))
(module+ test
  (require rackunit)
  (check-equal? (react "dabAcCaCBAcCcaDA") "dabCBAcaDA"))

;; Make one pass through the string eliminating adjacent opposite characters
(define (react-pass s)
  (define (react-pass-list xs)
    (cond
      [(empty? xs) empty]
      [(empty? (rest xs)) xs]
      [else
       (let ([x (first xs)] [y (first (rest xs))])
         (if (reactive x y)
             (react-pass-list (rest (rest xs)))
             (cons x (react-pass-list (rest xs)))))]))
  (list->string (react-pass-list (string->list s))))
(module+ test
  (check-equal? (react-pass "dabAcCaCBAcCcaDA") "dabAaCBAcaDA"))

;; Are the two characters reactive (both letters, same character, opposite case)
(define (reactive x y)
  (and
   (and (char-alphabetic? x) (char-alphabetic? y)
   (char-ci=? x y)
   (xor (char-upper-case? x) (char-upper-case? y)))))

;;-----------
;; Part b

(define (part-b s)
  (let-values ([(letter len) (minimizing-letter s)])
    len))

;; Find which letter minimizes the length of the reacted string
;; Returns two values - the letter and the length of the resulting string
(define (minimizing-letter s)
  (for/fold ([shortest-char #f]
             [shortest-len (string-length s)])
            ([c (in-string "abcdefghijklmnopqrstuvwxyz")])
    (define c-length (string-length (react (remove-letter c s))))
    (if (< c-length shortest-len)
        (values c c-length)
        (values shortest-char shortest-len))))

;; Remove all instances (case-insensitive) from the string
(define (remove-letter c s)
  (list->string
   (for/list ([x (in-string s)]
         #:unless (char-ci=? x c)
         )
     x)))
 (module+ test
   (check-equal? (remove-letter #\a "dabAcCaCBAcCcaDA") "dbcCCBcCcD"))

;;-----------
;; Main

(define input (file->string "../input/day05.txt"))
(printf "Day 05a: ~a\n" (part-a input))
(printf "Day 05b: ~a\n" (part-b input))

