#lang typed/racket

;; Day 1 - Advent of Code 2018 - Typed Racket version

;;-----------
;; Part a

;; part-a: sum of the frequency changes
(: part-a (-> (Listof Integer) Integer))
(define (part-a xs)
  (apply + xs))

(module+ test
  (require typed/rackunit)
  (check-equal? (part-a '(1 1 1)) 3)
  (check-equal? (part-a '(1 1 -2)) 0)
  (check-equal? (part-a '(-1 -2 -3)) -6))

;;-----------
;; Part b

;; part-b: First repeat of sums of the series (repeated)
;; Iterates over the cyclically repeating frequences while accumulating the running sum
;; and a set of frequencies already seen. Breaks loop when the sum repeats a previous value.
;; let-values is used to capture the multiple return values from the for/fold
(: part-b (-> (Listof Integer) Integer))
(define (part-b xs)
  (define empty-set : (Setof Integer) (set))
  (let-values ([([s-final : Integer] [seen-final : (Setof Integer)])
                (for/fold ([s 0]
                          [seen empty-set])
                         ([i (in-cycle xs)]
                          #:break (set-member? seen s))
                 (values (+ s i) (set-add seen s)))])
    s-final))
(module+ test
  (check-equal? (part-b '(1 -1 1)) 0)
  (check-equal? (part-b '(3 3 4 -2 -4)) 10)
  (check-equal? (part-b '(-6 3 8 5 -6)) 5)
  (check-equal? (part-b '(7 7 -2 -7 -4)) 14))

;;-----------
;; Main

;; Helper function to convert a string to an integer (exception thrown if not possible)
(: string->integer (-> String Integer))
(define (string->integer s)
  (assert (string->number s) exact-integer?))
(module+ test
  (check-equal? (string->integer "23") 23)
  (check-exn exn:fail? (lambda () (string->integer "23.5")))
  (check-exn exn:fail? (lambda () (string->integer "abc"))))

(define input : (Listof Integer) (map string->integer (file->lines "../input/day01.txt")))
(printf "Day 01a: ~a\n" (part-a input))
(printf "Day 02b: ~a\n" (part-b input))




