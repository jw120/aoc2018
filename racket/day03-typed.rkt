#lang typed/racket

;; Day 3 - Advent of Code 2018 - Typed Racket version

;;-----------
;; Claim data type

(struct claim ([ id : String] [x : Natural] [y : Natural] [w : Natural] [h : Natural]) #:transparent)

(define (line->claim [s : String]) : claim
  (match (regexp-match #px"^#(\\w+)\\s+@\\s+(\\d+),(\\d+):\\s+(\\d+)x(\\d+)$" s)
    [(list _ id x-str y-str w-str h-str)
     (claim (assert id) (assert-nat x-str) (assert-nat y-str) (assert-nat w-str) (assert-nat h-str))]
    [_ (error "No match")]))
(define (assert-nat [x : (U False String)]) : Natural
  (assert (string->number (assert x)) natural?))
(module+ test
  (require typed/rackunit)
  (check-equal? (line->claim "#32 @ 264,844: 23x18") (claim "32" 264 844 23 18)))


;;-----------
;; Part a

;; Given a list of strings returns the product of the number of strings that contain at least one character with exactly 2 occurrences
;; and the number of strings that contain at least one character with exactly 3 occurreces.


;;-----------
;; Part b


;;-----------
;; Main



;(define input-lines (file->lines "../input/day03.txt"))
;(define input-claims (map line->claim input-lines))
(printf "Day 03a: ~a\n" "NYI")
(printf "Day 03b: ~a\n" "NYI")


