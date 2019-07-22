#lang racket

;; Day 3 - Advent of Code 2018 - Racket version

;;-----------
;; Claim data type

(struct claim (id x y w h) #:transparent)

(define (line->claim s) 
  (match (regexp-match #px"^#(\\w+)\\s+@\\s+(\\d+),(\\d+):\\s+(\\d+)x(\\d+)$" s)
    [(list _ id x y w h)
     (claim id (string->number x) (string->number y) (string->number w) (string->number h))]
    [_ (error "No match")]))
(module+ test
  (require rackunit)
  (check-equal? (line->claim "#32 @ 264,844: 23x18") (claim "32" 264 844 23 18)))

;;-----------
;; Part a

;; We store all the claims in a hash table with one element for each square inch of claim with value
;; equal to the number of claims for that square inch
(define (fold-to-grid claims)
  (foldl add-claim (make-hash) claims))
(define (add-claim c h)
  (for*
      ([x (in-range (claim-x c) (+ (claim-x c) (claim-w c)))]
       [y (in-range (claim-y c) (+ (claim-y c) (claim-h c)))])
    (hash-update! h (cons x y) (lambda (n) (+ n 1)) (lambda () 0)))
  h)

;; Return the number of square inches in the grid that have more than once claim
(define (count-overlaps h)
  (length (filter (lambda (i) (> i 1)) (hash-values h))))

;;-----------
;; Part b


;;-----------
;; Main



(define input-lines (file->lines "../input/day03.txt"))
(define input-claims (map line->claim input-lines))
(define input-grid (fold-to-grid input-claims))
(printf "Day 03a: ~a\n" (count-overlaps input-grid))
(printf "Day 03b: ~a\n" "NYI")


