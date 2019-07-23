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
(define (add-claim c grid)
  (for*
      ([x (in-range (claim-x c) (+ (claim-x c) (claim-w c)))]
       [y (in-range (claim-y c) (+ (claim-y c) (claim-h c)))])
    (hash-update! grid (cons x y) (lambda (n) (+ n 1)) (lambda () 0)))
  grid)

;; Return the number of square inches in the grid that have more than one claim
(define (count-overlaps grid)
  (length (filter (lambda (i) (> i 1)) (hash-values grid))))

;;-----------
;; Part b

;; Return the claim that has no squares with overlaps 
(define (find-unoverlapped-claim claims grid)
  (define (unoverlapped? c)
    (for*/and
      ([x (in-range (claim-x c) (+ (claim-x c) (claim-w c)))]
       [y (in-range (claim-y c) (+ (claim-y c) (claim-h c)))])
    (eq? (hash-ref grid (cons x y)) 1)))
  (match (filter unoverlapped? claims)
    [(list single-claim) (claim-id single-claim)]
    [_ (error "Match failed in find-unoverlapped-claim")]))

;;-----------
;; Main

(define input-lines (file->lines "../input/day03.txt"))
(define input-claims (map line->claim input-lines))
(define input-grid (fold-to-grid input-claims))
(printf "Day 03a: ~a\n" (count-overlaps input-grid))
(printf "Day 03b: ~a\n" (find-unoverlapped-claim input-claims input-grid))


