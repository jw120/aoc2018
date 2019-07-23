#lang racket

;; Day 4 - Advent of Code 2018 - Racket version

;;-----------
;; Data wrangling

;; After sorting, our input has lines like the below
;; [1518-11-01 00:00] Guard #10 begins shift
;; [1518-11-01 00:05] falls asleep
;; [1518-11-01 00:25] wakes up

;; We first parse these into events (rolling the date into a single integer and throwing away the hour)
;; (guard 10)
;; (sleep 15181101 5)
;; (wake 15181101 25)
(struct guard (id) #:transparent)
(struct sleep (day minute) #:transparent)
(struct wake (day minute) #:transparent)

;; And then we fold these into sleeping intervals
;; (interval 10 5 25)
(struct interval (id start end) #:transparent)

;; Conversion from lines to events
(define (line->event s)
  (match (regexp-match (pregexp (string-append
                                 "^\\[(\\d\\d\\d\\d)-(\\d\\d)-(\\d\\d) " ; year month day
                                 "\\d\\d:(\\d\\d)\\] " ; hour minute (hour is not needed)
                                 "(\\w+) #?(\\w+)")) s) ; first two words of text
    [(list matched y m d min w1 w2)
     (let
         ([ day (+ (* 10000 (string->number y)) (* 100 (string->number m))  (string->number d))])
       (event day (string->number min) w1 w2))]
    [p (error "Parse failed " s p)]))
(define (event day minute word1 word2)
  (cond
    [(equal? word1 "Guard") (guard (string->number word2))]
    [(equal? word1 "falls") (sleep day minute)]
    [(equal? word1 "wakes") (wake day minute)]
    [else (error "Event creation failed" day minute word1 word2)]))
(module+ test
  (require rackunit)
  (check-equal? (line->event "[1518-11-01 00:05] falls asleep") (sleep 15181101 5))
  (check-equal? (line->event "[1518-11-01 00:00] Guard #10 begins shift") (guard 10))
  (check-equal? (line->event "[1518-11-01 00:25] wakes up") (wake 15181101 25))
  (check-exn exn:fail? (lambda () (line->event "[090-11-01")))
  (check-exn exn:fail? (lambda () (line->event "[1518-11-01 00:25] rolls over"))))

;; Conversion from events to sleeping intervals
(define (to-intervals events)
  (struct acc (id asleep intervals)) ; id of current guard; time he fell asleep (or #f); list of complete intervals
  (acc-intervals
   (for/fold ([a (acc #f #f '())]) ([e events])
    (cond
      [(guard? e) (struct-copy acc a [id (guard-id e)])]
      [(sleep? e) (struct-copy acc a [asleep (sleep-minute e)])]
      [(wake? e)  (struct-copy acc a [intervals (cons (interval (acc-id a) (acc-asleep a) (wake-minute e)) (acc-intervals a))])]
      [else (error "Fail in to-intervals cond" e a)]))))
       
;;-----------
;; Part a

(define (part-a intervals)
  (define g (guard-with-most-time-asleep intervals))
  (define m (minute-asleep-the-most (filter (lambda (i) (eq? (interval-id i) g)) intervals)))
  (* g m))

(define (guard-with-most-time-asleep intervals)
  (define duration-hash
    (for/fold ([h (hash)]) ([i intervals])
      (hash-update h (interval-id i) (lambda (d) (+ d (- (interval-end i) (interval-start i)))) (lambda () 0))))
  (key-of-max-value duration-hash))

(define (minute-asleep-the-most intervals)
  (define h (make-hash (for/list ([n (in-range 0 60)]) (cons n 0))))
  (for ([i intervals])
    (for ([x (in-range (interval-start i) (interval-end i))])
      (hash-update! h x (lambda (z) (+ z 1)))))
  (key-of-max-value h))

;; Helper function to return the key which has the highest value in a hash (one of the keys if multiple) 
(define (key-of-max-value h)
  (define max-value (apply max (hash-values h)))
  (car (first (filter (lambda (p) (eq? (cdr p) max-value)) (hash->list h)))))

;;-----------
;; Part b
(define (part-b intervals)
  (define guard-minute-pair (max-guard-minute-pair intervals))
  (* (car guard-minute-pair) (cdr guard-minute-pair)))

(define (max-guard-minute-pair intervals)
  (define h (make-hash))
  (for ([i intervals])
    (for ([x (in-range (interval-start i) (interval-end i))])
      (hash-update! h (cons (interval-id i) x) (lambda (z) (+ z 1)) (lambda () 0))))
  (key-of-max-value h))

;;-----------
;; Main

(define input-lines (file->lines "../input/day04.txt"))
(define input-events (map line->event (sort input-lines string<?)))
(define input-intervals (to-intervals input-events))
(printf "Day 04a: ~a\n" (part-a input-intervals))
(printf "Day 04b: ~a\n" (part-b input-intervals))
