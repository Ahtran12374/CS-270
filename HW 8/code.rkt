#lang racket
(require rackunit)
(require rackunit/text-ui)

(define (spec_sum n)
  (if (equal? n 1)
      6
      (+ (* 6 (* n n)) (spec_sum (- n 1)))
      )
)

(define (evenzeros X)
  (if (null? X)
      #t
      (xor (equal? (first X) 0) (evenzeros (rest X)))
      )
)

(define (duplicate X)
  (if (null? X)
      '()
      (cons (first X) (cons (first X) (duplicate (rest X))))
      )
)

(define (cut_end L)
  (if (null? (rest L))
      '()
      (cons (first L) (cut_end (rest L)))
      )
)

(define (add_pairs L)
  (if (null? L)
      '()
      (cons (+ (first L) (second L)) (add_pairs (rest(rest L))))
      )
)

(display "Question 3a duplicate Tests (10 points)\n")
(define-test-suite test_duplicate
  (check-equal? (duplicate '()) '())
  (check-equal? (duplicate '(1)) '(1 1))
  (check-equal? (duplicate '(1 2)) '(1 1 2 2))
  (check-equal? (duplicate '(4 6)) '(4 4 6 6))
  (check-equal? (duplicate '((1) (2 3))) '((1) (1) (2 3) (2 3)))
  (check-equal? (duplicate '(4 5 6)) '(4 4 5 5 6 6))
  (check-equal? (duplicate '(7 8 9 10)) '(7 7 8 8 9 9 10 10))
  (check-equal? (duplicate '(1 2 3 4 5)) '(1 1 2 2 3 3 4 4 5 5))
  (check-equal? (duplicate '(9 9 9)) '(9 9 9 9 9 9))
  (check-equal? (duplicate '(1 4 5 6 4 3 4 5))
                '(1 1 4 4 5 5 6 6 4 4 3 3 4 4 5 5))
)
(define q3a_score (- 10 (run-tests test_duplicate 'verbose)))