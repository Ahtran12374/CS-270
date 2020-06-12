#lang racket
(require rackunit)
(require rackunit/text-ui)

(define (ori e1 e2)
  (if e1
      #t
      (if e2 #t #f)
      )
)
;Question 13
; Write a recursive function exactly_one_q to check if a list of symbols
; contains exactly one q symbol.
; Don't forget the base case and the necessary recursion.
; You may use any previously written function.

;Hint: The answer to question 11 is helpful to use here.

; Check if a list contains exactly one q
; Input:  L is a list of symbols (a,b,c,...,z).
; Output: a boolean value which is true when exactly one of the elements
;          in L is equal to q and false otherwise.
; The empty list should return false.
(define (exactly_one_q L)
  (if (null? L)
      #f
      (if (ori ((lambda (x) (equal? x 'q)) (first L)) (exactly_one_q (rest L)))
          (if (= 1 (foldr + 0 (map (lambda (L) (if (equal? L 'q) 1 0)) L)))
              #t
              #f
              )
          #f
          )
      )
)

(define-test-suite test_exactly_one_q
  (check-equal? (exactly_one_q '(q)) #t)
  (check-equal? (exactly_one_q '(x)) #f)
  (check-equal? (exactly_one_q '(z r)) #f)
  (check-equal? (exactly_one_q '(q d)) #t)
  (check-equal? (exactly_one_q '(q q)) #f)
  (check-equal? (exactly_one_q '(d e p)) #f)
  (check-equal? (exactly_one_q '(q b q)) #f)
  (check-equal? (exactly_one_q '(q q q)) #f)
  (check-equal? (exactly_one_q '(q n q q)) #f)
  (check-equal? (exactly_one_q '(m n m q)) #t)
)

(display "Question 13 exactly_one_q (10 points)")
(newline)
(define q13_score (- 10 (run-tests test_exactly_one_q 'verbose)))
 