#lang racket

(require rackunit)
(require rackunit/text-ui)

(define (int_to_bool n)
  (int_to_bool_h n '())
)

(define (int_to_bool_h n L)
  (if (= n 0)
      L
      (int_to_bool_h (quotient n 2) (cons(if (= (remainder n 2) 0) #f #t) L))
      )
  )

(define (pad num_bits bit_list)
  (if (or (= num_bits 0) (= num_bits (length bit_list)))
      bit_list
      (pad num_bits (cons #f bit_list))
      )
  )

(define (tt_inputs n)
  (tt_inputs_h n (- (expt 2 n) 1))
)
(define (tt_inputs_h bits row_val)
  (if (= bits 0)
      (list '())
      (if (= row_val 0)
          (cons (pad bits (int_to_bool row_val)) '())
          (cons (pad bits (int_to_bool row_val)) (tt_inputs_h bits (- row_val 1)))
          )
      )
  )

(define (implies_verify boolean_vars)
  (let (;Start of name list
        (a (list-ref boolean_vars 0));Pairs (name value)
        (b (list-ref boolean_vars 1))
      );End of name list
    (equal? (implies a b ) (or (not a) b))
 );end of let
)

(define (demorgan_verify bool_vars)
  (let (
        (a (list-ref bool_vars 0))
        (b (list-ref bool_vars 1))
        )
    (equal? (not (and a b)) (or (not a) (not b)))
    )
  )

(define (absorp_verify bool_vars)
  (let (
        (a (list-ref bool_vars 0))
        (b (list-ref bool_vars 1))
        )
    (equal? (and a (or a b)) a)
    )
  )

(define (assoc_verify bool_vars)
  (let (
        (a (list-ref bool_vars 0))
        (b (list-ref bool_vars 1))
        (c (list-ref bool_vars 2))
        )
    (equal? (or a (or b c)) (or (or a b) c))
    )
  )


(define (evaluate_tt fun tt)
  (map fun tt)
)

(define (tautology result_list)
  (cond
    [(null? result_list) #t]
    [(equal? (first result_list) #t) (tautology (rest result_list))]
    [else #f]
    )
  )
      


(define-test-suite test_is_taut
  (check-equal? (is_taut implies_verify 2) #t)
  (check-equal? (is_taut demorgan_verify 2) #t)
  (check-equal? (is_taut absorp_verify 2) #t)
  (check-equal? (is_taut assoc_verify 3) #t)
  (check-equal? (is_taut (lambda (X) (and (first X) (second X))) 2) #f)
  (check-equal? (is_taut (lambda (X) (or (first X) (second X))) 2) #f)
)
(display "7.) Results of Is Tautology Question (12 points)\n")
(define q7_score (- 12 (* 2 (run-tests test_is_taut 'verbose))))
