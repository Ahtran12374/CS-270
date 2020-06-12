#lang racket

(require racket/contract)
(require rackunit)
(require rackunit/text-ui)

(define (zero? n)
  (eq? n 'zero))

(define (nat? x)
  (cond
    [(zero? x) #t]
    [(pair? x) (and (eq? (first x) 'succ) (nat? (second x)))]
    [else #f]))

(define (succ n)
  (list 'succ n))

(define (pred n)
  (if (zero? n) 'zero (second n)))

(define zero 'zero)
(define one (succ zero))
(define two (succ one))
(define three (succ two))
(define four (succ three))
(define five (succ four))
(define six (succ five))
(define seven (succ six))
(define eight (succ seven))
(define nine (succ eight))
(define ten (succ nine))

(define (plus m n)
  (if (zero? m)
      n
      (succ (plus (pred m) n))))

(define (mult m n)
  (if (eq? m 'zero)
      'zero
      (plus n (mult (pred m) n))))

(define (ltnat? m n)
  (cond
    [(zero? n) #f]
    [(zero? m) #t]
    [else (ltnat? (pred m) (pred n))]))

(define (sub m n)
  (if (ltnat? m n)
      (print "undefined")
      (if (zero? n)
          m
          (sub (pred m) (pred n)))
      )
  )

(define (div m n)
  (cond
    [(zero? n) (void)]
    [(ltnat? m n) zero]
    [else (if (zero? m)
              'zero
              (plus one (div (sub m n) n))
              )
          ]
    )
  )

(define (rem m n)
  (sub m (mult n (div m n)))
  )

(define (gcd m n)
  (cond
    [(zero? n) m]
    [(ltnat? m n) (gcd n m)]
    [else (if (zero? (rem m n))
              n
              (gcd n (rem m n))
              )
          ]
    )
)

;--------------------------------------------------------------------------------------------------

(define binzero 'zero)

(define (binzero? b)
  (eq? b binzero))

(define (double b)
  (list 'D b))

(define (double-plus1 b)
  (list 'DP1 b))

(define (double? b)
  (cond
    [(not (pair? b)) #f]
    [(eq? (first b) 'D) #t]
    [else #f]))

(define (double-plus1? b)
  (cond
    [(not (pair? b)) #f]
    [(eq? (first b) 'DP1) #t]
    [else #f]))

(define (op b)
  (second b))

(define (binone? b)
  (equal? b binone))

(define binone (double-plus1 binzero))
(define bintwo (double binone))
(define binthree (double-plus1 binone))
(define binfour (double bintwo))
(define binfive (double-plus1 bintwo))
(define binsix (double binthree))
(define binseven (double-plus1 binthree))
(define bineight (double binfour))
(define binnine (double-plus1 binfour))
(define binten (double binfive))

; increment a binary number
; Inputs: a binary number b
; Output: a binary whose value is the value of b + 1.
;         if b is normalized (inc b) will be normalized.
(define(inc b)
  (cond
    [(binzero? b) (double-plus1 b)]
    [(double? b) (double-plus1 (op b))]
    [(double-plus1? b) (double (inc (op b)))]))

(define (binplus a b)
  (cond
    [(binzero? a) b]
    [(binzero? b) a]
    [(and (double? a) (double? b)) (double (binplus (op a) (op b)))]
    [(and (double-plus1? a) (double-plus1? b)) (double (inc (binplus (op a) (op b))))]
    [else (inc (double (binplus (op a) (op b))))]
    )
  )

(define (binmult a b)
  (cond
    [(binzero? a) binzero]
    [(binzero? b) binzero]
    [else (if (double? b)
              (double (binmult a (op b)))
              (binplus a (double (binmult a (op b))))
              )
          ]
    )
  )
    

(display "Question 6 - Binary Multiply (20 points)\n")
(define-test-suite bin-mult-test
  (check-equal? (binmult binzero binone) binzero)
  (check-equal? (binmult binone binzero) binzero)
  (check-equal? (binmult binzero binthree) binzero)
  (check-equal? (binmult binnine binzero) binzero)
  (check-equal? (binmult binone binthree) binthree)
  (check-equal? (binmult binone binseven) binseven)
  (check-equal? (binmult binfive binone) binfive)
  (check-equal? (binmult binten binone) binten)
  (check-equal? (binmult bintwo binone) bintwo)
  (check-equal? (binmult bintwo bintwo) binfour)
  (check-equal? (binmult bintwo binthree) binsix)
  (check-equal? (binmult bintwo binfour) bineight)
  (check-equal? (binmult bintwo binfive) binten)
  (check-equal? (binmult binthree bintwo) binsix)
  (check-equal? (binmult binthree binthree) binnine)
  (check-equal? (binmult binfour bintwo) bineight)
  (check-equal? (binmult binfive bintwo) binten)
  (check-equal? (binmult binfive binfour) (double binten))
  (check-equal? (binmult binfour binthree) (double binsix))
  (check-equal? (binmult binfive binthree) (double-plus1 binseven))
)
(define q6_score  (- 20 (run-tests bin-mult-test 'verbose)))