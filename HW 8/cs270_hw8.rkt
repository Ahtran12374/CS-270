#lang racket
(require rackunit)
(require rackunit/text-ui)

;CS 270
;Homework 8
;Professor B. Char, M. Boady,  J. Johnson, and G. Long

;Important Rules:
;1.) You may not use loop constructs like while/for/sum. If used, your answer will get a zero.
;2.) If the instructions state something must be recursive, you will recieve a zero if it is not recursive.
;    Recursive helper functions are allowed (the main function not being recursive).
;3.) You may not use the set! command. If used, your answer will get a zero.
;4.) Using If/Cond to explicitly pass tests instead of following the instructions
;    will always result in a zero for that question.

;Each of the below questions has two parts.
;First, you will be asked to write a Racket function to solve a problem.
;Secondly, you will be asked to prove by induction that your
;Racket code has some property.


;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Question 1a (10 points)
;Write a recursive function to compute
;the sum( 6*x^2 , x = 1..n) for a given n
;You must write a recursive function.
;If you use any iterative commands (for/loop/sum/etc you will receive a 0)

; Computes sum( 6*x^2 , x = 1..n)
; Input:  n an integer >= 1
; Output: an integer, the result of the summation
(define (spec_sum n)
  (if (equal? n 1)
      6
      (+ (* 6 (* n n)) (spec_sum (- n 1)))
      )
)
      

;Test Bed
(display "Question 1a spec_sum Tests (5 points)\n")
(define-test-suite test_spec_sum
  (check-equal? (spec_sum 1) 6)
  (check-equal? (spec_sum 2) 30)
  (check-equal? (spec_sum 3) 84)
  (check-equal? (spec_sum 4) 180)
  (check-equal? (spec_sum 5) 330)
  (check-equal? (spec_sum 6) 546)
  (check-equal? (spec_sum 7) 840)
  (check-equal? (spec_sum 8) 1224)
  (check-equal? (spec_sum 9) 1710)
  (check-equal? (spec_sum 10) 2310)
)
(define q1a_score (- 10 (run-tests test_spec_sum 'verbose)))

;Question 1b (10 points)
;Prove by induction that
;for all integers n >= 1 -> (spec_sum n) = 2n^3+3n^2+n
;Give your proof below in comments
#|

Base Case
---------
(spec_sum 1)                    |Call function on 1
(if (equal? 1 1) 6 ...)         |Apply func. def.
(if #t 6 ...)                   |Eval equal?
6                               |Eval if

Indutive Hypothesis
-------------------
Assume that there exist a 'W' whole number that is greater than or equal to 1, such that
(spec_sum w) = 2w^3+3w^2+w

Inductive case
--------------
(spec_sum (+ w 1))                                       |Call function on n + 1
(if (equal? (+ w 1) 1) ...)                              |Apply func. def.
(if #f ...)                                              |Eval equal?
(+ (* 6 (* (+ w 1) (+ w 1))) (spec_sum (- (+ w 1) 1)))   |Eval if
(+ ... (2w^3+3w^2+w))                                    |By IH
((6*((w+1)*(w+1))) + (2w^3+3w^2+w))                      |Rewrite algebraically
(6w^2+12w+6+2w^3+3w^2+w)                                 |Expand
(3w^2+6w+3)+(2w^3+6w^2+6w+2)+(w+1)                       |Rewrite
2*(w+1)^3+3(w+1)*2+(w+1)                                 |Factor

Conclusion
----------
Since the case holds true, the function will continue to hold true as long as
long as the input command contract is met.

|#


;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Question 2 (10 points)
; Write a recursive function evenzeros to check if a list of integers
; contains an even number of zeros.
; Don't forget the base case and the necessary recursion. 

; Check if a list contains an even number of zeros
; Input:  L is a list of integers.
; Output: a boolean value which is true when an even number of the elements
;          in L is equal to zero and false otherwise.
; 0 is even, so the Null list should return true
(define (evenzeros X)
  (if (null? X)
      #t
      (xor (equal? (first X) 0) (evenzeros (rest X)))
      )
)
;Test Bed
(display "Question 2a evenzeros Tests (10 points)\n")
(define-test-suite test_even_zeros
  (check-equal? (evenzeros '()) #t)
  (check-equal? (evenzeros '(1)) #t)
  (check-equal? (evenzeros '(0)) #f)
  (check-equal? (evenzeros '(0 0)) #t)
  (check-equal? (evenzeros '(7 0)) #f)
  (check-equal? (evenzeros '(1 -2)) #t)
  (check-equal? (evenzeros '(0 0 1)) #t)
  (check-equal? (evenzeros '(4 0 1)) #f)
  (check-equal? (evenzeros '(1 0 8)) #f)
  (check-equal? (evenzeros '(0 11 0 -9)) #t)
)
(define q2a_score (- 10 (run-tests test_even_zeros 'verbose)))
;Question 2b (10 points)
;Prove by induction, algebra, and equational reasoning that
;If L contains an even number of zeros then (equal? (evenzeros L) #t)
;If L contains an odd number of zeros then (equal? (evenzeros L) #f)
;Hint: You need 4 cases (cons 0 E), (cons x E), (cons 0 O), (cons x O)
;Where, x!=0, E is a list with an even number of zeros and O is a list
;with an odd number of zeros.

#|

Base Case
---------
(spec_sum '())                  |Call function on '()
(if (null? '()) #t ...)         |Apply func. def.
(if #t #t ...)                  |Eval null?
#t                              |Eval if

Indutive Hypothesis
-------------------
Assume that there exist a list 'E' of even 0 and a list 'O' of odd 0, then
(evenzeros E) = #t
(evenzeros O) = #f 

Inductive case 1
----------------
(evenzeros (cons 0 E))                                              |Call function on (cons 0 E)
(if (null? (cons 0 E)) ...)                                         |Apply func. def.
(if #f ... )                                                        |Eval null?
(xor (equal? (first (cons 0 E)) 0) (evenzeros (rest (cons 0 E))))   |Eval if
(xor (equal? 0 0) (evenzeros (rest (cons 0 E))))                    |First and cons cancel out
(xor (equal? 0 0) (evenzeros E))                                    |Rest and cons cancel out
(xor #t (evenzeros E))                                              |Eval equal?
(xor #t #t)                                                         |By IH
#f                                                                  |Eval xor

Inductive case 2
----------------
(evenzeros (cons x E))                                              |Call function on (cons x E)
(if (null? (cons x E)) ...)                                         |Apply func. def.
(if #f ... )                                                        |Eval null?
(xor (equal? (first (cons x E)) 0) (evenzeros (rest (cons x E))))   |Eval if
(xor (equal? x 0) (evenzeros (rest (cons x E))))                    |First and cons cancel out
(xor (equal? x 0) (evenzeros E))                                    |Rest and cons cancel out
(xor #f (evenzeros E))                                              |Eval equal?
(xor #f #t)                                                         |By IH
#t                                                                  |Eval xor

Inductive case 3
----------------
(evenzeros (cons 0 O))                                              |Call function on (cons 0 O)
(if (null? (cons 0 O)) ...)                                         |Apply func. def.
(if #t ... )                                                        |Eval null?
(xor (equal? (first (cons 0 O)) 0) (evenzeros (rest (cons 0 O))))   |Eval if
(xor (equal? 0 0) (evenzeros (rest (cons 0 O))))                    |First and cons cancel out
(xor (equal? 0 0) (evenzeros O))                                    |Rest and cons cancel out
(xor #t (evenzeros O))                                              |Eval equal?
(xor #t #f)                                                         |By IH
#t                                                                  |Eval xor

Inductive case 4
----------------
(evenzeros (cons x O))                                              |Call function on (cons x O)
(if (null? (cons x O)) ...)                                         |Apply func. def.
(if #f ... )                                                        |Eval null?
(xor (equal? (first (cons x O)) O) (evenzeros (rest (cons x O))))   |Eval if
(xor (equal? x O) (evenzeros (rest (cons x O))))                    |First and cons cancel out
(xor (equal? x O) (evenzeros O))                                    |Rest and cons cancel out
(xor #f (evenzeros O))                                              |Eval equal?
(xor #f #f)                                                         |By IH
#f                                                                  |Eval xor

Conclusion
----------
Assuming that that 'O' list and the 'E' list is evaluated correctly, the function meet's its
output contract for all cases.

|#



;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Q3a (10 Points)
;Write a recursive function duplicate that takes every element in a list
;and makes a second copy of the item.
;For example if we started with (1 2 3)
;then the duplicated list would be (1 1 2 2 3 3)


; Duplicates Elements in a list
; Input:  X a list
; Output: A new list with two copies of even value in X
(define (duplicate X)
  (if (null? X)
      '()
      (cons (first X) (cons (first X) (duplicate (rest X))))
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

;Q3b (10 Points)
;Prove By Induction
;(length (duplicate L)) = (* 2 (length L))
;You may use the following properties of length
;Length Property 1: (length '()) = 0 
;Length Property 2: If a is an object and B is a list
;(length (cons a B)) = (+ 1 (length B))
;You may Justify lines by saying [By Length Property 1]
;Hint: An equals can be used both ways.
#|

Base Case
---------
(length (duplicate '()))               |Call function on '()
(length (if (null? '()) ...))          |Apply func. def.
(length (if #t ...))                   |Eval null?
(length '())                           |Eval if
0                                      |By length property 1

Indutive Hypothesis
-------------------
Assume that there exist an 'L' list for which the function reads,
((length (duplicate L)) = (* 2 M)
where M the length of L i.e (length L)

Inductive Case
--------------
(length (duplicate (cons a L)))                                                                               |Call function on (cons a L)
(length (if (null? (cons a L)) ...))                                                                          |Apply func. def.
(length (if #f ...))                                                                                          |Eval null?
(length (cons (first (cons a L)) (cons (first (cons a L)) (duplicate (rest (cons a L))))))                    |Eval if
(length (cons a (cons a (duplicate (rest (cons a L))))))                                                      |first and cons cancel
(length (cons a (cons a (duplicate L))))                                                                      |rest and cons cancel
(+ 1 (length (cons a (duplicate L))))                                                                         |By length property 2
(+ 1 (+ 1 (length (duplicate L))))                                                                            |By length property 2
(+ 1 (+ 1 (* 2 M))                                                                                            |By IH
(+ 2 (* 2 M))                                                                                                 |Eval add algebraically
(* 2 (length (cons a L)))                                                                                     |Call right hand of function
(* 2 (+ 1 (length L)))                                                                                        |By length property 2
(+ 2 (* 2 (length L)))                                                                                        |Distribute algebraically
(+ 2 (* 2 M))                                                                                                 |By IH

Conclusion
----------
The first element is duplicated successfully, and the rest of the list is assume to be duplicated correctly.
As the base case and both the right and left hand side of the function holds in the inductive case, we can say that
the function meets its output contract.

|#


;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Question 4a (10pts)
;Write a recursive function (cut_end L) that removes the last element from the list

; Removes the last element in a list
; Input:  X non-empty a list
; Output: A new list with the last element removed
(define (cut_end L)
  (if (null? (rest L))
      '()
      (cons (first L) (cut_end (rest L)))
      )
)

(display "Question 4a cut_end Tests (10 points)\n")
(define-test-suite test_cut_end
  (check-equal? (cut_end '(1)) '())
  (check-equal? (cut_end '(1 2)) '(1))
  (check-equal? (cut_end '(3 4 5)) '(3 4))
  (check-equal? (cut_end '( (1) (2) (3) )) '( (1) (2) ))
  (check-equal? (cut_end '((1 2 3 4))) '())
  (check-equal? (cut_end '((1 2) (3 4))) '((1 2)))
  (check-equal? (cut_end '(9 9 8)) '(9 9))
  (check-equal? (cut_end '(AND A B)) '(AND A))
  (check-equal? (cut_end '(NOT X)) '(NOT))
)
(define q4a_score (- 10 (run-tests test_cut_end 'verbose)))

;Question 4b
;Prove by Induction that (length (cut_end L)) = (- (length L) 1)
;for all L where (length L) >= 1
;You may use the properties of length from Question 3

#|

Base Case
---------
(length (cut_end '(1)))                 |Call function on '(1)
(length (if (null? (rest '(1))))        |Apply func. def.
(length (if (null? '())))               |Eval rest
(length (if #t ...))                    |Eval null?
(length '())                            |Eval if
0                                       |By length property 1

Indutive Hypothesis
-------------------
Assume that there exist an 'L' list for which the function reads,
(length (cut_end L)) = (- M 1)
where M is the length of the list i.e. (length L)

Inductive Case
--------------
(length (cut_end (cons a L)))                                       |Call function on (cons a L)
(length (if (null? (cons a L)) ...))                                |Apply funct. def. 
(length (if #f ...))                                                |Eval null?
(length (cons (first (cons a L)) (cut_end (rest (cons a L))))       |Eval if
(length (cons a (cut_end (rest (cons a L))))                        |first and cons cancel
(length (cons a (cut_end L)))                                       |rest and cons cancel
(+ 1 (length (cut_end L)))                                          |By length property 2
(+ 1 (- M 1))                                                       |By IH
M                                                                   |add and subtract cancel
(- (length (cons a L)) 1)                                           |Call right hand of function on (cons a L)
(- (+ 1 (length L)) 1)                                              |By length property 2
(- (+ 1 M) 1)                                                       |By IH
M                                                                   |add and subtract cancel

Conclusion
----------
The last element is eliminated correctly, and the remaining element is assumed to be counted correctly.
As the base case and both the right and left hand side of the function holds in the inductive case, we can say that
the function meets its output contract.

|#



;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;Question 5a (10pts)
;Write a recursive function (add_pairs L)
;that adds pairs of numbers.
;You may assume the length of L will always be even.

; Adds pairs of numbers
; Input:  L a list (the list must have even length)
; Output: A new list with pairs of elements added together.
(define (add_pairs L)
  (if (null? L)
      '()
      (cons (+ (first L) (second L)) (add_pairs (rest(rest L))))
      )
)

(display "Question 5a add_pairs Tests (10 points)\n")
(define-test-suite test_add_pairs
  (check-equal? (add_pairs '()) '())
  (check-equal? (add_pairs '(1 2)) '(3))
  (check-equal? (add_pairs '(1 2 3 4)) '(3 7))
  (check-equal? (add_pairs '(2 2 2 2)) '(4 4))
  (check-equal? (add_pairs '(0 -1 -2 3)) '(-1 1))
  (check-equal? (add_pairs '(1 1 1 1)) '(2 2))
  (check-equal? (add_pairs '(1 2 3 4 5 6 7 8)) '(3 7 11 15))
  (check-equal? (add_pairs '(9 9 9 9 9 9)) '(18 18 18))
  (check-equal? (add_pairs '(7 3 4 6 5 5)) '(10 10 10))
  (check-equal? (add_pairs '(-9 9 -8 8)) '(0 0))
  
)
(define q5a_score (- 10 (run-tests test_add_pairs 'verbose)))

;Question 5b
;Prove by Induction that (length (add_pairs L)) = 1/2*(length L)
;for all L where (even? (length L)) and (length L) >= 0
;You may use the properties of length from Question 3
#|

Base Case
---------
(length (add_pairs '()))               |Call function on '()
(length (if (null? '()) ...))          |Apply func. def.
(length (if #t ...))                   |Eval null?
(length '())                           |Eval if
0                                      |By length property 1

Inductive Hypothesis
--------------------
Assume that there exist an 'L' list for which the function reads,
(length (add_pairs L)) = 1/2*M
where M is the length of the list i.e. (length L)

Inductive Case
--------------
(length (add_pairs (cons a (cons a L))))                                                                                       |Call function on (cons a (cons a L))
(length (if (null? (cons a (cons a L)))) ...)                                                                                  |Apply func. def.
(length (if #f ...))                                                                                                           |Eval null?
(length (cons (+ (first (cons a (cons a L))) (second (cons a (cons a L)))) (add_pairs (rest(rest (cons a (cons a L)))))))      |Eval if
(length (cons (+ a (second (cons a (cons a L)))) (add_pairs (rest(rest (cons a (cons a L)))))))                                |first and cons cancel
(length (cons (+ a a) (add_pairs (rest(rest (cons a (cons a L)))))))                                                           |second and cons cancel 
(length (cons (+ a a) (add_pairs L)))                                                                                          |rest and cons cancel
(+ 1 (length (add_pairs L)))                                                                                                   |By length property 2
(+ 1 (1/2*M))                                                                                                                  |By IH
1/2*(length (cons a (cons a L)))                                                                                               |Call right hand of function on (cons a (cons a L))
1/2*(+ 1 (length (cons a L)))                                                                                                  |By length property 2
1/2*(+ 1 (+ 1 (length L)))                                                                                                     |By length property 2
1/2*(+ 1 (+ 1 M))                                                                                                              |By IH
1/2*(+ 2 M)                                                                                                                    |Algebraic simplification
(+ 1 (1/2*M))                                                                                                                  |Factor algebraically

Conclusion
----------
The first pair is added correctly, and the remaining pairs are assumed to be added correctly.
As the base case and both the right and left hand side of the function holds in the inductive case, we can say that
the function meets its output contract.

|#


;---------------------------------------------------------------------
;---------------------------------------------------------------------
;---------------------------------------------------------------------
;;;;;;;;;;;;;;Grade Summary;;;;;;;;;;;;;;;;;;;;;;;
(display "------Grade Summary------\n")
(display "Q1a Scored: ")
(display q1a_score)
(display "/10\n")
(display "Q1b Scored: ?/10 (Graded by TA)\n")
(display "Q2a Scored: ")
(display q2a_score)
(display "/10\n")
(display "Q2b Scored: ?/10 (Graded by TA)\n")
(display "Q3a Scored: ")
(display q3a_score)
(display "/10\n")
(display "Q3b Scored: ?/10 (Graded by TA)\n")
(display "Q4a Scored: ")
(display q4a_score)
(display "/10\n")
(display "Q4b Scored: ?/10 (Graded by TA)\n")
(display "Q5a Scored: ")
(display q5a_score)
(display "/10\n")
(display "Q5b Scored: ?/10 (Graded by TA)\n")


(define grand_total (+ q1a_score q2a_score q3a_score q4a_score q5a_score))
(display "\n")
(display "Total: ")
(display grand_total)
(display "/100\n")