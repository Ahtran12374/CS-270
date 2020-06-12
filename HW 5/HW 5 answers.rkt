#lang racket
(require rackunit)
(require rackunit/text-ui)

(define (lookup target environment)
  (cond
    [(null? environment) (error 'lookup "Variable Name Not Found")]
    [(equal? target (first(first environment))) (second(first environment))]
    [else
     (lookup target (rest environment))]
    )
  )

(define (is-reserved-word? word)
  (cond
   [ (equal? word '#t) #t]
   [ (equal? word '#f) #t]
   [ (equal? word 'OR) #t]
   [ (equal? word 'AND) #t]
   [ (equal? word 'NOT) #t]
   [ (equal? word 'IMPLIES) #t]
   [ (equal? word 'IFF) #t]
   ;Otherwise
   [ else #f]
   )
  )

(define (is-constant? expression)
  ;This is true when the expression is #t or #f
  ;It is false otherwise
  (or (equal? expression #t) (equal? expression #f))
 )

(define (eval-constant expression environment)
  expression
)

(define (is-variable? expression)
  (and
   (symbol? expression)
   (not (is-reserved-word? expression))
  )
)

(define (eval-variable variable environment)
  (lookup variable environment)
)

(define (is-or? expression)
  (equal? (first expression) 'OR)
)

(define (eval-or expression environment)
  ;Use the built in or to find the actual value
  ;so the or looks like (or something1 something2)
  ; (first expression) the word or
  ; (first (rest expression)) the expression something1
  ; (first (rest (rest expression))) the expression something2
  (or 
   (bool-eval (first (rest expression)) environment)
   (bool-eval (first (rest (rest expression))) environment)
  )
)

(define (is-and? expression) ;implemented
  (equal? (first expression) 'AND)
)

(define (eval-and expression environment) ;implemented
  (and 
   (bool-eval (first (rest expression)) environment)
   (bool-eval (first (rest (rest expression))) environment)
   )
  )

(define (is-not? expression) ;implemented
  (equal? (first expression) 'NOT)
)

(define (eval-not expression environment) ;implemented
  (not
   (bool-eval (first (rest expression)) environment)
   )
)

(define (is-implies? expression) ;implemented
  (equal? (first expression) 'IMPLIES)
)

(define (eval-implies expression environment) ;implemented
  (or
   (not (bool-eval (first (rest expression)) environment))
   (bool-eval (first (rest (rest expression))) environment)
   )
)

(define (is-iff? expression) ;implemented
  (equal? (first expression) 'IFF)
)

;need sprucing up
(define (eval-iff expression environment) ;implemented
  (and
   (or
   (not (bool-eval (first (rest expression)) environment))
   (bool-eval (first (rest (rest expression))) environment)
   )
   (or
   (not (bool-eval (first (rest (rest expression))) environment))
   (bool-eval (first (rest expression)) environment)
   )
   )
)

(define (bool-eval expression environment)
  (cond
    [;Case 1 Constants
     (is-constant? expression)
     (eval-constant expression environment)
    ]
    [;Case 2 Variables
     (is-variable? expression)
     (eval-variable expression environment)
    ]
    [;Case 3 or statements
     (is-or? expression)
     (eval-or expression environment)
    ]
    ;Case 4 not statements
    [(is-not? expression)
     (eval-not expression environment)
    ]
    ;Case 5 and statements
    [(is-and? expression)
     (eval-and expression environment)
    ]
    ;Case 6 implies statements
    [(is-implies? expression)
     (eval-implies expression environment)
    ]
    ;Case 7 iff
    [(is-iff? expression)
     (eval-iff expression environment)
    ]
    [;Else Case
     else
     (display "Expression given was invalid")
    ]
  )
)

(define (get-variables expression) ;implemented
  (cond
    [(is-constant? expression) '()]
    [(is-variable? expression) (cons expression '())]
    [else
     (remove-duplicates (append (get-variables (second expression)) (get-variables (last expression)) ))
     ] 
    )
  )




(define (make_bindings name)
  (list(list(list name #t)) (list(list name #f)))
)

(define (insert_binding binding environments) ;implemented
 (cond
   [(null? environments) '()]
   [else
    (map (lambda (x) (cons (first binding) x)) environments)
    ]
   )
  )


(define (insert_multiple_bindings bindings environments) ;implemented
 (cond
   [(null? environments) bindings]
   [(null? bindings) '()]
   [else
    (append (map (lambda (x) (append (first bindings) x)) environments) (insert_multiple_bindings (rest bindings) environments))
    ]
   )
  )

(define (extend_table var_name current_table) ;implemented
  (if (null? current_table)
      (make_bindings var_name)
      (insert_multiple_bindings (make_bindings var_name) current_table)
      )
  )
#|
(define (make-truth-table var_names) ;implemented
  (cond
    [(null? var_names) '()]
    [else
     (extend_table (first var_names) (make-truth-table (rest var_names)))
     ]
    )
)

(define (run-on-truth-table expression tt) ;implemented
  (if (null? tt)
      '()
      (cons (bool-eval expression (first tt)) (run-on-truth-table expression (rest tt)))
      )
)

(define (atleast-one-true my_list) ;implemented
  (if (null? my_list)
      #f
      (or (first my_list) (atleast-one-true (rest my_list)))
      )
)

(define (is-satisfied? bool-expression) ;implemented
  (atleast-one-true (run-on-truth-table bool-expression (make-truth-table (get-variables bool-expression))))
)
|#


