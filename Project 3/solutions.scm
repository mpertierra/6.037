#lang racket
;;
;; eval.scm - 6.037
;;
(require r5rs)
(define first car)
(define second cadr)
(define third caddr)
(define fourth cadddr)
(define rest cdr)
;; Tell DrRacket to print mutable pairs using the compact syntax for
;; ordinary pairs.
(print-as-expression #f)
(print-mpair-curly-braces #f)

;; mutable cons cell version of map
(define (mmap f lst)
  (if (null? lst)
      '()
      (cons (f (car lst)) (mmap f (cdr lst)))))

(define (tagged-list? exp tag)
  (and (pair? exp) (eq? (car exp) tag)))

(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        ((boolean? exp) #t)
        (else #f)))

(define (quoted? exp) (tagged-list? exp 'quote))
(define (text-of-quotation exp) (cadr exp))

(define (variable? exp) (symbol? exp))
(define (assignment? exp) (tagged-list? exp 'set!))
(define (assignment-variable exp) (cadr exp))
(define (assignment-value exp) (caddr exp))
(define (make-assignment var expr)
  (list 'set! var expr))

(define (definition? exp) (tagged-list? exp 'define))
(define (definition-variable exp)
  (if (symbol? (cadr exp))   (cadr exp)   (caadr exp)))
(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda (cdadr exp) (cddr exp))))  ; formal params, body
(define (make-define var expr)
  (list 'define var expr))

(define (lambda? exp) (tagged-list? exp 'lambda))
(define (lambda-parameters lambda-exp) (cadr lambda-exp))
(define (lambda-body lambda-exp) (cddr lambda-exp))
(define (make-lambda parms body) (cons 'lambda (cons parms body)))

(define (if? exp) (tagged-list? exp 'if))
(define (if-predicate exp) (cadr exp))
(define (if-consequent exp) (caddr exp))
(define (if-alternative exp) (cadddr exp))
(define (make-if pred conseq alt) (list 'if pred conseq alt))

(define (cond? exp) (tagged-list? exp 'cond))
(define (cond-clauses exp) (cdr exp))
(define first-cond-clause car)
(define rest-cond-clauses cdr)
(define (make-cond seq) (cons 'cond seq))

(define (let? expr) (tagged-list? expr 'let))
(define (let-bound-variables expr) (mmap first (second expr)))
(define (let-values expr) (mmap second (second expr)))
(define (let-body expr) (cddr expr)) ;differs from lecture--body may be a sequence
(define (make-let bindings body)
  (cons 'let (cons bindings body)))

(define (begin? exp) (tagged-list? exp 'begin))
(define (begin-actions begin-exp) (cdr begin-exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else (make-begin seq))))
(define (make-begin exp) (cons 'begin exp))

(define (application? exp) (pair? exp))
(define (operator app) (car app))
(define (operands app) (cdr app))
(define (no-operands? args) (null? args))
(define (first-operand args) (car args))
(define (rest-operands args) (cdr args))
(define (make-application rator rands)
  (cons rator rands))

(define (time? exp) (tagged-list? exp 'time))

;;
;; this section is the actual implementation of meval
;;

(define (m-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((unset? exp) (eval-unset exp env)) ;; ==== QUESTION 4 ====
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((and? exp) (eval-and exp env)) ;; ==== QUESTION 2 ====
        ((lambda? exp)
         (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (m-eval (cond->if exp) env))
        ((let? exp) (m-eval (let->application exp) env))
        ((until? exp) (m-eval (until->transformed exp) env)) ;; ==== QUESTION 3 ====
        ((time? exp) (time (m-eval (second exp) env)))
        ((application? exp)
         (m-apply (m-eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (error "Unknown expression type -- EVAL" exp))))

(define (m-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment (make-frame (procedure-parameters procedure)
                                          arguments)
                              (procedure-environment procedure))))
        (else (error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (cond ((no-operands? exps) '())
        (else (cons (m-eval (first-operand exps) env)
                    (list-of-values (rest-operands exps) env)))))

(define (eval-if exp env)
  (if (m-eval (if-predicate exp) env)
      (m-eval (if-consequent exp) env)
      (m-eval (if-alternative exp) env)
      ))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (m-eval (first-exp exps) env))
        (else (m-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (m-eval (assignment-value exp) env)
                       env))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (m-eval (definition-value exp) env)
                    env))

(define (let->application expr)
  (let ((names (let-bound-variables expr))
        (values (let-values expr))
        (body (let-body expr)))
    (make-application (make-lambda names body)
                      values)))

(define (cond->if expr)
  (let ((clauses (cond-clauses expr)))
    (if (null? clauses)
        #f
        (if (eq? (car (first-cond-clause clauses)) 'else)
            (sequence->exp (cdr (first-cond-clause clauses)))
            (make-if (car (first-cond-clause clauses))
                     (sequence->exp (cdr (first-cond-clause clauses)))
                     (make-cond (rest-cond-clauses clauses)))))))

(define input-prompt ";;; M-Eval input level ")
(define output-prompt ";;; M-Eval value:")

(define (driver-loop) (repl #f))

(define (repl port)
  (if port #f (prompt-for-input input-prompt))
  (let ((input (if port (read port) (read))))
    (cond ((eof-object? input)   'meval-done)
          ((eq? input '**quit**) 'meval-done)
          (else
           (let ((output (m-eval input the-global-environment)))
             (if port #f (begin
                           (announce-output output-prompt)
                           (pretty-display output)))
             (repl port))))))

(define (prompt-for-input string)
  (newline) (newline) (display string) (display meval-depth) (newline))

(define (announce-output string)
  (newline) (display string) (newline))


;;
;;
;; implementation of meval environment model
;;

; double bubbles
(define (make-procedure parameters body env)
  (list 'procedure parameters body env))
(define (compound-procedure? proc)
  (tagged-list? proc 'procedure))
(define (procedure-parameters proc) (second proc))
(define (procedure-body proc) (third proc))
(define (procedure-environment proc) (fourth proc))


; bindings
(define (make-binding var val)
  (list 'binding var val))
(define (binding? b)
  (tagged-list? b 'binding))
(define (binding-variable binding)
  (if (binding? binding)
      (second binding)
      (error "Not a binding: " binding)))
(define (binding-value binding)
  (if (binding? binding)
      (third binding)
      (error "Not a binding: " binding)))
; (define (set-binding-value! binding val)     ;; ==== QUESTION 4 ====  Redefined, moved to end of file.
;   (if (binding? binding)
;       (set-car! (cddr binding) val)
;       (error "Not a binding: " binding)))

; frames
(define (make-frame variables values)
  (define (make-frame-bindings rest-vars rest-vals)
    (cond ((and (null? rest-vars) (null? rest-vals))
           '())
          ((null? rest-vars)
           (error "Too many args supplied" variables values))
          ((symbol? rest-vars)
           (list (make-binding rest-vars rest-vals)))
          ((null? rest-vals)
           (error "Too few args supplied" variables values))
          (else
           (cons (make-binding (car rest-vars) (car rest-vals))
                 (make-frame-bindings (cdr rest-vars) (cdr rest-vals))))))
  (make-frame-from-bindings (make-frame-bindings variables values)))

(define (make-frame-from-bindings list-of-bindings)
  (cons 'frame list-of-bindings))

(define (frame? frame)
  (tagged-list? frame 'frame))
(define (frame-variables frame)
  (if (frame? frame)
      (mmap binding-variable (cdr frame))
      (error "Not a frame: " frame)))
(define (frame-values frame)
  (if (frame? frame)
      (mmap binding-value (cdr frame))
      (error "Not a frame: " frame)))
(define (add-binding-to-frame! binding frame)
  (if (frame? frame)
      (if (binding? binding)
          (set-cdr! frame (cons binding (cdr frame)))
          (error "Not a binding: " binding))
      (error "Not a frame: " frame)))
(define (find-in-frame var frame)
  (define (search-helper var bindings)
    (if (null? bindings)
        #f
        (if (eq? var (binding-variable (first bindings)))
            (first bindings)
            (search-helper var (rest bindings)))))
  (if (frame? frame)
      (search-helper var (cdr frame))
      (error "Not a frame: " frame)))

; environments
(define the-empty-environment '(environment))
(define (extend-environment frame base-env)
  (if (environment? base-env)
      (if (frame? frame)
          (list 'environment frame base-env)
          (error "Not a frame: " frame))
      (error "Not an environment: " base-env)))
(define (environment? env)
  (tagged-list? env 'environment))
(define (enclosing-environment env)
  (if (environment? env)
      (if (eq? the-empty-environment env)
          (error "No enclosing environment of the empty environment")
          (third env))
      (error "Not an environment: " env)))
(define (environment-first-frame env)
  (if (environment? env)
      (second env)
      (error "Not an environment: " env)))
(define (find-in-environment var env)
  (if (eq? env the-empty-environment)
      #f
      (let ((frame (environment-first-frame env)))
        (let ((binding (find-in-frame var frame)))
          (if binding
              binding
              (find-in-environment var (enclosing-environment env)))))))


; name rule
(define (lookup-variable-value var env)
  (let ((binding (find-in-environment var env)))
    (if binding
        (binding-value binding)
        (error "Unbound variable -- LOOKUP" var))))

(define (set-variable-value! var val env)
  (let ((binding (find-in-environment var env)))
    (if binding
        (set-binding-value! binding val)
        (error "Unbound variable -- SET" var))))

(define (define-variable! var val env)
  (let ((frame (environment-first-frame env)))
    (let ((binding (find-in-frame var frame)))
      (if binding
          (reset-binding! binding val) ;; ==== QUESTION 4 ====
          (add-binding-to-frame!
           (make-binding var val)
           frame)))))

; primitives procedures - hooks to underlying Scheme procs
(define (make-primitive-procedure implementation)
  (list 'primitive implementation))
(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (primitive-procedures)
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'set-car! set-car!)
        (list 'set-cdr! set-cdr!)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '< <)
        (list '> >)
        (list '= =)
        (list 'display display)
        (list 'not not)
        ;; ==== QUESTION 1 ====
        (list '* *)
        (list '/ /)
        (list 'list list)
        (list 'cadr cadr)
        (list 'cddr cddr)
        (list 'newline newline)
        (list 'printf printf)
        (list 'length length)
        ))

(define (primitive-procedure-names) (mmap car (primitive-procedures)))

(define (primitive-procedure-objects)
  (mmap make-primitive-procedure (mmap cadr (primitive-procedures))))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

; used to initialize the environment
(define (setup-environment)
  (extend-environment (make-frame (primitive-procedure-names)
                                  (primitive-procedure-objects))
                      the-empty-environment))

(define the-global-environment (setup-environment))



;;;;;;;; Code necessary for question 6
;;
;; This section doesn't contain any user-servicable parts -- you
;; shouldn't need to edit it for any of the questions on the project,
;; including question 5.  However, if you're curious, comments provide a
;; rough outline of what it does.

;; Keep track of what depth we are into nesting
(define meval-depth 1)

;; These procedures are needed to make it possible to run inside meval
(define additional-primitives
  (list (list 'eof-object?      eof-object?)
        (list 'read             read)
        (list 'read-line        read-line)
        (list 'open-input-file  open-input-file)
        (list 'this-expression-file-name
                    (lambda () (this-expression-file-name)))
        (list 'pretty-display   pretty-display)
        (list 'error            error)
        (list 'apply            m-apply))) ;; <-- This line is somewhat interesting
(define stubs
  '(require r5rs mzlib/etc print-as-expression print-mpair-curly-braces))
(define additional-names (mmap first additional-primitives))
(define additional-values (mmap make-primitive-procedure
                               (mmap second additional-primitives)))

(require mzlib/etc)
(define (load-meval-defs)
  ;; Jam some additional bootstrapping structures into the global
  ;; environment
  (set! the-global-environment
        (extend-environment
         (make-frame stubs
                     (mmap (lambda (name)
                             (m-eval '(lambda (x) x) the-global-environment)) stubs))
         (extend-environment
          (make-frame additional-names
                      additional-values)
          the-global-environment)))
  ;; Open this file for reading
  (let ((stream (open-input-file (this-expression-file-name))))
    (read-line stream) ;; strip off "#lang racket" line
    (repl stream))     ;; feed the rest of the definitions into meval

  ;; Update the meval-depth variable inside the environment we're simulating
  (set-variable-value! 'meval-depth (+ meval-depth 1) the-global-environment)
  'loaded)


;; ==== QUESTION 1 ====

;; See the procedure "primitive-procedures" for implementation.

;; Test cases:
; (* 5 10)     ;; => 50
; (/ 5 10)     ;; => 1/2
; (list 1 2 3) ;; => (1 2 3)
; (define x (list 1 2 3 4 5)) ;; => #<void>
; (cadr x)     ;; => 2
; (cddr x)     ;; => (3 4 5)
; (newline)    ;; => Should print a new line. There should be two blank lines now (instead of one) before the next ";;; M-Eval value:"
; (printf "Testing: ~a" 5) ;; Should print "Testing: 5"
; (length x)   ;; => 5


;; ==== QUESTION 2 ====

;; Attempting to define an "and" procedure at the M-Eval prompt will not work because
;; "and" needs to be special form and not just a procedure. Attempting to implement "and"
;; as a procedure at the M-Eval prompt could result in some unintended side-effects, as the arguments
;; passed to our "and" procedure would be evaluated before the procedure is applied.

;; Suppose we enter the definition below at the M-Eval prompt.

;; (define and (lambda (x y) (if x (begin (if y y #f)) #f)))

;; It defines "and" to be a procedure that takes two arguments "x" and "y".
;; If "x" is truthy, it checks if "y" is truthy, and returns "y" if it is truthy; it returns false, otherwise.
;; If "x" is false then it returns false.

;; It almost seems like it would work, as it seems to have the same short-ciruit logic that we would expect.
;; However, consider the sequence of expressions below:

;; (define z 5)
;; (and #f (set! z 500000))  ;; => #f
;; z                         ;; => 500000

;; What is the value of "z" after this?
;; We would expect short-ciruit evaluation of "and" to stop after seeing the first argument being false,
;; and so "z" would still have value 5.

;; However, as mentioned before, because "and" here is not a special form, its arguments will be evaluated before
;; the "and" is applied, so the second argument (set! z 500000) will always be evaluated no matter what the first
;; argument is, and thus "z" will have value 500000.

;; We can test that "and" as a special form does have the proper short-circuit evaluation and if we ran the
;; same expressions above without using our evaluator, "z" would have value 5.

(define (and? exp) (tagged-list? exp 'and))
(define (and-clauses exp) (cdr exp))
(define (first-and-clause exp) (car (and-clauses exp)))
(define (rest-and-clauses exp) (cdr (and-clauses exp)))
(define (empty-and-clauses? exp) (null? (and-clauses exp)))
(define (last-and-clause? exp) (null? (cdr (and-clauses exp))))
(define (make-and clauses) (cons 'and clauses))
(define (eval-and exp env)
  (cond
    ((empty-and-clauses? exp) #t)
    ((last-and-clause? exp) (m-eval (first-and-clause exp) env))
    (else
      (and
        (m-eval (first-and-clause exp) env)
        (eval-and (make-and (rest-and-clauses exp)) env)))))

;; Test cases:
; (and)             ;; => #t
; (and #f)          ;; => #f
; (and 5)           ;; => 5
; (and 1 2 3)       ;; => 3
; (and 1 (< 3 1) 3) ;; => #f
; (define x 3)
; (and (< x 5) (= 6 6) (null? '())) ;; => #t
; (set! x 6)
; (and (< x 5) (= 6 6) (null? '())) ;; => #f
; (define z 5)
; (and #f (set! z 500000))  ;; => #f
; (and z)                   ;; => 5
; (and (set! z 500000) z)   ;; => 500000
; (and z)                   ;; => 500000


;; ==== QUESTION 3 ====

(define (until? exp) (tagged-list? exp 'until))
(define (until-test exp) (cadr exp))
(define (until-exps exp) (cddr exp))
(define (until->transformed exp)
  `(let ()
    (define (loop)
      (if ,(until-test exp)
        #t
        (begin
          ,@(until-exps exp)
          (loop))))
    (loop)))

;; Test cases:
;; Running this outside of our evaluator
; (define u1 '(until (> x n) (printf "~s~n" x) (set! x (+ x 1))))
; (until->transformed u1)    ;; => (let () (define (loop) (if (> x n) #t (begin (printf "~s~n" x) (set! x (+ x 1)) (loop)))) (loop))
; (define u2 '(until test))
; (until->transformed u2)    ;; => (let () (define (loop) (if test #t (begin (loop)))) (loop))
; (define u3 '(until test exp1))
; (until->transformed u3)    ;; => (let () (define (loop) (if test #t (begin exp1 (loop)))) (loop))
; (define u4 '(until test exp1 exp2 exp3))
; (until->transformed u4)    ;; => (let () (define (loop) (if test #t (begin exp1 exp2 exp3 (loop)))) (loop))
;; Running this using our evaluator
; (define x 0)
; (define n 5)
; (until (> x n) (printf "~s~n" x) (set! x (+ x 1))) ;; Should print 0 1 2 3 4 5 (on separate lines) and return true


;; ==== QUESTION 4 ====

;; A binding keeps track of the all of the values the variable has had over time
;; If a variable "var" has had values "val1", "val2", ..., "valn-1", "valn" over time (starting with "val1" and ending with "valn"),
;; then the binding would be (list 'binding var valn valn-1 ... val2 val1)

;; If n > 1, then when we unset, the binding becomes (list 'binding var valn-1 ... val2 val1)
;; Else, if n == 1 (that is, the binding is currently (list 'binding var val1)),
;;       then when we unset, the binding remains the same (list 'binding var val1)

;; When we define a variable that already had a binding, we reset the binding by clearing the history of previous values

(define (one-binding-value? binding) (null? (cdddr binding)))
(define (set-binding-value! binding val)
  (if (binding? binding)
      (set-cdr! (cdr binding) (cons val (cddr binding)))
      (error "Not a binding: " binding)))
(define (unset-binding-value! binding)
  (cond
    ((not (binding? binding)) (error "Not a binding: " binding))
    ((one-binding-value? binding) (void))
    (else
      (set-cdr! (cdr binding) (cdddr binding)))))
(define (reset-binding! binding val)
  (if (binding? binding)
      (set-cdr! (cdr binding) (cons val '()))
      (error "Not a binding: " binding)))


(define (unset? exp) (tagged-list? exp 'unset!))
(define (unset-variable exp) (cadr exp))
(define (eval-unset exp env)
  (let ((var (unset-variable exp)))
    (let ((binding (find-in-environment var env)))
      (if binding
        (unset-binding-value! binding)
        (error "Unbound variable -- UNSET" var)))))

;; Test cases:
; (unset! x) ;; Should throw error with message "Unbound variable -- UNSET x"
; (define x 5)
; x ;; => 5
; (set! x 10)
; (set! x 11)
; (set! x 12)
; x ;; => 12
; (unset! x)
; x ;; => 11 (unset! undoes one of the set!'s)
; (unset! x)
; x ;; => 10 (unset! undoes the previous set!)
; (unset! x)
; x ;; => 5 (and again)
; (unset! x)
; x ;; => 5 (unset! more times than set! is fine)
; (define x 15)
; (unset! x)
; x ;; => 15 (most recent value defined to x)



