#lang racket
;;
;; 6.037
;;
;; PROJECT 4: Implementation of an OO System
;;            (a.k.a. "The Adventure Game")


; Racket-specific setup
(print-as-expression #f)
(print-mpair-curly-braces #f)

(require r5rs)
(define first car)
(define second cadr)
(define third caddr)
(define fourth cadddr)
(define (fifth x) (car (cddddr x)))
(define rest cdr)


;; ---- Scheme-like evaluator in scheme -----------------------------------
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
(define (let-bound-variables expr) (map first (second expr)))
(define (let-values expr) (map second (second expr)))
(define (let-body expr) (cddr expr))
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

;;
;; this section is the actual implementation of oo-eval
;;

(define (oo-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-parameters exp) (lambda-body exp) env))
        ((begin? exp) (eval-sequence (begin-actions exp) env))
        ((cond? exp) (oo-eval (cond->if exp) env))
        ((let? exp) (oo-eval (let->application exp) env))
        ((and? exp) (eval-and exp env))
        ((or? exp) (eval-or exp env))
        ((make-class? exp) (eval-make-class exp env)) ;; PROBLEM 2
        ((application? exp)
         (oo-apply (oo-eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else (oo-error "Unknown expression type -- EVAL" exp))))

(define (oo-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment (make-frame (procedure-parameters procedure)  
                                          arguments)
                              (procedure-environment procedure))))
        ((instance? procedure) (oo-apply-instance procedure arguments)) ;; PROBLEM 4
        (else (oo-error "Unknown procedure type -- APPLY" procedure))))

(define (list-of-values exps env)
  (cond ((no-operands? exps) '())
        (else (cons (oo-eval (first-operand exps) env)
                    (list-of-values (rest-operands exps) env)))))

(define (eval-if exp env)
  (if (oo-eval (if-predicate exp) env)
      (oo-eval (if-consequent exp) env)
      (oo-eval (if-alternative exp) env)
      ))

(define (eval-sequence exps env)
  (cond ((last-exp? exps) (oo-eval (first-exp exps) env))
        (else (oo-eval (first-exp exps) env)
              (eval-sequence (rest-exps exps) env))))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-variable exp)
                       (oo-eval (assignment-value exp) env)
                       env))

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
                    (oo-eval (definition-value exp) env)
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

(define (and? exp) (tagged-list? exp 'and))
(define (and-clauses exp) (cdr exp))

(define (eval-and exp env)
  (define (and-helper clauses)
    (let ((val (oo-eval (car clauses) env)))
      (cond ((last-pair? clauses)
             val)
            (val
             (and-helper (cdr clauses)))
            (else
             #f))))
  (if (last-pair? exp)
      #t
      (and-helper (and-clauses exp))))

(define (or? exp) (tagged-list? exp 'or))
(define (or-clauses exp) (cdr exp))

(define (eval-or exp env)
  (define (or-helper clauses)
    (let ((val (oo-eval (car clauses) env)))
      (cond ((last-pair? clauses)
             val)
            (val
             val)
            (else
             (or-helper (cdr clauses))))))
  (if (last-pair? exp)
      #f
      (or-helper (or-clauses exp))))

;; Assumes lst is not-null
(define (last-pair? lst)
  (null? (cdr lst)))

(define input-prompt ";;; OO-Eval input")
(define output-prompt ";;; OO-Eval value:")
(define error-prompt ";;; OO-Eval error:")
(define (driver-loop) (repl #f))

(define *restart-repl* (lambda () (error "oo-eval error outside of REPL")))

(define oo-error error)

(define (oo-error-restart . msgs)
  (newline) (display error-prompt) (pretty-display msgs) (newline)
  (*restart-repl*))

(define (repl port)
  (set! oo-error oo-error-restart)
  (if (not port)
      (begin
        (call/cc (lambda (c) (set! *restart-repl* c)))
        (prompt-for-input input-prompt)))
  (let ((input (if port (read port) (read))))
    (cond ((eof-object? input)   'oo-eval-done)
          ((eq? input '**quit**) 'oo-eval-done)
          (else
           (let ((output (oo-eval input the-global-environment)))
             (if (not port)
                 (begin
                   (announce-output output-prompt)
                   (pretty-display output)))
             (repl port))))))

(define (prompt-for-input string)
  (newline) (newline) (display string) (newline))

(define (announce-output string)
  (newline) (display string) (newline))

(define (run-file filename)
  (repl (open-input-file filename))
  (set! oo-error error) ;; reset, no longer in repl
  'loaded)


;;
;;
;; implementation of oo-eval environment model
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
(define (make-binding-shared L)
  (cons 'binding L))
(define (binding? b)
  (tagged-list? b 'binding))
(define (binding-variable binding)
  (if (binding? binding)
      (second binding)
      (oo-error "Not a binding: " binding)))
(define (binding-value binding)
  (if (binding? binding)
      (third binding)
      (oo-error "Not a binding: " binding)))
(define (set-binding-value! binding val)
  (if (binding? binding)
      (set-car! (cddr binding) val)
      (oo-error "Not a binding: " binding)))

; frames
(define (make-frame variables values)
  (define (make-frame-bindings rest-vars rest-vals)
    (cond ((and (null? rest-vars) (null? rest-vals))
           '())
          ((null? rest-vars)
           (oo-error "Too many args supplied" variables values))
          ((symbol? rest-vars)
           (list (make-binding rest-vars rest-vals)))
          ((null? rest-vals)
           (oo-error "Too few args supplied" variables values))
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
      (map binding-variable (cdr frame))
      (oo-error "Not a frame: " frame)))
(define (frame-values frame)
  (if (frame? frame)
      (map binding-value (cdr frame))
      (oo-error "Not a frame: " frame)))
(define (add-binding-to-frame! binding frame)
  (if (frame? frame)
      (if (binding? binding)
          (set-cdr! frame (cons binding (cdr frame)))
          (oo-error "Not a binding: " binding))
      (oo-error "Not a frame: " frame)))
(define (find-in-frame var frame)
  (define (search-helper var bindings)
    (if (null? bindings)
        #f
        (if (eq? var (binding-variable (first bindings)))
            (first bindings)
            (search-helper var (rest bindings)))))
  (if (frame? frame)
      (search-helper var (cdr frame))
      (oo-error "Not a frame: " frame)))

; environments
(define the-empty-environment '(environment))
(define (extend-environment frame base-env)
  (if (environment? base-env)
      (if (frame? frame)
          (list 'environment frame base-env)
          (oo-error "Not a frame: " frame))
      (oo-error "Not an environment: " base-env)))
(define (environment? env)
  (tagged-list? env 'environment))
(define (enclosing-environment env)
  (if (environment? env)
      (if (eq? the-empty-environment env)
          (oo-error "No enclosing environment of the empty environment")
          (third env))
      (oo-error "Not an environment: " env)))
(define (environment-first-frame env)
  (if (environment? env)
      (second env)
      (oo-error "Not an environment: " env)))
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
        (oo-error "Unbound variable -- LOOKUP" var))))

(define (set-variable-value! var val env)
  (let ((binding (find-in-environment var env)))
    (if binding
        (set-binding-value! binding val)
        (oo-error "Unbound variable -- SET" var))))

(define (define-variable! var val env)
  (let ((frame (environment-first-frame env)))
    (let ((binding (find-in-frame var frame)))
      (if binding
          (set-binding-value! binding val)
          (add-binding-to-frame!
           (make-binding var val)
           frame)))))

; primitive procedures - hooks to underlying Scheme procs
(define (make-primitive-procedure implementation)
  (list 'primitive implementation))
(define (primitive-procedure? proc) (tagged-list? proc 'primitive))
(define (primitive-implementation proc) (cadr proc))
(define (primitive-procedures)
  (list (list 'car car)
	(list 'cadr cadr)
        (list 'cdr cdr)
	(list 'first first)
	(list 'second second)
	(list 'third third)
	(list 'fourth fourth)
	(list 'fifth fifth)
	(list 'rest cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list 'memq memq)
	(list 'assq assq)

        (list 'not not)
        (list 'eq? eq?)
        (list 'equal? equal?)
        (list 'pair? pair?)
        (list 'symbol-append
              (lambda args
                (string->symbol (apply string-append
                                       (map symbol->string args)))))

        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        (list '< <)
        (list '> >)
        (list '= =)
        (list 'random random)
        (list 'symbol-prefix? symbol-prefix?)

        (list 'display display)
        (list 'newline newline)

        (list 'run-file run-file)
        (list 'apply oo-apply)
  (list 'new make-instance) ;; PROBLEM 3
  ;; PROBLEM 10
  (list 'default-metaclass default-metaclass)
  (list 'create-class create-class)

        ))

(define (primitive-procedure-names) (map car (primitive-procedures)))

(define (primitive-procedure-objects)
  (map make-primitive-procedure (map cadr (primitive-procedures))))

(define (apply-primitive-procedure proc args)
  (apply (primitive-implementation proc) args))

; used to initialize the environment
(define (setup-environment)
  (extend-environment (make-frame (primitive-procedure-names)
                                  (primitive-procedure-objects))
                      the-empty-environment))

;; ----optional stuff for problem 6--------------------------------------------

; Tools needed to make opaque instance abstractions, whenver I get
; around to that.  Must remember to modify instance abstraction to use
; later. -BB <PROBLEM 6> [OPTIONAL]
; make-label: produces a symbol for this class to display instead of representation
(define (make-label class)
  (class-name class))

; box-instance: given an instance and a label, put that instance in the global instance table and
; return a unique id that can look it up later via lookup-instance. The key should include the
; descriptive label
(define (hide-instance label instance)
  (let ((uid (box label)))
    (hash-set! *the-instance-table* uid instance)
    uid))

; Use to test if something is an instance uid.
(define instance-uid? box?)

(define (lookup-instance instance-uid)
  (if (not (box? instance-uid))
      (oo-error "Lookup failed: Opaque instance (uid) required, but was given:" instance-uid)
      (let ((instance (hash-ref *the-instance-table* instance-uid #f)))
        (if (not instance)
            (oo-error "Could not find the object in the table. Bug or you are cheating. Given:" instance-uid)
            instance))))

(define *the-instance-table* (make-hasheq))

;; --------------NEW FOR PROJECT 4: OOP! ------------------------------

; An instance is a 2 element list of: the symbol 'instance
; and an association list of all slot names and current values
; Note that the slots include :class, the instance's class

(define (instance? obj)
  (tagged-list? obj 'instance))

(define (instance-class inst)
  (read-slot inst ':class))

(define (instance-state inst)
  (second inst))

; Given an object instance and slot name, find the slot's current value
(define (read-slot instance varname)
  (let ((result (assq varname (instance-state instance))))
    (if result
        (cadr result)
        (oo-error "read-slot: No state named" varname))))

; Similarly, update the value in a slot
(define (write-slot! instance varname value)
  (let ((result (assq varname (instance-state instance))))
    (if result
        (set-car! (cdr result) value)
        (oo-error "write-slot!: No state named" varname))))

; Create an object instance from a class
; Store list of slots' data (including class as :class) in a tagged-list
; All slots other than :class start out with the value 'uninitialized
; Need to call the constructor if there is one
(define (make-instance class . args)
  (let ((instance
         (list 'instance
               (cons (list ':class class)
                     (map
                      (lambda (x) (list x 'uninitialized))
                      (invoke class 'GET-SLOTS))))))

    (if (class-has-method? class 'CONSTRUCTOR)
        (method-call instance 'CONSTRUCTOR class args))

    ; return the constructed instance
    instance))

;; class abstraction: classes are instances of the default-metaclass
(define (class? c) (instance? c))
(define (class-name class)
  (read-slot class ':name))

(define (create-class name parent-class slots methods)
  (make-instance default-metaclass name parent-class slots methods)) ;; PROBLEM 1


; What's the class of a class? You just met-ah!
; Manually create an instance representing the default-metaclass, to bootstrap
; These methods are procedures in underlying scheme, not ones created by oo-eval;
; as a side effect on how we deal with these, they have to take a "self" arg.
; This version contains just the necessities to allow class creation 
; (CONSTRUCTOR), instantiation (GET-SLOTS), and method calls (FIND-METHOD).
(define default-metaclass
  `(instance
    ((:class <put-default-metaclass-here>)
     (:name class)
     (:parent-class #f)
     (:slots (:name :parent-class :slots :methods))
     (:methods
      ((CONSTRUCTOR ,(lambda (self name parent slots methods)
                       (write-slot! self ':name name)
                       (write-slot! self ':parent-class parent)
                       (write-slot! self ':slots slots)
                       (write-slot! self ':methods methods)))
       ;; Returns "method-info" see below
       (FIND-METHOD ,(lambda (self methodname)
		      (let ((my-method (assq methodname (read-slot self ':methods))))
			(if my-method
			    (make-method-info methodname (second my-method) (read-slot self ':parent-class))
			    (if (read-slot self ':parent-class)
				 (invoke (read-slot self ':parent-class) 'FIND-METHOD methodname)
				 #f)))))

       (GET-SLOTS ,(lambda (self)
		     (append (read-slot self ':slots)
			     (if (read-slot self ':parent-class)
				 (invoke (read-slot self ':parent-class) 'GET-SLOTS)
				 '()))))

       ;; These are not needed to bootstrap, but enable some introspection
       (GET-CLASS   ,(lambda (self) (read-slot self ':class)))
       (GET-METHODS ,(lambda (self)
		       (append (map car (read-slot self ':methods))
			       (if (read-slot self ':parent-class)
				   (invoke (read-slot self ':parent-class) 'GET-METHODS)
				   '()))))


       ;; PROBLEM 12
       (GET-PARENT-CLASS ,(lambda (self) (read-slot self ':parent-class)))

       ;; PROBLEM 5
       (GET-TYPES ,(lambda (self)
         (append (list (read-slot self ':name))
           (if (read-slot self ':parent-class)
         (invoke (read-slot self ':parent-class) 'GET-TYPES)
         '())))))))))



;; The default-metaclass's class? Why, the default-metaclass, of course.
(set-car! (cdaadr default-metaclass) default-metaclass)


;; oo-eval should call this to handle the make-class special form
(define (make-class? exp) (tagged-list? exp 'make-class))
(define (make-class-name exp) (second exp))
(define (make-class-parent-class exp) (third exp))
(define (make-class-slots exp) (fourth exp))
(define (make-class-methods exp) (fifth exp))
(define (eval-make-class exp env)  ;; PROBLEM 2
  (create-class
    (oo-eval (make-class-name exp) env)
    (oo-eval (make-class-parent-class exp) env)
    (make-class-slots exp)
    (map
      ;; method variable is a two-element list containing the method's name and its procedure
      ;; we must evaluate the procedure but not the name for each method
      (lambda (method) (list (first method) (oo-eval (second method) env)))
      (make-class-methods exp))))


;; PROBLEM 4
(define (oo-apply-instance instance arguments)
  (if (null? arguments)
      (oo-error "Applications of instances must include a method name as the first argument. Instance:" instance)
      (let ((methodname (car arguments))
            (methodargs (cdr arguments)))
        (method-call instance methodname (instance-class instance) methodargs))))

;; Call a method on an object (variable arguments form)
(define (invoke instance method . args)
  (method-call instance method (instance-class instance) args))

;; Call a method on an object, starting the method search at current-class. args are in a list.
(define (method-call instance method current-class args)
  (let ((info (find-class-method-info method current-class)))
    (if info
	(apply-method instance info args)
	(oo-error "No such method" method))))


;; Apply a method procedure. Normally an oo-eval procedure, although can also be a normal underlying scheme procedure
;; in the case of the original default-metaclass definition
(define (apply-method instance methodinfo args)
  (let ((proc (method-info-proc methodinfo))
	(parent-class (method-info-parent-class methodinfo)))
    (if (procedure? proc)          ;; Detect procedure from underlying scheme
	(apply proc instance args) ;; Kludge to make the default-metaclass' methods work and bootstrap us.
	(let* ((proc-env  
	        (procedure-environment proc)) ;; Normal case, method defined by user through oo-eval with make-class
         (slots-env  ;; PROBLEM 4
            (extend-environment
              (make-frame-from-bindings
                (map make-binding-shared (instance-state instance)))
              proc-env))
	       (args-env
	        (extend-environment (make-frame (procedure-parameters proc) 
                                                args)
                                    slots-env))
	       (selfsuper-env 
                (extend-environment (make-frame '(self super)
                                                (list instance (make-super instance parent-class)))
                                    args-env)))
	  (eval-sequence
	   (procedure-body proc)
	   selfsuper-env)))))

; builds a procedure that starts a method search at parent, for "super"
(define (make-super instance parent-class)
  (make-primitive-procedure
   (lambda (method . args)
     (method-call instance method parent-class args))))

;; We want to let the metaclass' idea of method-searching define our behavior.
;; In order to call the FIND-METHOD method, we need some other way of getting at it.
;; Here we reach into the metaclass directly to look in its :methods slot and get FIND-METHOD.
;; This is an assumption we're making about the metaclass.
(define (find-class-method-info methodname class)
  (if (class? class)
      (let ((get-method-alist-entry (assq 'FIND-METHOD (read-slot (instance-class class) ':methods))))
	(if get-method-alist-entry
	    (apply-method class 
			  (make-method-info 'FIND-METHOD (second get-method-alist-entry) (read-slot (instance-class class) ':parent-class))
			  (list methodname))
	    (oo-error "Metaclass does not define FIND-METHOD method; impossible to call methods")))
      #f))

;; FIND-METHOD is supposed to return a "method-info" object
(define (make-method-info name proc parent-class)
  (list 'method-info name proc parent-class))
(define method-info-name second)
(define method-info-proc third)
(define method-info-parent-class fourth)

(define (class-has-method? class method)
  (if (find-class-method-info method class)
      #t
      #f))


;; ----------------------------------------------------------------------

;; Might come in handy sometime
(define (symbol-prefix? prefix sym)
  (regexp-match (string-append "^" 
                               (symbol->string prefix)
                               ".*")
                (symbol->string sym)))

;; ----------------------------------------------------------------------

;;; Define the global environment now that all the primitive procedures exist
(define the-global-environment (setup-environment))

;; Load some additional useful procedures into the evaluator's GE
;; The contents of this file are interpreted by oo-eval
(run-file "oo-eval-extras.scm")



;; if outside oo-eval
(define (run-game yourname)
  (run-file "oo-world.scm")
  (oo-eval `(setup ',yourname) the-global-environment)
  (driver-loop))

;; if inside
(oo-eval
 '(define (run-game yourname)
    (run-file "oo-world.scm")
    (setup yourname))
 the-global-environment)


;; Load Alyssa's tests
(require racket/include)
(include "alyssa-tests.scm")

;; Run all the tests
(run-all-tests)

;; If desired, uncomment to start up the evaluator REPL when you run this file
;(driver-loop)

;; Or to start the game:
;(run-game 'your-name)


;; PROBLEM 7
;; A funny excerpt, louis-reasoner and cy-d-fect fight over shotgun
; At green-building-roof louis-reasoner says -- Hi cy-d-fect 
; At green-building-roof louis-reasoner says -- I take shotgun from cy-d-fect 
; At green-building-roof cy-d-fect says -- Hey! You took my shotgun 
; At green-building-roof cy-d-fect says -- Yaaah! I am upset! 
; At green-building-roof cy-d-fect says -- I take shotgun from louis-reasoner 
; At green-building-roof louis-reasoner says -- Hey! You took my shotgun 
; At green-building-roof louis-reasoner says -- Yaaah! I am upset! 
;; Running '((me 'GET-CLASS) 'GET-METHODS) gives output below. This works because it looks up all
;; the methods for me's class (AVATAR), and recursively calls 'GET-METHODS on the parent class.
;; Thus, we get to see all of the methods defined throughout our "chain" of inheritance.
;;     AVATAR -> PERSON -> MOBILE-THING -> THING -> CONTAINER -> NAMED-OBJECT -> ROOT-CLASS
;; We see duplicates because some classes overwrite methods already defined in a parent class (for example, the CONSTRUCTOR method).
;; To fix this, we need to change the code where the 'GET-METHODS method is defined. This is found
;; in default-metaclass (all classes inherit from this class). In the implementation of 'GET-METHODS, 
;; we could just filter out (from the current class's methods) the methods that are already found 
;; in the list of methods obtained by calling 'GET-METHODS on the parent class.
; (LOOK-AROUND
;  GO
;  CONSTRUCTOR
;  SAY
;  PEOPLE-AROUND
;  STUFF-AROUND
;  PEEK-AROUND
;  TAKE
;  DROP
;  HAVE-FIT
;  LOSE
;  GO-EXIT
;  GO
;  ENTER-ROOM
;  SUFFER
;  DIE
;  CHANGE-LOCATION
;  ENTER-ROOM
;  LEAVE-ROOM
;  CONSTRUCTOR
;  LOCATION
;  EMIT
;  DESTROY
;  CONSTRUCTOR
;  ADD-THING
;  DEL-THING
;  THINGS
;  HAVE-THING?
;  CONSTRUCTOR
;  NAME
;  DESTROY
;  GET-CLASS)

