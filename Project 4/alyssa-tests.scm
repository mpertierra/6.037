;; This file is loaded automatically when you run oo-eval.scm. Do not
;; try to run this file directly.

(define *all-tests* '())

(define (make-test desc test)
  (let ((result
         (lambda ()
           (display "RUNNING TEST: ")
           (display desc)
           (newline)
           (test error))))
    (set! *all-tests* (append *all-tests* (list result)))
    result))

(define (run-all-tests)
  (for-each (lambda (test) (display (test)) (newline)) *all-tests*))

(define (make-oo-eval-test desc exps)
  (make-test desc (lambda (fail)
                    (set! oo-error error)
                    (define-variable! 'fail
                      (make-primitive-procedure fail) the-global-environment)
                    (oo-eval '(define (verify-result msg actual expected)
                               (if (not (equal? actual expected))
                                   (fail msg 'expected: expected 'got: actual)
                                   'test-passed))
                            the-global-environment)
                    (eval-sequence exps the-global-environment))))

(define (verify-slots instance fail names values)
  (cond ((null? names) 'test-passed)
        ((not (equal? (read-slot instance (car names)) (car values)))
         (fail "slot" (car names) 'should 'be (car values) 'was: (read-slot instance (car names))))
        (else
         (verify-slots instance fail (cdr names) (cdr values)))))

(define instances-test-class `(instance ((:class ,default-metaclass) (:parent-class #f) (:name 'test-class) (:slots (foo bar baz)) (:methods ()))))

(define test-instances-0slots
  (make-test "Getting started: make-instance with no extra args."
             (lambda (fail)
               (if (not (class? instances-test-class))
                   'skipping-because-instances-are-opaque
                   (let ((obj (make-instance instances-test-class)))
                     (cond ((not (instance? obj))
                            (fail "make-instance did not produce an instance: " obj))
                           (else
                            (verify-slots obj fail '(foo) '(uninitialized))
                            'test-passed)))))))


(define test-instances-3slots
  (make-test "Getting started: make-instance with arguments."
             (lambda (fail)
               (if (not (class? instances-test-class))
                   'skipping-because-instances-are-opaque
                   (let ((obj (make-instance instances-test-class 1 2 3 4)))
                     (cond ((not (instance? obj))
                            (fail "make-instance did not produce an instance"))
                           (else
                            (verify-slots obj fail '(foo bar baz) '(uninitialized uninitialized uninitialized))
                            'test-passed)))))))

(define test-problem1-simple
  (make-test "Problem 1: create-class"
             (lambda (fail)
               (let* ((class (create-class 'test-class #f '(a b c) `((CONSTRUCTOR ,(lambda (self a) (write-slot! self 'a a)))))))
                 (if (not (class? class))
                     (fail "create-class did not produce a class. Instead, it produced: " class)
                     (begin
                       (verify-slots class fail '(:name :parent-class :slots) `(test-class #f (a b c)))
                       (let ((obj (make-instance class 1)))
                         (verify-slots obj fail '(a) '(1))
                         'test-passed)))))))

(define test-problem1-subclass
  (make-test "Problem 1: create-class with parent class"
             (lambda (fail)
               (let* ((parent (create-class 'test-class #f '(a b c) `((CONSTRUCTOR ,(lambda (self a) (write-slot! self 'a a))))))
                      (child  (create-class 'test-class-subclass parent '(d e f) '())))

                 (if (not (class? child))
                     (fail "create-class did not produce a class (an instance)")
                     (begin
                       (verify-slots child fail '(:name :parent-class :slots) `(test-class-subclass ,parent (d e f)))
                       (let ((obj (make-instance child 1)))
                         (verify-slots obj fail '(a b c d e f) '(1 uninitialized uninitialized uninitialized uninitialized uninitialized))
                         'test-passed)))))))

(define test-problem2-simple
  (make-test "Problem 2: make-class special form"
         (lambda (fail)
             (let* ((exp '(define class (make-class 'test-class #f (a b c) ((METHOD1 (lambda () 1)) (METHOD2 (lambda () 2))))))
                    (class (oo-eval `(begin ,exp class) the-global-environment)))
               (cond ((not (class? class))
                      (fail "oo-eval of make-class did not produce a class"))
                     ((not (box? class)) ; Violate abstraction if possible
                      (verify-slots class fail '(:name :parent-class :slots) '(test-class #f (a b c)))
                      (let* ((method-alist (read-slot class ':methods))
                             (names (map car method-alist))
                             (methods (map cadr method-alist)))
                        (cond ((not (= (length names) 2))
                               (fail "wrong number of methods in list. Should be 2, got:" (length names)))
                              ((not (equal? names '(METHOD1 METHOD2)))
                               (fail "wrong method names. Should be: (METHOD1 METHOD2), got:" names))
                              ((not (compound-procedure? (car methods)))
                               (fail "method1 is not a compound-procedure. Did you oo-eval the lambda expression? Got:" (car methods)))
                              ((not (compound-procedure? (cadr methods)))
                               (fail "method2 is not a compound-procedure. Did you oo-eval the lambda expression? Got:" (cadr methods)))
                              (else 'test-passed))))
                     (else 'test-passed))))))

(define test-problem2-subclass
  (make-test "Problem 2: make-class special form with subclass"
        (lambda (fail)
             (let* ((exp1 '(define parent (make-class 'test-class #f (a b c) ((METHOD1 (lambda () 1)) (METHOD2 (lambda () 2))))))
                    (exp2 '(define child (make-class 'test-class-subclass parent (d e f) ((METHOD2 (lambda () 3)) (METHOD3 (lambda () 4))))))
                    (parent (oo-eval `(begin ,exp1 parent) the-global-environment))
                    (child (oo-eval `(begin ,exp2 child) the-global-environment)))

               (cond ((not (class? child))
                      (fail "oo-eval of make-class did not produce a class"))
                     ((not (box? child)) ; Violate abstraction if possible
                      (verify-slots child fail '(:name :parent-class :slots) `(test-class-subclass ,parent (d e f)))
                      (let* ((method-alist (read-slot child ':methods))
                             (names (map car method-alist))
                             (methods (map cadr method-alist)))
                        (cond ((not (= (length names) 2))
                               (fail "wrong number of methods in list. Should be 2, got:" (length names)))
                              ((not (equal? names '(METHOD2 METHOD3)))
                               (fail "wrong method names. Should be: (METHOD2 METHOD3), got:" names))
                              ((not (compound-procedure? (car methods)))
                               (fail "method2 is not a compound-procedure. Did you oo-eval the lambda expression? Got:" (car methods)))
                              ((not (compound-procedure? (cadr methods)))
                               (fail "method3 is not a compound-procedure. Did you oo-eval the lambda expression? Got:" (cadr methods)))
                              (else 'test-passed))))
                     (else 'test-passed))))))

(define test-problem3
  (make-test "Problem 3: new with subclass"
        (lambda (fail)
             (let* ((exp1 '(define parent (make-class 'test-class #f (a b c) ((METHOD1 (lambda () 1)) (METHOD2 (lambda () 2))))))
                    (exp2 '(define *test-val* 'nothing-yet))
                    (exp3 '(define child (make-class 'test-class-subclass parent (d e f) ((CONSTRUCTOR (lambda (x) (set! *test-val* x))) (METHOD2 (lambda () 3)) (METHOD3 (lambda () 4))))))
                    (exp4 '(define obj (new child (* 2 21))))
                    (parent (oo-eval `(begin ,exp1 parent) the-global-environment))
                    (child  (oo-eval `(begin ,exp2 ,exp3 child)  the-global-environment))
                    (obj    (oo-eval `(begin ,exp4 obj)    the-global-environment))
                    (testval (oo-eval '*test-val* the-global-environment)))

               (cond ((not (instance? obj))
                      (fail "oo-eval of new did not produce an instance"))
                     ((not (= testval 42))
                      (fail "Expected CONSTRUCTOR to set test var to 42. Instead, value is: " testval))
                     ((not (box? obj)) ;; Violate abstraction if possible
                      (verify-slots obj fail '(b f c) '(uninitialized uninitialized uninitialized))
                      'test-passed)
                     (else
                      'test-passed))))))

(define test-problem3-no-args
  (make-test "Problem 3: new with subclass, no constructor args"
        (lambda (fail)
             (let* ((exp1 '(define parent (make-class 'test-class #f (a b c) ((METHOD1 (lambda () 1)) (METHOD2 (lambda () 2))))))
                    (exp2 '(define child (make-class 'test-class-subclass parent (d e f) ((METHOD2 (lambda () 3)) (METHOD3 (lambda () 4))))))
                    (exp3 '(define obj (new child)))
                    (parent (oo-eval `(begin ,exp1 parent) the-global-environment))
                    (child  (oo-eval `(begin ,exp2 child)  the-global-environment))
                    (obj    (oo-eval `(begin ,exp3 obj)    the-global-environment)))

               (cond ((not (instance? obj))
                      (fail "oo-eval of new did not produce an instance"))
                     ((not (box? obj)) ; Violate abstraction if possible
                      (verify-slots obj fail '(b f c) '(uninitialized uninitialized uninitialized))
                      'test-passed)
                     (else
                      'test-passed))))))

(define test-problem4-trivial
  (make-oo-eval-test "Problem 4: method invocation (trivial)"
                   '((define base (make-class 'BASE #f () ((METHOD1 (lambda () (+ 1 2))) (METHOD2 (lambda (x y) (* x y))))))
                     (define obj (new base))
                     (verify-result "Method invocation failed" (obj 'METHOD1) 3)
                     (verify-result "Method invocation failed" (obj 'METHOD2 2 21) 42))))

(define test-problem4-inheritance
  (make-oo-eval-test "Problem 4: method invocation (inheritance)"
                   '((define base (make-class 'BASE #f () ((METHOD1 (lambda () (+ 1 2))) (METHOD2 (lambda (x y) (* x y))))))
                     (define sub  (make-class 'SUB  base () ((METHOD3 (lambda () (/ 2 1))) (METHOD4 (lambda () (+ 2 21))))))
                     (define obj (new sub))
                     (verify-result "Method invocation failed" (obj 'METHOD4) 23)
                     (verify-result "Method invocation failed" (obj 'METHOD2 2 21) 42))))

(define test-problem4-slot-read
  (make-oo-eval-test "Problem 4: method invocation (slot reading)"
                   '((define base (make-class 'BASE #f (a b c) ((METHOD1 (lambda (a) a)))))
                     (define sub  (make-class 'SUB  base (d e f) ((METHOD3 (lambda () e)))))
                     (define obj (new sub))
                     (verify-result "Method invocation failed" (obj 'METHOD3) 'uninitialized)
                     (verify-result "Method parameters should shadow slots of the same name." (obj 'METHOD1 42) 42))))

(define test-problem4-slot-write
  (make-oo-eval-test "Problem 4: method invocation (slot writing)"
                   '((define base (make-class 'BASE #f (a b c) ((METHOD1 (lambda (a) (set! a 'lose) a)) (METHOD2 (lambda () a)))))
                     (define sub  (make-class 'SUB  base (d e f) ((METHOD3 (lambda () e)) (METHOD4 (lambda (x) (set! e (+ x 5)) e)))))
                     (define obj (new sub))
                     (verify-result "Method invocation failed" (obj 'METHOD4 21) 26)
                     (verify-result "Method invocation failed" (obj 'METHOD3) 26)
                     (verify-result "Method invocation failed" (obj 'METHOD1 5) 'lose)
                     (verify-result "Method invocation failed" (obj 'METHOD2) 'uninitialized)

                     (define base (make-class 'BASE #f (a b c) ((METHOD1 (lambda (a) a)) (METHOD2 (lambda (x) (* a x))))))
                     (define sub  (make-class 'SUB  base (d e f) ((CONSTRUCTOR (lambda (v1 v2 v3) (set! a v1) (set! e v2) (set! f v3))) (METHOD3 (lambda () e)) (METHOD4 (lambda (x) (+ x f))))))
                     (define obj (new sub 2 3 4))
                     (verify-result "Method invocation failed" (obj 'METHOD3) 3)
                     (verify-result "Method invocation failed" (obj 'METHOD4 21) 25)
                     (verify-result "Method invocation failed" (obj 'METHOD2 5) 10)
                     (verify-result "Method invocation failed" (obj 'METHOD1 42) 42))))

(define test-problem4-self-and-super
  (make-oo-eval-test "Problem 4: method invocation (self and super)"
                   '((define base (make-class 'BASE #f () ((METHOD1 (lambda () 5)) (METHOD2 (lambda () 6)))))
                     (define sub  (make-class 'SUB  base () ((METHOD3 (lambda () (+ (self 'METHOD2) (self 'METHOD1)))) (METHOD2 (lambda () (* 2 (super 'METHOD2)))))))
                     (define obj (new sub))
                     (verify-result "Method invocation failed." (obj 'METHOD2) 12)
                     (verify-result "Method invocation failed." (obj 'METHOD3) 17))))


(define test-problem-5
  (make-oo-eval-test "Problem 5: GET-TYPES"
                   '((define root-object (make-class 'ROOT-OBJECT #f () ((GET-CLASS (lambda () :class)))))
                     (define named-object (make-class 'NAMED-OBJECT root-object (name) ((CONSTRUCTOR (lambda (_name) (set! name _name))) (NAME (lambda () name)))))
                     (define sicp (new named-object 'SICP!))
                     (define (is-a type)
                       (lambda (obj)
                         (if (memq type ((obj 'GET-CLASS) 'GET-TYPES)) #t #f)))
                     (verify-result "sicp is namedobject" ((is-a 'NAMED-OBJECT) sicp) #t)
                     (verify-result "sicp is rootobject"  ((is-a 'ROOT-OBJECT) sicp) #t)
                     (verify-result "sicp is not yourmom" ((is-a 'YOUR-MOM) sicp) #f))))

(define test-problem6-basic
  (make-test "Problem 6: abstraction opacity: regression check"
        (lambda (fail)
          (let* ((base (create-class 'BASE #f '() '()))
                (obj (make-instance base)))
            (cond ((not (class? base)) (fail "class fails class? check"))
                  ((not (instance? obj)) (fail "instance fails instance? check"))
                  (else 'test-passed))))))

(define test-problem6-oo-eval
    (make-oo-eval-test "Problem 6: abstraction opacity in oo-eval [optional] LAST TEST"
                   '((define base (make-class 'BASE #f () ((METHOD1 (lambda () 5)) (METHOD2 (lambda () 6)))))
                     (define sub  (make-class 'SUB  base () ((METHOD3 (lambda () (+ (self 'METHOD2) (self 'METHOD1)))) (METHOD2 (lambda () (* 2 (super 'METHOD2)))))))
                     (define obj (new sub))
                     (cond ((pair? base) (fail "Representation of class leaked"))
                           ((pair? sub)  (fail "Representation of sub class leaked"))
                           ((pair? (sub 'GET-CLASS)) (fail "Representation of metaclass leaked"))
                           ((pair? ((sub 'GET-CLASS) 'GET-CLASS)) (fail "Representation of metaclass leaked"))
                           ((pair? obj) (fail "Representation of object leaked")))
                     (verify-result "Method invocation failed." (obj 'METHOD2) 12)
                     (verify-result "Method invocation failed." (obj 'METHOD3) 17))))

