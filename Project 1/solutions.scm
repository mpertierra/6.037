#lang racket


"Part 1: Numerical integration"

"Problem 1: Integrating any function"

(define (integral func num-steps x1 x2)
    (define (integral-rect func x1 x2)
        (* (- x2 x1) (func x1)))
    (if (= num-steps 0)
        0
        (+ (integral-rect func x1 (+ x1 (/ (- x2 x1) num-steps)))
            (integral func (- num-steps 1) (+ x1 (/ (- x2 x1) num-steps)) x2))))

;; Test cases:
;; With only one step, the integral of y = x^2 from 3 to 5
;; should be 3^2 * 2 = 18
;; (integral (lambda (x) (expt x 2)) 1 3 5)
;; (integral (lambda (x) (expt x 2)) 2 3 5)  ;; With two steps, we should get 3^2 + 4^2 = 25
;; (integral (lambda (x) (expt x 2)) 0 3 5)  ;; num-steps = 0, so should be 0
;; (integral (lambda (x) (expt x 2)) 10 3 3) ;; x1 = x2, so should be 0


"Problem 2: Area of a unit circle"

(define (approx-pi num-steps)
    (*
        4
        (integral
            (lambda (x) (sqrt (- 1 (* x x))))
            num-steps
            0
            1)))

;; Test cases:
;; (approx-pi 0)      ;; Should be 0
;; (approx-pi 1)      ;; Should be 4
;; (approx-pi 2)      ;; Hopefully lower than 4
;; (approx-pi 600)    ;; Right to the first two decimal places?
;; (approx-pi 100000) ;; Right to the first three decimal places?


"Problem 3: Integrating with pieces of any shape"

(define (rectangle func x1 x2)
    (* (- x2 x1) (func x1)))

(define (trapezoid func x1 x2)
    (*
        (- x2 x1)
        (/ (+ (func x1) (func x2)) 2)))

(define (integral-with piece func num-steps x1 x2)
    (if (= num-steps 0)
        0
        (+ (piece func x1 (+ x1 (/ (- x2 x1) num-steps)))
            (integral-with piece func (- num-steps 1) (+ x1 (/ (- x2 x1) num-steps)) x2))))

;; Test cases:
;; (integral-with rectangle ...) should be the same as calling (integral ...)
;; (integral-with rectangle (lambda (x) (expt x 2)) 1 3 5) ;; Should be 18
;; (integral-with rectangle (lambda (x) (expt x 2)) 2 3 5) ;; Should be 25
;; x1 = x2 should give 0
;; (integral-with rectangle (lambda (x) (expt x 2)) 10 3 3)
;; (integral-with trapezoid (lambda (x) (expt x 2)) 10 3 3)
;; num-steps = 0 should give 0
;; (integral-with rectangle (lambda (x) (expt x 2)) 0 3 5)
;; (integral-with trapezoid (lambda (x) (expt x 2)) 0 3 5)
;; Integral of x^2 from 3 to 5 is approximately 32.667
;; Using trapezoid as our piece procedure should give closer answers for smaller num-steps
;; (integral-with trapezoid (lambda (x) (expt x 2)) 1 3 5) ;; Should be larger than 18, smaller than 47.334
;; (integral-with trapezoid (lambda (x) (expt x 2)) 2 3 5) ;; Should be larger than 25, smaller than 40.334


"Problem 4: Better approximation of pi"

(define (better-pi num-steps)
    (*
        4
        (integral-with
            trapezoid
            (lambda (x) (sqrt (- 1 (* x x))))
            num-steps
            0
            1)))

;; Test cases:
;; (better-pi 0)      ;; Should be 0
;; (better-pi 1)      ;; Should be 2
;; (better-pi 2)      ;; Should be larger than 2
;; (better-pi 600)    ;; Right to the first four decimal places
;; (better-pi 100000) ;; Right to the first seven decimal places


"Part 2: Symbolic differentiation"

(define (deriv-constant wrt constant)
    0)


"Problem 5: Derivative of a variable"

(define (deriv-variable wrt var)
    (if (eq? wrt var) 1 0))

;; Test cases:
;; (deriv-variable 'x 'x) ;; Should be 1
;; (deriv-variable 'x 'X) ;; Should be 0
;; (deriv-variable 'x 'y) ;; Should be 0

"Problem 6: Calling the right function"

(define (derivative wrt expr)
    (cond
        ((number? expr) (deriv-constant wrt expr))
        ((symbol? expr) (deriv-variable wrt expr))
        ((and (list? expr) (= 3 (length expr)) (eq? `+ (first expr))) (deriv-sum wrt expr))
        ((and (list? expr) (= 3 (length expr)) (eq? `* (first expr))) (deriv-product wrt expr))
        (else (error "Don't know how to differentiate" expr))))

;; Test cases:
;; (derivative 'x 3)  ;; Should be 0
;; (derivative 'x '3) ;; Should be 0
;; (derivative 'x 'y) ;; Should be 0
;; (derivative 'x 'x) ;; Should be 1


"Problem 7: Derivative of a sum"

(define (deriv-sum wrt expr)
    (list `+ (derivative wrt (second expr)) (derivative wrt (third expr))))

;; Problem 6 code is modified to call dervi-sum when appropriate.
;; It checks if expr is a list, has length 3, and has `+ as the first element.

;; Test cases:
;; (derivative 'x '(+ 1 2)) ;; Should be (+ 0 0)
;; (derivative 'x '(+ x 2)) ;; Should be (+ 1 0)
;; (derivative 'x '(+ x x)) ;; Should be (+ 1 1)


"Problem 8: Derivative of a product"

(define (deriv-product wrt expr)
    (list `+
        (list `* (second expr) (derivative wrt (third expr)))
        (list `* (derivative wrt (second expr)) (third expr))))

;; Problem 6 code is modified to call dervi-product when appropriate.
;; It checks if expr is a list, has length 3, and has `* as the first element.

;; Test cases:
;; (derivative 'x '(* x 3)) ;; Should be (+ (* x 0) (* 1 3))
;; (derivative 'x '(* 2 3)) ;; Should be (+ (* 2 0) (* 0 3))
;; (derivative 'x '(* x x)) ;; Should be (+ (* x 1) (* 1 x))


"Problem 9: Additional testing"

; Additional test cases for 'derivative':
;; (derivative 'x '(+ 3 (* x x))) ;; Should be (+ 0 (+ (* x 1) (* 1 x))), evaluates to 2x
;; (derivative 'x '(* 3 (* x x))) ;; Should be (+ (* 3 (+ (* x 1) (* 1 x))) (* 0 (* x x))), evaluates to 6x
;; (derivative 'x '(+ x (* x x))) ;; Should be (+ 1 (+ (* x 1) (* 1 x))), evaluates to 1 + 2x

