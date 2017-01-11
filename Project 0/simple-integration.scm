#lang racket


"Part 1: Numerical integration"

"Problem 1: Bitdiddle's function"

(define (bitfunc x)
    'your-code-here)

;; Some simple test cases, based on by-eye examination of a graph of the
;; function: https://www.google.com/search?q=x^4-5*x^2%2B4   Run these,
;; and check that they match with the expectations.
(bitfunc 0)  ;; Should be 4
(bitfunc 1)  ;; Should be 0, or very close
(bitfunc 2)  ;; Should also be very close to 0
(bitfunc -1) ;; Should also also be very close to 0
(bitfunc 10) ;; Should be pretty big, and positive


"Problem 2: A rectangle under Bitdiddle's function"

(define (bitfunc-rect x1 x2)
    'your-code-here)

;; Test cases:
(bitfunc-rect 0 1)   ;; Should be 4
(bitfunc-rect 0 0.5) ;; Should be 2
(bitfunc-rect 1.5 2) ;; Should be negative

"Problem 3: Integrating Bitdiddle's function"

(define (bitfunc-integral-recur num-steps x1 x2)
    'your-code-here)

(define (bitfunc-integral-iter num-steps x1 x2)
    'your-code-here)

;; Provide your own test cases for this function.  Think about what are
;; the simplest input values to know are correct, and show that those
;; work as expected before moving on to test a couple more complicated
;; situations.


"Problem 4: Comparing the two integrators"

(define (bitfunc-integral-difference num-steps x1 x2)
    'your-code-here)

;; Provide test cases for this one as well; only a couple should be
;; needed, as this function should be fairly straightforward.
