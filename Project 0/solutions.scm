#lang racket


"Part 1: Numerical integration"

"Problem 1: Bitdiddle's function"

;; A polynomial of degree n can be expressed as: a_n * x^n + a_{n-1} * x^(n-1) + ... + a_1 * x + a_0
;; Here, we have that n = 4 and a_4 = 1, a_3 = 0, a_2 = -5, a_1 = 0, a_0 = 4
(define (bitfunc x)
    (+ (expt x 4) (* -5 (expt x 2)) 4))


;; Test cases:
;(bitfunc 0)  ;; Should be 4
;(bitfunc 1)  ;; Should be 0, or very close
;(bitfunc 2)  ;; Should also be very close to 0
;(bitfunc -1) ;; Should also also be very close to 0
;(bitfunc 10) ;; Should be pretty big, and positive
;(bitfunc -2) ;; Should be the same as (bitfunc 2), which is 0
;(bitfunc -10);; Should be the same as (bitfunc 10), which is 9504


"Problem 2: A rectangle under Bitdiddle's function"

;; Assuming that x1 <= x2, we have that the width of the rectangle is
;; x2 - x1 and the height is f(x1) so the area of the rectangle is (x2 - x1) * f(x1)
(define (bitfunc-rect x1 x2)
    (* (- x2 x1) (bitfunc x1)))

;; Test cases:
;(bitfunc-rect 0 1)   ;; Should be 4
;(bitfunc-rect 0 0.5) ;; Should be 2
;(bitfunc-rect 1.5 2) ;; Should be negative
;(bitfunc-rect 10 10) ;; Should be 0
;(bitfunc-rect 1 2)   ;; Should be 0


"Problem 3: Integrating Bitdiddle's function"

;; With N steps (or rectangles) between x1 and x2, each rectangle will have width (x2 - x1)/N.
;; The total area of a number of rectangles with N steps between x1 and x2 is equal to
;; the area of the first rectangle (which is bitfunc-rect of x1 and x1 + (x2-x1)/N), plus
;; the total area of the number of rectangles with N-1 steps between x1 + (x2-x1)/N and x2.
;; The base case is when the number of steps is 0. In this case, the area is 0.
(define (bitfunc-integral-recur num-steps x1 x2)
  (if (= num-steps 0)
      0
      (+ (bitfunc-rect x1 (+ x1 (/ (- x2 x1) num-steps)))
         (bitfunc-integral-recur
          (- num-steps 1)
          (+ x1 (/ (- x2 x1) num-steps))
          x2))))

;; For the iterative algorithm, we use a helper procedure that keeps an additional argument to keep track
;; of the area, as we iterate from N steps to 0 steps. Initially, the area argument is 0.
;; When we reach 0 steps, we return the area argument, which holds the answer.
(define (bitfunc-integral-iter num-steps x1 x2)
    (bitfunc-integral-iter-helper num-steps x1 x2 0))

(define (bitfunc-integral-iter-helper num-steps x1 x2 area)
  (if (= num-steps 0)
      area
      (bitfunc-integral-iter-helper
       (- num-steps 1)
       (+ x1 (/ (- x2 x1) num-steps))
       x2
       (+ area (bitfunc-rect x1 (+ x1 (/ (- x2 x1) num-steps)))))))

;; Test cases:
;; Note that if num-steps 0, the area should always be 0
;(bitfunc-integral-recur 0 10 10) ;; Should be 0
;(bitfunc-integral-recur 0 5 10)  ;; Should be 0

;(bitfunc-integral-iter 0 10 10)  ;; Should be 0
;(bitfunc-integral-iter 0 5 10)   ;; Should be 0

;; Note that (bitfunc-integral-recur 1 x1 x2) and (bitfunc-integral-iter 1 x1 x2) should be the same as (bitfunc-rect x1 x2)
;(bitfunc-integral-recur 1 0 1)   ;; Should be 4
;(bitfunc-integral-recur 1 0 0.5) ;; Should be 2
;(bitfunc-integral-recur 1 1.5 2) ;; Should be negative
;(bitfunc-integral-recur 1 10 10) ;; Should be 0
;(bitfunc-integral-recur 1 1 2)   ;; Should be 0

;(bitfunc-integral-iter 1 0 1)   ;; Should be 4
;(bitfunc-integral-iter 1 0 0.5) ;; Should be 2
;(bitfunc-integral-iter 1 1.5 2) ;; Should be negative
;(bitfunc-integral-iter 1 10 10) ;; Should be 0
;(bitfunc-integral-iter 1 1 2)   ;; Should be 0

;; Keeping x1 and x2 fixed, incrementing num-steps should result in areas that converge
;(bitfunc-integral-recur 5 3 4)   ;; Should be close to 98.533
;(bitfunc-integral-recur 10 3 4)  ;; Should be closer to 98.533
;(bitfunc-integral-recur 50 3 4)  ;; Should be closer-er to 98.533
;(bitfunc-integral-recur 100 3 4) ;; Should be closer-er-er to 98.533
;
;(bitfunc-integral-iter 5 3 4)   ;; Should be close to 98.533
;(bitfunc-integral-iter 10 3 4)  ;; Should be closer to 98.533
;(bitfunc-integral-iter 50 3 4)  ;; Should be closer-er to 98.533
;(bitfunc-integral-iter 100 3 4) ;; Should be closer-er-er to 98.533


"Problem 4: Comparing the two integrators"

(define (bitfunc-integral-difference num-steps x1 x2)
    (abs (-
          (bitfunc-integral-recur num-steps x1 x2)
          (bitfunc-integral-iter num-steps x1 x2))))

;; Test cases:
;(bitfunc-integral-difference 0 10 15)  ;; Should be 0
;(bitfunc-integral-difference 10 10 10) ;; Should be 0
;(bitfunc-integral-difference 1 5 10)   ;; Should be 0
;(bitfunc-integral-difference 10 5 10)  ;; Should be 0
