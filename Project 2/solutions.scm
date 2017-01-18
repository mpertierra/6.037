#lang racket

;; By default, Racket doesn't have set-car! and set-cdr! functions.  The
;; following line allows us to use those:
(require r5rs)
;; Unfortunately, it does this by making cons return a "mutable pair"
;; instead of a "pair", which means that many built-ins may fail
;; mysteriously because they expect a pair, but get a mutable pair.
;; Re-define a few common functions in terms of car and friends, which
;; the above line make work with mutable pairs.
(define first car)
(define rest cdr)
(define second cadr)
(define third caddr)
(define fourth cadddr)
;; We also tell DrRacket to print mutable pairs using the compact syntax
;; for ordinary pairs.
(print-as-expression #f)
(print-mpair-curly-braces #f)


"Problem 1"

;; Note: We use the "assoc" procedure internally to check if a key is in our table and to get its value.
;; This procedure uses the "equal?" operator to test for key-equality. This works for our implementation
;; because we need to be able to handle both numbers as keys as well as symbols as keys. We can not use 
;; the "=" operator to test for equality because this will raise an error for symbols as keys. We can not use
;; the "eq?" operator to test for equality because this will not correctly handle many numbers (e.g. 2^100).
;; Therefore, the "equal?" operator will work for the majority of our common cases.
(define (make-table)
  (list 'table)
)
(define (table? table)
  (and
    (list? table)
    (not (empty? table))
    (eq? 'table (first table))
  )
)
(define (table-put! table key value)
  (cond
    ((not (table? table)) (error "'table' argument is not a table"))
    ((table-has-key? table key) (set-car! (rest (assoc key (rest table))) value))
    (else (set-cdr! table (cons (list key value) (rest table))))
  )
)
(define (table-has-key? table key)
  (cond 
    ((not (table? table)) (error "'table' argument is not a table"))
    ((not (assoc key (rest table))) #f)
    (else #t)
  )
)
(define (table-get table key)
  (cond 
    ((not (table? table)) (error "'table' argument is not a table"))
    ((not (assoc key (rest table))) (error "table does not contain key"))
    (else (second (assoc key (rest table))))
  )
)

;; Additional test cases:
; (define t (make-table))
; (table? t)                ;; => #t
; (table? `(table))         ;; => #t
; (equal? t `(table))       ;; => #t
; (table-has-key? t 4)      ;; => #f
; (table-has-key? t `foo)   ;; => #f
; (table-put! t 4 `four)
; (table-has-key? t 4)      ;; => #t
; (table-get t 4)           ;; => `four
; (table-has-key? t `foo)   ;; => #f
; (table-put! t `foo `bar)
; (table-has-key? t `foo)   ;; => #t
; (table-get t `foo)        ;; => `bar
; (table-put! t `foo `baz)
; (table-has-key? t `foo)   ;; => #t
; (table-get t `foo)        ;; => `baz
; (table-has-key? `(table (3 foo) (bar baz)) 3)    ;; => #t
; (table-has-key? `(table (3 foo) (bar baz)) `bar) ;; => #t
; (table-has-key? `(table (3 foo) (bar baz)) 5)    ;; => #f
; (table-get `(table (3 foo) (bar baz)) 3)         ;; => `foo
; (table-get `(table (3 foo) (bar baz)) `bar)      ;; => `baz


"Problem 2"

(define (fib n)
  (if (< n 2)
      n
      (+ (fib (- n 1)) (fib (- n 2)))))

(define (make-monitored func)
  (let ((call-count 0))
    (lambda (input)
      (cond 
        ((eq? input 'how-many-calls?) call-count)
        ((eq? input 'reset-call-count) (set! call-count 0))
        (else
          (set! call-count (+ 1 call-count))
          (func input)
        )
      )
    )
  )
)

;; Test cases:
; (fib 8) ;; => 21
; (set! fib (make-monitored fib))
; (fib 8) ;; => 21
; (fib 'how-many-calls?) ;; => 67
; (fib 8) ;; => 21
; (fib 'how-many-calls?) ;; => 134
; (fib 'reset-call-count)
; (fib 'how-many-calls?) ;; => 0
;; The code below does not behave as intended because the recursive step uses the definition of fib
;; instead of the monitored version, so it produces the right answer for the 8th Fibonacci number but
;; does not increment the call-count as it recurses.
; (define mon-fib (make-monitored fib))
; (mon-fib 8) ;; => 21
; (mon-fib 'how-many-calls?) ;; => 1
;; Additional test cases: Check that multiple monitored functions have independent call counts
; (define (fib2 n)
;   (if (< n 2)
;       n
;       (+ (fib2 (- n 1)) (fib2 (- n 2)))))
; (set! fib2 (make-monitored fib2))
; (fib 8)                 ;; => 21
; (fib 8)                 ;; => 21
; (fib 'how-many-calls?)  ;; => 134
; (fib2 'how-many-calls?) ;; => 0
; (fib2 8)                ;; => 21
; (fib2 'how-many-calls?) ;; => 67
; (fib 'how-many-calls?)  ;; => 134


"Problem 3"

(define (make-num-calls-table func max)
  (let ((num-calls-table (make-table)))
    (define (helper input)
      (cond
        ((not (integer? input)) num-calls-table)
        ((<= input 0)           num-calls-table)
        (else
          (func 'reset-call-count)
          (func input)
          (table-put! num-calls-table input (func 'how-many-calls?))
          (helper (- input 1))
        )
      )
    )
    (helper max)
  )
)

;; Test cases:
; (make-num-calls-table fib -5)  ;; => (table)
; (make-num-calls-table fib 0)   ;; => (table)
; (make-num-calls-table fib 1.5) ;; => (table)
; (make-num-calls-table fib 1.0) ;; => (table (1.0 1))
; (make-num-calls-table fib 1)   ;; => (table (1 1))
; (make-num-calls-table fib 5)   ;; => (table (1 1) (2 3) (3 5) (4 9) (5 15))
; (make-num-calls-table fib 10)  ;; => (table (1 1) (2 3) (3 5) (4 9) (5 15) (6 25) (7 41) (8 67) (9 109) (10 177))
; (make-num-calls-table fib 1)   ;; => (table (1 1))
;; We see that there are 21891 calls to the "fib" procedure when we evaluate (fib 20),
;; and there are 2692537 calls to the "fib" procedure when we evaluate (fib 30).
; (make-num-calls-table fib 20)  ;; => (table (1 1) (2 3) ...[shortened by me]... (19 13529) (20 21891))
; (make-num-calls-table fib 30)  ;; => (table (1 1) (2 3) ...[shortened by me]... (29 1664079) (30 2692537))


"Problem 4"

;; memoize

"Problem 5 (optional)"

;; advise

"Problem 6 (optional)"

;; make-monitored-with-advice


;; Allow this file to be included from elsewhere, and export all defined
;; functions from it.
(provide (all-defined-out))
