;; THIS FILE IS TO BE RUN UNDER OO-EVAL, AND IS LOADED WHEN YOU RUN
;; oo-eval.scm DO NOT TRY TO RUN DIRECTLY IN RACKET

;;;;;;;;;;
;; The following are basic scheme list manipulation procedures,
;; implemented in terms of primitives that oo-eval understands, for
;; bootstrapping.
;;

(define (list . args)
  args)

(define (length lst)
  (if (null? lst)
      0
      (+ 1 (length (cdr lst)))))

(define (list-ref lst n)
  (if (= n 0)
      (car lst)
      (list-ref (cdr lst) (- n 1))))

(define (delq elem lst)
  (cond ((null? lst) lst)
        ((eq? elem (car lst)) (cdr lst))
        (else (cons (car lst) (delq elem (cdr lst))))))

(define (append lst1 lst2)
  (if (null? lst1)
      lst2
      (cons (car lst1)
            (append (cdr lst1) lst2))))

(define (pick-random lst)
  (if (null? lst)
      #f
      (list-ref lst (random (length lst)))))

(define (filter op lst)
  (cond ((null? lst) lst)
        ((op (car lst))
         (cons (car lst) (filter op (cdr lst))))
        (else            (filter op (cdr lst)))))

(define (map op lst)
  (if (null? lst)
      lst
      (cons (op (car lst))
            (map op (cdr lst)))))

(define (append-map op lst)
  (if (null? lst)
      lst
      (append (op (car lst))
              (append-map op (cdr lst)))))

(define (for-each op lst)
  (if (null? lst)
      'done
      (begin
        (op (car lst))
        (for-each op (cdr lst)))))

(define (reverse lst)
  (define (helper lst result)
    (if (null? lst)
        result
        (helper (cdr lst) (cons (car lst) result))))
  (helper lst '()))

(define (fold-right op init lst)
  (if (null? lst)
      init
      (op (car lst)
          (fold-right op init (cdr lst)))))

