#lang racket

(require r5rs)
(require rackunit)
(require "solutions.scm")


(test-case
 "table?"
 (check-pred procedure? make-table
             "make-table is a function")
 (check-pred procedure? table?
             "table? is a function")
 (check-true (table? (make-table))
             "New tables are tables")
 (check-false (table? 'moose)
              "A symbol is not a table")
 (check-false (table? '())
              "The empty list is not a table")
 (check-false (table? '(1 2 3))
              "A list of numbers is not a table")
 )

(test-case
 "Table equality"
 (check-not-eq? (make-table) (make-table)
                "Two new tables are different objects (are not eq?)")
 (check-equal? (make-table) (make-table)
               "Two new tables are identical (are equal?)")
 )

(test-case
 "Putting"
 (define t (make-table))
 (check-pred procedure? table-put!)
 (check-pred procedure? table-has-key?)
 (check-false (table-has-key? t 'foo)
              "Table doesn't start with keys")
 (table-put! t 'foo 17)
 (check-true (table-has-key? t 'foo)
             "Table has a key after it has been put into the table")
 (check-not-equal? (make-table) t
                   "Table has been modified by putting")
 (check-equal? (table-get t 'foo) 17
               "The value with key 'foo is the same as the one we put")
 )

(test-case
 "Table with multiple values"
 (define t (make-table))
 (table-put! t 'foo 'bar)
 (table-put! t 'bar 'baz)
 (table-put! t 'yellow 'red)
 (table-put! t 3 7)
 (table-put! t 7 'green)
 (check-equal? 'baz (table-get t 'bar)
               "Key bar has correct value")
 (check-equal? 7 (table-get t 3)
               "Key 3 has correct value")
 (check-equal? 'green (table-get t 7)
               "Key 7 has correct value")
 (check-exn exn:fail? (lambda () (table-get t 'qux))
            "Table does not have key qux")
 )


(test-case
 "Multiple tables at once"
 (define t (make-table))
 (define other-t (make-table))
 (table-put! t 1 2)
 (check-false (table-has-key? other-t 1)
              "Adding a key/value to one table does not affect other tables")
 (table-put! other-t 2 3)
 (check-false (table-has-key? t 2)
              "Adding a key/value to the second table does not affect the first")
 )

(test-case
 "Exceptions that should be raised"
 (check-exn exn:fail? (lambda () (table-put! '(1 2 3 4) 17 5))
            "Can't call table-put! on a non-table (list)")
 (check-exn exn:fail? (lambda () (table-put! 17 17 5))
            "Can't call table-put! on a non-table (number)")
 (check-exn exn:fail? (lambda () (table-has-key? '(1 2 3 4) 1))
            "Can't call table-has-key on a non-table (list)")
 (check-exn exn:fail? (lambda () (table-has-key? 5 1))
            "Can't call table-has-key on a non-table (number)")
 (check-exn exn:fail? (lambda () (table-get '(1 2 3 4) 5))
            "Can't call table-get on a non-table (list)")
 (check-exn exn:fail? (lambda () (table-get 17 5))
            "Can't call table-get on a non-table (number)")
 )


(display "Done running tests.")(newline)
