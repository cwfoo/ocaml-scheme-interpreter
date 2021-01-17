;;;; Unit tests for primitives implemented in OCaml.
(load "lib-assert.scm")

; Missing tests: lots of primitives implemented in OCaml.

;; Ensure that the predicate in 'if' is able to handle non-booleans.
(assert-equal (if 123 'a 'b) 'a)
(assert-equal (if 'xyz 'a 'b) 'a)

(assert-true (list? '(a b c)))
(assert-false (list? (cons 'a 'b)))

(assert-true (pair? (cons 'a 'b)))
(assert-true (pair? '(a b c)))
(assert-false (pair? '()))

(define l1 '(a b c))
(define l2 '(a b c))
(assert-true (eqv? l1 l1))
(assert-false (eqv? l1 l2))
(assert-true (eqv? '() '()))
(assert-true (eqv? (car l1) (car l1)))
(assert-true (eqv? (cdr l1) (cdr l1)))
(assert-false (eqv? (cdr l1) (cdr l2)))
(assert-true (equal? l1 l1))
(assert-true (equal? l1 l2))
(assert-true (equal? '() '()))
(assert-true (eq? l1 l1))
(assert-false (eq? l1 l2))
(assert-true (eq? '() '()))

(assert-true (vector? #()))
(assert-true (vector? #(1 2 3)))
(assert-false (vector? 1))
(assert-false (vector? '()))

(assert-equal (vector-length #()) 0)
(assert-equal (vector-length #(1 2 3)) 3)

(assert-equal (vector-ref #(1 2 3) 0) 1)
(assert-equal (vector-ref #(1 2 3) 2) 3)

(define v #(1 2 3))
(vector-set! v 0 3)
(vector-set! v 2 1)
(assert-equal (vector-ref v 0) 3)
(assert-equal (vector-ref v 2) 1)

(assert-equal (vector-length (make-vector 0)) 0)
(assert-equal (vector-length (make-vector 6)) 6)
