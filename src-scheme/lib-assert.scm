;;;; Library useful for unit tests.

(define-macro assert-true
  (lambda (exp)
    (list 'cond (list (cadr exp)
                      (list 'display "OK: ")
                      (list 'displayln (list 'quote exp)))
                (list 'else
                      (list 'display "FAILED: ")
                      (list 'displayln (list 'quote exp))
                      (list 'error "ASSERT-TRUE failed.")))))

(define-macro assert-false
  (lambda (exp)
    (list 'cond (list (list 'not (cadr exp))
                      (list 'display "OK: ")
                      (list 'displayln (list 'quote exp)))
                (list 'else
                      (list 'display "FAILED: ")
                      (list 'displayln (list 'quote exp))
                      (list 'error "ASSERT-FALSE failed.")))))

(define-macro assert-equal
  (lambda (exp)
    (list 'cond (list (list 'equal? (cadr exp) (caddr exp))
                      (list 'display "OK: ")
                      (list 'displayln (list 'quote exp)))
                (list 'else
                      (list 'display "FAILED: ")
                      (list 'displayln (list 'quote exp))
                      (list 'error "ASSERT-EQUAL failed.")))))
