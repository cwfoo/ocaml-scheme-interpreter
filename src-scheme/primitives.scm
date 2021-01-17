;;;; Additional primitives implemented in Scheme.
;;;; Useful reference on what to implement as primitives:
;;;; * https://docs.racket-lang.org/r5rs/r5rs-std/index.html
;;;; * https://small.r7rs.org

;;; Begin.
;; (begin e ...) -> ((lambda () e ...))
(define-macro begin
  (lambda (exp)
    (cons (cons 'lambda
                (cons '()
                      (cdr exp)))
          '())))

;;; Delay and force.
;; (delay exp) -> (lambda () exp)
(define-macro delay
  (lambda (exp)
    (cons 'lambda
          (cons '()
                (cdr exp)))))

(define (force exp)
  (exp))

;;; Boolean.
;; (and x y) -> (if x y #f)
(define-macro and
  (lambda (exp)
    (cons 'if
          (cons (cadr exp)
                (cons (caddr exp)
                      (cons #f
                            '()))))))

;; (or x y) -> (if x #t y)
(define-macro or
  (lambda (exp)
    (cons 'if
          (cons (cadr exp)
                (cons #t
                      (cons (caddr exp)
                            '()))))))

(define true #t)
(define false #f)

(define (not x)
  (if x
      #f
      #t))

;;; Numbers.
(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (* -1 x))))

(define (quotient n1 n2)
  (/ n1 n2))

(define (remainder n1 n2)
  (- n1 (* (/ n1 n2) n2)))

(define (zero? x)
  (= x 0))

(define (positive? x)
  (> x 0))

(define (negative? x)
  (< x 0))

(define (odd? x)
  (= (remainder x 2) 1))

(define (even? x)
  (= (remainder x 2) 0))

(define (min x y)
  (if (< x y)
    x
    y))

(define (max x y)
  (if (> x y)
    x
    y))

;;; Lists.
(define null '())

(define (caar l) (car (car l)))
(define (cddr l) (cdr (cdr l)))
(define (cdar l) (cdr (car l)))
(define (cadr l) (car (cdr l)))

(define (caaar l) (car (caar l)))
(define (cdadr l) (cdr (cadr l)))
(define (cadar l) (car (cdar l)))
(define (caddr l) (car (cddr l)))
(define (caadr l) (car (cadr l)))
(define (cdaar l) (cdr (caar l)))
(define (cddar l) (cdr (cdar l)))
(define (cdddr l) (cdr (cddr l)))

(define (first l) (car l))
(define (second l) (cadr l))
(define (third l) (caddr l))
(define (rest l) (cdr l))
(define (last l) (car (reverse l)))

;; (list (+ 1 0) (+ 1 1) (+ 1 2)) -> '(1 2 3)
(define (list . xs)
  xs)

;; (make-list 2) -> '(banana! banana!)
;; (make-list 2 "a") -> '("a" "a")
(define (make-list n . other)
  (define fill (cond ((null? other)
                      'banana!)  ; Default value is unspecified.
                     ((> (length other) 1)
                      (error "MAKE-LIST invalid number of arguments."))
                     (else
                       (car other))))
  (define (loop n fill acc)
    (cond ((< n 0)
           (error "MAKE-LIST number of elements is less than zero."))
          ((= n 0)
           acc)
          (else
            (loop (- n 1) fill (cons fill acc)))))
  (loop n fill '()))

(define (length l)
  (define (loop accumulated remaining)
    (if (null? remaining)
      accumulated
      (loop (+ 1 accumulated) (cdr remaining))))
  (loop 0 l))

;; (list-ref '(a b c) 2) -> c
(define (list-ref l n)
  (cond ((null? l) (error "LIST-REF list is empty."))
        ((= n 0) (car l))
        ((< n 0) (error "LIST-REF index must be >= 0."))
        ((> n (- (length l) 1)) (error "LIST-REF index out of bounds."))
        (else (list-ref (cdr l) (- n 1)))))

(define (reverse l)
  (define (loop accumulated remaining)
    (if (null? remaining)
      accumulated
      (loop (cons (car remaining) accumulated)
            (cdr remaining))))
  (loop '() l))

(define (append l1 l2)
  (cond ((null? l1) l2)
        (else
          (cons (car l1)
                (append (cdr l1) l2)))))

(define (map f l)
  (cond ((null? l) '())
        (else
          (cons (f (car l))
                (map f (cdr l))))))

(define (filter f l)
  (cond ((null? l) '())
        ((f (car l))
         (cons (car l)
               (filter f (cdr l))))
        (else
          (filter f (cdr l)))))

;; Tail recursive. Constant space use.
(define (foldl f acc l)
  (if (null? l)
    acc
    (foldl f
           (f acc (car l))
           (cdr l))))

;; Not tail recursive. Space use is proportional to length of list.
(define (foldr f acc l)
  (if (null? l)
    acc
    (f (car l)
       (foldr f acc (cdr l)))))

(define (for-each f l)
  (cond ((null? l) '())
         (else
           (f (car l))
           (for-each f (cdr l)))))

(define (member x l)
  (cond ((null? l) #f)
        ((equal? x (car l)) #t)
        (else (member x (cdr l)))))

(define (index-of l x)
  (define (loop l x acc)
    (cond ((null? l) #f)
          ((equal? (car l) x)
           acc)
          (else
           (loop (cdr l) x (+ acc 1)))))
  (loop l x 0))

;; (assoc 2 '((1 a) (2 b))) -> '(2 b)
;; (assoc 2 '((1 a))) -> #f
(define (assoc x al)
  (cond ((null? al) #f)
        ((equal? x (caar al)) (car al))
        (else (assoc x (cdr al)))))

;; (remove 2 '(1 2 3 2 1)) -> '(1 3 1)
(define (remove x l)
  (filter (lambda (e) (not (equal? e x)))
          l))

;; (take '(1 2 3 4) 2) -> '(1 2)
(define (take l n)
  (cond ((= n 0) '())
        (else
          (cons (car l)
                (take (cdr l) (- n 1))))))

;; (drop '(1 2 3 4) 2) -> '(3 4)
(define (drop l n)
  (cond ((= n 0) l)
        (else
          (drop (cdr l) (- n 1)))))
(define list-tail drop)  ; R5RS procedure.

;;; Vectors,
(define (list->vector l)
  (define v (make-vector (length l)))
  (define (loop l index v)  ; Fill the array with the contents of the list.
    (cond ((null? l)
           v)
          (else
            (vector-set! v index (car l))
            (loop (cdr l) (+ index 1) v))))
  (loop l 0 v)
  v)

(define (vector->list v)
  (define v-len (vector-length v))
  (define (loop index acc)
    (cond ((> index (- v-len 1))
           acc)
          (else
            (loop (+ index 1)
                  (cons (vector-ref v index) acc)))))
  (reverse (loop 0 '())))

;;; Characters.
;;; WARN: The list of lowercase and uppercase characters are duplicated within
;;; the functions to prevent the list of letters from leaking into the user's
;;; initial environment.
(define (char-alphabetic? c)
  (define lowercase '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
                      #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
  (define uppercase '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
                      #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))
  (or (member c lowercase)
      (member c uppercase)))

(define (char-numeric? c)
  (define numeric '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
  (member c numeric))

(define (char-whitespace? c)
  (let ((whitespace-chars '(#\space #\tab #\newline)))
   (member c whitespace-chars)))

(define (char-upper-case? c)
  (define uppercase '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
                      #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))
  (member c uppercase))

(define (char-lower-case? c)
  (define lowercase '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
                      #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
  (member c lowercase))

(define (char-upcase c)
  (define lowercase '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
                      #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
  (define uppercase '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
                      #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))
  (cond ((char-lower-case? c)
         (define index (index-of lowercase c))
         (list-ref uppercase index))
        (else c)))

(define (char-downcase c)
  (define lowercase '(#\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
                      #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
  (define uppercase '(#\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
                      #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))
  (cond ((char-upper-case? c)
         (define index (index-of uppercase c))
         (list-ref lowercase index))
        (else c)))

;;; Strings.
(define (string-length s)
  (length (string->list s)))

(define (string-append a b)
  (list->string (append (string->list a)
                        (string->list b))))

(define (string=? s1 s2)
  (define (loop l1 l2)
    (cond ((or (null? l1) (null? l2))
           (and (null? l1) (null? l2)))
          ((equal? (car l1) (car l2))
           (loop (cdr l1) (cdr l2)))
          (else #f)))
  (loop (string->list s1)
        (string->list s2)))

;; (string-ref "abc" 1) -> #\b
(define (string-ref s n)
  (define char-list (string->list s))
  (list-ref char-list n))

;;; Display.
(define (newline)
  (display "\n"))

(define (displayln s)
  (display s)
  (newline))
