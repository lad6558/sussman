#| -*-Scheme-*-

Copyright (C) 2019 Chris Hanson and Gerald Jay Sussman

This file is part of SDF.  SDF is software supporting the book
"Software Design for Flexibility", by Chris Hanson and Gerald Jay
Sussman.

SDF is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

SDF is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with SDF.  If not, see <https://www.gnu.org/licenses/>.

|#

;;;; Functional Combinators

#|
;;; These are illustrations from the book

(define (first-compose f g)             ; p.23
  (lambda args
    (f (apply g args))))

(define (second-compose f g)            ; p.24
  (define (the-composition . args)
    (f (apply g args)))
  the-composition)
|#
;;; This is a final version

;; (define (compose f g)                   ; p.32
;;   (define (the-composition . args)
;;     (call-with-values (lambda () (apply g args)) ;
;;       f))
;;   (restrict-arity the-composition (get-arity g)))

(define (compose . funcs)
  (if (= (length funcs) 1)
      ; I would have used identity function here and change 1 to 0, but it has the wrong fixed arity of 1. Max-arity of inf would have fixed the problem.
      (car funcs)
      (let ((f (car funcs))
            (g (apply compose (cdr funcs))))
        (define (the-composition . args)
          (call-with-values (lambda () (apply g args))
           f))
        (restrict-arity the-composition (get-arity g)))))

(define (parallel-combine h f g)        ;p.26
  (define (the-combination . args)
    (h (apply f args) (apply g args)))
  the-combination)

(define (simple-spread-combine h f g)   ; p.28
  (let ((n (get-arity f)) (m (get-arity g)))
    (let ((t (+ n m)))
      (define (the-combination . args)
        (assert (= (length args) t))
        (h (apply f (list-head args n))
           (apply g (list-tail args n))))
      (restrict-arity the-combination t))))

(define (spread-apply f g)              ; p.32
  (let ((n (get-arity f))
        (m (get-arity g)))
    (let ((t (+ n m)))
      (define (the-combination . args)
        (assert (= (length args) t))
        (let-values ((fv (apply f (list-head args n)))
                     (gv (apply g (list-tail args n))))
          (apply values (append fv gv))))
      (restrict-arity the-combination t))))

(define (spread-combine h f g)          ; p.31
  (compose h (spread-apply f g)))

;; Returns a wrapper, which, when wrapped around a function, gives a version of that function that takes one more argument, while discarding the argument at position i
;; (define (discard-argument i)
;;   (assert (exact-nonnegative-integer? i))
;;   (lambda (f)
;;     (let ((m (+ (get-arity f) 1)))
;;       (define (the-combination . args)
;;         (assert (= (length args) m))
;;         (apply f (list-remove args i)))
;;       (assert (< i m))
;;       (restrict-arity the-combination m))))

;; helper for discard-argument
;; takes in a list->list function, returns a fixed-arity list-func with args already curried that returns multiple values
(define (list-op list-func arity . list-func-args)
  (define (args-op . args)
    (apply values (apply list-func (cons args list-func-args))))
  (restrict-arity args-op arity))

;; my implementation of discard argument using compose
(define (discard-argument i)
  (assert (exact-nonnegative-integer? i))
  (lambda (f)
    ;; (compose f (lambda (lst) (list-remove lst i)))))
    ;; QUESTION: how do I make lambda function accept any number of args?
    ;; lambda (. args) doesn't work
    (compose f (list-op list-remove (+ (get-arity f) 1) i))))

;; Here, assemble is a more general function than compose
;; it takes in a function factory and any number of args
;; and compose the functions produced by the factory using the args
;; compose is just a special case where the factory is identity
(define (assemble factory . args)
    ;; (display args)
    (if (null? args)
        identity-procedure
        (compose (factory (car args)) (apply assemble factory (cdr args)))))
;; I wrote the general compose only to find this out. But it requires binary compose.

(define (discard-arguments . unsorted-args)
  (let ((args (sort unsorted-args >)))
    (apply assemble discard-argument args)))

;; You can do curry-arguments using assemble too. Omitted here.

#|
(define ((curry-argument i) . args)     ; p.34
  (lambda (f)
    (assert (= (length args) (- (get-arity f) 1)))
    (lambda (x)
      (apply f (list-insert args i x)))))
|#

;;; Actually, contrary to the definition in the book
;;; given above, it is more useful to define curry-argument
;;; as follows (changing the order of F and ARGS).

;; (define ((curry-argument i) f) ; i is the position of the argument that will be actually passed in
;;   (lambda args ; the non-i args that are pre-determined
;;     (assert (= (length args) (- (get-arity f) 1)))
;;     (lambda (x) ; x is that one argument that is passed in
;;       (apply f (list-insert args i x)))))

; my implementation of curry-argument using compose
(define ((curry-argument i) f)
  (lambda args
    (compose f (lambda (x) (apply values (list-insert args i x))))))
;I have a feeling that this could be done using list-op too, but haven't come up with how

(define (list-remove lst index)
  (let lp ((lst lst) (index index))
    (assert (pair? lst))
    (if (= index 0)
        (cdr lst)
        (cons (car lst) (lp (cdr lst) (- index 1))))))

(define (list-insert lst index value)
  (let lp ((lst lst) (index index))
    (if (= index 0)
        (cons value lst)
        (begin
          (assert (pair? lst))
          (cons (car lst) (lp (cdr lst) (- index 1)))))))

;; permspec looks like (1 0 3 2), permute-arguments returns a decorator that, when wrapped around a function, returns a version of the function that has its arguments permuted
;; (define (permute-arguments . permspec)  ; p.36
  ;; (let ((permute (make-permutation permspec)))
  ;;   (lambda (f)
  ;;     (define (the-combination . args)
  ;;       (apply f (permute args)))
  ;;     (let ((n (get-arity f)))
  ;;       (assert (= n (length permspec)))
  ;;       (restrict-arity the-combination n)))))

(define (permute-arguments . permspec)
  (let ((permute (make-permutation permspec))) ; permute is a list->list func
    (lambda (f)
      (let ((arity (get-arity f)))
      (compose f (list-op
                   ;; (lambda (lst) (apply values (permute lst)))
                  permute
                   arity))))))

;;; Given a permutation (represented as
;;; a list of numbers), and a list to be
;;; permuted, construct the list so
;;; permuted.

(define (make-permutation permspec)
  (define (the-permuter lst)
    (map (lambda (p) (list-ref lst p))
         permspec))
  the-permuter)


;; For min-max arity, I will just talk about the big idea
;; For compose it is basically the same, still use the arity of g
;; For parallel combine, one has to take the intersection of f and g
;; And other functions are likewise just add some extra mins and maxs

(define arity-table (make-key-weak-eqv-hash-table))

(define (restrict-arity proc nargs)
  (hash-table-set! arity-table proc nargs)
  proc)

(define (get-arity proc)
  (or (hash-table-ref/default arity-table proc #f)
      (let ((a (procedure-arity proc)))
        (assert (eqv? (procedure-arity-min a)
                      (procedure-arity-max a)))
        (procedure-arity-min a))))

; Test funcs
(define (foo-list a b) (list `foo a b))
(define (bar-pair x) (values (list `bar x)(list `baz x)))
(define (identity x) x)

(define discard1 (discard-argument 1))
(define d1-bar-pair (discard1 bar-pair))
(d1-bar-pair 5 6)
;Value: (bar 5)
;Value: (baz 5)
((discard1 foo-list) 5 6 7)
;Value: (foo 5 7)

(define curry-foo  ((curry-argument 0) foo-list))
(define curry2-foo (curry-foo 2))
(curry2-foo 1)
;Value: (foo 1 2)

(define permute-foo  ((permute-arguments 1 0) foo-list))
(permute-foo `first `second)
;Value: (foo second first)

(define d01 (assemble discard-argument 1 0)) ; 0 1 doesn't work, will give 1 3
((d01 foo-list) 0 1 2 3)
;Value: (foo 2 3)

(define (times2 x) (* x 2))
(define (square x) (* x x))
;; ((compose) 5)
; I can't pass this because if I terminate the recursion at length 0, it requires the identity function to have a fixed arity. Whichever fixed arity I choose for the identity function is not well-defined here.
;Value: 5
((compose times2) 5)
;Value: 10
((compose times2 square) 5)
;Value: 50

(define bar-foo (compose foo-list bar-pair))
(bar-foo `test)
;Value: (foo (bar test) (baz test))

(define 2to2 (lambda (x y) (values y x)))
(define 2to1 (lambda (x y) (+ x y)))
((compose 2to1 2to2) 3 4)
