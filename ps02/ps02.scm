;;;; Author: Andi Liu
;;;; Date: Feb 28
;;;; Edited and executed on a remote Ubuntu machine, using Geiser-mit plugin for Spacemacs


;;; a dirty trick to make GJS's package manager work with auto-complete
(let ((geiser-env (->environment '(runtime geiser))))
  (eval '(define (package/environment package)
           (nearest-repl/environment))
        geiser-env)
  (eval '(define (->environment object)
           (nearest-repl/environment))
        geiser-env))

;; (load "~/sussman/sdf/manager/load" )
;; (manage `new `combining-arithmetics)

(define (function-extender codomain-arithmetic)
  (let ((codomain-predicate
         (arithmetic-domain-predicate codomain-arithmetic)))
    (make-arithmetic `function
                     (disjoin codomain-predicate function?)
                     (list codomain-arithmetic)
                     (lambda (name codomain-constant)
                       codomain-constant)
                     (lambda (operator codomain-operation)
                       (make-operation operator
                                       (any-arg (operator-arity operator)
                                                function?
                                                codomain-predicate)
                                       (lambda things
                                         (lambda args
                                           (apply-operation codomain-operation
                                                            (map (lambda (thing)
                                                                   ;; here is the coercion:
                                                                   (display "thing:")
                                                                   (pp thing)
                                                                   (display "args:")
                                                                   (pp args)
                                                                   (if (function? thing)
                                                                       (apply thing args)
                                                                       thing))
                                                                 things)))))))))

(define boolean-arithmetic
  (make-arithmetic `boolean boolean? `()
                   (lambda (name)
                     (case name
                       ((additive-identity) #f)
                       ((multiplicative-identity) #t)
                       (else (default-object))))
                   (lambda (operator)
                     (let ((procedure
                            (case operator
                              ((+) (lambda (arg1 arg2) (or arg1 arg2)))
                              ((-) not)
                              ((*) (lambda (arg1 arg2) (and arg1 arg2)))
                              ((negate) not)
                              (else
                               (lambda args
                                 (error "Operator undefined in Boolean" operator))))))
                       (simple-operation operator boolean? procedure)))))




(define (vector-element-wise element-procedure)
  (lambda vecs ; Note: this takes multiple vectors
    (ensure-vector-lengths-match vecs)
    (apply vector-map element-procedure vecs)))

(define (ensure-vector-lengths-match vecs)
  (let ((first-vec-length (vector-length (car vecs))))
    (if (any (lambda (v)
               (not (n:= (vector-length v)
                         first-vec-length)))
             vecs)
        (error "Vector dimension mismatch:" vecs))))

(register-predicate! vector? 'vector)

;; Failed attempt to implement vector-arithmetic
;; It says the object (1 2), passed as the first arg to integer-add is not the right type
;; It seems like I need to write an extender instead of this.
;; (define vector-arithmetic
;;   (make-arithmetic `vector vector? `()
;;                    (lambda (name)
;;                      (case name
;;                        ;; ((additive-identity) #f)
;;                        ;; ((multiplicative-identity) #t)
;;                        (else (default-object))))
;;                    (lambda (operator)
;;                      (let ((procedure
;;                             (case operator
;;                               ((+) (vector-element-wise +))
;;                               ((-) (vector-element-wise -))
;;                               ((*) (vector-element-wise *))
;;                               ((negate) (vector-element-wise negate))
;;                               (else
;;                                (lambda args
;;                                  (error "Operator undefined in Boolean" operator))))))
;;                        (simple-operation operator boolean? procedure)))))

(define (dot-product-maker + *)
  (lambda (vec1 vec2)
    (let ((v1 (vector->list vec1))
          (v2 (vector->list vec2)))
      (apply + (map * v1 v2))))) ;looking back, I coulda used vector-map

;; I wonder why we have to pass in + and * here
(define (vector-magnitude-maker + * sqrt)
  (let ((dot-product (dot-product-maker + *)))
    (define (vector-magnitude v)
      (sqrt (dot-product v v)))
    vector-magnitude))

; Assume a vector exists in the list
(define (get-vector-from-list vec-list)
  (if (vector? (car vec-list))
      (car vec-list)
      (get-vector-from-list (cdr vec-list))))

(define (expand-to-list element remaining-length)
  (if (= remaining-length 0)
      '()
      (cons element (expand-to-list element (- remaining-length 1)))))

(define (expand-to-vector element remaining-length)
  (list->vector (expand-to-list element remaining-length)))

(define (transform-vector-list elt-list)
  (let ((vector-len(vector-length (get-vector-from-list elt-list))))
    (transform-vector-list* elt-list vector-len)))

(define (transform-vector-list* elt-list vector-len)
  (if (= (length elt-list) 0)
      '()
      (let* ((elt (car elt-list))
             (transformed-elt
              (if (vector? elt)
                   elt
                   (expand-to-vector elt vector-len))))
        (cons transformed-elt (transform-vector-list* (cdr elt-list) vector-len)))))

;(transform-vector-list '(5 #(1 3 3) 7 ))
;Value: (#(5 5 5) #(1 3 3) #(7 7 7))

(define (vector-extender codomain-arithmetic)
  (let ((codomain-predicate
         (arithmetic-domain-predicate codomain-arithmetic)))
    (make-arithmetic `vector
                     (disjoin codomain-predicate vector?)
                     (list codomain-arithmetic)
                     (lambda (name codomain-constant)
                       default-object)
                     (lambda (operator codomain-operation)
                       ;; (pp operator)
                       ;; (if (eqv? operator `*)
                       ;;     (display "This is multiplication")
                       ;;     ;; (pp operator)
                       ;;     (display "This is not multi")
                       ;;     )

                       (operation-union
                        operator

                        ;; First case: all-vector case
                        (make-operation
                         operator
                         ;; (any-arg
                         ;;  (operator-arity operator)
                         ;;  vector?
                         ;;  codomain-predicate)
                         (all-args
                          (operator-arity operator)
                          vector?)
                         (lambda things

                           (case operator
                             ((*) (apply (dot-product-maker + *) things))
                             ((magnitude) (apply (vector-magnitude-maker + * sqrt) things))
                             (else
                              (apply (vector-element-wise (operation-procedure codomain-operation))
                                     things)
                              ;; I thought about using case here to hijack result-vector
                              ;; but that is not very generalizable and results in complicated code
                              ;; so I thought I better use the magnitude-maker to hijack the operator instead
                              ;; Deprecated code:
                              ;; (case operator
                              ;;   ((*) ))
                              ))))
                        ;; Below code handles not-all-vector case
                        ;; Copies all the non-vecs many times to make them vecs
                        ;; Not only scalar product, also broadcasts addition
                        ;; This doesn't handle the case where vecs have different len
                        ;; which is not well-defined anyway
                        (make-operation
                         operator
                         (any-arg
                          (operator-arity operator)
                          vector?
                          codomain-predicate)
                         (lambda things
                              (apply
                               (vector-element-wise
                                (operation-procedure codomain-operation))
                                     (transform-vector-list things)))))))))

(define vector-arithmetic (extend-arithmetic vector-extender combined-arithmetic))
(install-arithmetic! vector-arithmetic)

(define v12 (vector 1 2))
(define v13 (vector 1 3))
;(+ v12 v13) -> #(2 5)
;; (+(+ #(1 `a) #(1 1))
;; ;Value: #(2 (+ (quasiquote a) 1))
;; (* v12 v13)
;; ;Value: 7
;; (+ 1 v12)
;; ;Value: #(2 3)
;; (* 4 v12)
;; ;Value: #(4 8)
;; (* 4 v12 5)
;; ;Value: #(20 40)
;; (* 4 v12 5 v13)
;; ;Value: 140

;; 1 (user) => (+ #(1 2) #('a 5) #('a 3))
;;;Value: #((+ (+ 1 (quote a)) (quote a)) 10)
;; 1 (user) => (+ #(1 'a))
;;;Value: #(1 (quote a))

(define vec-before-func
  (extend-arithmetic
   function-extender
   (extend-arithmetic vector-extender combined-arithmetic)))
(define func-before-vec
  (extend-arithmetic
   vector-extender
   (extend-arithmetic function-extender combined-arithmetic)))



