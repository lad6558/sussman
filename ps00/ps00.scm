;;;; Author: Andi Liu (the boy who asks a lot of questions)
;;;; Date: Feb 16
;;;; Edited and executed on a remote Ubuntu machine, using Geiser-mit plugin for Spacemacs


;;; Problem 1

(modulo 13 8)
;Value: 5

(remainder 13 8)
;Value: 5

(modulo -13 8)
;Value: 3

(remainder -13 8)
;value: -5

(modulo -13 -8)
;value: -5

(remainder -13 -8)
;value: -5

(remainder 5 -8)

;What is the difference between remainder and modulo? Which one is the best choice for implementing modular arithmetic as described above?

;Answer: modulo's output is guaranteed to be between 0 (inclusive) and modulus (exclusive), while remainder's output is guaranteed to be between -modulus (exclusive) and modulus (exclusive). Modulus is closer to what we want



(define +mod
  (lambda (a b n)
    (modulo (+ a b) n)))

(define -mod
  (lambda (a b n)
    (modulo (- a b) n)))

(define *mod
  (lambda (a b n)
    (modulo (* a b) n)))

(+mod 7 5 8) ; -> 4
(+mod 10 10 3) ; -> 2
(-mod 5 12 2) ; -> 1
(*mod 6 6 9) ; -> 0
(+mod 99 99 100) ; -> 98
(*mod 50 -3 100) ; -> 50

(define (modular modulus op)
    (lambda (a1 a2)
      (modulo (op a1 a2) modulus)))

((modular 17 +) 13 11) ; -> 7
((modular 17 -) 13 11) ; -> 2
((modular 17 *) 13 11) ; -> 7


;;; Problem 2

;; The order of growth in time is O(n), where n is the exponent
;; Growth in space is O(n)
;; It uses an recursive algorithm because it cannot be tail-call optimized (due to mod*at the end)

(define (zero? number) (= number 0))
(define (even? number) (zero? (modulo number 2)))
(define (odd? number) (not (even? number)))

(define (exptmod p)
  (let ((mod* (modular p *)))
    (define (square x)
      (mod* x x))
    (define (em base exponent)
      (if (zero? exponent)
          1
          (if (even? exponent)
              (em (square base) (/ exponent 2))
              (mod* base (em (square base) (/ (- exponent 1) 2)))
              )
          ))
    em)
)

;; Time: O(log n)
;; Space: O(log n)
;; Still recursive, because of the mod*


(define (random-k-digit-number k)
  (begin
    ;(display k)
    (if (zero? k)
        0
        (+ (* 10 (random-k-digit-number (- k 1))) (random 10))
    )
  )
)

(random-k-digit-number 1) ; -> 7
(random-k-digit-number 3) ; -> 815
(random-k-digit-number 3) ; -> 713 (it is different!)
(random-k-digit-number 50) ; -> 2491018140705041207024206330833122340503842216183


(define (count-digits num)
  (if (< num 1)
      0
      (+ (count-digits (/ num 10)) 1)
  )
)

(define (big-random limit)
  (let ((k (count-digits limit)))
    (define (loop)
       (let ((x (random-k-digit-number k)))
         (if (< x limit)
             x
             (loop)
             )
       )
    )
  (loop)
  )
)

;;; Problem 4

;; Time: O(n)
;; Space: O(1) (Tail-optimized)
;; Iterative

;; If we only check the factors up to n, this would reduce time to O(sqrt(n))
;; If we only check the odd factors, the time complexity remains O(n)

(define (prime? p)
  (let ((exptmodp (exptmod p)))
    (define (test-fermat k)
      (if (zero? k)
          #t
          (let* ((a (big-random p))
                 (a^p (exptmodp a p)))
            (if (= a a^p)
                (test-fermat (- k 1))
                #f
                )
            )
          )
      )
    (if (< p 2)
        #f
        (test-fermat 20)
        )
    )
  )

(prime? 2) ; -> #t
(prime? 4) ; -> #f
(prime? 1) ; -> #f
(prime? 0) ; -> #f
(prime? 200) ; -> #f
(prime? 199) ; -> #t

;;; Problem 5
(define (random-k-digit-prime k)
  (let ((k-digit-number (random-k-digit-number k)))
    (if (prime? k-digit-number)
        k-digit-number
        (random-k-digit-prime k))
    )
  )

(random-k-digit-prime 1) ; 2
(random-k-digit-prime 2) ; 53
(random-k-digit-prime 10) ; 3170450953
(count-digits (random-k-digit-prime 100)) ; 100
(count-digits (random-k-digit-prime 100)) ; 99 (indeed not 100)

;;; Problem 6

;; Requires a b to be relatively prime
(define (ax+by=1 a b)
  (if (zero? b)
      (if (= a 1)
          (cons 1 0)
          (error "Not relatively prime")
          )
      (let* ((pair (ax+by=1 b (modulo a b)))
             (x (car pair))
             (y (cdr pair)))
        (cons y (- x (* (quotient a b) y)))
       )
      )
  )

(ax+by=1 17 11) ; (2 -3)
(ax+by=1 34 7) ; (-1 5)
(ax+by=1 17 13) ; (-3 4)
(ax+by=1 7 3) ; (1 -2)
(ax+by=1 10 27) ; (-8 3)

(define (inversemod p)
  (lambda (x)
    (let ((a (car (ax+by=1 x p))))
       (if (> a 0)
           a
           (+ a p))
       )
    )
  )

((inversemod 11) 5) ; 9
((inversemod 11) 9) ; 5
((inversemod 11) 7) ; 8
((inversemod 12) 5) ; 5
;; ((inversemod 12) 8) ; error "Not relatively prime"
((inversemod 101) (random-k-digit-prime 2)) ; 28, where 101 * 28 mod 11 = 1

;; Problem 7
(load "p0utils.scm")

(define (eg-make-message message receiver)
  (let ((public-key (eg-receiver-public-key receiver))
        (message (string->integer message)))
    (let ((dh-system (eg-public-key-system public-key))
          (advertised-number (eg-public-key-number public-key)))
      (let ((p (dh-system-prime dh-system))
            (size (dh-system-size dh-system))
            (a (dh-system-primitive-root dh-system)))
        (let* ((my-secret (random-k-digit-prime size))
	             (mod-expt (exptmod p))
	             (mod-* (modular p *))
	             (mod-inv (inversemod p))
               (x (mod-expt a my-secret))
               (y (mod-* message (mod-expt advertised-number my-secret))))
          (eg-make-ciphertext x y))))))

(define (eg-send-message message receiver)
  (let* ((decryption-procedure (eg-receiver-decryption-procedure receiver))
         (ciphertext (eg-make-message message receiver)))
    (decryption-procedure ciphertext)))

; Test
(define dh-system (public-dh-system 100))
(define Alyssa (eg-receiver dh-system))
;; (eg-send-message "Hi there." Alyssa)
;; (eg-send-message "Hiiii there." Alyssa)

;;Value: "Hiiii there."

;;(eg-send-message "This is a test sentence to see exactly how far this can go." Alyssa)
;;Value: "å³§E\x7f;\x88;Òë\x88;/7\x9d;½Ò¬;Æ^Fa\x15;\x9d;A\xb;\x87;ë\x8a;ÔÌ«(7æ\"Ï\x93;ÃkFdT\x4;"

;;(eg-send-message "This is a test sentence to see exactly how far" Alyssa)
;;Value: "\x19;¸\x1b;Ûo°\x1a;È?\x1d;\xb;\x5;?Ú»\x1d;ÅÞ\x91;Òß[ý\x8c;G#aÙ¨\x9e;\n\x97;eøí\x88;>\tI\x88;4\x1;"

;; (eg-send-message "This is a test sentence to see exactly " Alyssa)
;;Value: "This is a test sentence to see exactly "

;; (eg-send-message "This is a test sentence to see exactly how" Alyssa)
;; Value: "\x89;\t\x83;OyÍ\x9a;\x18;ÐÜ^B5ÕeÇrÏ×=\x9b;ô\x81; \x1d;í\x8b;¯5õï\\\xad;\x94;¾\x93;Úµ¼ÝV\x6;"

;;It can send around 30 character. More than that, it can only produce gibberish of a certain length. I wonder, why 30 characters? The log of 10^100 is around 41, which is its theoretical upper limit. But what is preventing us from reaching that?

;;; problem 8

(define Alyssa (Eve Alyssa))
(define (Eve receiver)
  (let ((receiver-public-key
	       (eg-receiver-public-key receiver))
	      (receiver-decryption-procedure
	       (eg-receiver-decryption-procedure receiver)))
    (let ((my-spying-procedure
	         (lambda (ciphertext)
	           (write ciphertext)
	           (newline)
             (set! ciphertext (eg-make-message "Malicious Text" receiver))
	           (receiver-decryption-procedure ciphertext))))
      (eg-make-receiver receiver-public-key
			                  my-spying-procedure))))

(eg-send-message "Hi there." Alyssa) ; Value: "Malicious Text"
;; Here because Alyssa is actually receiving ciphertext from Eve, Eve can mutate the message and give Alyssa Eve's malicious message instead.
;; I am confused though, why don't Bob send the message to Alyssa directly? In real life, why someone send the message via someone they don't trust in the middle?
