#lang racket
;;zadanie 4
(define (sum-of-squares a b c)
  (define (square x)
    (* x x))
  (define (sum x y)
    (+ (square x) (square y)))
  (cond
    [(and (or (< a b) (= a b)) (or (< a c) (= a c)) ) (sum b c)]
    [(and (or (< b c) (= b c)) (or (< b a) (= b a)) ) (sum a c)]
    [else (sum a b )]))

;;zadanie 8
(define (power-close-to b n)
  (define (is-bigger? b x n)
    (> (expt b x) n))
  (define (iter x)
    (cond
      [(is-bigger? b x n) x]
      [else (iter (+ x 1))]))
  (iter 1))