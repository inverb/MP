#lang racket

(define (cube-root value)
  (define (cube approx)
    (* approx approx approx))
  (define (close-enough? cube-of-approx)
    (< (abs (- cube-of-approx value)) (abs( * 0.001 value))))
  (define (improve approx)
    (/ (+ (/ value (* approx approx)) (* 2 approx)) 3))
  (define (iter approx)
    (if (close-enough? (cube approx) )
        approx
        (iter (improve approx))))
  (iter 1.0))

;; testy
(define (tests)
  (define (test1)
    (< (abs (- 3 (cube-root 27))) 0.001))
  (define (test2)
    (< (abs (- 1.5874010 (cube-root 4))) 0.001))
  (define (test3)
    (< (abs (- 3.97905720 (cube-root 63))) 0.001))
  (define (test4)
    (< (abs (- 0.1 (cube-root 0.001))) 0.001))
  (define (test5)
    (< (abs (- -1 (cube-root -1))) 0.001))
  (and (test1) (test2) (test3) (test4) (test5)))