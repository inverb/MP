#lang racket

(define (square x) (* x x))
(define (inc x) (+ x 1))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (identity x) x)

;;zad 3
(define (repeated p n)
  (if (= n 0)
      (lambda (x) (identity x))
      (compose p (repeated p (- n 1)))))

(define sum
  (lambda (term next s e)
    (if (> s e)
        0
        (+ (term s) (sum term next (next s) e)))))
;; zad 4
(define prod
  (lambda (term next s e)
    (if (> s e)
        1
        (* (term s) (prod term next (next s) e)))))

(define prod-iter
  (lambda (term next s e p)
    (if (> s e)
        p
        (prod-iter term next (next s) e (* p (term s))))))

(define (pi x)
  (let ([y  (+ (* 2 x) 1)])
  ( / (* 4 2
         (prod
          (lambda(z) (/ (* (+ z 1) (+ z 1)) (* z z)))
          (lambda (z) (+ z 2))
          3 y)) (+ y 1))))

;;zad 5
(define accumulate
  (lambda (combiner null-value term next s e)
    (if (> s e)
        null-value
        (combiner (term s) (accumulate combiner null-value term next (next s) e)))))

(define accumulate-iter
  (lambda (combiner null-value term next s e p)
    (if (> s e)
        p
        (accumulate-iter combiner null-value term next (next s) e (combiner p (term s))))))

;;suma
(define (suma term next s e)
  (accumulate (lambda (x y) (+ x y)) 0 term next s e))

;;iloczyn
(define (iloczyn term next s e)
  (accumulate (lambda (x y) (* x y)) 1 term next s e))

;;zad 6
(define (cont-frac num den k)
  (cont-frac-pom num den 1 k))

(define cont-frac-pom
  (lambda (num den s k)
    (if (= k s)
        0
        (/ (num s) (+ (den s) (cont-frac-pom num den (+ s 1) k))))))

;;iteracyjnie
(define cont-frac-it
  (lambda (num den s k p)
    (if (= k s)
        p
        (/ (num s) (+ (den s) (cont-frac-it num den (+ s 1) k
                                            (/ (num k) (+ p (den k)))))))))

(define (cont-frac-iter num den k)
  (cont-frac-it num den 1 k 0))

;;zad 7
(define pii (+ (cont-frac
                (lambda (x) (* (- (* 2 x) 1) (- (* 2 x) 1)))
                (lambda (x) 6) 21) 3.0))
