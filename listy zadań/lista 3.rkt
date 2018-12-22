#lang racket

;;zadanie 2
(define (make-point x y)
  (cons x y))

(define (point-x s) (car s))

(define (point-y s) (cdr s))

(define (point? s) (pair? s))

(define (make-vect x y)
  (cons x y))

(define (vect-begin s) (car s))

(define (vect-end s) (cdr s))

(define (vect? s)
  (if(pair? s)
     (and (point? (car s))
          (point? (cdr s)))
     null))

(define (square x) (* x x))

(define (dist x y) (- x y))

(define (point-dist x y)
  (+ (square (dist (car x) (car y)))
     (square (dist (cdr x) (cdr y)))))


(define (vect-length v)
  (point-dist (car v) (cdr v)))

(define (x-dist x y)
  (dist (car y) (car x)))

(define (y-dist x y)
  (dist (cdr y) (cdr x)))

(define (upgrade-point x y p)
  (cons (+ (car p) x) (+ (cdr p) y)))

(define (vect-scale v k)
  (let ((x (x-dist (car v) (cdr v)))
        (y (y-dist (car v) (cdr v))))
    (cons (car v) (upgrade-point (* x k) (* y k) (car v)))))

(define (vect-translate v p)
  (let ((x (x-dist (car v) p))
        (y (y-dist (car v) p)))
    (cons p (upgrade-point x y (cdr v)))))

;;wypisywanie
( define ( display-point p )
   ( display "(")
   ( display ( point-x p ) )
   ( display ", ")
   ( display ( point-y p ) )
   ( display ")") )

( define ( display-vect v )
   ( display "[")
   ( display-point ( vect-begin v ) )
   ( display ", ")
   ( display-point ( vect-end v ) )
   ( display "]") )

;;zadanie 5
(define (insert xs n)
  (if(null? xs)
     (cons n null)
     (if(< n (car xs))
        (cons n xs)
        (cons (car xs) (insert (cdr xs) n)))))

(define (insert-sort xs)
  (define (sort ys zs)
    (if(null? ys)
       zs
       (sort (cdr ys) (insert zs (car ys)))))
  (sort xs null))

;;zadanie 6
(define (permi l n)
  (define (insert-at xs y n)
     (if(= n 0)
        (cons y xs)
        (cons (car xs) (insert-at (cdr xs) y (- n 1)))))
  (define (rek j lista)
    (cond [(null? lista) null]
          [(= j n) (rek 0 (cdr lista))]
          [else (cons (insert-at (car lista) (car l) n) (rek (+ j 1) lista))]))
  (if(null? l)
     (list(null))
     (rek 0 (permi (cdr l) (- n 1)))))