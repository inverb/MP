#lang racket

(define (diff x y)
  (abs (- x y)))

(define (close-enough? x y)
  (< (diff x y) (* (min x y) 0.0001)))

(define (inc x)
  (+ x 1))

(define (fraction num den)
  (define new-fraction
    (lambda (x0 x1 n d) (+ (* d x1) (* n x0))))
  
  (define (iter i a0 a1 b0 b1 n d)
    (let ([a2 (new-fraction a0 a1 n d)]
          [b2 (new-fraction b0 b1 n d)])
      (let ([f1 (/ a1 b1)]
            [f2 (/ a2 b2)])
        (if (close-enough? f1 f2)
            f2
            (iter (inc i) a1 a2 b1 b2 (num (inc i)) (den (inc i)))))))
  
  ;;i=1 a-1=1 a0=0 b-1=0 b0=1
  (iter 1 1.0 0 0 1.0 (num 1) (den 1)))


(define (tests)
  ;;liczba pi
  (define pi
    (+ 3 (fraction (lambda (x) (* (- (* 2 x) 1) (- (* 2 x) 1))) (lambda (x) 6))))

  ;;złoty podział, czyli ułamek dla Ni = Di = 1 dla każdego i, równy (1 + (sqrt(5))/2
  (define golden-ratio
    (+ 1 (fraction (lambda (x) 1) (lambda (x) 1))))
  
  ;;arctg(x)
  (define (arctg x)
    (fraction
     (lambda (y) (if(= y 1)
                    x
                    (* (- y 1) (- y 1) x x)))
     (lambda (y) (- (* 2 y) 1))))
  
  (and (close-enough? pi 3.1415)
       (close-enough? golden-ratio 1.618)
       (close-enough? (arctg 1) 0.785398)
       (close-enough? (arctg 10) 1.47112767)))
               