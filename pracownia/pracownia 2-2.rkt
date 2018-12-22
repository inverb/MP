#lang racket

(define (make-function f g)
  (lambda (x) (f x (g x))))

(define (repeated p f n)  
  (if(= n 0)
     (lambda (x) (f x))
     (make-function p (repeated p f (- n 1)))))

(define (close-enough? x y)
  (define (dist x y) (abs (- x y)))
  
  (< (dist x y) (* (min x y) 0.00001)))

(define (fixed-point f x0)
  (let ([x1 (f x0)])
    (if(close-enough? x0 x1)
       x1
       (fixed-point f x1))))

(define (average-damp x y)
  (/ (+ x y) 2))

;; y do potegi n
(define (power y n)
  (define (iter m p)
    (if(= m 0)
       p
       (iter (- m 1) (* p y))))
  (iter n 1))

;;podłoga z logarytmu dwójkowego z n
(define (logarytm n)
  (define (iter i x)
    (if(< n x)
       i
       (iter (+ i 1) (* x 2))))
  (iter 0 2))

;; pierwiastek n-tego stopnia z x
(define (nth-root n x)
  (fixed-point
   (repeated (average-damp)
             (lambda (y) (/ x (power y (- n 1))))
             (logarytm n)) 1.0))

;;Pierwiastek n-tego stopnia z x, tłumiony
;;o jeden raz za mało przy każdej iteracji.
;;Jeśli moje wyliczenia są poprawne
;;obliczenie powinno nie być zbieżne.
(define (nth-root-incorrect n x)
  (fixed-point
   (repeated (average-damp)
             (lambda (y) (/ x (power y (- n 1))))
             (- (logarytm n) 1)) 1.0))

;;Testy poprawnego programu...
(define (tests)
  (define test1
    (close-enough? (nth-root 2 100) 10.0))
  (define test2
    (close-enough? (nth-root 4 81) 3.0))
  (define test3
    (close-enough? (nth-root 4 28561) 13.0))
  (define test4
    (close-enough? (nth-root 5 16807) 7.0))
  (define test5
    (close-enough? (nth-root 7 10460353203) 27.0))
  (define test6
    (close-enough? (nth-root 8 390625) 5.0))
  (define test7
    (close-enough? (nth-root 16 65536) 2.0))
  
  (and test1 test2 test3 test4 test5 test6 test7))

;;...i testy programu, który tłumi jeden raz mniej.
;;Wszystkie testy za wyjątkiem testu 5 się zapętlają co pozwala wysunąć tezę, że logarytm
;;jest możliwie dobrym przybliżeniem liczby tłumień.
(define (test1-incorrect)
  (close-enough? (nth-root-incorrect 2 100) 10.0))
(define (test2-incorrect)
  (close-enough? (nth-root-incorrect 4 81) 3.0))
(define (test3-incorrect)
  (close-enough? (nth-root-incorrect 4 28561) 13.0))
(define (test4-incorrect)
  (close-enough? (nth-root-incorrect 5 16807) 7.0))
(define (test5-incorrect)
  (close-enough? (nth-root-incorrect 7 10460353203) 27.0))
(define (test6-incorrect)
  (close-enough? (nth-root-incorrect 8 390625) 5.0))
(define (test7-incorrect)
  (close-enough? (nth-root-incorrect 16 65536) 2.0))