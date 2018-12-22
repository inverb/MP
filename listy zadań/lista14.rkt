#lang racket

;;zadanie 1

(define (make-account sec-pass balance)
  (define (dispatch pass x)
    (cond [(not (eq? pass sec-pass)) (lambda (x) 'incorrect pass)]))
  (dispatch sec-pass balance))

;;zadanie 2

(define (make-cycle mlist)
  (define (iter l)
    (if (null? (mcdr l))
               (set-mcdr! l mlist)
               (iter (mcdr l))))
  (begin (iter mlist) mlist))

;;zadanie 5

(define (bucket-sort l)
  (define v (make-vector (+ 1 (apply max (map (lambda (x) (car x)) l) '()))))
  (define (proc lst)
    (if (null? lst)
        v
        (begin (vector-set! v (caar lst) (cons (car lst)
                                               (vector-ref v (caar lst))))
               (proc (cdr lst)))))
  (foldr append '() (vector->list (vector-map! reverse (proc l)))))

;;zadanie 6

;;(define next-factors
;;  (lcons 1
;;         (lambda ()
;;           (lmap * next-factors (integer-starting-from 2)))))

;;zadanie 7

(define (merge xs ys)
  (cond [(null? xs) ys]
        [(null? ys) xs]
        [else (let ((x (head xs))
                    (y (head ys)))
                (cond [(= x y) (lcons x (lambda () (merge (tail xs) (tail ys))))]
                      [(< x y) (lcons x (lambda () (merge (tail xs ys))))]
                      [else (lcons y (lambda () (merge xs (tail ys))))]))]))