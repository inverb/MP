#lang racket

;;zadanie 1

(define/contract (suffixes l)
  (let ([a (new-∀/c 'a)])
    (-> (listof a) (listof (listof a))))
  (if (null? l)
      null
      (cons l (suffixes (cdr l)))))

;;zadanie 2

(define/contract (dist x y)
  (-> number? number? number?)
  (abs (- x y)))

(define/contract (average x y)
  (-> number? number? number?)
  (/ (+ x y) 2))

(define/contract (square x)
  (-> number? number?)
  (* x x))

(define/contract (sqrt x)
  (->i ([x positive?])
        [result positive?]
       #:post (result x)
       (< (abs (- (* result result) x)) (* 0.001 x)))
  ;; lokalne definicje
  ;; poprawienie przybliżenia pierwiastka z x
  (define (improve approx)
    (average (/ x approx) approx))
  ;; nazwy predykatów zwyczajowo kończymy znakiem zapytania
  (define (good-enough? approx)
    (< (dist x (square approx)) 0.0001))
  ;; główna procedura znajdująca rozwiązanie
  (define (iter approx)
    (cond
      [(good-enough? approx) approx]
      [else                  (iter (improve approx))]))
  
  (iter 1.0))

;;zadanie 3

;;kontrakt parametryczny
(define/contract (filter pred l)
  (let ([a (new-∀/c 'a)])
    (-> (-> a (or/c true false)) (listof a) (listof a)))
  (cond [(null? l) null]
        [(pred (car l)) (cons (car l) (filter pred (cdr l)))]
        [else (filter pred (cdr l))]))

;;kontrakt zależny
;(define/contract (filter1 pred l)
 ; (->i
  ;  (let ([a (new-∀/c 'a)])
   ;  [pred (-> a boolean?)]
    ;  [l (listof a)])
     ;(result list?)
;     #:post  (result pred l)
 ;    (andmap (lambda (x) (member? x l)) result))
  ;(cond [(null? l) null]
   ;     [(pred (car l)) (cons (car l) (filter1 pred (cdr l)))]
    ;    [else (filter1 pred (cdr l))]))

;;zadanie 4

(define-signature monoid^
  ((contracted
    [elem? (-> any/c boolean?)]
    [neutral elem?]
    [oper (-> elem? elem? elem?)])))

(define-unit monoid-ints@
  (import)
  (export monoid^)

  (define (elem? e) (number? e))

  (define neutral 0)

  (define (oper e1 e2) (+ e1 e2)))

(define-unit monoid-lists@
  (import)
  (export monoid^)

  (define (elem? e) (list? e))

  (define neutral null)

  (define (oper e1 e2) (append e1 e2)))

(define-values/invoke-unit/infer monoid-ints@)
;(define-values/invoke-unit/infer monoid-lists@)

;;zadanie 5

(require quickcheck)

;;dla liczb
(quickcheck
 (property ([s arbitrary-integer])
           (eq? s (oper s neutral))))

(quickcheck
 (property ([s arbitrary-integer])
           (eq? s (oper neutral s))))

(quickcheck
 (property ([s arbitrary-integer]
            [p arbitrary-integer]
            [q arbitrary-integer])
           (eq? (oper s (oper p q)) (oper (oper s p) q))))

;;dla list
;(quickcheck
; (property ([s (arbitrary-list arbitrary-symbol)])
;           (equal? s (oper s neutral))))
;
;(quickcheck
; (property ([s (arbitrary-list arbitrary-symbol)])
;           (equal? s (oper neutral s))))
;
;(quickcheck
; (property ([s (arbitrary-list arbitrary-symbol)]
;            [p (arbitrary-list arbitrary-symbol)]
;            [q (arbitrary-list arbitrary-symbol)])
;           (equal? (oper s (oper p q)) (oper (oper s p) q))))