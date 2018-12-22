#lang racket

;;z wykładu


;; pomocnicza funkcja dla list tagowanych o określonej długości

(define (tagged-tuple? tag len p)
  (and (list? p)
       (= (length p) len)
       (eq? (car p) tag)))

(define (tagged-list? tag p)
  (and (pair? p)
       (eq? (car p) tag)
       (list? (cdr p))))
;;

(define (node l r)
  (list 'node l r))

(define (node? n)
  (tagged-tuple? 'node 3 n))

(define (node-left n)
  (second n))

(define (node-right n)
  (third n))

(define (leaf? n)
  (or (symbol? n)
      (number? n)
      (null? n)))

;;

(define (res v s)
  (cons v s))

(define (res-val r)
  (car r))

(define (res-state r)
  (cdr r))

;;

(define (rename t)
  (define (rename-st t i)
    (cond [(leaf? t) (res i (+ i 1))]
          [(node? t)
           (let* ([rl (rename-st (node-left t) i)]
                  [rr (rename-st (node-right t) (res-state rl))])
             (res (node (res-val rl) (res-val rr))
                  (res-state rr)))]))
  (res-val (rename-st t 0)))

;;

(define (st-app f x y)
  (lambda (i)
    (let* ([rx (x i)]
           [ry (y (res-state rx))])
      (res (f (res-val rx) (res-val ry))
           (res-state ry)))))

(define get-st
  (lambda (i)
    (res i i)))

(define (modify-st f)
  (lambda (i)
    (res null (f i))))

;;

(define (inc n)
  (+ n 1))

(define (rename2 t)
  (define (rename-st t)
    (cond [(leaf? t)
           (st-app (lambda (x y) x)
                   get-st
                   (modify-st inc))]
          [(node? t)
           (st-app node
                   (rename-st (node-left  t))
                   (rename-st (node-right t)))]))
  (res-val ((rename-st t) 0)))

;;zadanie 1

(define (st-app2 f . args)
  (lambda (i)
    (define (rec arr)
      (if (null? (cdr arr))
          (list ((car arr) i))
          (let ((prev (rec (cdr arr))))
            (cons ((car arr) (res-state (car prev)) prev)))))
    (let ((result (rec (reverse args))))
      (res (f (map (lambda (x) (res-val x) (reverse result) (res-state (car result)))))))))

;;zadanie 2

(define (rand max)
  (lambda (i)
    (let ([v (modulo (+ (* 1103515245 i) 12345) (expt 2 32))])
      (res (modulo v max) v))))

(define (rename3 t)
  (define (rename-st t i)
    (cond [(leaf? t) (res (rand max) i)]
          [(node? t)
           (let* ([rl (rename-st (node-left t) i)]
                  [rr (rename-st (node-right t) (res-state rl))])
             (res (node (res-val rl) (res-val rr))
                  (res-state rr)))]))
  (res-val (rename-st t 0)))

;;drugi kod z wykładu


;;
;; WHILE
;;

; memory

(define empty-mem
  null)

(define (set-mem x v m)
  (cond [(null? m)
         (list (cons x v))]
        [(eq? x (caar m))
         (cons (cons x v) (cdr m))]
        [else
         (cons (car m) (set-mem x v (cdr m)))]))

(define (get-mem x m)
  (cond [(null? m) 0]
        [(eq? x (caar m)) (cdar m)]
        [else (get-mem x (cdr m))]))

; arith and bools

(define (const? t)
  (number? t))

(define (op? t)
  (and (list? t)
       (member (car t) '(+ - * / = > >= < <=))))

(define (op-op e)
  (car e))

(define (op-args e)
  (cdr e))

(define (op->proc op)
  (cond [(eq? op '+) +]
        [(eq? op '*) *]
        [(eq? op '-) -]
        [(eq? op '/) /]
        [(eq? op '=) =]
        [(eq? op '>) >]
        [(eq? op '>=) >=]
        [(eq? op '<)  <]
        [(eq? op '<=) <=]))

(define (var? t)
  (symbol? t))

(define (eval-arith e m)
  (cond [(var? e) (get-mem e m)]
        [(op? e)
         (apply
          (op->proc (op-op e))
          (map (lambda (x) (eval-arith x m))
               (op-args e)))]
        [(const? e) e]))

;;

(define (assign? t)
  (and (list? t)
       (= (length t) 3)
       (eq? (second t) ':=)))

(define (assign-var e)
  (first e))

(define (assign-expr e)
  (third e))

(define (if? t)
  (tagged-tuple? 'if 4 t))

(define (if-cond e)
  (second e))

(define (if-then e)
  (third e))

(define (if-else e)
  (fourth e))

(define (while? t)
  (tagged-tuple? 'while 3 t))

(define (while-cond t)
  (second t))

(define (while-expr t)
  (third t))

(define (block? t)
  (list? t))

;;

(define (eval e m)
  (cond [(assign? e)
         (set-mem
          (assign-var e)
          (eval-arith (assign-expr e) m)
          m)]
        [(if? e)
         (if (eval-arith (if-cond e) m)
             (eval (if-then e) m)
             (eval (if-else e) m))]
        [(while? e)
         (if (eval-arith (while-cond e) m)
             (eval e (eval (while-expr e) m))
             m)]
        [(block? e)
         (if (null? e)
             m
             (eval (cdr e) (eval (car e) m)))]
        [(++? e)
         (set-mem (++var e)
                  (eval-arith '(+ (++var e) 1) m)
                  m)]
        [(for? e)
         (eval-for e (eval (for-init e) m))]))

(define (run e)
  (eval e empty-mem))

;;

(define fact10
  '( (i := 10)
     (r := 1)
     (while (> i 0)
       ( (r := (* i r))
         (i := (- i 1)) ))))

(define (computeFact10)
  (run fact10))


;;zadanie 3

;;fibonacci
(define (fib)
  (eval '( (i := 2)
           (n1 := 1)
           (n2 := 2)
           (n := 5) ;;zmienna
           (if (<= n 1)
               (if (= n 0)
                   (n1 := 0)
                   (n1 := 1))
               (while (<= i n)
                      ( (temp := n1)
                        (n1 := (+ n1 n2))
                        (n2 := temp)
                        (i := (+ i 1)))))) '()))

;;l. pierwsze

(define (primes)
  (eval '( (n := 5) ;;zmienna
           (sum := 0)
           (i := 2)
           (while (<= i n)
                  ( (j := 2)
                    (flay := 1)
                    (while (< j i)
                           ( (temp := i)
                             (while (> temp 0)
                                    ( (temp := (- temp j))))
                             (if (= temp 0)
                                 (flag := 0)
                                 ())
                             (j := (+ j 1))
                             (if (= flag 1)
                                 (sum := (+ sum i))
                                 ())
                             (i := (+ 1 i))))))) '()))

;;zadanie 4

(define (find-vars prog)
  (define (fr l p)
    (cond [(assign? p)
           (if (member (assign-var p) l)
               l
               (cons (assign-var p) l))]
          [(if? p)
           (fr (fr l (if-then p)) (if-else p))]
          ;;pozostałe przypadki, while, for itd.
          ))
    (fr '() prog))

;;zadanie 5

(define (++? t)
  (tagged-tuple? '++ 2 t))

(define (++var t)
  (second t))

;;reszta w eval

;;zadanie 6

(define (for? t)
  (and (tagged-tuple? 'for 3 t)
       (list? (second t))
       (= 3 (length (second t)))))

(define for-init (compose first second))
(define for-cond (compose second second))
(define for-step (compose third second))
(define for-expr third)

(define (eval-for e m)
  (if (eval-arith (for-cond e) m)
      (let* ((m-expr (eval (for-expr e) m))
             (m-step (eval (for-step e) m-expr)))
        (eval-for e m-step))
      m))

;;reszta w eval (dodanie przypadku (for? e)

;;zadanie 7

;;bardzo proste, wymienianie forów na while

;;zadanie 8

;;(define (eval-expr e m)
  ;;(cond [(or (assign? e)
;;(increment? e)
;;(label? e)) m]
;;[(for? e)                                        ;;jak wyżej
;; #t]                                             ;; w ogóle źle
;;[(goto? e)
;;(set-int '(looking-for-label (goto-name e) m))]))

;;trochę to nie działa, idea jest taka, żeby przejsć po całym programie, znaleźć etykietę i zacząć wykonywać program od niej