#lang racket

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

; arith and bool expressions: syntax and semantics

(define (const? t)
  (number? t))

(define (true? t)
  (eq? t 'true))

(define (false? t)
  (eq? t 'false))

(define (op? t)
  (and (list? t)
       (member (car t) '(+ - * / = > >= < <= not and or mod floor))))

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
        [(eq? op '<=) <=]
        [(eq? op 'not) not]
        [(eq? op 'and) (lambda x (andmap identity x))]
        [(eq? op 'or) (lambda x (ormap identity x))]
        [(eq? op 'mod) modulo]
        [(eq? op 'floor) floor]))

(define (var? t)
  (symbol? t))

;; 3 added procedures
(define (rand? t)
  (tagged-tuple? 'rand 2 t))

(define (rand-expr t)
  (second t))

(define (eval-args xs m seed)
  (if (null? xs)
      (res null seed)
      (let* ((new-arg (eval-arith (car xs) m seed))
             (rest-of-args (eval-args (cdr xs) m (res-state new-arg))))
        (res (cons (res-val new-arg) (res-val rest-of-args))
             (res-state rest-of-args)))))

(define (eval-arith e m seed)
  (cond [(true? e) (res true seed)]
        [(false? e) (res false seed)]
        [(var? e) (res (get-mem e m) seed)]
        [(rand? e)
         (let ((rand-max (eval-arith (rand-expr e) m seed)))
           ((rand (res-val rand-max)) (res-state rand-max)))]
        [(op? e)
         (let ((list-of-args (eval-args (op-args e) m seed)))
           (res (apply
                 (op->proc (op-op e))
                 (res-val list-of-args))
                (res-state list-of-args)))]
        [(const? e) (res e seed)]))

;; syntax of commands

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

;; state

(define (res v s)
  (cons v s))

(define (res-val r)
  (car r))

(define (res-state r)
  (cdr r))

;; psedo-random generator

(define initial-seed
  123456789)

(define (rand max)
  (lambda (i)
    (let ([v (modulo (+ (* 1103515245 i) 12345) (expt 2 32))])
      (res (modulo v max) v))))

;; WHILE interpreter

;; I don't use st-app, instead I keep seed in variable 'seed
(define (eval e m seed)
  (define (evaluate e m)
    (cond [(assign? e)
           (let ((expr (eval-arith (assign-expr e) m (get-mem 'seed m))))
             (set-mem
              'seed
              (res-state expr)
              (set-mem
               (assign-var e)
               (res-val expr)
               m)))]
          [(if? e)
           (let* ((expr (eval-arith (if-cond e) m (get-mem 'seed m)))
                  (memory (set-mem 'seed (res-state expr) m)))
             (if (res-val expr)
               (evaluate (if-then e) memory)
               (evaluate (if-else e) memory)))]
          [(while? e)
           (let* ((expr (eval-arith (while-cond e) m (get-mem 'seed m)))
                  (memory (set-mem 'seed (res-state expr) m)))
             (if (res-val expr)
                 (evaluate e (evaluate (while-expr e) memory))
                 m))]
          [(block? e)
           (if (null? e)
               m
               (evaluate (cdr e) (evaluate (car e) m)))]))
  
  (let ((memory (set-mem 'seed seed m)))
    (evaluate e memory)))

(define (run e)
  (eval e empty-mem initial-seed))

;;

;;I added floor to my arithmetic evaluator, to make integer division possible.
(define fermat-test
  '( (composite := false)
     (while (> k 0)
            ( (a := (+ 2 (rand (- n 3))))
              (p := (- n 1))
              (w := 1)
              (while (> p 0)                       ;;fast exponantiation algorithm
                     ( (if (= 1 (mod p 2))
                           ( (w := (mod (* w a) n))
                             (a := (mod (* a a) n)))
                           (a := (mod (* a a) n)))
                       (p := (floor (/ p 2)))))
              (if (= w 1)
                  (k := (- k 1))
                  ( (composite := true)
                    (k := 0)))))))

(define (probably-prime? n k) ; check if a number n is prime using
                              ; k iterations of Fermat's primality
                              ; test
  (let ([memory (set-mem 'k k
                (set-mem 'n n empty-mem))])
    (not (get-mem
           'composite
           (eval fermat-test memory initial-seed)))))

(define (tests)
  ;;testing  for 1, 2 or 3 makes no sense,
  ;;because set 2,3,...,(n-2) that we have to draw from is empty for those numbers
  (and (eq? (probably-prime? 4 10) #f)
       (eq? (probably-prime? 5 20) #t)
       (eq? (probably-prime? 9 30) #f)
       (eq? (probably-prime? 17 40) #t)
       (eq? (probably-prime? 2048 50) #f)
       (eq? (probably-prime? 997 40) #t)
       (eq? (probably-prime? 21377 30) #t)
       (eq? (probably-prime? 10000000 20) #f)
       (eq? (probably-prime? 1234321 10) #f)))

(tests)