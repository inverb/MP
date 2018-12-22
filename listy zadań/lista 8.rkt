#lang racket

;;zadanie 1
(define (llist x1 x2 f)
  (list x1 x2 f))

(define (lhead e)
  (car e))

(define (lsecond e)
  (cadr e))

(define (ltail e)
  ((caddr e)))

(define (next-fib e1 e2)
  (+ e1 e2))

(define (fibs-from e1 e2)
  (llist
   e1
   e2
   (lambda () (fibs-from e2 (next-fib e1 e2)))))

(define fibs
  (fibs-from 0 1))

(define (intcons x f)
  (cons x f))

(define (inthead l)
  (car l))

(define (inttail l)
  ((cdr l)))

(define (ints-from e)
  (intcons
   e
   (if (< e 0)
       (lambda () (ints-from (- 0 e)))
       (lambda () (ints-from (- 0 (+ e 1)))))))

(define ints
  (ints-from 0))

;;zadanie 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;kod interpretera z wykładu;;;;;;;;;;;;;;;;

;; pomocnicza funkcja dla list tagowanych o określonej długości

(define (tagged-tuple? tag len p)
  (and (list? p)
       (= (length p) len)
       (eq? (car p) tag)))

(define (tagged-list? tag p)
  (and (pair? p)
       (eq? (car p) tag)
       (list? (cdr p))))

;; self-evaluating expressions

(define (const? t)
  (or (number? t)
      (eq? t 'true)
      (eq? t 'false)
      (eq? t 'null)))

;; arithmetic expressions
  
(define (op? t)
  (and (list? t)
       (member (car t) '(+ - * / = > >= < <=))))

(define (op-op e)
  (car e))

(define (op-args e)
  (cdr e))

(define (op-cons op args)
  (cons op args))

(define (op->proc op)
  (cond [(eq? op '+) +]
        [(eq? op '*) *]
        [(eq? op '-) -]
        [(eq? op '/) /]
        [(eq? op '=)  (compose bool->val =)]
        [(eq? op '>)  (compose bool->val >)]
        [(eq? op '>=) (compose bool->val >=)]
        [(eq? op '<)  (compose bool->val <)]
        [(eq? op '<=) (compose bool->val <=)]))

;; lets

(define (let-def? t)
  (and (list? t)
       (= (length t) 2)
       (symbol? (car t))))

(define (let-def-var e)
  (car e))

(define (let-def-expr e)
  (cadr e))

(define (let-def-cons x e)
  (list x e))

(define (let? t)
  (and (tagged-tuple? 'let 3 t)
       (let-def? (cadr t))))

(define (let-def e)
  (cadr e))

(define (let-expr e)
  (caddr e))

(define (let-cons def e)
  (list 'let def e))

;; variables

(define (var? t)
  (symbol? t))

(define (var-var e)
  e)

(define (var-cons x)
  x)

;; pairs

(define (cons? t)
  (tagged-tuple? 'cons 3 t))

(define (cons-fst e)
  (second e))

(define (cons-snd e)
  (third e))

(define (cons-cons e1 e2)
  (list 'cons e1 e2))

(define (car? t)
  (tagged-tuple? 'car 2 t))

(define (car-expr e)
  (second e))

(define (cdr? t)
  (tagged-tuple? 'cdr 2 t))

(define (cdr-expr e)
  (second e))

(define (pair?? t)
  (tagged-tuple? 'pair? 2 t))

(define (pair?-expr e)
  (second e))

(define (pair?-cons e)
  (list 'pair? e))


;; if

(define (if? t)
  (tagged-tuple? 'if 4 t))

(define (if-cons b t f)
  (list 'let b t f))

(define (if-cond e)
  (second e))

(define (if-then e)
  (third e))

(define (if-else e)
  (fourth e))

;; cond

(define (cond-clause? t)
  (and (list? t)
       (= (length t) 2)))

(define (cond-clause-cond c)
  (first c))

(define (cond-clause-expr c)
  (second c))

(define (cond-claue-cons b e)
  (list b e))

(define (cond? t)
  (and (tagged-list? 'cond t)
       (andmap cond-clause? (cdr t))))

(define (cond-clauses e)
  (cdr e))

(define (cond-cons cs)
  (cons 'cond cs))

;; lists

(define (null?? t)
  (tagged-tuple? 'null? 2 t))

(define (null?-expr e)
  (cdr e))

(define (null?-cons e)
  (list 'null? e))

;; lambdas

(define (lambda? t)
  (and (tagged-tuple? 'lambda 3 t)
       (list? (cadr t))
       (andmap symbol? (cadr t))))

(define (lambda-cons vars e)
  (list 'lambda vars e))

(define (lambda-vars e)
  (cadr e))

(define (lambda-expr e)
  (caddr e))

;; lambda-rec

(define (lambda-rec? t)
  (and (tagged-tuple? 'lambda-rec 3 t)
       (list? (cadr t))
       (>= (length (cadr t)) 1)
       (andmap symbol? (cadr t))))

(define (lambda-rec-cons vars e)
  (list 'lambda-rec vars e))

(define (lambda-rec-expr e)
  (third e))

(define (lambda-rec-name e)
  (car (second e)))

(define (lambda-rec-vars e)
  (cdr (second e)))

;; applications

(define (app? t)
  (and (list? t)
       (> (length t) 0)))

(define (app-cons proc args)
  (cons proc args))

(define (app-proc e)
  (car e))

(define (app-args e)
  (cdr e))

;; expressions

(define (expr? t)
  (or (const? t)
      (and (op? t)
           (andmap expr? (op-args t)))
      (and (let? t)
           (expr? (let-expr t))
           (expr? (let-def-expr (let-def t))))
      (and (cons? t)
           (expr? (cons-fst t))
           (expr? (cons-snd t)))
      (and (car? t)
           (expr? (car-expr t)))
      (and (cdr? t)
           (expr? (cdr-expr t)))
      (and (pair?? t)
           (expr? (pair?-expr t)))
      (and (null?? t)
           (expr? (null?-expr t)))
      (and (if? t)
           (expr? (if-cond t))
           (expr? (if-then t))
           (expr? (if-else t)))
      (and (cond? t)
           (andmap (lambda (c)
                     (and (expr? (cond-clause-cond c))
                          (expr? (cond-clause-expr c))))
                   (cond-clauses t)))
      (and (lambda? t)
           (expr? (lambda-expr t)))
      (and (lambda-rec? t)
           (expr? (lambda-rec-expr t)))
      (var? t)
      (and (app? t)
           (expr? (app-proc t))
           (andmap expr? (app-args t)))
      (and (cxxxr? t)
           (expr? (cxxxr-expr t)))))

;; environments

(define empty-env
  null)

(define (add-to-env x v env)
  (cons (list x v) env))

(define (find-in-env x env)
  (cond [(null? env) (error "undefined variable" x)]
        [(eq? x (caar env)) (cadar env)]
        [else (find-in-env x (cdr env))]))

;; closure

(define (closure-cons xs expr env)
  (list 'closure xs expr env))

(define (closure? c)
  (and (list? c)
       (= (length c) 4)
       (eq? (car c) 'closure)))

(define (closure-vars c)
  (cadr c))

(define (closure-expr c)
  (caddr c))

(define (closure-env c)
  (cadddr c))

;; closure-rec

(define (closure-rec? t)
  (tagged-tuple? 'closure-rec 5 t))

(define (closure-rec-name e)
  (second e))

(define (closure-rec-vars e)
  (third e))

(define (closure-rec-expr e)
  (fourth e))

(define (closure-rec-env e)
  (fifth e))

(define (closure-rec-cons f xs e env)
  (list 'closure-rec f xs e env))

;; evaluator

(define (bool->val b)
  (if b 'true 'false))

(define (val->bool s)
  (cond [(eq? s 'true)  true]
        [(eq? s 'false) false]
        [else (error "could not convert symbol to bool")]))

(define (eval-env e env)
  (cond [(const? e)
         e]
        [(op? e)
         (apply (op->proc (op-op e))
                (map (lambda (a) (eval-env a env))
                     (op-args e)))]
        [(let? e)
         (eval-env (let-expr e)
                   (env-for-let (let-def e) env))]
        [(var? e)
         (find-in-env (var-var e) env)]
        [(cons? e)
         (cons (eval-env (cons-fst e) env)
               (eval-env (cons-snd e) env))]
        [(car? e)
         (car (eval-env (car-expr e) env))]
        [(cdr? e)
         (cdr (eval-env (cdr-expr e) env))]
        [(pair?? e)
         (bool->val (pair? (eval-env (pair?-expr e) env)))]
        [(null?? e)
         (bool->val (null? (eval-env (null?-expr e) env)))]
        [(if? e)
         (if (val->bool (eval-env (if-cond e) env))
             (eval-env (if-then e) env)
             (eval-env (if-else e) env))]
        [(cond? e)
         (eval-cond-clauses (cond-clauses e) env)]
        [(lambda? e)
         (closure-cons (lambda-vars e) (lambda-expr e) env)]
        [(lambda-rec? e)
         (closure-rec-cons (lambda-rec-name e)
                           (lambda-rec-vars e)
                           (lambda-rec-expr e)
                           env)]
        [(app? e)
         (apply-closure
          (eval-env (app-proc e) env)
          (map (lambda (a) (eval-env a env))
               (app-args e)))]
        [(cxxxr? e)
         (apply-cxxxr (cxxxr-s e) (eval-env (cxxxr-expr e) env))]))

(define (eval-cond-clauses cs env)
  (if (null? cs)
      (error "no true clause in cond")
      (let ([cond (cond-clause-cond (car cs))]
            [expr (cond-clause-expr (car cs))])
        (if (val->bool (eval-env cond env))
            (eval-env expr env)
            (eval-cond-clauses (cdr cs) env)))))

(define (apply-closure c args)
  (cond [(closure? c)
         (eval-env
          (closure-expr c)
          (env-for-closure
           (closure-vars c)
           args
           (closure-env c)))]
        [(closure-rec? c)
         (eval-env
          (closure-rec-expr c)
          (add-to-env
           (closure-rec-name c)
           c
           (env-for-closure
            (closure-rec-vars c)
            args
            (closure-rec-env c))))]))

(define (env-for-closure xs vs env)
  (cond [(and (null? xs) (null? vs)) env]
        [(and (not (null? xs)) (not (null? vs)))
         (add-to-env
          (car xs)
          (car vs)
          (env-for-closure (cdr xs) (cdr vs) env))]
        [else (error "arity mismatch")]))

(define (env-for-let def env)
  (add-to-env
   (let-def-var def)
   (eval-env (let-def-expr def) env)
   env))

(define (eval e)
  (eval-env e empty-env))

;;;;;;;;;;;;;;;;koniec kodu z wykładu;;;;;;;;;;;;;;;;;;;;;;;;;;

;;funkcje w naszym metajęzyku

((lambda-rec (my-append xs ys)
    (if (null? xs)
        ys
        (cons (car xs) (my-append (cdr xs ys)))))
 (list 1 2)
 (list 3 4))

;;trzeba dodać implementację list, ale jest prosta

((lambda-rec (my-map proc xs)
    (if (null? xs)
        null
        (cons (proc (car xs)) (my-map proc (cdr xs))))) (lambda (x) (+ x 1)) (list 1 2 3))

((lambda-rec (my-rev xs ys)
             (if (null? xs)
                 ys
                 (my-rev (cdr xs) (cons (car xs) ys)))) (list 1 2 3) null)

;;dla leniwych list
(define (lazy-append xs ys)
  (lcons
   (lhead xs)
   (lambda () (lazy-append s (ltail xs)))))

(define (lmap f l)
  (lcons (f (head l))
         (lambda () (lazy-map f (ltail l)))))

;;zadanie 3
;;chyba jakieś proste bo Maciek robił xD

;;zadanie 4
(define (split-symbol s)
  (define (char->symbol a)
     (string->symbol (list->string (list a))))
  (map char->symbol (string->list (symbol->string s))))

(define (cxxxr-expr e) (cdr e))

(define (cxxxr? e)
  (and (list? e)
       (= 2 (length e))
       (symbol? (cxxxr-s e))
       (let ((b (split-symbol e)))
         (and (eq? 'c (car b))
              (xxxr? (cdr? b))))))

(define (xxxr? e)
  (if (eqal? 'r e)
      true
      (if (or (eq? 'a (car e))
              (eq? 'd (car e)))
          (xxxr? (cdr e))
          false)))
;;dalsza część dodana do expr? i eval-env
;;ale trzeba dodać apply-cxxxr
(define (apply-cxxxr s)
  (let ((b (split-symbol s)))
    (apply-xxxr (cdr b) l)))

(define (apply-xxxr b l)
  (if (eq? b (list 'r l))
      l
      (cond [(eq? 'a (car b))
             (car (apply-xxxr (cdr b) l))]
            [(eq? 'd (car b))
             (cdr (apply-xxxr (cdr b) l))])))

;;zadanie 5
