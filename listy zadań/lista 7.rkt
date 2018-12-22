#lang racket

;; arithmetic expressions

(define (op? t)
  (and (list? t)
       (member (car t) '(+ - * /))))

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
        [(eq? op '/) /]))

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
  (and (list? t)
       (= (length t) 3)
       (eq? (car t) 'let)
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
  (and (list? t)
       (= (length t) 3)
       (eq? (car t) 'cons)))

(define (cons-fst e)
  (second e))

(define (cons-snd e)
  (third e))

(define (cons-cons e1 e2)
  (list 'cons e1 e2))

(define (car? t)
  (and (list? t)
       (= (length t) 2)
       (eq? (car t) 'car)))

(define (car-expr e)
  (second e))

(define (cdr? t)
  (and (list? t)
       (= (length t) 2)
       (eq? (car t) 'cdr)))

(define (cdr-expr e)
  (second e))

;; lambdas

(define (lambda? t)
  (and (list? t)
       (= (length t) 3)
       (eq? (car t) 'lambda)
       (list? (cadr t))
       (andmap symbol? (cadr t))))

(define (lambda-vars e)
  (cadr e))

(define (lambda-expr e)
  (caddr e))

;; applications

(define (app? t)
  (and (list? t)
       (> (length t) 0)))

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
      (var? t)
      (and (cons? t)
           (expr? (cons-fst t))
           (expr? (cons-snd t)))
      (and (car? t)
           (expr? (car-expr t)))
      (and (cdr? t)
           (expr? (cdr-expr t)))
      (and (lambda? t)
           (expr? (lambda-expr t)))
      (and (app? t)
           (expr? (app-proc t))
           (andmap expr? (app-args t)))
      (nil? t)))

;; environments

(define empty-env
  null)

(define (add-to-env x v env)
  (cons (list x v) env))

(define (find-in-env x env)
  (cond [(null? env) (error "undefined variable" x)]
        [(eq? x (caar env)) (cadar env)]
        [else (find-in-env x (cdr env))]))

;; closures

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

;; evaluator

(define (eval-env e env)
  (cond [(const? e) e]
        [(op? e)
         (apply (op->proc (op-op e))
                (map (lambda (a) (eval-env a env))
                     (op-args e)))]
        [(let? e)
         (eval-env (let-expr e)
                   (env-for-let (let-def e) env))]
        [(var? e) (find-in-env (var-var e) env)]
        [(cons? e)
         (cons (eval-env (cons-fst e) env)
               (eval-env (cons-snd e) env))]
        [(car? e)
         (car (eval-env (car-expr e) env))]
        [(cdr? e)
         (cdr (eval-env (cdr-expr e) env))]
        [(lambda? e)
         (closure-cons (lambda-vars e) (lambda-expr e) env)]
        [(app? e)
         (apply-closure
           (eval-env (app-proc e) env)
           (map (lambda (a) (eval-env a env))
                (app-args e)))]
        [(nil? e) nil-cons]
        [(if? e)
         (if (eval-env (if-cond e) env)
             (eval-env (if-true e) env)
             (eval-env (if-false e) env))]
        [(and? (eval-and (and-cases e) env))]))

(define (apply-closure c args)
  (eval-env (closure-expr c)
            (env-for-closure
              (closure-vars c)
              args
              (closure-env c))))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Lista 7

;;zadanie 3
(define nil-cons 'null)

(define (nil? a)
  (eq? a' null))

;;zadanie 4
(define (const? t)
  (or (number? t)
      (boolean? t)))

(define (if? t)
  (and (list? t)
       (= 4 (length t))
       (eq? 'if (car t))))

(define if-cond second)
(define if-true third)
(define if-false fourth)

(define (cond? t)
  (and (list? t)
       (> (length t) 1)
       (eq? 'cond (car t))
       (andmap cond-case? (cdr t))))

(define (cond-case? t)
  (and (list? t)
       (= 2 (length t))))

(define cond-cases cdr)
(define and-cases cdr)

;;trzeba to ewentualnie napisać
(define (and? e)
  #t)

(define (eval-and cs env)
  (cond [(null? cs) #t]
        [(null? (cdr cs)) (eval-env (car cs) env)]
        [else (and (eval-env (car cs) env)
                   (eval-and (cdr cs) env))]))