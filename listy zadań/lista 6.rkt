#lang racket

(define (const? t)
  (number? t))

(define (binop? t)
  (and (list? t)
       (= (length t) 3)
       (member (car t) '(+ - * /))))

(define (binop-op e)
  (car e))

(define (binop-left e)
  (cadr e))

(define (binop-right e)
  (caddr e))

(define (binop-cons op l r)
  (list op l r))

(define (arith-expr? t)
  (or (const? t)
      (and (binop? t)
           (arith-expr? (binop-left  t))
           (arith-expr? (binop-right t)))))

;; calculator

(define (op->proc op)
  (cond [(eq? op '+) +]
        [(eq? op '*) *]
        [(eq? op '-) -]
        [(eq? op '/) /]))

(define (eval-arith e)
  (cond [(const? e) e]
        [(binop? e)
         ((op->proc (binop-op e))
            (eval-arith (binop-left  e))
            (eval-arith (binop-right e)))]))

;;let expressions

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

(define (var? t)
  (symbol? t))

(define (var-var e)
  e)

(define (var-cons x)
  x)

(define (arith/let-expr? t)
  (or (const? t)
      (and (binop? t)
           (arith/let-expr? (binop-left  t))
           (arith/let-expr? (binop-right t)))
      (and (let? t)
           (arith/let-expr? (let-expr t))
           (arith/let-expr? (let-def (let-def-expr t))))
      (var? t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;zadanie 1

(define (arith->vpn expr)
  (define (rek expr list)
    (if (const? expr)
        (cons expr list)
        (rek (binop-right expr)
             (rek (binop-left expr))
             (cons (binop-op expr) list))))
  (rek expr '()))

;;zadanie 2

(define (stack? st)
  (list? st))

(define (push x st)
  (cons x st))

(define (pop st)
  (cdr st))

;;zadanie 3

(define (eval-vpn l)
  (define (calc t stos)
    (cond [(null? t) (car stos)]
          [(const? (car t))
           (calc (cdr t) (push (car t) stos))]
          [(symbol? (car t))
           (calc (cdr t)
                 (push (eval-arith (list (car t)
                                         (car stos)
                                         (cadr stos)
                                         (cddr stos)))))]))
  (calc l '()))

;;zadanie 4
;;porozmawiajmy

;;zadanie 5

(define (if-zero? e)
  (and (list? e)
       (= (length e) 4)
       (eq? (car e) 'if-zero)
       (arith/let-expr? (cadr e))
       (arith/let-expr? (caddr e))
       (arith/let-expr? (cadddr e))))

(define (if0-if e)
  (cadr e))

(define (if0-if0 e)
  (caddr e))

(define (if0-if1 e)
  (cadddr e))

;;z wykładu

(define empty-env
  null)

(define (add-to-env x v env)
  (cons (list x v) env))

(define (find-in-env x env)
  (cond [(null? env) (error "undefined variable" x)]
        [(eq? x (caar env)) (cadar env)]
        [else (find-in-env x (cdr env))]))

(define (env-for-let def env)
  (add-to-env
    (let-def-var def)
    (eval-env (let-def-expr def) env)
    env))

;;koniec z wykładu

(define (eval-env e env)
  (cond [(const? e) e]
        [(binop? e)
         ((op->proc (binop-op e))
            (eval-env (binop-left  e) env)
            (eval-env (binop-right e) env))]
        [(let? e)
         (eval-env
           (let-expr e)
           (env-for-let (let-def e) env))]
        [(var? e) (find-in-env (var-var e) env)]
        [(if-zero? e)
         (cond [(= (eval (if0-if e)) 0)
                (eval (if0-if0 e))]
               [else (eval (if0-if1 e))])]))

;;zadanie 6

(define (arith-cons2 op . a)
  (cons op a))

(define (arith-op2 e)
  (car e))

(define (arith-arguments e)
  (cdr e))

(define (arith? t)
  (and (list? t)
       (> (length t) 2)
       (member (car t) '(+ - / *))))

(define (eval-arith2 t)
  (cond [(const? t) t]
        [(arith? t)
         (apply (op->proc (arith-op2 t))
                (map eval-arith2
                 (arith-arguments t)))]))

;;zadanie 7

(define (lets? e)
  (and (list? e)
       (= (length e) 3)
       (eq? (car e) 'let)
       (andmap let-def? (cadr e))))

(define (let-defs e)
  (cadr e))

(define (eval-lets e env)
  (cond [(const? e) e]
        [(binop? e)
         ((op->proc (binop-op e))
            (eval-env (binop-left  e) env)
            (eval-env (binop-right e) env))]
        [(lets? e)
         (eval-lets (let-expr e)
                    (foldl (lambda (def env)
                             (env-for-let def env))
                           env
                           (let-defs e)))]
        [(var? e) (find-in-env (var-var e) env)]
        [(if-zero? e)
         (cond [(= (eval (if0-if e)) 0)
                (eval (if0-if0 e))]
               [else (eval (if0-if1 e))])]))

;;zadanie 8

(define (let-exp->db exp)
  (define (rec e l)
    (cond [(const? e) e]
          [(binop? e)
           (list (binop-op e)
                 (rec (binop-left e) l)
                 (rec (binop-right e) l))]
          [(let? e) (list 'let
                          (rec (let-def-expr (let-def e))
                            (cons (let-def-var (let-def e)) l))
                          (rec (let-expr e)
                            (cons (let-def-var (let-def e)) l)))]
          [(var? e) (find-in-l e l)]))
  (rec exp '()))

(define (find-in-l e l)
  (define (rec lista licznik)
    (cond [(null? lista) (error "undefined" e)]
          [(eq? (car lista) e) (list 'index licznik)]
          [else (rec (cdr lista) (+ 1 licznik))]))
          (rec l 0))

;;zadanie 9

;;podobnie jak w drugą stronę, uznali to za trywialne
(define (let-db-def e)
  (1))
(define (let-db-expr e)
  (1))
(define (db-index? e)
  (1))
(define (index e)
  (1))
(define (find e)
  (1))
;;odtąd znowu poprawny kod

(define (number->symbol i)
   (string->symbol (string-append "x" (number->string i))))

(define (db->let exp)
  (define (rec e licznik l)
    (cond [(const? e) e]
          [(binop? e)
           (list (binop-op e)
                 (rec (binop-left e) licznik l)
                 (rec (binop-right e) licznik l))]
          [(let? e) (list 'let
                          (list (number->symbol licznik)
                                (rec (let-db-def e)
                                  licznik
                                  l))
                          (rec (let-db-expr e)
                            (+ licznik 1)
                            (cons (number->symbol licznik) l)))]
          [(db-index? e) (find-in-l-index (index e) l)]))
  (rec exp '()))

(define (find-in-l-index i l)
  (if (null? l)
      (error)
      (if (= 0 i)
          (car l)
          (find-in-l-index (- i 1) (cdr l)))))