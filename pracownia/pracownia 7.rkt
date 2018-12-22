#lang racket

;; expressions

(define (const? t)
  (number? t))

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
      (and (op? t)
           (andmap arith/let-expr? (op-args t)))
      (and (let? t)
           (arith/let-expr? (let-expr t))
           (arith/let-expr? (let-def-expr (let-def t))))
      (var? t)))

;; let-lifted expressions

(define (arith-expr? t)
  (or (const? t)
      (and (op? t)
           (andmap arith-expr? (op-args t)))
      (var? t)))

(define (let-lifted-expr? t)
  (or (and (let? t)
           (let-lifted-expr? (let-expr t))
           (arith-expr? (let-def-expr (let-def t))))
      (arith-expr? t)))

;; generating a symbol using a counter

(define (number->symbol i)
  (string->symbol (string-append "x" (number->string i))))

;; environments (could be useful for something)

(define empty-env
  null)

(define (add-to-env x v env)
  (cons (list x v) env))

(define (find-in-env x env)
  (cond [(null? env) (error "undefined variable" x)]
        [(eq? x (caar env)) (cadar env)]
        [else (find-in-env x (cdr env))]))

;; the let-lift procedure

;;funkcje pomocnicze
(define (found-lets-expr found-lets)
  (car found-lets))

(define (found-lets-lets found-lets)
  (cadr found-lets))

(define (found-lets-counter found-lets)
  (caddr found-lets))

(define (found-lets-cons expr lets counter)
  (list expr lets counter))

;;specjalna procedura do znajdowania let-wyrażeń w
;;wyrażeniach arytmetycznych o dowolnie wielu argumentach
(define (lets-in-op expr env i)
  (if (null? expr)
      (list '() expr i)
      (let* ((found-lets (lets-in-op (cdr expr) env i))
            (new-let (find-lets (car expr) env (found-lets-counter found-lets))))
        (found-lets-cons (append (list (found-lets-expr new-let)) (found-lets-expr found-lets))
                         (append (found-lets-lets new-let) (found-lets-lets found-lets))
                         (found-lets-counter new-let)))))

;;znajduje wyrażenia typu let
(define (find-lets expr env i)
  (cond [(const? expr)
         (found-lets-cons expr '() i)]
        [(var? expr)
         (found-lets-cons (find-in-env expr env) '() i)]
        [(op? expr)
         (let ((found-lets (lets-in-op (cdr expr) env i)))
           (found-lets-cons (append (list (op-op expr)) (found-lets-expr found-lets))
                            (found-lets-lets found-lets)
                            (found-lets-counter found-lets)))]
        [(let? expr)
         (let* ((new-var (number->symbol i))
                (def-lets (find-lets (let-def-expr (let-def expr)) env (+ i 1)))
                (expr-lets (find-lets (let-expr expr)
                                      (add-to-env (let-def-var (let-def expr)) new-var env)
                                      (found-lets-counter def-lets))))
           (found-lets-cons (found-lets-expr expr-lets)
                            (append (found-lets-lets def-lets)
                                    (list (list new-var (found-lets-expr def-lets)))
                                    (found-lets-lets expr-lets))
                            (found-lets-counter expr-lets)))]))
           

(define (let-lift e)
  (define (construct-let-lift expr let-list)
    (if (null? let-list)
        expr
        (let-cons (car let-list)
                  (construct-let-lift expr (cdr let-list)))))
  
  (let ((let-form (find-lets e empty-env 0)))
    (construct-let-lift (found-lets-expr let-form)
                        (found-lets-lets let-form))))

(define (tests)
  ;;dodatkowa procedura dla wyrażeń arytmetycznych
  (define (vars-in-order-op? expr env)
    (if (null? expr)
        #t
        (and (vars-in-order? (car expr) env)
             (vars-in-order-op? (cdr expr) env))))
  ;;procedura sprawdzająca, czy zmienne zostały wcześniej zadeklarowane
  (define (vars-in-order? expr env)
    (cond [(const? expr) #t]
          [(var? expr) (if (false? (find-in-env expr env))
                           #f
                           #t)]
          [(op? expr) (vars-in-order-op? (op-args expr) env)]
          ;;w przypadku, gdy mam let-wyrażenie musze dodatkowo sprawdzić, czy deklarowana zmienna nie była zadeklarowana
          ;;już gdzieś wcześniej
          [(let? expr) (and (null? (filter (lambda (x) (eq? (car x) (let-def-var (let-def expr)))) env))
                            (vars-in-order? (let-def-expr (let-def expr)) env)
                            (vars-in-order? (let-expr expr) (add-to-env (let-def-var (let-def expr)) #t env)))]))
  ;;procedura testująca
  (define (test expr)
    (let ((let-lifted (let-lift expr)))
      (and (let-lifted-expr? let-lifted)
           (vars-in-order? let-lifted empty-env))))
  (and
   (test '(let (x (+ (let (y 3) y)(let (z (let (a 5) (+ a 2))) (- z 1)))) (+ x 1)))
   (test '(+ 2 (let (x 2) (let (y 3) (- y x)))))
   (test '(let (x (+ 1 2)) (let (y (+ x 3)) (let (z (+ y 4 1)) (- z 10)))))
   (test '(+ (* 1 1) (- 2 (/ 3 3))))
   (test '(- 10 (let (x (+ 2 (let (y (let (v (let (z (* 3 5)) (/ z 5))) (- 4 v))) (* y 10)))) (- x 3))))
   (test '(let (x 1) (let (y (+ x 1)) (let (z (+ x y)) (let (v (+ x y z)) (+ x y z v))))))
   (test '(+ (let (a 1) a) (let (b 2) b)))
   (test '(let (x 100) (- 100 x)))
   (test '1)
   (test '(let (x 0) (/ 100 x)))
   (test '(let (x (let (y 2) (+ 2 y))) (let (a x) (- a 1))))
   (test '(+ (let (x 2) (+ x 1)) (let (x 3) (- x 1))))
   (test '(- (let (a (let (b 6) (/ b 2))) (let (c 1) (+ c 1))) (let (a 1) (let (b 2) (let (c 3) (- c (+ b a)))))))))

(tests)