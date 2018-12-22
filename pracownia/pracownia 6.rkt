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

(define (hole? t)
  (eq? t 'hole))

(define (arith/let/holes? t)
  (or (hole? t)
      (const? t)
      (and (binop? t)
           (arith/let/holes? (binop-left  t))
           (arith/let/holes? (binop-right t)))
      (and (let? t)
           (arith/let/holes? (let-expr t))
           (arith/let/holes? (let-def-expr (let-def t))))
      (var? t)))

(define (num-of-holes t)
  (cond [(hole? t) 1]
        [(const? t) 0]
        [(binop? t)
         (+ (num-of-holes (binop-left  t))
            (num-of-holes (binop-right t)))]
        [(let? t)
         (+ (num-of-holes (let-expr t))
            (num-of-holes (let-def-expr (let-def t))))]
        [(var? t) 0]))

(define (arith/let/hole-expr? t)
  (and (arith/let/holes? t)
       (= (num-of-holes t) 1)))

(define (hole-context e)
  (define (find-variables expr vars)
    (cond [(hole? expr) vars]
          [(const? expr) vars]
          [(var? expr) vars]
          [(binop? expr)
           (if (arith/let/hole-expr? (binop-left expr))
               (find-variables (binop-left expr) vars)
               (find-variables (binop-right expr) vars))]
          [(let? expr)
           (if (arith/let/hole-expr? (let-def-expr (let-def expr)))
               (find-variables (let-def-expr (let-def expr)) vars)
               (find-variables (let-expr expr)
                               (remove-duplicates (append (list (let-def-var (let-def expr))) vars))))]
          [else (error "Not an arith/let/hole expression")]))
  (find-variables e '()))

(define (test)
  ;;procedury pomocnicze do sprawdzania rozwiązań
  (define (check answer solution)
    (cond [(null? answer)
           (if (null? solution)
               #t
               #f)]
          [(null? solution) #f]
          [(false? (memv (car answer) solution)) #f]
          [else (check (cdr answer) (remove (car answer) solution))]))
  (define (find-and-check expr solution)
    (let ((answer (hole-context expr)))
      (check answer solution)))
  ;;TESTY
  (and
   (find-and-check 'a '())
   (find-and-check 997666213769 '())
   (find-and-check '(/ (+ 5 (- 10 2)) (+ (- 6 6) (* 2 (/ 1 1)))) '())
   (find-and-check '(let (x hole) (+ x 3)) '())
   (find-and-check '(let (x (+ 3 4)) (/ 2 hole)) '(x))
   (find-and-check '(let (x
                          (let (y 5)
                            (let (z 6) (- z y)))) (+ x 1)) '(x))
   (find-and-check '(let (Wesoły 1)
                      (let (nam 2)
                        (let (dzień 3)
                          (let (dziś 4)
                            (let (nastał 5) (+ Wesoły (+ nam (+ dzień (+ dziś (+ nastał hole)))))))))) '(Wesoły nam dzień dziś nastał))
   (find-and-check '(let (x 3) (+
                                (let (y 2) (let (z 1) (+ y z)))
                                (let (a x) (let (b 1) (- b hole))))) '(x a b))
   (find-and-check '(let (x 1) (let (y 2) (let (x 3) (let (y 4) (let (x 5) (+ x y)))))) '(x y))
   (find-and-check '(+ a (let (x 1) (- y x))) '(x))
   (find-and-check '(+ 3 (let (x 2) (+ hole x))) '(x))
  ;;testy z treści zadania
   (find-and-check '(+ 3 hole) '())
   (find-and-check '(let (x 3) (let (y 7) (+ x hole))) '(y x))
   (find-and-check '(let (x 3) (let (y hole) (+ x 3))) '(x))
   (find-and-check '(let (x hole) (let (y 7) (+ x 3))) '())
   (find-and-check '(let (piesek 1)
                      (let (kotek 7)
                        (let (piesek 9)
                          (let (chomik 5) hole)))) '(chomik piesek kotek))
   (find-and-check '(+ (let (x 4) 5) hole) '())))