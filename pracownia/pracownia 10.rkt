#lang racket

(require "calc.rkt")

(define (def-name p)
  (car p))

(define (def-prods p)
  (cdr p))

(define (rule-name r)
  (car r))

(define (rule-body r)
  (cdr r))

(define (lookup-def g nt)
  (cond [(null? g) (error "unknown non-terminal" g)]
        [(eq? (def-name (car g)) nt) (def-prods (car g))]
        [else (lookup-def (cdr g) nt)]))

(define parse-error 'PARSEERROR)

(define (parse-error? r) (eq? r 'PARSEERROR))

(define (res v r)
  (cons v r))

(define (res-val r)
  (car r))

(define (res-input r)
  (cdr r))

;;

(define (token? e)
  (and (list? e)
       (> (length e) 0)
       (eq? (car e) 'token)))

(define (token-args e)
  (cdr e))

(define (nt? e)
  (symbol? e))

;;

(define (parse g e i)
  (cond [(token? e) (match-token (token-args e) i)]
        [(nt? e) (parse-nt g (lookup-def g e) i)]))

(define (parse-nt g ps i)
  (if (null? ps)
      parse-error
      (let ([r (parse-many g (rule-body (car ps)) i)])
        (if (parse-error? r)
            (parse-nt g (cdr ps) i)
            (res (cons (rule-name (car ps)) (res-val r))
                 (res-input r))))))

(define (parse-many g es i)
  (if (null? es)
      (res null i)
      (let ([r (parse g (car es) i)])
        (if (parse-error? r)
            parse-error
            (let ([rs (parse-many g (cdr es) (res-input r))])
              (if (parse-error? rs)
                  parse-error
                  (res (cons (res-val r) (res-val rs))
                       (res-input rs))))))))

(define (match-token xs i)
  (if (and (not (null? i))
           (member (car i) xs))
      (res (car i) (cdr i))
      parse-error))

;;

(define num-grammar
  '([digit {DIG (token #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)}]
    [numb {MANY digit numb}
          {SINGLE digit}]))

(define (node-name t)
  (car t))

(define (c->int c)
  (- (char->integer c) (char->integer #\0)))

(define (walk-tree-acc t acc expon)
  (cond [(eq? (node-name t) 'MANY)
         (walk-tree-acc
          (third t)
          (+ acc (* expon (c->int (second (second t)))))
          (* 10 expon))]
        [(eq? (node-name t) 'SINGLE)
         (+ acc (* expon (c->int (second (second t)))))]))

(define (walk-tree t)
  (walk-tree-acc t 0 1))

;;

(define arith-grammar
  (append num-grammar
     '([add-expr {ADD-MANY   sub-expr (token #\+) add-expr}
                 {ADD-SINGLE sub-expr}]
       [sub-expr {SUB-TWO mult-expr (token #\-) add-expr}
                  {SUB-SINGLE mult-expr}]
       [mult-expr {MULT-MANY div-expr (token #\*) mult-expr}
                  {MULT-SINGLE div-expr}]
       [div-expr {DIV-TWO base-expr (token #\/) mult-expr}
                  {DIV-SINGLE base-expr}]
       [base-expr {BASE-NUM numb}
                  {PARENS (token #\)) add-expr (token #\()}])))

(define (arith-walk-tree t)
  (cond [(eq? (node-name t) 'ADD-SINGLE)
         (arith-walk-tree (second t))]
        [(eq? (node-name t) 'MULT-SINGLE)
         (arith-walk-tree (second t))]
        [(eq? (node-name t) 'SUB-SINGLE)
         (arith-walk-tree (second t))]
        [(eq? (node-name t) 'DIV-SINGLE)
         (arith-walk-tree (second t))]
        [(eq? (node-name t) 'ADD-MANY)
         (binop-cons
          '+
          (arith-walk-tree (second t))
          (arith-walk-tree (fourth t)))]
        [(eq? (node-name t) 'MULT-MANY)
         (binop-cons
          '*
          (arith-walk-tree (second t))
          (arith-walk-tree (fourth t)))]
        [(eq? (node-name t) 'SUB-TWO)
         (binop-cons
          '-
          (arith-walk-tree (fourth t))
          (arith-walk-tree (second t)))]
        [(eq? (node-name t) 'DIV-TWO)
         (binop-cons
          '/
          (arith-walk-tree (fourth t))
          (arith-walk-tree (second t)))]
        [(eq? (node-name t) 'BASE-NUM)
         (walk-tree (second t))]
        [(eq? (node-name t) 'PARENS)
         (arith-walk-tree (third t))]))

(define (calc s)
 (eval
  (arith-walk-tree
   (car
    (parse
       arith-grammar
       'add-expr
       (reverse (string->list s)))))))

(define (tests)
  (and
   (= (calc "5-2-1") (calc "(5-2)-1"))
   (= (calc "5-2-1") 2)
   (= (calc "10-4-3-2-1") 0)
   (= (calc "1+10/5*2") 5)
   (= (calc "100/(25*2)") 2)
   (= (calc "((10-5)+(14/7))") 7)
   (= (calc "1+2*3*4*5/6") 21)
   (= (calc "12*15/120*2") 3)
   (= (calc "10-8/4-3") 5)
   (= (calc "(1+6)/7") 1)
   (= (calc "21/7+144/24+81/27") 12)
   (= (calc "100-1-2-3-4-5-10-15-20") 40)))

(tests)