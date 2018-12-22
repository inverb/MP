#lang racket

(define (var? t)
  (symbol? t))

(define (neg? t)
  (and (list? t)
       (= 2 (length t))
       (eq? 'neg (car t))))

(define (conj? t)
  (and (list? t)
       (= 3 (length t))
       (eq? 'conj (car t))))

(define (disj? t)
  (and (list? t)
       (= 3 (length t))
       (eq? 'disj (car t))))

(define (prop? f)
  (or (var? f)
      (and (neg? f)
           (prop? (neg-subf f)))
      (and (disj? f)
           (prop? (disj-left f))
           (prop? (disj-rght f)))
      (and (conj? f)
           (prop? (conj-left f))
           (prop? (conj-rght f)))))

;;zadanie 1

;;konstruktory
(define (neg arg)
  (list 'neg arg))

(define (conj arg1 arg2)
  (list 'conj arg1 arg2))

(define (disj arg1 arg2)
  (list 'disj arg1 arg2))

;;selektory

(define (neg-subf f) (cadr f))
(define (disj-left f) (cadr f))
(define (disj-rght f) (caddr f))
(define (conj-left f) (cadr f))
(define (conj-rght f) (caddr f))

;;zadanie 2

(define (free-vars f)
  (define (loop f)
        (cond [(var? f) (list f)]
              [(lit? f) (list (lit-arg f))]
              [(neg? f) (loop (neg-subf f))]
              [(disj? f) (append (loop (disj-left f)) (loop (disj-rght f)))]
              [(conj? f) (append (loop (conj-left f)) (loop (conj-rght f)))]))
  (if (not (prop? f))
      (error "Not a formula!")
      (remove-duplicates (loop f))))

;;(free-vars (conj (disj 'p (neg 'q)) 'q))

;;zadanie 3

(define (gen-vals xs)
  (if (null? xs)
      (list null)
      (let*
          ((vss (gen-vals (cdr xs)))
           (x (car xs))
           (vst (map (lambda (vs) (cons (list x true) vs)) vss))
           (vsf (map (lambda (vs) (cons (list x false) vs)) vss)))
        (append vst vsf))))

(define (eval-formula xs vals)
  (define (val? x v)
    (if (null? v)
        (error "Zmienna nie występuje w formule")
        (if (eq? (caar v) x)
            (cadar v)
            (val? x (cdr v)))))
  (cond [(var? xs) (val? xs vals)]
        [(lit? xs) (if (= 0 (lit-neg xs))
                       (val? (lit-arg xs) vals)
                       (not (val? (lit-arg xs) vals)))]
        [(neg? xs) (not (eval-formula (neg-subf xs) vals))]
        [(disj? xs) (or (eval-formula (disj-left xs) vals)
                        (eval-formula (disj-rght xs) vals))]
        [(conj? xs) (and (eval-formula (conj-left xs) vals)
                         (eval-formula (conj-rght xs) vals))]))

(define (falsifable-eval? xs)
  (define (iter vals)
    (if (null? vals)
        #f
        (if (eval-formula xs (car vals))
            (iter (cdr vals))
            (car vals))))
  (let ((vals (gen-vals
               (free-vars xs))))
    (iter vals)))

;;zadanie 4

(define (nnf? xs)
  (or (lit? xs)
      (and (disj? xs)
           (nnf? (disj-left xs))
           (nnf? (disj-rght xs)))
      (and (conj? xs)
           (nnf? (conj-left xs))
           (nnf? (conj-rght xs)))))

(define (lit arg negation)
  (list 'lit arg negation))

(define (lit? t)
  (and (list? t)
       (= 3 (length t))
       (eq? 'lit (car t))
       (var? (second t))
       (boolean? (third t))))

(define (lit-arg t) (cadr t))
(define (lit-neg t) (caddr t))

;;zadanie 5

(define (convert-to-nnf xs)
  (define (negate xs x)
    (cond [(var? xs) (lit xs #f)]
          [(lit? xs) (lit (lit-arg xs) (not (lit-neg xs)))]
          [(neg? xs) (if (= x 0)
                         (negate (neg-subf xs) 1)
                         (negate (neg-subf xs) 0))]
          [(disj? xs) (if (= x 0)
                          (list 'disj
                            (negate (disj-left xs) 0)
                            (negate (disj-rght xs) 0))
                          (list 'conj
                            (negate (disj-left xs) 1)
                            (negate (disj-rght xs) 1)))]
          [(conj? xs) (if (= x 0)
                          (list 'conj
                            (negate (conj-left xs) 0)
                            (negate (conj-rght xs) 0))
                          (list 'disj
                            (negate (conj-left xs) 1)
                            (negate (conj-rght xs) 1)))]))
  (negate xs 0))

;;zadanie 6

(define (cnf? f)
  (define (isdisj? f)
    (and (list? f)
         (<= 1 (length f))
         (eq? 'cnf-disj (car f))
         (andmap lit? (cdr f))))  ;;sprawdza czy wszystkie elementy listy spełniają predykat
  (and (list? f)
       (eq? (car f) 'cnf-conj)
       ;;sprawdzenie czy elementy to dysjunkcje
       ))

(define (lit->cnf l)
  (list 'cnf-conj (list 'cnf-disj l)))

(define (conj->cnf c1 c2)
  (append (list 'cnf-conj (cdr c1) (cdr c2))))

(define (disj->cnf c1 c2)
  (define (mmerge xs i)
    (append (list 'cnf-disj) (apply append xs)))
  (append (list 'cnf-conj)
          (map mmerge (cartesian-product
                       (map cdr (cdr c1))
                       (map cdr (cdr c2))))))

(define (convert-to-cnf f)
  (cond [(lit? f) (lit->cnf f)]
        [(conj? f)
         (let ((left (convert-to-cnf (conj-left f)))
               (rght (convert-to-cnf (conj-rght f))))
           (conj->cnf left rght))]
         [(disj? f)
          ;;analogiczne do conj
          ]))

;;zadanie 7

(define (eval-cnf f vals)
  (define (eval-cnf-disj t)
    (ormap (lambda (t) (xor (lit-neg t)
                            ;;(?)(value (lit-arg t) vals))
                            ))
           (cdr t))
    (andmap eval-cnf-disj (cdr f)))
  ;;dalej coś jeszcze ma być
  (eval-cnf-disj f)
  )