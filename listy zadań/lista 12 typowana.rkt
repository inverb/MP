#lang typed/racket

;;zadanie 6

(: prefixes (All (A) (-> (Listof A) (Listof (Listof A)))))
(define (prefixes l)
  (: loop (All (A) (-> (Listof A) (Listof A) (Listof (Listof A)))))
  (define (loop w l)
    (if (null? l)
        (cons w null)
        (cons w (loop (append w (list (car l))) (cdr l)))))
  (loop null l))

;;zadanie 7

(define-type Leaf 'leaf)
(define-type (Node A B) (Pair A (Listof B)))
(define-type (Rose-Tree A) (U Leaf (Node A (Rose-Tree A))))

(define-predicate leaf? Leaf)

(: node-val (All (A B) (-> (Node A B) A)))
(define (node-val n) (car n))

(: node-list (All (A B) (-> (Node A B) (Listof B))))
(define (node-list n) (cdr n))

(: preorder (All (A) (-> (Rose-Tree A) (Listof A))))
(define (preorder t)
  (if (leaf? t)
      null
      (cons (node-val t)
            (apply append
                   (map (lambda ([x : (Rose-Tree A)])
                          (preorder x))
                        (node-list t))))))