#lang racket

;; sygnatura: grafy
(define-signature graph^
  ((contracted
    [graph        (-> list? (listof edge?) graph?)]
    [graph?       (-> any/c boolean?)]
    [graph-nodes  (-> graph? list?)]
    [graph-edges  (-> graph? (listof edge?))]
    [edge         (-> any/c any/c edge?)]
    [edge?        (-> any/c boolean?)]
    [edge-start   (-> edge? any/c)]
    [edge-end     (-> edge? any/c)]
    [has-node?    (-> graph? any/c boolean?)]
    [outnodes     (-> graph? any/c list?)]
    [remove-node  (-> graph? any/c graph?)]
    )))

;; prosta implementacja grafów
(define-unit simple-graph@
  (import)
  (export graph^)

  (define (graph? g)
    (and (list? g)
         (eq? (length g) 3)
         (eq? (car g) 'graph)))

  (define (edge? e)
    (and (list? e)
         (eq? (length e) 3)
         (eq? (car e) 'edge)))

  (define (graph-nodes g) (cadr g))

  (define (graph-edges g) (caddr g))

  (define (graph n e) (list 'graph n e))

  (define (edge n1 n2) (list 'edge n1 n2))

  (define (edge-start e) (cadr e))

  (define (edge-end e) (caddr e))

  (define (has-node? g n) (not (not (member n (graph-nodes g)))))
  
  (define (outnodes g n)
    (filter-map
     (lambda (e)
       (and (eq? (edge-start e) n)
            (edge-end e)))
     (graph-edges g)))

  (define (remove-node g n)
    (graph
     (remove n (graph-nodes g))
     (filter
      (lambda (e)
        (not (eq? (edge-start e) n)))
      (graph-edges g)))))

;; sygnatura dla struktury danych
(define-signature bag^
  ((contracted
    [bag?       (-> any/c boolean?)]
    [empty-bag  (and/c bag? bag-empty?)]
    [bag-empty? (-> bag? boolean?)]
    [bag-insert (-> bag? any/c (and/c bag? (not/c bag-empty?)))]
    [bag-peek   (-> (and/c bag? (not/c bag-empty?)) any/c)]
    [bag-remove (-> (and/c bag? (not/c bag-empty?)) bag?)])))

;; struktura danych - stos
(define-unit bag-stack@
  (import)
  (export bag^)

  (define (bag? b)
    (and (list? b)
         (eq? (length b) 2)
         (eq? (car b) 'bag)))

  (define (bag-list b) (cadr b))
  
  (define (bag-empty? b)
    (and (bag? b)
         (eq? (bag-list b) '())))

  (define (bag l) (list 'bag l))
  
  (define empty-bag (bag null))

  (define (bag-insert b x)
    (list 'bag (cons x (bag-list b))))

  (define (bag-peek b)
    (car (bag-list b)))

  (define (bag-remove b)
    (list 'bag (cdr (bag-list b)))))

;; struktura danych - kolejka FIFO
;; do zaimplementowania przez studentów
(define-unit bag-fifo@
  (import)
  (export bag^)
  
  (define (bag? b)
    (and (list? b)
         (eq? (length b) 3)
         (eq? (car b) 'bag)))

  (define (bag-list-in b) (cadr b))
  (define (bag-list-out b) (caddr b))
  
  (define (bag-empty? b)
    (and (bag? b)
         (eq? (bag-list-in b) '())
         (eq? (bag-list-out b) '())))

  (define (bag in out)
    (if (null? out)
        (list 'bag null (reverse in))
        (list 'bag in out)))

  (define empty-bag (bag null null))

  (define (bag-insert b x)
    (bag (cons x (bag-list-in b)) (bag-list-out b)))

  (define (bag-peek b)
    (car (bag-list-out b)))

  (define (bag-remove b)
    (bag (bag-list-in b) (cdr (bag-list-out b)))))

;; sygnatura dla przeszukiwania grafu
(define-signature graph-search^
  (search))

;; implementacja przeszukiwania grafu
;; uzależniona od implementacji grafu i struktury danych
(define-unit/contract graph-search@
  (import bag^ graph^)
  (export (graph-search^
           [search (-> graph? any/c (listof any/c))]))
  (define (search g n)
    (define (it g b l)
      (cond
        [(bag-empty? b) (reverse l)]
        [(has-node? g (bag-peek b))
         (it (remove-node g (bag-peek b))
             (foldl
              (lambda (n1 b1) (bag-insert b1 n1))
              (bag-remove b)
              (outnodes g (bag-peek b)))
             (cons (bag-peek b) l))]
        [else (it g (bag-remove b) l)]))
    (it g (bag-insert empty-bag n) '()))
  )

;; otwarcie komponentu grafu
(define-values/invoke-unit/infer simple-graph@)

;; graf testowy
(define test-graph
  (graph
   (list 1 2 3 4)
   (list (edge 1 3)
         (edge 1 2)
         (edge 2 4))))

;;prosta
(define graph1
  (graph
   (list 1 2 3 4 5)
   (list (edge 1 2) (edge 2 3) (edge 3 4) (edge 4 5))))

;;pęk
(define graph2
  (graph
   (list 1 2 3 4 5 6)
   (list (edge 1 2) (edge 1 3) (edge 1 4) (edge 1 5) (edge 1 6))))

;;cykl
(define graph3
  (graph
   (list 10 20 30 40)
   (list (edge 20 10) (edge 30 20) (edge 40 30) (edge 10 40))))

;;klika
(define graph4
  (graph
   (list 1 2 3 4)
   (list (edge 1 2) (edge 1 3) (edge 1 4)
         (edge 2 1) (edge 2 3) (edge 2 4)
         (edge 3 1) (edge 3 2) (edge 3 4)
         (edge 4 1) (edge 4 2) (edge 4 3))))

;;drzewo binarne
(define graph5
  (graph
   (list 1 2 3 4 5 6 7)
   (list (edge 1 2) (edge 1 3) (edge 2 4) (edge 2 5) (edge 3 6) (edge 3 7))))

;;graf niespójny
(define graph6
  (graph
   (list 1 2 3 4 5 6 7)
   (list (edge 1 2) (edge 1 3) (edge 2 1) (edge 2 3) (edge 3 1) (edge 3 2)
         (edge 4 5) (edge 5 4)
         (edge 6 7) (edge 7 6))))

;;pętle
(define graph7
  (graph
   (list 1 2 3)
   (list (edge 1 1) (edge 2 2) (edge 3 3))))

;;graf losowy
(define graph8
  (graph
   (list 1 2 3 4 5 6 7 8 9 10)
   (list (edge 1 2) (edge 1 4) (edge 4 3) (edge 4 5) (edge 4 6) (edge 5 6) (edge 10 7) (edge 7 9) (edge 8 7) (edge 6 7))))

;; otwarcie komponentu stosu
(define-values/invoke-unit/infer bag-stack@)
;; opcja 2: otwarcie komponentu kolejki
;(define-values/invoke-unit/infer bag-fifo@)

;; testy w Quickchecku
(require quickcheck)

;; test przykładowy: jeśli do pustej struktury dodamy element
;; i od razu go usuniemy, wynikowa struktura jest pusta
(quickcheck
 (property ([s arbitrary-symbol])
           (bag-empty? (bag-remove (bag-insert empty-bag s)))))

;;sprawdzanie, czy empty-bag jest bag
(quickcheck
 (property ()
           (bag? empty-bag)))

;;sprawdzanie, czy działa predykat bag?
(quickcheck
 (property ([s (arbitrary-list arbitrary-symbol)])
           (not (bag? s))))

;;sprawdzanie kolejności wypisywania dla bag-stack
(quickcheck
 (property ([s arbitrary-symbol]
            [p arbitrary-symbol]
            [q arbitrary-symbol])
           (eq? (bag-peek (bag-insert (bag-insert (bag-insert empty-bag s) p) q)) q)))

;;sprawdzanie działania insert i remove dla bag-stack
(quickcheck
 (property ([s arbitrary-symbol]
            [p arbitrary-symbol]
            [q arbitrary-symbol]
            [r arbitrary-symbol])
           (eq? (bag-peek (bag-remove (bag-insert (bag-remove (bag-insert (bag-insert (bag-insert empty-bag s) p) q)) r))) p)))

;;sprawdzanie kolejności wypisywania dla bag-fifo
;(quickcheck
; (property ([s arbitrary-symbol]
;            [p arbitrary-symbol]
;            [q arbitrary-symbol])
;           (eq? (bag-peek (bag-insert (bag-insert (bag-insert empty-bag s) p) q)) s)))

;;sprawdzanie działania insert i remove dla bag-fifo
;(quickcheck
; (property ([s arbitrary-symbol]
;            [p arbitrary-symbol]
;            [q arbitrary-symbol]
;            [r arbitrary-symbol])
;           (eq? (bag-peek (bag-remove (bag-insert (bag-remove (bag-insert (bag-insert (bag-insert empty-bag s) p) q)) r))) q)))


;; otwarcie komponentu przeszukiwania
(define-values/invoke-unit/infer graph-search@)

;;Przeszukiwanie przykładowych grafów
(define (graph-tests)
  ;;checkerka, sprawdza czy algorytm
  ;;doszedł do dobrego zbioru wierzchołków
  ;;(nie sprawdza kolejności, bo różni się ona w zależności od metody)
  (define (comp l1 l2)
    (cond [(and (null? l1) (null? l2)) #t]
          [(or (null? l1) (null? l2)) #f]
          [else (and (member (car l1) l2)
                     (comp (cdr l1) (remove (car l1) l2)))]))
           
  (and (comp (search test-graph 1) '(1 2 3 4))
       (comp (search graph1 1) '(1 2 3 4 5))
       (comp (search graph1 3) '(3 4 5))
       (comp (search graph2 1) '(1 2 3 4 5 6))
       (comp (search graph2 2) '(2))
       (comp (search graph3 40) '(10 20 30 40))
       (comp (search graph4 2) '(1 2 3 4))
       (comp (search graph4 3) '(1 2 3 4))
       (comp (search graph5 1) '(1 2 3 4 5 6 7))
       (comp (search graph6 2) '(1 2 3))
       (comp (search graph6 7) '(6 7))
       (comp (search graph7 3) '(3))
       (comp (search graph8 1) '(1 2 3 4 5 6 7 9))
       (comp (search graph8 4) '(3 4 5 6 7 9))
       (comp (search graph8 5) '(5 6 7 9))
       (comp (search graph8 8) '(7 8 9))))

(graph-tests)