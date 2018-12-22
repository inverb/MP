#lang racket

;;zadanie 1

(define (concatMap f xs)
  (if (null? xs)
      null
      (append (f (car xs)) (concatMap f (cdr xs)))))

(define (from-to s e)
  (if (= s e)
      (list s)
      (cons s (from-to (+ s 1) e))))

(define (queens board-size)
  
  ;; Return the representation of a board with 0 queens inserted
  (define (empty-board)
  (define (iter i)
    (if(= i board-size)
       null
       (cons 0 (iter (+ i 1)))))
    (iter 0))
  ;; Return the representation of a board with a new queen at
  ;; (row, col) added to the partial representation `rest'
  (define (adjoin-position row col rest)
  (define (rek i res)
    (cond [(null? res) null]
          [(= i col) (cons row (cdr res))]
          [else (cons (car res) (rek (+ i 1) (cdr res)))]))
  (rek 1 rest))
  ;; Return true if the queen in k-th column does not attack any of
  ;; the others
  (define (safe? k positions)
  (define (val1 i pos)
    (if(= i k)
       (abs (- i (car pos)))
       (val1 (+ i 1) (cdr pos))))
  (define (val2 i pos)
    (if(= i k)
       (+ i (car pos))
       (val2 (+ i 1) (cdr pos))))
  (define (val3 i pos)
    (if(= i k)
       (car pos)
       (val3 (+ i 1) (cdr pos))))
  (let ([v1 (val1 1 positions)]
        [v2 (val2 1 positions)]
        [v3 (val3 1 positions)])
    (define (iter i pos)
      (cond [(= i k) #t]
            [(= (abs (- i (car pos))) v1) #f]
            [(= (+ i (car pos)) v2) #f]
            [((= (car pos) v3)) #f]
            [else (iter (+ i 1) (cdr pos))]))
    (iter 1 positions)))
  ;; Return a list of all possible solutions for k first columns
  (define (queen-cols k)
    (if (= k 0)
        (list (empty-board))
        (filter
         (lambda (positions) (safe? k positions))
         (concatMap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (from-to 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

;;zadanie 3

;;wersja bez abstrakcji danych
(define (mirror1 t)
  (if (eq? 'leaf t)
      'leaf
      (list 'node (cadr t) (mirror1 (cadddr t))
            (mirror1 (caddr t)))))

;;wersja z abstrakcją

(define (leaf? tree)
  (eq? tree 'leaf))

(define leaf 'leaf)

(define (make-node v l r)
  (list 'node v l r))

(define (node-val t)
  (second t))

(define (node-left t)
  (third t))

(define (node-right t)
  (fourth t))

(define (mirror t)
  (if (leaf? t)
      leaf
      (make-node (node-val t)
                 (mirror (node-right t))
                 (mirror (node-left t)))))

;;zadanie 4

(define (flatten tree)
  (define (pom t l)
    (cond [(leaf? t) l]
          [else
           (pom (node-left t) (cons (node-val t)
                                    (pom (node-right t) l)))]))
  (pom tree '()))

;; (pom t l) = (append (flatten t) l)

;;zadanie 5

(define (bst-insert x t)
  (cond [(leaf? t)
         (make-node x leaf leaf)]
        [(< x (node-val t))
         (make-node (node-val t)
                    (bst-insert x (node-left t))
                    (node-right t))]
        [else
         (make-node (node-val t)
                    (node-left t)
                    (bst-insert x (node-right t)))]))


(define (treesort l)
  (define (iter ls t)
    (if (null? ls)
        t
        (iter (cdr l) (bst-insert t (car l)))))
  (flatten (iter l leaf)))

;;zadanie 6

(define (bst-delete k t)
  (define (bst-min-gt t)
    (cond [(leaf? t) #f]
          [(leaf? (node-left t)) (node-val t)]
          [else (bst-min-gt (node-left t))]))
  (cond [(leaf? t) t]
        [(< k (node-val t))
         (make-node (node-val t)
                    (bst-delete k (node-left t))
                    (node-right t))]
        [(> k (node-val t))
         (make-node (node-val t)
                    (node-left t)
                    (bst-delete k (node-right t)))]
        [(and (leaf? (node-left t)) (leaf? (node-right t))) leaf]
        [(leaf? (node-left t)) (node-right t)]
        [(leaf? (node-right t)) (node-left t)]
        [else (let ((s (bst-min-gt (node-right t))))
                (make-node (node-val s)
                           (node-left t)
                           (bst-delete (node-val s) (node-right t))))]))