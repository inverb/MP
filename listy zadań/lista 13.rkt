#lang racket

(require racklog)

;; predykat unarny %male reprezentuje zbiór mężczyzn
(define %male
  (%rel ()
        [('adam)]
        [('john)]
        [('joshua)]
        [('mark)]
        [('david)]))

;; predykat unarny %female reprezentuje zbiór kobiet
(define %female
  (%rel ()
        [('eve)]
        [('helen)]
        [('ivonne)]
        [('anna)]))

;; predykat binarny %parent reprezentuje relację bycia rodzicem
(define %parent
  (%rel ()
        [('adam 'helen)]
        [('adam 'ivonne)]
        [('adam 'anna)]
        [('eve 'helen)]
        [('eve 'ivonne)]
        [('eve 'anna)]
        [('john 'joshua)]
        [('helen 'joshua)]
        [('ivonne 'david)]
        [('mark 'david)]))

;; predykat binarny %sibling reprezentuje relację bycia rodzeństwem
(define %sibling
  (%rel (a b c)
        [(a b)
         (%parent c a)
         (%parent c b)]))

;; predykat binarny %sister reprezentuje relację bycia siostrą
(define %sister
  (%rel (a b)
        [(a b)
         (%sibling a b)
         (%female a)]))

;; predykat binarny %ancestor reprezentuje relację bycia przodkiem
(define %ancestor
  (%rel (a b c)
        [(a b)
         (%parent a b)]
        [(a b)
         (%parent a c)
         (%ancestor c b)]))

;;zadanie 1

(define %grandson
  (%rel (a b c)
        [(a b)
         (%parent c a)
         (%parent b c)
         (%male a)]))

(define %cousin
  (%rel (a b c d)
        [(a b)
         (%is a b)]
        [(a b)
         (%is (%which () (%sibling a b)) #f)
         (%parent c a)
         (%parent d b)
         (%sibling c d)]))

(define %is-mother?
  (%rel (a b)
        [(a)
         (%parent a b)
         (%female a)]))

(define %is-father?
  (%rel (a b)
        [(a)
         (%parent a b)
         (%male a)]))

;;zadanie 2

(%find-all () (%ancestor 'mark 'john))
(%find-all (x) (%ancestor 'adam x))
(%find-all (x) (%sister 'ivonne x))
(%find-all (x y) (%cousin x y)) ;;(?)

;;zadanie 3

;;;;;;;;;;;;;kod z wykładu;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define %my-append
  (%rel (x xs ys zs)
        [(null ys ys)]
        [((cons x xs) ys (cons x zs))
         (%my-append xs ys zs)]))

(define %my-member
  (%rel (x xs y)
        [(x (cons x xs))]
        [(y (cons x xs))
         (%my-member y xs)]))

(define %select
  (%rel (x xs y ys)
        [(x (cons x xs) xs)]
        [(y (cons x xs) ys)
         (%select y xs ys)]))

;; prosta rekurencyjna definicja
(define %simple-length
  (%rel (x xs n m)
        [(null 0)]
        [((cons x xs) n)
         (%simple-length xs m)
         (%is n (+ m 1))]))

;; test w trybie +- (działa)
;(%find-all (a) (%simple-length (list 1 2) a))
;; test w trybie ++ (działa)
;(%find-all () (%simple-length (list 1 2) 2))
;; test w trybie -+ (1 odpowiedź, pętli się)
;(%which (xs) (%simple-length xs 2))
;; test w trybie -- (nieskończona liczba odpowiedzi)
;(%which (xs a) (%simple-length xs a))

;; definicja zakładająca, że długość jest znana
(define %gen-length
  (%rel (x xs n m)
        [(null 0) !]
        [((cons x xs) n)
         (%is m (- n 1))
         (%gen-length xs m)]))
;; test w trybie ++ (działa)
;(%find-all () (%gen-length (list 1 2) 2))
;; test w trybie -+ (działa)
;(%find-all (xs) (%gen-length xs 2))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(%find-all (xs ys) (%my-append xs xs ys))
(%find-all (xs) (%my-append '(1 2 3) xs '(1 2 3 4)))
;;trzeci podobnie do drugiego

;;zadanie 6

(define %sublist
  (%rel (x y xs ys)
        [(null ys)]
        [ ((cons x xs) (cons x ys))
          (%sublist xs ys)]
        [((cons x xs) (cons y ys))
         (%sublist (cons x xs) ys)]))

;;zadanie 7

(define %auxperm0
  (%rel (xs ys x)
        [(null ys) !]
        [((cons x xs) ys)
        (%my-member x ys)
        (%auxperm0 xs ys)]))

(define %reverse0
  (%rel (x)
        [(x)]))

(define %perm0
  (%rel (xs ys x)
        [(xs ys)
         (%simple-length xs x)
         (%simple-length ys x)
         (%auxperm0 xs ys)
         (%if-then-else (%reverse0 xs ys) ! %true)])) ;; wykrzyknik (odcięcie) kończy wyszukiwanie - rzeczy dalej już nawet nie sprawdzamy

;; inaczej

(define %perm
  (%rel (xs ys x zs as bs)
        [(null null)]
        [((cons x xs) ys)
         (%member x ys)
         (%append as (cons x bs) ys)
         ((%append as bs zs)
          (%perm xs zs))]))

;;zadanie 8

(define (list->num xs)
  (define (iter ys acc)
    (if (null? ys)
        acc
        (iter (cdr ys)
              (+ (* 10 acc) (car ys)))))
  (iter xs 0))

(define %lamiglowka
  (%rel (d e m n o r s y s1 s2 res xs)
        [(d e m n o r s y)
         (%gen-length xs 8)
         (%sublist xs '(1 2 3 4 5 6 7 8 9 0))
         (%perm (list d e m n o r s y) xs)
         (%=/= s 0)
         (%=/= m 0)
         (%is s1 (list->num (list s e n d)))
         (%is s2 (list->num (list m o r e)))
         (%is res (list->num (list m o n e y)))
         (%is res (+ s1 s2))]))