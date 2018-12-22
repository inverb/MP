#lang racket

(require racklog)

;; transpozycja tablicy zakodowanej jako lista list
(define (transpose xss)
  (cond [(null? xss) xss]
        ((null? (car xss)) (transpose (cdr xss)))
        [else (cons (map car xss)
                    (transpose (map cdr xss)))]))

;; procedura pomocnicza
;; tworzy listę n-elementową zawierającą wyniki n-krotnego
;; wywołania procedury f
(define (repeat-fn n f)
  (if (eq? 0 n) null
      (cons (f) (repeat-fn (- n 1) f))))

;; tworzy tablicę n na m elementów, zawierającą świeże
;; zmienne logiczne
(define (make-rect n m)
  (repeat-fn m (lambda () (repeat-fn n _))))

;; predykat binarny
;; (%row-ok xs ys) oznacza, że xs opisuje wiersz (lub kolumnę) ys
(define %row-ok
  (%rel (x y xs ys)
        [(null null)]
        [((list 0) null)]
        [((list -1) null)]
        [((list -1 0) null)]
        [((cons x xs) (cons '* ys))
         (%is #t (> x 0))
         (%is y (- x 1))
         (%row-ok (cons y xs) ys)]
        [((cons -1 xs) (cons '* ys))
         (%row-ok xs (cons '* ys))]
        [((cons 0 xs) (cons '_ ys))
         (%row-ok (cons -1 xs) ys)]
        [((cons -1 xs) (cons '_ ys))
         (%row-ok (cons -1 xs) ys)]
        [(null (cons '_ ys))
         (%row-ok null ys)]))

;; TODO: napisz potrzebne ci pomocnicze predykaty
(define %board-ok
  (%rel (x y xs ys)
        [(null null)]
        [((cons x xs) (cons y ys))
         (%row-ok (cons -1 x) y)
         (%board-ok xs ys)]))

(define %transpose-board-ok
  (%rel (x y xs ys)
        [(xs ys)
         (%board-ok xs (transpose ys))]))

;; funkcja rozwiązująca zagadkę
(define (solve rows cols)
  (define board (make-rect (length cols) (length rows)))
  (define tboard (transpose board))
  (define ret (%which (xss) 
                      (%= xss board)
                      (%board-ok rows board)
                      (%board-ok cols tboard)
                      ))
  (and ret (cdar ret)))

;; testy
(define (tests)
  (and
   (equal? (solve '((2) (1) (1)) '((1 1) (2)))
           '((* *)
             (_ *)
             (* _)))
   
   (equal? (solve '((2) (2 1) (1 1) (2)) '((2) (2 1) (1 1) (2)))
           '((_ * * _)
             (* * _ *)
             (* _ _ *)
             (_ * * _)))
   
   (equal? (solve '((1 1) (1) (1 1)) '((1 1) (1) (1 1)))
           '((* _ *)
             (_ * _)
             (* _ *)))
   
   (equal? (solve '((1 4 1)) '((1) (0) (1) (1) (1) (1) (0) (0) (0) (1) (0)))
           '((* _ * * * * _ _ _ * _)))
   
   (equal? (solve '((2) (1 1) (2) (1 1) (1 1)) '((5) (1 1) (1 2)))
           '((* * _)
             (* _ *)
             (* * _)
             (* _ *)
             (* _ *)))
   
   (equal? (solve '((1) (1 1) (3) (1 1) (1 1)) '((4) (1 1) (4)))
           '((_ * _)
             (* _ *)
             (* * *)
             (* _ *)
             (* _ *)))
   
   (equal? (solve '((1) (1 1) (1) (1 1) (1)) '((3) (1 1) (1 1)))
           '((_ * _)
             (* _ *)
             (* _ _)
             (* _ *)
             (_ * _)))
   
   (equal? (solve '((1 1) (1 1) (2) (1 1) (1 1)) '((5) (1) (2 2)))
           '((* _ *)
             (* _ *)
             (* * _)
             (* _ *)
             (* _ *)))
   
   (equal? (solve '((3) (1) (2) (1) (3)) '((5) (1 1 1) (1 1)))
           '((* * *)
             (* _ _)
             (* * _)
             (* _ _)
             (* * *)))
   
   (equal? (solve '((3) (1) (1) (1) (1)) '((1) (5) (1)))
           '((* * *)
             (_ * _)
             (_ * _)
             (_ * _)
             (_ * _)))

   (equal? (solve '((1 1 1) (3) (5) (3) (1 1 1)) '((1 1 1) (3) (5) (3) (1 1 1)))
           '((* _ * _ *)
             (_ * * * _)
             (* * * * *)
             (_ * * * _)
             (* _ * _ *)))))

(tests)
; Ten test jest przerywany, bo wykonuje się za długo.
;(equal? (solve '( (1 2) (2) (1) (1) (2) (2 4) (2 6) (8) (1 1) (2 2))
;               '( (2) (3) (1) (2 1) (5) (4) (1 4 1) (1 5) (2 2) (2 1)))
;        '((_ _ _ _ _ _ * _ * *)
;          (_ _ _ _ _ _ _ _ * *)
;          (_ _ _ _ _ _ _ * _ _)
;          (_ _ _ _ _ _ _ _ _ *)
;          (_ _ _ _ _ * * _ _ _)
;          (* * _ _ * * * * _ _)
;          (* * _ * * * * * * _)
;          (_ * * * * * * * * _)
;          (_ _ _ _ * _ _ * _ _)
;          (_ _ _ * * _ * * _ _)))