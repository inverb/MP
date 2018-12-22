#lang racket

(define (rozszerzony-Euklides m e)
  (define (iter a b x1 x2 y1 y2)
    (if(= b 0)
       (cons x2 y2)
       (let ((r (modulo a b))
             (q (floor (/ a b))))
         (iter b r (- x2 (* q x1)) x1 (- y2 (* q y1)) y1))))
    (iter m e 0 1 1 0))