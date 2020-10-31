#lang rosette/safe

(define (build-sliding xs)
  (cond
    [(empty? xs) (list)]
    [(= 1 (length xs)) (list)]
    [else (cons (take xs 2) (build-sliding (drop xs 1)))]))

(define example (list 1 2 3 4 1 3 2 4))
(define example-sliding (build-sliding example))
(define-symbolic a b c d integer?)
(define xs (list a b))
(define sol (solve (begin
                     (assert (member (list a b) example-sliding))
                     (assert (member (list c d) example-sliding))
                     (assert (member (list a c) example-sliding))
                     (assert (member (list b d) example-sliding))
                     (assert (not (equal? b c))))))

(evaluate (list (list a b) (list c d)) sol)