#lang rosette/safe

(define example (list 1 2 3 4 1 3 2 4))
(define example-sliding (list (list 1 2) (list 2 3) (list 3 4) (list 4 1) (list 1 3) (list 3 2) (list 2 4)))
(define-symbolic a b c d integer?)
(define xs (list a b))
(define sol (solve (begin
                     (assert (member (list a b) example-sliding))
                     (assert (member (list c d) example-sliding))
                     (assert (member (list a c) example-sliding))
                     (assert (member (list b d) example-sliding))
                     (assert (not (equal? b c))))))
(evaluate (list (list a b) (list c d)) sol)