#lang rosette

(define-symbolic x y integer?)
(define xs (list x y))
(define sol (solve (assert (member xs (list (list 1 2) (list 2 3))))))
(evaluate xs sol)