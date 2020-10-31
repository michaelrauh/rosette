#lang rosette

(define-symbolic x y integer?)
(define sol (solve (assert (member (list x y) (list (list 1 2) (list 2 3))))))
(evaluate x sol)
(evaluate y sol)