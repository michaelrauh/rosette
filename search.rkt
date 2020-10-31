#lang rosette

(define-symbolic x integer?)
(define sol (solve (assert (member x (list 1 2 3)))))
(evaluate x sol)