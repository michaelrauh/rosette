#lang rosette/safe
(require "tree.rkt")


(define-symbolic a b c d e f g h i integer?)

(define (check l)
  (for-each (λ (x) (assert (tree-member? x l))) (list (list a b c) (list d e f) (list g h i) (list a d g) (list b e h) (list c f i)))
  (for-each (λ (x) (assert (not (equal? (first x) (second x))))) (list (list b d) (list c e) (list e g) (list f h) (list c g))))
(define sol (solve (check example-sliding)))

(define answer (evaluate (list a b c d e f g h i) sol))
answer
