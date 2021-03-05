#lang rosette/safe
(require "tree.rkt")
(require "helper.rkt")

(define-symbolic a b c d e f integer?)

(define (check l-one l-two)
  (for-each (λ (x) (assert (tree-member? x l-two))) (list (list a d) (list b e) (list c f)))
  (for-each (λ (x) (assert (tree-member? x l-one))) (list (list a b c) (list d e f)))
  (for-each (λ (x) (assert (not (equal? (first x) (second x))))) (list (list b d) (list c e))))
(define sol (solve (check (build-example-sliding 3) (build-example-sliding 2))))

(define answer (evaluate (list a b c d e f) sol))
(convert-back answer)