#lang rosette/safe
(require "tree.rkt")
(require "helper.rkt")

(define-symbolic a b c d e f g h integer?)

(define (check l)
  (for-each (λ (x) (assert (tree-member? x l))) (list (list a b) (list c d) (list e f) (list g h)))
  (for-each (λ (x) (assert (tree-member? x l))) (list (list a c) (list b d) (list e g) (list f h)))
  (for-each (λ (x) (assert (tree-member? x l))) (list (list a e) (list c g) (list b f) (list d h)))
  (for-each (λ (x) (assert (not (equal? (first x) (second x))))) (list (list b d) (list c e))))
(define sol (solve (check (build-example-sliding 2))))

(define answer (evaluate (list a b c d e f g h) sol))
(convert-back answer)

; a b   e f
; c d   g h
