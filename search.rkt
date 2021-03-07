#lang rosette/safe
(require "tree.rkt")
(require "helper.rkt")
(require math)

(define-symbolic a b c d e f g h integer?)

(define arr (array-reshape (array #[a b c d e f g h]) #(2 2 2)))

(define p1 (array-axis-swap arr 1 2))
(define p2 (array-axis-swap arr 0 2))

(define a1 (array->list* (array-reshape arr #(4 2))))
(define a2 (array->list* (array-reshape p1 #(4 2))))
(define a3 (array->list* (array-reshape p2 #(4 2))))

(define (check l)
  (for-each (λ (x) (assert (tree-member? x l))) a1)
  (for-each (λ (x) (assert (tree-member? x l))) a2)
  (for-each (λ (x) (assert (tree-member? x l))) a3)
  (for-each (λ (x) (assert (not (equal? (first x) (second x))))) (list (list b c) (list b e) (list d g) (list f g))))
(define sol (solve (check (build-example-sliding 2))))

(define answer (evaluate (list a b c d e f g h) sol))
(convert-back answer)

; a b   e f
; c d   g h

; 0 1   1 2
; 1 2   2 3
