#lang rosette/safe
(require "tree.rkt")
(require "helper.rkt")
(require math)

(define (create-symbol)
 (define-symbolic* x integer?) x)

(define-symbolic a b c d e f g h integer?)
(define symbols (list a b c d e f g h))

(define arr (array-reshape (list->array symbols) #(2 2 2))) ; replace array with n calls to define-symbolic*. Replace dims with passed in dims

(define p1 (array-axis-swap arr 1 2)) ; investigate how these permutations progress for higher dimensions
(define p2 (array-axis-swap arr 0 2))

(define a1 (array->list* (array-reshape arr #(4 2)))) ; in this case 2 is the length of the target dimension, and 4 is what is left over. This will be different on each line if it's not square.
(define a2 (array->list* (array-reshape p1 #(4 2))))
(define a3 (array->list* (array-reshape p2 #(4 2))))

(define (check l)
  (for-each (λ (x) (assert (tree-member? x l))) a1)
  (for-each (λ (x) (assert (tree-member? x l))) a2)
  (for-each (λ (x) (assert (tree-member? x l))) a3)
  (for-each (λ (x) (assert (not (equal? (first x) (second x))))) (list (list b c) (list b e) (list d g) (list f g)))) ; calculate this instead of hard code
(define sol (solve (check (build-example-sliding 2))))

(define answer (evaluate symbols sol))
(convert-back answer)
