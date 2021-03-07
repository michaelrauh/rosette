#lang rosette/safe
(require "tree.rkt")
(require "helper.rkt")
(require math)

(define dims '(2 2 2))

(define (create-symbol)
 (define-symbolic* x integer?) x)

(define symbols (map (λ (_) (create-symbol)) (range (apply * dims))))

(define arr (array-reshape (list->array symbols) (list->vector dims)))

(define p1 (array-axis-swap arr 1 2)) ; investigate how these permutations progress for higher dimensions
(define p2 (array-axis-swap arr 0 2))

(define a1 (array->list* (array-reshape arr #(4 2)))) ; in this case 2 is the length of the target dimension, and 4 is what is left over. This will be different on each line if it's not square.
(define a2 (array->list* (array-reshape p1 #(4 2))))
(define a3 (array->list* (array-reshape p2 #(4 2))))

(define (check l)
  (for-each (λ (x) (assert (tree-member? x l))) a1)
  (for-each (λ (x) (assert (tree-member? x l))) a2)
  (for-each (λ (x) (assert (tree-member? x l))) a3)
  (for-each (λ (x) (assert (not (equal? (first x) (second x))))) (list (list (list-ref symbols 1) (list-ref symbols 2)) (list (list-ref symbols 1) (list-ref symbols 4)) (list (list-ref symbols 3) (list-ref symbols 7)) (list (list-ref symbols 5) (list-ref symbols 7))))) ; calculate this instead of hard code
(define sol (solve (check (build-example-sliding 2))))

(define answer (evaluate symbols sol))
;(convert-back answer)


(define dims2 '(2 2 2 2))
(define ex (range (apply * dims2)))

(define arr2 (array-reshape (list->array ex) (list->vector dims2)))
arr2

(array-axis-swap arr2 0 3)
(array-axis-swap arr2 1 3)
(array-axis-swap arr2 2 3)
(array-axis-swap arr2 3 3)

; 0 1   4 5
; 2 3   6 7


; 8  9   12 13
; 10 11  14 15
