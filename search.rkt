#lang rosette/safe
(require "tree.rkt")
(require "helper.rkt")
(require math)

(define dims '(2 2 2))

(define (create-symbol)
 (define-symbolic* x integer?) x)
(define volume (apply * dims))
(define dimensionality (- (length dims) 1))
(define symbols (map (λ (_) (create-symbol)) (range volume)))

(define arr (array-reshape (list->array symbols) (list->vector dims)))

(define ps (map (λ (ax) (array-axis-swap arr ax dimensionality)) (range (+ 1 dimensionality))))

(define as (map (λ (pos) (array->list* (array-reshape (list-ref ps pos) (list->vector (list (/ volume (list-ref dims pos)) (list-ref dims pos))))))  (range (+ 1 dimensionality))))

(define (check l)
  (for-each (λ (x) (assert (tree-member? x l))) (list-ref as 0))
  (for-each (λ (x) (assert (tree-member? x l))) (list-ref as 1))
  (for-each (λ (x) (assert (tree-member? x l))) (list-ref as 2))
  (for-each (λ (x) (assert (not (equal? (first x) (second x))))) (list (list (list-ref symbols 1) (list-ref symbols 2)) (list (list-ref symbols 1) (list-ref symbols 4)) (list (list-ref symbols 3) (list-ref symbols 7)) (list (list-ref symbols 5) (list-ref symbols 7))))) ; calculate this instead of hard code
(define sol (solve (check (build-example-sliding 2))))

(define answer (evaluate symbols sol))
(convert-back answer)