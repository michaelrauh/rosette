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

(define original_orthotope (array-reshape (list->array symbols) (list->vector dims)))

(define all_orthotope_views (map (λ (ax) (array-axis-swap original_orthotope ax dimensionality)) (range (+ 1 dimensionality))))

(define phrases (map (λ (pos) (array->list* (array-reshape (list-ref all_orthotope_views pos) (list->vector (list (/ volume (list-ref dims pos)) (list-ref dims pos))))))  (range (+ 1 dimensionality))))

(define (check)
  (for-each (λ (pos)
   (for-each (λ (x) (assert (tree-member? x (build-example-sliding (list-ref dims pos))))) (list-ref phrases pos)))
            (range (+ 1 dimensionality)))
   
  (for-each (λ (x) (assert (not (equal? (first x) (second x))))) (list (list (list-ref symbols 1) (list-ref symbols 2)) (list (list-ref symbols 1) (list-ref symbols 4)) (list (list-ref symbols 3) (list-ref symbols 7)) (list (list-ref symbols 5) (list-ref symbols 7))))) ; calculate this instead of hard code
(define sol (solve (check)))

(define answer (evaluate symbols sol))
(convert-back answer)