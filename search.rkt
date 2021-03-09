#lang rosette/safe
(require "tree.rkt")
(require "helper.rkt")
(require math)

(define dims '(2 2 2))

(define (create-symbol)
  (define-symbolic* x integer?) x)

(define (dist dims)
  (define (make-dist dims offset)
    (define offset-range (range offset (+ offset (car dims))))
    (if (= 1 (length dims))
        offset-range
        (map (λ (os) (make-dist (cdr dims) os)) offset-range)))
  
  (make-dist (reverse dims) 0))

(define (no-repeats l)
  (eq? l (remove-duplicates l)))

(define (get-diags all-dists all-symbols)
  (define (insert-at l loc x)
    (list-set l loc (cons x (list-ref l loc))))
  (define (corr all-dists all-symbols acc)
    (if (empty? all-dists) acc
        (corr (cdr all-dists) (cdr all-symbols) (insert-at acc (car all-dists) (car all-symbols)))))
  (define empties (make-list (add1 (apply max dists)) '()))
  (corr all-dists all-symbols empties))

(define dists (flatten (dist dims)))

(define volume (apply * dims))
(define dimensionality (length dims))
(define symbols (map (λ (_) (create-symbol)) (range volume)))
(define for-dim (range dimensionality))
(define diagonals (get-diags dists symbols))

(define original_orthotope (array-reshape (list->array symbols) (list->vector dims)))

(define all_orthotope_views (map (λ (ax) (array-axis-swap original_orthotope ax (sub1 dimensionality))) for-dim))

(define phrases (map (λ (pos) (array->list* (array-reshape (list-ref all_orthotope_views pos) (list->vector (list (/ volume (list-ref dims pos)) (list-ref dims pos)))))) for-dim))

(define (check)
  (for-each (λ (pos)
              (for-each (λ (x) (assert (tree-member? x (build-example-sliding (list-ref dims pos)))))
                        (list-ref phrases pos)))
            for-dim)
   
  (for-each (λ (l) (assert (no-repeats l))) diagonals))
(define sol (solve (check)))

(define answer (evaluate symbols sol))
(array->list* (array-reshape (list->array (convert-back answer)) (list->vector dims)))