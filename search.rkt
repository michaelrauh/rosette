#lang rosette/safe
(require "tree.rkt")
(require "helper.rkt")
(require math)

(define dims '(2 2 2))

(define (dist dims)
  (define (make-dist dims offset)
    (define offset-range (range offset (+ offset (car dims))))
    (if (= 1 (length dims))
        offset-range
        (map (λ (os) (make-dist (cdr dims) os)) offset-range)))
  
  (make-dist (reverse dims) 0))

(define (no-repeats l)
  (eq? l (remove-duplicates l)))

(define (corr all-dists all-symbols acc)
  (if (empty? all-dists) acc
      (begin 
        (define current-symbol (car all-symbols))
        (define current-dist (car all-dists))
        (define new-acc (insert-at acc current-dist current-symbol))
        (corr (cdr all-dists) (cdr all-symbols) new-acc))))

(define dists (flatten (dist dims)))

(define (insert-at l loc x)
  (list-set l loc (cons x (list-ref l loc))))

(define (create-symbol)
  (define-symbolic* x integer?) x)
(define volume (apply * dims))
(define dimensionality (length dims))
(define symbols (map (λ (_) (create-symbol)) (range volume)))
(define for-dim (range dimensionality))
(define highest-axis (sub1 dimensionality))

(define empties (make-list (add1 (apply max dists)) '()))
(define diagonals (corr dists symbols empties))

(define original_orthotope (array-reshape (list->array symbols) (list->vector dims)))

(define all_orthotope_views (map (λ (ax) (array-axis-swap original_orthotope ax highest-axis)) for-dim))

(define phrases (map (λ (pos) (array->list* (array-reshape (list-ref all_orthotope_views pos) (list->vector (list (/ volume (list-ref dims pos)) (list-ref dims pos)))))) for-dim))

(define (check)
  (for-each (λ (pos)
              (for-each (λ (x) (assert (tree-member? x (build-example-sliding (list-ref dims pos)))))
                        (list-ref phrases pos)))
            for-dim)
   
  (for-each (λ (l) (assert (no-repeats l))) diagonals))
(define sol (solve (check)))

(define answer (evaluate symbols sol))
(convert-back answer)