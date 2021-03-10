#lang rosette/safe
(require "tree.rkt")
(require "helper.rkt")
(require math)
(require racket/serialize)

(define (create-symbol)
  (define-symbolic* x integer?) x)

(define (dist dims)
  (define (make-dist dims offset)
    (define offset-range (range offset (+ offset (car dims))))
    (if (= 1 (length dims))
        offset-range
        (map (λ (os) (make-dist (cdr dims) os)) offset-range)))
  
  (make-dist dims 0))

(define (no-repeats l)
  (eq? l (remove-duplicates l)))

(define (make-tree-hash dims)
  (define ks (remove-duplicates dims))
  (define vs (map (λ (k) (build-example-sliding k)) ks))
  (make-immutable-hash (map (lambda (x y) (cons x y)) ks vs)))

(define (search dims)
  (displayln dims)
  (define trees (make-tree-hash dims))

  (define dists (flatten (dist dims)))
  (define (get-diags all-dists all-symbols)
    (define (insert-at l loc x)
      (list-set l loc (cons x (list-ref l loc))))
    (define (corr all-dists all-symbols acc)
      (if (empty? all-dists) acc
          (corr (cdr all-dists) (cdr all-symbols) (insert-at acc (car all-dists) (car all-symbols)))))
    (define empties (make-list (add1 (apply max dists)) '()))
    (corr all-dists all-symbols empties))

  (define volume (apply * dims))
  (define dimensionality (length dims))
  (define symbols (map (λ (_) (create-symbol)) (range volume)))
  (define for-dim (range dimensionality))
  (define diagonals (get-diags dists symbols))

  (define original_orthotope (array-reshape (list->array symbols) (list->vector dims)))

  (define all_orthotope_views (map (λ (ax) (array-axis-swap original_orthotope ax (sub1 dimensionality))) for-dim))

  (define phrases (map (λ (pos) (array->list* (array-reshape (list-ref all_orthotope_views pos) (list->vector (list (/ volume (list-ref dims pos)) (list-ref dims pos)))))) for-dim))

  (define (check)
    (for-each (λ (dim-index)
                (define dim-size (list-ref dims dim-index))
                (define tree (hash-ref trees dim-size))
                (for-each (λ (x) (assert (tree-member? x tree)))
                          (list-ref phrases dim-index)))
              for-dim)
   
    (for-each (λ (l) (assert (no-repeats l))) diagonals))
  (define sol (solve (check)))
  (if (unsat? sol)
      (begin
        (displayln "search failed")
        #f)
      (begin
        (define ans (array->list* (array-reshape (list->array (convert-back (evaluate symbols sol))) (list->vector dims))))
        (displayln ans)
        ans)
      ))
(provide search)