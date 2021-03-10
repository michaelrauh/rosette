#lang racket
(require "search.rkt")

(struct search-res (res dims))

(define (find-dimensionality)
  (define (go acc)
    (define ans (search (search-res-dims acc)))
    (if (not ans)
        acc
        (go (search-res ans (cons 2 (search-res-dims acc))))))
  (go (search-res #f '(2 2))))

(define dimensionality (find-dimensionality))