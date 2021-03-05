#lang rosette/safe

(require "helper.rkt")

(struct tree (left data right) #:transparent)
  
(define (from-sorted-list l)
  (define (first-half l)
    (define middle-index (floor (/ (length l) 2)))
    (take l middle-index))

  (define (middle l)
    (define middle-index (floor (/ (length l) 2)))
    (cond [(empty? l) null]
          [else (list-ref l middle-index)]))
  
  (define (second-half l)
    (define middle-index (+ 1 (floor (/ (length l) 2))))
    (cond [(< (length l) 2) null]
          [else (drop l middle-index)]))
  (cond [(empty? l) null]
        [else (tree (from-sorted-list (first-half l)) (middle l) (from-sorted-list (second-half l)))]))

(define (from-list l less-than)
  (from-sorted-list (sort (remove-duplicates l) less-than)))

(define (member? less-than t x)
  (cond [(empty? t) false]
        [(equal? x (tree-data t)) true]
        [(less-than x (tree-data t)) (member? less-than (tree-left t) x)]
        [else (member? less-than (tree-right t) x)]))


(define (list-less-than x y)
  (cond [(and (empty? x) (empty? y)) false]
        [(empty? x) true]
        [(empty? y) false]
        [(not (equal? (car x) (car y))) (< (car x) (car y))]
        [else (list-less-than (cdr x) (cdr y))]))

(define (build-sliding xs)
  (cond
    [(< (length xs) 3) (list)]
    [else (cons (take xs 3) (build-sliding (drop xs 1)))]))

(define example-sliding (from-list (build-sliding example) list-less-than))

(define (tree-member? x t)
  (member? list-less-than t x))

(provide tree-member? example-sliding)