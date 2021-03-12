#lang racket
(require "search.rkt")

(define (find-dimensionality)
  (define (go dims)
    (define ans (search dims))
    (if (not ans)
        dims
        (go (cons 2 dims))))
  (cdr (go '(2 2))))

(define dimensionality (find-dimensionality))

(define (find-shape dims cur jumped)
  (define failed  (not (search dims)))
  (define at-end (= cur (sub1 (length dims))))
  (cond
    [(and failed jumped) (unbump dims cur)]
    [(and failed at-end) (unbump dims cur)]
    [failed (find-shape (bump (unbump dims cur) (add1 cur)) (add1 cur) #t)]
    [else (find-shape (bump dims cur) cur #f)]))
      
(define (bump l pos)
  (list-update l pos add1))

(define (unbump l pos)
  (list-update l pos sub1))

(define (metasearch)
  (if (> (length dimensionality) 1)
      (find-shape (bump dimensionality 0 #t) 0)
      (display "zero interesting things found")))

(metasearch)

(define (combined-search dims cur jumped)
  (define failed (not (search dims)))
  (define at-end (= cur (sub1 (length dims))))
  (cond
    [(and failed jumped) (unbump dims cur)]
    [(and failed at-end) (combined-search ((make-list (add1 (length dims)) 2) dims) 0 #t)]
    [failed (combined-search (bump (unbump dims cur) (add1 cur)) (add1 cur) #t)]
    [else (combined-search (bump dims cur) cur #f)]))

(define (new-search)
  (combined-search '(2 2) 0 #t))