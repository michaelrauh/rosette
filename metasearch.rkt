#lang racket
(require "search.rkt")
      
(define (bump l pos)
  (list-update l pos add1))

(define (unbump l pos)
  (list-update l pos sub1))

(define (combined-search dims cur new-cur new-dim)
  (define failed (not (search dims)))
  (define at-end (= cur (sub1 (length dims))))
  (cond
    [(and failed new-dim) (begin (displayln "failed after new dimension added.") (cdr dims))]
    [(and failed new-cur) (begin (displayln "failed after changing dimension. Skipping to add dimension") (combined-search (make-list (add1 (length dims)) 2) 0 #f #t))]
    [(and failed at-end) (begin (displayln "got to end of dimension. adding new.") (combined-search (make-list (add1 (length dims)) 2) 0 #f #t))]
    [failed (begin (displayln "failed on current. Moving to new dimension.") (combined-search (bump (unbump dims cur) (add1 cur)) (add1 cur) #t #f))]
    [else (begin (displayln "succeeded. Adding to dimension.") (combined-search (bump dims cur) cur #f #f))]))

(define (new-search)
  (combined-search '(2 2) 0 #f #t))

(new-search)