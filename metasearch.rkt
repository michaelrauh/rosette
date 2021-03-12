#lang racket
(require "search.rkt")
      
(define (bump l pos)
  (list-update l pos add1))

(define (at-end dims cur)
  (= cur (sub1 (length dims))))

(define (metasearch dims cur bootstrap) 
  (if bootstrap
      (check-if-safe dims cur)
      (search-safe dims cur)))

(define (check-if-safe dims cur)
  (if (search dims)
      (search-safe dims cur)
      (display "nothing interesting found")))

(define (search-safe dims cur)
  (if (search (bump dims cur))
      (search-safe (bump dims cur) cur)
      (search-next-dimension dims cur)))

(define (search-next-dimension dims cur)
  (if (not (at-end dims cur))
      (safe-search-next-dimension dims cur)
      (add-dimension dims cur)))

(define (safe-search-next-dimension dims cur)
  (if (search (bump dims (add1 cur)))
      (search-safe (bump dims (add1 cur)) (add1 cur))
      (add-dimension dims cur)))

(define (add-dimension dims cur)
  (if (search (make-list (add1 (length dims)) 2))
      (search-safe (make-list (add1 (length dims)) 2) 0)
      (display "end of search")))

(metasearch '(2 2) 0 #t)