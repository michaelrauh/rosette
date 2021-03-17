#lang racket
(require "search.rkt")

(define (bump l pos)
  (list-update l pos add1))

(define (at-end l cur)
  (= cur (sub1 (length l))))

(define (bump0 l)
  (bump l 0))

(define (filter-candidates candidates hist)
  (define (not-in-history candidate)
    (define history (map (λ (y) (sort y <)) hist))
    (not (member candidate history)))
  (define sorted-candidates (map (λ (x) (sort x <)) candidates))
  
  (filter not-in-history sorted-candidates))

(define (remove-redundancies l)
  (remove-duplicates l #:key (λ (x) (sort x <))))

(define (make-candidates barrier acc)
  (if (list-equals (car acc) barrier) acc
      (make-candidates barrier (bump-next barrier acc))))

(define (list-equals x y)
  (cond
    [(and (empty? x) (empty? y)) #t]
    [(empty? x) #f]
    [(empty? y) #f]
    [else (and (eq? (car x) (car y)) (list-equals (cdr x) (cdr y)))]))

(define (bump-next barrier acc)
  (define recent (car acc))
  (define (find-bump pos)
    (define cur (list-ref recent pos))
    (define cur-barrier (list-ref barrier pos))
    (if (< cur cur-barrier)
        pos
        (find-bump (add1 pos))))
  (define to-bump (find-bump 0))
  (cons (clear-and-bump to-bump recent) acc))

(define (clear-and-bump pos template)
  (define front (make-list pos 2))
  (define back (drop template pos))
  (bump (append front back) pos))

(define (route-second-dimension hist)
  (define recent (car hist))
  (define culprit (car recent))
  (define barrier (list culprit culprit))
  (define plan (build-plan barrier hist))
  (if (empty? plan)
      (display "done")
      (search-plan plan hist)))

(define (metasearch)
  (if (not (search '(2 2)))
      (display "nothing interesting found")
      (find-first-dimension (list (list 2 2)))))

(define (find-first-dimension hist)
  (define to-search (bump0 (car hist)))
  (if (search to-search)
      (find-first-dimension (cons to-search hist))
      (route-second-dimension hist)))

(define (search-plan plan hist)
  (when (empty? plan) (route hist))
  (define cur (car plan))
  (define search-result (search cur))
  (cond
    [(and (not search-result) (all-twos cur)) (display "done")]
    [(not search-result) (route hist)]
    [else (search-plan (cdr plan) (cons cur hist))]))

(define (all-twos l)
  (empty? (filter (λ (x) (not (= 2 x))) l)))

(define (build-plan barrier hist)
  (define candidates (make-candidates barrier (list (make-list (length barrier) 2))))
  (define filtered (filter-candidates candidates hist))
  (map reverse (reverse (remove-redundancies filtered))))

(define (route hist)
  (define recent (car hist))
  (define last (list-ref recent (sub1 (length recent))))
  (define barrier (append recent (list last)))
  (define plan (build-plan barrier hist))
  (when (empty? plan) (display "done"))
  (search-plan plan hist))

(metasearch) 
