#lang racket

(struct tree (left data right) #:transparent)

(define (first-half l)
  (define middle-index (floor (/ (length l) 2)))
  (take l middle-index))

(module+ test
  (require rackunit)
  (check-equal? null (first-half null))
  (check-equal? null (first-half (list 1)))
  (check-equal? (list 1) (first-half (list 1 2)))
  (check-equal? (list 1) (first-half (list 1 2 3)))
  (check-equal? (list 1 2) (first-half (list 1 2 3 4))))

(define (middle l)
  (define middle-index (floor (/ (length l) 2)))
  (cond [(empty? l) null]
        [else (list-ref l middle-index)]))

(module+ test
  (require rackunit)
  (check-equal? null (middle null))
  (check-equal? 1 (middle (list 1)))
  (check-equal? 2 (middle (list 1 2)))
  (check-equal? 2 (middle (list 1 2 3)))
  (check-equal? 3 (middle (list 1 2 3 4))))


(define (second-half l)
  (define middle-index (+ 1 (floor (/ (length l) 2))))
  (cond [(< (length l) 2) null]
        [else (drop l middle-index)]))

(module+ test
  (require rackunit)
  (check-equal? null (second-half null))
  (check-equal? null (second-half (list 1)))
  (check-equal? null (second-half (list 1 2)))
  (check-equal? (list 3) (second-half (list 1 2 3)))
  (check-equal? (list 4) (second-half (list 1 2 3 4))))


(module+ test
  (require rackunit)
  (check-equal? null (flatten (list (first-half null) (middle null) (second-half null)))))

;(define (from-sorted-list l)
;  (cond [(= (length l) 1) (tree null (car l) null)]
;        [(= (length l) 2) (tree (tree null (car l) null) (second l) null)]
;        [(= (length l) 3) (tree (tree null (car l) null) (second l) (tree null (third l) null))]
;        [else (tree (from-sorted-list (first-half l)) (middle l) (from-sorted-list (second-half l)))]))
;
;(module+ test
;  (require rackunit)
;  (check-equal? (tree null 1 null) (from-sorted-list (list 1)))
;  (check-equal? (tree (tree null 1 null) 2 null) (from-sorted-list (list 1 2)))
;  (check-equal? (tree (tree null 1 null) 2 (tree null 3 null)) (from-sorted-list (list 1 2 3))))
