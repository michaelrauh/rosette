#lang racket


(define (build-sliding xs)
  (cond
    [(< (length xs) 2) (list)]
    [else (cons (take xs 2) (build-sliding (drop xs 1)))]))

(define example-string "a b c d a c b d")
(define tokenized (string-split example-string))
(define unique (remove-duplicates tokenized))
(define numbers (range (length unique)))
(define assocs (map (lambda (x y) (cons x y)) numbers unique))
(define rev-assocs (map (lambda (x y) (cons y x)) numbers unique))
(define corr (make-immutable-hash assocs))
(define rev-corr (make-immutable-hash rev-assocs))
(define example (map (lambda (x) (hash-ref rev-corr x)) tokenized))

(define example-sliding (build-sliding example))

(define (make-example-sliding)
  example-sliding)

(provide make-example-sliding)