#lang racket
(require 2htdp/batch-io)
(require math)

(define example-string (read-file "example.txt"))
(define tokenized (string-split example-string))
(define unique (remove-duplicates tokenized))
(define numbers (range (length unique)))
(define corr (make-immutable-hash (map (lambda (x y) (cons x y)) numbers unique)))
(define rev-corr (make-immutable-hash (map (lambda (x y) (cons y x)) numbers unique)))
(define example (map (lambda (x) (hash-ref rev-corr x)) tokenized))

(define (convert-back xs)
  (map (lambda (x) (hash-ref corr x)) xs))

(provide convert-back)
(provide example)

(define arr (array-reshape (array #['a 'b 'c 'd 'e 'f 'g 'h]) #(2 2 2)))

(define p1 (array-axis-swap arr 1 2))
(define p2 (array-axis-swap arr 0 2))

(array->list* (array-reshape arr #(4 2)))
(array->list* (array-reshape p1 #(4 2)))
(array->list* (array-reshape p2 #(4 2)))
