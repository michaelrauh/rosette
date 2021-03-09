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
(provide range)
(provide list-set)
(provide make-list)