#lang rosette/safe

(require "helper.rkt")

(define example-sliding (make-example-sliding))
(define-symbolic a b c d integer?)
(define xs (list a b))
(define sol (solve (begin
                     (assert (member (list a b) example-sliding))
                     (assert (member (list c d) example-sliding))
                     (assert (member (list a c) example-sliding))
                     (assert (member (list b d) example-sliding))
                     (assert (not (equal? b c))))))

(define answer (evaluate (list a b c d) sol))
(convert-back answer)
