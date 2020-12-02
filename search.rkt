#lang rosette/safe

(require "helper.rkt")

(define example-sliding (make-example-sliding))
(define-symbolic a b c d integer?)
(define sol (solve (begin
                     (assert (tree-member? (list a b) example-sliding))
                     (assert (tree-member? (list c d) example-sliding))
                     (assert (tree-member? (list a c) example-sliding))
                     (assert (tree-member? (list b d) example-sliding))
                     (assert (not (equal? b c))))))

(define answer (evaluate (list a b c d) sol))
(convert-back answer)
