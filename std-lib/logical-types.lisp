(in-package :tables.lang)

;;------------------------------------------------------------

(define-ttype boolean)

;; I dont like these two
(define-value-type b1 (1))
(define-value-type b8 (8))

(define-ttype (or type-a type-b))
(define-ttype (and type-a type-b))

;;------------------------------------------------------------
