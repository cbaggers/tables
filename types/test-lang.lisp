(in-package :ttype)

(define-ttype boolean)
(define-ttype integer)
(define-ttype (unordered-set type))
(define-ttype (unordered-foo type size)
  :where ((size integer)))
