(in-package :ttype)

(define-ttype boolean)
(define-ttype integer)
(define-ttype (unordered-set type))
(define-ttype (unordered-foo type size)
  :where ((size integer)))

(define-ttype disposable
  :purpose :constraint-only
  :satifies-this-p (lambda (this type-ref)
                     (format t "ooo! ~a ~a"
                             this type-ref)
                     t))
