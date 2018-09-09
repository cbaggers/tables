(in-package :ttype)

(define-parameter-type integer
  :valid-p #'integerp
  :equal #'=)

(define-ttype boolean)
(define-ttype integer)
(define-ttype (unordered-set type))
(define-ttype (unordered-foo type size)
  :where ((size integer)))

(define-ttype disposable
  :purpose :constraint-only
  :satifies-this-p (lambda (this type-ref)
                     (declare (ignore this))
                     ;; ↓ this is just to test the logic, would
                     ;; ↓ usually look at the type to see it implements
                     ;; ↓ disposable somehow
                     (ttype-p type-ref 'boolean)))
