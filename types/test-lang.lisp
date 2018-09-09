(in-package :ttype)

(define-parameter-type integer
  :valid-p #'integerp
  :equal #'=)

(define-ttype boolean
  :custom-spec-data ((implements . (disposable))))

(define-ttype integer)

(define-ttype (unordered-set type))

(define-ttype (unordered-foo type size)
  :where ((size integer))
  :custom-spec-data ((implements . (disposable))))

(define-ttype disposable
  :purpose :constraint-only
  :satifies-this-p (lambda (this type-ref)
                     (declare (ignore this))
                     (let ((implements
                            (cdr (assoc 'implements
                                         (ttype-custom-data type-ref)))))
                       (find 'disposable implements))))
