(in-package :ttype)

(defun hack-int-eq (x y)
  (print (list :oh x y))
  (print (= x y)))

(define-parameter-type integer
  :valid-p #'integerp
  :equal 'hack-int-eq)

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

(defun hack (designator)
  `(truly-the ,(designator->type designator) :NOUT)
  )
