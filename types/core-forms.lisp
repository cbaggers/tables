(in-package :ttype)

(defmethod infer-form (context (name (eql 'progn)) body-forms)
  (print "infer progn")
  (let ((typed-forms
         (loop :for bform :in body-forms :collect
            (multiple-value-bind (typed-form new-context)
                (infer context bform)
              (setf context new-context)
              typed-form))))
    (values
     (if typed-forms
         `(the ,(type-of-typed-expression (car (last typed-forms)))
               (progn ,@typed-forms))
         `(the ,(make-void) (progn)))
     context)))
