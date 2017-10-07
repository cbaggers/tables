(in-package #:tables)

;;------------------------------------------------------------

(defmacro define-function-query
    (name (&rest uniforms &key &allow-other-keys) &body body)
  (let* ((proto (make-proto-func-query name uniforms body))
         (processed (validate-proto-query proto)))
    (etypecase processed
      (query (add-query processed)
             `(defun ,(name processed) (,@(mapcar #'name (uniforms processed)))
                ,@body))
      (error (error processed) nil))))

;;------------------------------------------------------------

(defun make-proto-func-query (name uniforms body)
  )

;;------------------------------------------------------------
