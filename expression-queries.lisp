(in-package #:tables)

;;------------------------------------------------------------

(defmacro define-expression-query
    (name (&rest uniforms &key &allow-other-keys) &body expression)
  (let* ((proto (make-proto-expr-query name uniforms expression))
         (processed (validate-prototype proto)))
    (etypecase processed
      (query (add-query processed) nil)
      (error (error processed) nil))))

;;------------------------------------------------------------

(defun make-proto-expr-query (name uniforms expression)
  )

;;------------------------------------------------------------
