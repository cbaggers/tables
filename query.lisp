(in-package #:tables)

;;------------------------------------------------------------

(defmacro define-query (name typed-in-args-&-uniforms out-args
                        &body body)
  (destructuring-bind (in-args uniforms)
      (process-query-args typed-in-args-&-uniforms)
    (check-query-compile-time in-args uniforms out-args body)
    `(progn
       (build-and-add-query ',in-args ',uniforms ',out-args ',body)
       ',name)))

(defun process-query-args (typed-args)
  (declare (ignore typed-args))
  '(nil nil))

(defun check-query-compile-time (in-args uniforms out-args body)
  (declare (ignore in-args uniforms out-args body))
  nil)

(defun build-and-add-query (in-args uniforms out-args body)
  (declare (ignore in-args uniforms out-args body))
  nil)

(define-query test ((a single-float) (b vec2)
                    &uniform
                    (time single-float))
    ((scaled vec2))
  (setf scaled (test a b)))

;;------------------------------------------------------------
