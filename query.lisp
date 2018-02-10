(in-package #:tables)

;;------------------------------------------------------------
;; By forcing the definition of outputs we can more easily work
;; backwards to identify every allocation in the query, we know
;; that there are no 'instances' of data that can be passed out
;; of anywhere else and no arrays or loops so we know exactly
;; what vars will be needed in the query, this lets us allocate
;; all of that before the query runs and reuse it across calls.

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
