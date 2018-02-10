(in-package #:tables)

;;------------------------------------------------------------
;; The primitive operations that can be used in queries.
;; Whilst users can define these, they are more low level than
;; query-functions and can't be inlined or optimized.
;; Wherever possible query-functions should be used.

(defmacro define-query-op (name typed-args &body body)
  (destructuring-bind (in-args out-args)
      (process-query-op-args typed-args)
    (check-query-op-compile-time in-args out-args body)
    `(progn
       (build-and-add-query-op ',in-args ',out-args ',body)
       ',name)))

(defun process-query-op-args (typed-args)
  (declare (ignore typed-args))
  '(nil nil))

(defun check-query-op-compile-time (in-args out-args body)
  (declare (ignore in-args out-args body))
  nil)

(defun build-and-add-query-op (in-args out-args body)
  (declare (ignore in-args out-args body))
  nil)

(define-query-op test ((a single-float) (b vec2))
  blah
  blah
  blah)

;;------------------------------------------------------------
