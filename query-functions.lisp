(in-package #:tables)

;;------------------------------------------------------------
;; Functions that can be used in queries. These will always be
;; inlined. The code is made of macros, query-ops, query-functions and
;; a small section of special-operators.
;;
;; We perform cascading recompiles when query-function op is redefined.

(defmacro define-query-function (name typed-args &body body)
  (let ((in-args (process-query-func-args typed-args)))
    (check-query-func-compile-time in-args body)
    `(progn
       (build-and-add-query-func ',in-args ',body)
       ',name)))

(defun process-query-func-args (typed-args)
  (declare (ignore typed-args))
  nil)

(defun check-query-func-compile-time (in-args body)
  (declare (ignore in-args body))
  nil)

(defun build-and-add-query-func (in-args body)
  (declare (ignore in-args body))
  nil)

(define-query-function test ((a single-float) (b vec2))
  (* b s))

;;------------------------------------------------------------
