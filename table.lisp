(in-package :tables)

;;------------------------------------------------------------

(defmacro define-table (name &body columns)
  (let ((definiton (make-table-definition name columns)))
    `(progn
       (register-table ,definition)
       ',name)))

#+nil
(define-table test-table
  (position :type vec3)
  (rotation :type quaternion)
  (health :type (integer 0 100))
  (shot-recharge-time :type single-float))

;;------------------------------------------------------------

(defun make-table-definition (name columns)
  (declare (ignore name columns))
  nil)

(defun register-table (definition)
  (declare (ignore definition))
  nil)
