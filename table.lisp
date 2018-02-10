(in-package :table)

(defmacro define-table (name &body columns)
  (declare (ignore columns))
  `(progn ',name))

(define-table test-table
  (position :type vec3)
  (rotation :type quaternion)
  (health :type (integer 0 100))
  (shot-recharge-time :type single-float))
