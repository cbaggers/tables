(in-package :tables)

;;------------------------------------------------------------

(defmacro define-flat-type (name (&key packed) &body slots)
  (declare (ignore name slots))
  nil)

(defmacro define-enum (name &body constants)
  (declare (ignore name constants))
  nil)

;;------------------------------------------------------------

#+nil
(define-flat-type vec2 ()
  (x :type single-float)
  (y :type single-float))

#+nil
(define-flat-type vec3 ()
  (x :type single-float)
  (y :type single-float)
  (z :type single-float))

#+nil
(define-flat-type vec4 ()
  (x :type single-float)
  (y :type single-float)
  (z :type single-float)
  (w :type single-float))

#+nil
(define-enum entity-kinds
  player
  bat-enemy
  turtle-enemy)

;;------------------------------------------------------------

(defun type-spec->type-definiton (spec)
  spec)
