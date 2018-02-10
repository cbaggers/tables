(in-package :tables)

;;------------------------------------------------------------

(defmacro define-flat-type (name (&key packed) &body slots)
  (declare (ignore name slots))
  nil)

;;------------------------------------------------------------
;; it's a mask, we will try to pack this with other sub-word
;; sized data. tables can be clustered on enum values.
;;
;; define-enum should, when recompiled, keep current values valid
;; this means not reassigning all the values. If a value is removed
;; keep track of this value so it can be reused before extending
;; the bit-length of the type.

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
