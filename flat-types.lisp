(in-package :tables)

;;------------------------------------------------------------

(defvar *primitive-types* (make-hash-table))

(defmacro define-flat-primitive (name options &key size alignment lisp-type ffi-type)
  (assert (null options))
  (check-primtive-values name size alignment lisp-type ffi-type)
  (let ((def (make-instance 'flat-primitive-spec
                            :name name
                            :size size
                            :alignment alignment
                            :lisp-type lisp-type
                            :ffi-type ffi-type)))
    (setf (gethash name *primitive-types*) def)
    `(progn
       (setf (gethash ',name *primitive-types*) ,def)
       ',name)))

(defun check-primtive-values (name size alignment lisp-type ffi-type)
  (declare (ignore name size alignment lisp-type ffi-type))
  nil)

;;------------------------------------------------------------

(defmacro define-flat-data (name (&key)
                            &body slots)
  (declare (ignore name slots))
  nil)

;; with data layouts the name is really a formality, all layouts with the same
;; offset, alignment, etc values are the same.
;; We need to make a layout comparison function which will be used to specify
;; where a given layout can work for a given function. For example a function
;; that writes into a vec2 with 2 tightly packed f32 & 128bit alignment will
;; also work on a vec2 with 2 tightly packed f32 & 32bit alignment
;;
;; hmm, should alignment be in the type? stride & alignment kinda seem like
;; column/sequence parameters. an f32 is still an f32 if packed inside an i64..
;; it's just not a very accessible one.
;; In functions the accessors should abstract the read/write and any alignment
;; involved.
;;
;; due to ↑↑↑↑ size & alignment were removed from the top level decl of layout

(defmacro define-data-layout (name layout-family (satisfies &key packed-into)
                              &body slots)
  (declare (ignore name layout-family satisfies packed-into slots))
  nil)

#+nil
(define-flat-data vec2 ()
  (x :type f32)
  (y :type f32))

#+nil
(define-flat-data vec3 ()
  (x :type f32)
  (y :type f32)
  (z :type f32))

#+nil
(define-flat-data vec4 ()
  (x :type f32)
  (y :type f32)
  (z :type f32)
  (w :type f32))

#+nil
(define-data-layout vec2-128 aos (vec2)
  (x :offset 0)
  (y :offset 32))

#+nil
(define-data-layout vec3-128 aos (vec3)
  (x :offset 0)
  (y :offset 32)
  (z :offset 64))

#+nil
(define-data-layout vec4-128 aos (vec4)
  (x :offset 0)
  (y :offset 32)
  (z :offset 64)
  (w :offset 96))

#+nil
(define-data-layout vec2-32 aos (vec2)
  (x :offset 0)
  (y :offset 32))

#+nil
(define-data-layout vec3-32 aos (vec3)
  (x :offset 0)
  (y :offset 32)
  (z :offset 64))

#+nil
(define-data-layout vec4-32 aos (vec4)
  (x :offset 0)
  (y :offset 32)
  (z :offset 64)
  (w :offset 96))

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
(define-enum entity-kinds
  player
  bat-enemy
  turtle-enemy)

;;------------------------------------------------------------

(defun type-spec->type-definiton (spec)
  spec)
