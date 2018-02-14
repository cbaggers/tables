(in-package :tables)

;;------------------------------------------------------------

(defvar *packed-types* (make-hash-table))

(defmacro define-packed-type (name (lisp-type &key ffi-type) &body parts)
  ;; parts are ordered from most to least significant bits
  (let* ((parts (process-packed-values name lisp-type ffi-type parts))
         (def (make-instance 'packed-type-spec
                             :name name
                             :lisp-type lisp-type
                             :ffi-type ffi-type
                             :parts parts)))
    (setf (gethash name *packed-types*) def)
    `(progn
       (setf (gethash ',name *packed-types*) ,def)
       ',name)))

(defun process-packed-values (name lisp-type ffi-type parts)
  (declare (ignore name lisp-type ffi-type))
  (loop :for part :in parts :collect
     (destructuring-bind (size &optional name) (ensure-list part)
       (make-instance 'packed-part
                      :name name
                      :size size))))

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
  (x f32)
  (y f32))

#+nil
(define-flat-data vec3 ()
  (x f32)
  (y f32)
  (z f32))

#+nil
(define-flat-data vec4 ()
  (x f32)
  (y f32)
  (z f32)
  (w f32))

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

;;------------------------------------------------------------

#||

We provide some types that map directly to lisp types (or just used lisp types)
and then provide palletes of wordily named (potentially target specific) types.

ieee754-32bit-floating-point           <- usually single-float
two-complement-wrapping-32bit-signed-integer  <- wrapping signed integer
two-complement-undefined-out-of-bounds-32bit-signed-integer  <- non-wrapping signed integer
two-complement-wrapping-32bit-unsigned-integer  <- wrapping signed integer
two-complement-undefined-out-of-bounds-32bit-unsigned-integer  <- non-wrapping signed integer

It is then up to the user to define aliases to those types for their projects
(we can obviously make libraries for the common ones too)

On the platforms where ieee754-32bit-floating-point & single-float are
equivalent then all functions for single-float will work with
ieee754-32bit-floating-point too (and obviously visa versa) but on platforms
where they arent they wont. This allows for picking a different add when
requiring wrapping on platforms that dont have it.

OR

We dont 'provide some types that map directly to lisp types' we provide the
tables types and then allow the definition of coversion operators on those.

1 set of ptr<->val conversion operators to get to/from list & 1 set to get
to/from the ffi.


||#

#||

what is type of int literal? smallest type? most generic? both seem to have big
impacts on inference.

||#
