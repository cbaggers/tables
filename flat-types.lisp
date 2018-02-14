(in-package :tables)

;;------------------------------------------------------------

;; removed lisp-type & ffi-type. The lisp-type equivelence should
;; be made though some kind of implcicit casting function and the
;; ffi-type is irrelevent as the size of this will just be the sum
;; of the sizes of the parts, rounded up to the nearest machine type
;; size.
(defmacro define-packed-type (name (&key) &body parts)
  ;; parts are ordered from most to least significant bits
  (declare (ignore name parts))
  nil)

;; - needs some kind of default alignment info
(define-packed-type f32 ()
  (1 sign)
  (8 exponent)
  (23 mantissa))

;;------------------------------------------------------------

;; stride & alignment kinda seem like column/sequence parameters. an f32 is
;; still an f32 if packed inside an i64.. it's just not a very accessible one.
;; In functions the accessors should abstract the read/write and any alignment
;; involved.
;; due to this size & alignment were removed from these macros

(defmacro define-data-trait (name (&key)
                            &body slots)
  (declare (ignore name slots))
  nil)

;; in ffi impls maybe the name is really a formality, all layouts with the same
;; offset, alignment, etc values are the same.
(defmacro define-data-ffi-impl (name satisfies &body accessors)
  (declare (ignore name satisfies accessors))
  nil)

(defmacro define-data-lisp-impl (lisp-type satisfies constructor-function
                                 &body accessors)
  (declare (ignore lisp-type satisfies accessors))
  nil)


;;------------------------------------------------------------
;; it's a mask, we will try to pack this with other sub-word
;; sized data. tables can be clustered on enum values.
;;
;; define-enum should, when recompiled, keep current values valid
;; this means not reassigning all the values. If a value is removed
;; keep track of this value so it can be reused before extending
;; the bit-length of the type.
;;

(defmacro define-enum (name &body constants)
  (declare (ignore name constants))
  nil)

;; same syntax as enum but each entry can have multiple flags
;; so flag values increase by power of 2 (1,2,4,8,etc) whereas
;; enums increase by 1
;;
;; Also it is valid to a row to have 0 for flags (no flags) but
;; that is not legal for enums. Enum column entries must always
;; be one of the entries.
(defmacro define-flags (name &body constants)
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

;; TODO: gotta think about endianess and unions
