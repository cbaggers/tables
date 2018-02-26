(in-package :tables)

;;------------------------------------------------------------
;; Signedness is an operation thing, tables just cares about
;; bit layout. So u8 and i8 have identical definitions, the
;; user can then use/define functions to convert between them.

(define-data-type u8 (:packed t)
  8)

(define-data-type u16 (:packed t)
  16)

(define-data-type u32 (:packed t)
  32)

(define-data-type u64 (:packed t)
  64)

(define-data-type i8 (:packed t)
  8)

(define-data-type i16 (:packed t)
  16)

(define-data-type i32 (:packed t)
  32)

(define-data-type i64 (:packed t)
  64)

(define-data-type f32 (:packed t)
  (sign 1)
  (exponent 8)
  (mantissa 23))

(define-data-type f64 (:packed t)
  (sign 1)
  (exponent 11)
  (mantissa 52))

;;------------------------------------------------------------
;;
;; Provides the concept of vectors

(define-data-trait vec2 ()
  (x f32)
  (y f32))


(define-data-trait vec3 ()
  (x f32)
  (y f32)
  (z f32))


(define-data-trait vec4 ()
  (x f32)
  (y f32)
  (z f32)
  (w f32))

(define-data-trait quaternion ()
  (w f32)
  (x f32)
  (y f32)
  (z f32))

;;------------------------------------------------------------

(define-data-type soa-vec2 ()
  (x f32)
  (y f32))


(define-data-type soa-vec3 ()
  (x f32)
  (y f32)
  (z f32))


(define-data-type soa-vec4 ()
  (x f32)
  (y f32)
  (z f32)
  (w f32))

;;------------------------------------------------------------

;; Packed tightly packs data, often this can be an issue
;; but in this case we are ok as f32 put the next
;; element at a multiple of 16bits after each other, the
;; alignment restrictiosn of the functions will then push
;; the datatype to the correct alignment within the chunk

(define-data-type flat-vec2 (:packed t)
  (x f32)
  (y f32))


(define-data-type flat-vec3 (:packed t)
  (x f32)
  (y f32)
  (z f32))


(define-data-type flat-vec4 (:packed t)
  (x f32)
  (y f32)
  (z f32)
  (w f32))

;;------------------------------------------------------------
;;
;; Tells tables how a lisp type can conform to the trait. Not
;; sure how this helps us though as we will be storing data
;; in foreign blocks, not lisp arrays.. hmm maybe though as
;; we need to unpack into something.. probably helps more with
;; integer types than container types like this.

;; hmm, I think we may need a way to distinguish lisp types
;; from tables types.
;; or maybe just (subtypep 'rtg-math.types:vec2 t) will do.


;; These are incorrect, vec2 wants f32, not single-float
;; (define-trait-impl rtg-math.types:vec2 vec2
;;   (x v:x)
;;   (y v:y))
;;
;;
;; (define-trait-impl rtg-math.types:vec3 vec3
;;   (x v:x)
;;   (y v:y)
;;   (z v:z))
;;
;;
;; (define-trait-impl rtg-math.types:vec4 vec4
;;   (x v:x)
;;   (y v:y)
;;   (z v:z)
;;   (w v:w))

;;------------------------------------------------------------

#||

Hmm, so we have a conundrum. We have f32, which is an 32bit ieee-754 float
common lisp does not have this.

We have vec2, which is a data-trait which has f32 slots.

We cant represent an f32 in lisp.

Whilst we inline functions (and thus dont need to worry about calls for the
most part) we still need to be able to have f32 vars.

A given impl/machine combo may have a compatible type. For example sbcl's
single-float is an f32 (unless boxed). However once it is a single-float it
isnt valid to assume we can extract the f32 fields
Q: is it not?
A: No as we cant mask or shift a single-float

We could allow some implicit conversions for types known to be structurally
identical

(define-var-type f32 single-float (ptr val)
  :from (cffi:mem-aref ptr :float)
  :to (setf (cffi:mem-aref ptr :float) val))

I think we can get away with this, we can then pass f32 to funcs taking
single-float (and vice-versa)

blah blah drink all the gin



||#
