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

(define-trait-impl rtg-math.types:vec2 vec2
  (x v:x)
  (y v:y))


(define-trait-impl rtg-math.types:vec3 vec3
  (x v:x)
  (y v:y)
  (z v:z))


(define-trait-impl rtg-math.types:vec4 vec4
  (x v:x)
  (y v:y)
  (z v:z)
  (w v:w))

;;------------------------------------------------------------
