(in-package :tables)

;;------------------------------------------------------------

;; functions & columns/sequences to resolve

(define-data-type u8 ()
  8)

(define-data-type u16 ()
  16)

(define-data-type u32 ()
  32)

(define-data-type u64 ()
  64)

(define-data-type i8 ()
  8)

(define-data-type i16 ()
  16)

(define-data-type i32 ()
  32)

(define-data-type i64 ()
  64)

(define-data-type f32 ()
  (sign 1)
  (exponent 8)
  (mantissa 23))

(define-data-type f64 ()
  (sign 1)
  (exponent 11)
  (mantissa 52))

;;------------------------------------------------------------


#+nil
(progn
  (define-data-type 3bit (nil)
    3)

  (define-data-type foo ((unsigned-byte 64)
                           :ffi-type :uint64)
    5
    (3bit threefiddy)
    (56 body))

  (unpack-as (unsigned-byte 8) some-foo '3bit)
  (unpack-into place some-foo '3bit))
