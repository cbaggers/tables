(in-package :tables)

;;------------------------------------------------------------

;; functions & columns/sequences to resolve

(define-packed-type u8 ((unsigned-byte 8)
                        :ffi-type :uint8)
  8)

(define-packed-type u16 ((unsigned-byte 16)
                         :ffi-type :uint16)
  16)

(define-packed-type u32 ((unsigned-byte 32)
                         :ffi-type :uint32)
  32)

(define-packed-type u64 ((unsigned-byte 64)
                         :ffi-type :uint64)
  64)

(define-packed-type i8 ((signed-byte 8)
                        :ffi-type :int8)
  8)

(define-packed-type i16 ((signed-byte 16)
                         :ffi-type :int16)
  16)

(define-packed-type i32 ((signed-byte 32)
                         :ffi-type :int32)
  32)

(define-packed-type i64 ((signed-byte 64)
                         :ffi-type :int64)
  64)

(define-packed-type f32 (single-float
                         :ffi-type :float)
  (1 sign)
  (8 exponent)
  (23 mantissa))

(define-packed-type f64 (single-float
                         :ffi-type :double)
  (1 sign)
  (11 exponent)
  (52 mantissa))

;;------------------------------------------------------------


#+nil
(progn
  (define-packed-type 3bit (nil)
    3)

  (define-packed-type foo ((unsigned-byte 64)
                           :ffi-type :uint64)
    5
    (3bit threefiddy)
    (56 body))

  (unpack-as (unsigned-byte 8) some-foo '3bit)
  (unpack-into place some-foo '3bit))
