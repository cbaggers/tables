(in-package :tables.lang)

;;------------------------------------------------------------

(define-value-type f32 (32)
  (sign 1)
  (exponent 8)
  (mantissa 23))

(tables.internals:define-op-func f32+ (f32 f32) f32)
(tables.internals:define-op-func f32- (f32 f32) f32)
(tables.internals:define-op-func f32* (f32 f32) f32)
(tables.internals:define-op-func f32/ (f32 f32) f32)

(define-trait-impl addable () f32
  (+ f32+))

(define-trait-impl subtractable () f32
  (- f32-))

(define-trait-impl multiplyable () f32
  (* f32*))

(define-trait-impl dividable () f32
  (/ f32/))

;;------------------------------------------------------------

(tables.internals:define-op-func f32= (f32 f32) boolean)

(define-trait-impl partial-numeric-equality () f32
  (= f32=))

;;------------------------------------------------------------

(tables.internals:define-op-func f32-negate (f32) f32)

(define-trait-impl negatable () f32
  (negate f32-negate))

;;------------------------------------------------------------

(tables.internals:define-op-func f32-sqrt (f32) f32)

;;------------------------------------------------------------

(tables.internals:define-op-func f32-abs (f32) f32)

(define-trait-impl absolutable () f32
  (abs f32-abs))
