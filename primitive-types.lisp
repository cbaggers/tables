(in-package :tables)

;;------------------------------------------------------------

(define-flat-primitive u8 ()
  :size 8
  :alignment 8
  :lisp-type (unsigned-byte 8)
  :ffi-type :uint8)

(define-flat-primitive u16 ()
  :size 16
  :alignment 16
  :lisp-type (unsigned-byte 16)
  :ffi-type :uint16)

(define-flat-primitive u32 ()
  :size 32
  :alignment 32
  :lisp-type (unsigned-byte 32)
  :ffi-type :uint32)

(define-flat-primitive u64 ()
  :size 64
  :alignment 64
  :lisp-type (unsigned-byte 64)
  :ffi-type :uint64)

(define-flat-primitive i8 ()
  :size 8
  :alignment 8
  :lisp-type (signed-byte 8)
  :ffi-type :int8)

(define-flat-primitive i16 ()
  :size 16
  :alignment 16
  :lisp-type (signed-byte 16)
  :ffi-type :int16)

(define-flat-primitive i32 ()
  :size 32
  :alignment 32
  :lisp-type (signed-byte 32)
  :ffi-type :int32)

(define-flat-primitive i64 ()
  :size 64
  :alignment 64
  :lisp-type (signed-byte 64)
  :ffi-type :int64)

(define-flat-primitive f32 ()
  :size 32
  :alignment 32
  :lisp-type single-float
  :ffi-type :float)

(define-flat-primitive f64 ()
  :size 64
  :alignment 64
  :lisp-type double-float
  :ffi-type :double)

;;------------------------------------------------------------
