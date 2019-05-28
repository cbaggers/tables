(in-package :tables.backends.fallback)

(define-value-rw-emitters (i8 ptr value) fallback
  :read `(cffi:mem-aref ,ptr :int8)
  :write `(setf (cffi:mem-aref ,ptr :int8) ,value))

(define-value-rw-emitters (u8 ptr value) fallback
  :read `(cffi:mem-aref ,ptr :uint8)
  :write `(setf (cffi:mem-aref ,ptr :uint8) ,value))

(define-value-rw-emitters (f32 ptr value) fallback
  :read `(cffi:mem-aref ,ptr :float)
  :write `(setf (cffi:mem-aref ,ptr :float) ,value))

:b1
:b8
:i16
:i32
:i64
:u16
:u32
:u64
