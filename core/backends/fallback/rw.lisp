(in-package :tables.backends.fallback)

(define-value-rw-emitters (i8 ptr value) fallback
  :read `(cffi:mem-aref ,ptr :int8)
  :write `(setf (cffi:mem-aref ,ptr :int8) ,value))

(define-value-rw-emitters (u8 ptr value) fallback
  :read `(cffi:mem-aref ,ptr :uint8)
  :write `(setf (cffi:mem-aref ,ptr :uint8) ,value))

(define-value-rw-emitters (i16 ptr value) fallback
  :read `(cffi:mem-aref ,ptr :int16)
  :write `(setf (cffi:mem-aref ,ptr :int16) ,value))

(define-value-rw-emitters (u16 ptr value) fallback
  :read `(cffi:mem-aref ,ptr :uint16)
  :write `(setf (cffi:mem-aref ,ptr :uint16) ,value))

(define-value-rw-emitters (i32 ptr value) fallback
  :read `(cffi:mem-aref ,ptr :int32)
  :write `(setf (cffi:mem-aref ,ptr :int32) ,value))

(define-value-rw-emitters (u32 ptr value) fallback
  :read `(cffi:mem-aref ,ptr :uint32)
  :write `(setf (cffi:mem-aref ,ptr :uint32) ,value))

(define-value-rw-emitters (i64 ptr value) fallback
  :read `(cffi:mem-aref ,ptr :int64)
  :write `(setf (cffi:mem-aref ,ptr :int64) ,value))

(define-value-rw-emitters (u64 ptr value) fallback
  :read `(cffi:mem-aref ,ptr :uint64)
  :write `(setf (cffi:mem-aref ,ptr :uint64) ,value))


(define-value-rw-emitters (f32 ptr value) fallback
  :read `(cffi:mem-aref ,ptr :float)
  :write `(setf (cffi:mem-aref ,ptr :float) ,value))

:b1
:b8
