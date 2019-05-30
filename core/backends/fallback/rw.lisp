(in-package :tables.backends.fallback)

(define-value-rw-emitters (i8 ptr idx value) fallback
  :read `(cffi:mem-ref ,ptr :int8 ,idx)
  :write `(setf (cffi:mem-ref ,ptr :int8 ,idx) ,value))

(define-value-rw-emitters (u8 ptr idx value) fallback
  :read `(cffi:mem-ref ,ptr :uint8 ,idx)
  :write `(setf (cffi:mem-ref ,ptr :uint8 ,idx) ,value))

(define-value-rw-emitters (i16 ptr idx value) fallback
  :read `(cffi:mem-ref ,ptr :int16 ,idx)
  :write `(setf (cffi:mem-ref ,ptr :int16 ,idx) ,value))

(define-value-rw-emitters (u16 ptr idx value) fallback
  :read `(cffi:mem-ref ,ptr :uint16 ,idx)
  :write `(setf (cffi:mem-ref ,ptr :uint16 ,idx) ,value))

(define-value-rw-emitters (i32 ptr idx value) fallback
  :read `(cffi:mem-ref ,ptr :int32 ,idx)
  :write `(setf (cffi:mem-ref ,ptr :int32 ,idx) ,value))

(define-value-rw-emitters (u32 ptr idx value) fallback
  :read `(cffi:mem-ref ,ptr :uint32 ,idx)
  :write `(setf (cffi:mem-ref ,ptr :uint32 ,idx) ,value))

(define-value-rw-emitters (i64 ptr idx value) fallback
  :read `(cffi:mem-ref ,ptr :int64 ,idx)
  :write `(setf (cffi:mem-ref ,ptr :int64 ,idx) ,value))

(define-value-rw-emitters (u64 ptr idx value) fallback
  :read `(cffi:mem-ref ,ptr :uint64 ,idx)
  :write `(setf (cffi:mem-ref ,ptr :uint64 ,idx) ,value))


(define-value-rw-emitters (f32 ptr idx value) fallback
  :read `(cffi:mem-ref ,ptr :float ,idx)
  :write `(setf (cffi:mem-ref ,ptr :float ,idx) ,value))

:b1
:b8
