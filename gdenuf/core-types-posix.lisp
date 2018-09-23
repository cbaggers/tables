(in-package #:gdenuf)

#+(or (and arm linux) ;; ← was what cl-autowrap gave, but is odd
      (and (not x86-64) x86 (or openbsd darwin)))
(progn
  (cffi:defctype ulong :unsigned-long)
  (cffi:defctype ushort :unsigned-short)
  (cffi:defctype uint :unsigned-int)
  (cffi:defctype u_int8_t :unsigned-char)
  (cffi:defctype u_int16_t :unsigned-short)
  (cffi:defctype u_int32_t :unsigned-int)
  (cffi:defctype u_int64_t :unsigned-long-long)
  (cffi:defctype register_t :int))

#+(and (not x86-64) x86 (not darwin) (or freebsd linux))
(progn
  (cffi:defctype ulong :unsigned-long)
  (cffi:defctype ushort :unsigned-short)
  (cffi:defctype uint :unsigned-int)
  (cffi:defctype u_int8_t :unsigned-char)
  (cffi:defctype u_int16_t :unsigned-short)
  (cffi:defctype u_int32_t :unsigned-int)
  (cffi:defctype u_int64_t :unsigned-long-long)
  (cffi:defctype register_t :int))

#+(and x86-64 (or darwin freebsd linux openbsd))
(progn
  (cffi:defctype ulong :unsigned-long)
  (cffi:defctype ushort :unsigned-short)
  (cffi:defctype uint :unsigned-int)
  (cffi:defctype u_int8_t :unsigned-char)
  (cffi:defctype u_int16_t :unsigned-short)
  (cffi:defctype u_int32_t :unsigned-int)
  (cffi:defctype u_int64_t :unsigned-long)
  (cffi:defctype register_t :long))
