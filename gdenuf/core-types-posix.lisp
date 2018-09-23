(in-package #:gdenuf)

#+(or (and arm linux) ;; ← was what cl-autowrap gave, but is odd
      (and (not x86-64) x86 (or openbsd darwin)))
(progn
  (defctype ulong :unsigned-long)
  (defctype ushort :unsigned-short)
  (defctype uint :unsigned-int)
  (defctype u_int8_t :unsigned-char)
  (defctype u_int16_t :unsigned-short)
  (defctype u_int32_t :unsigned-int)
  (defctype u_int64_t :unsigned-long-long)
  (defctype register_t :int))

#+(and (not x86-64) x86 (not darwin) (or freebsd linux))
(progn
  (defctype ulong :unsigned-long)
  (defctype ushort :unsigned-short)
  (defctype uint :unsigned-int)
  (defctype u_int8_t :unsigned-char)
  (defctype u_int16_t :unsigned-short)
  (defctype u_int32_t :unsigned-int)
  (defctype u_int64_t :unsigned-long-long)
  (defctype register_t :int))

#+(and x86-64 (or darwin freebsd linux openbsd))
(progn
  (defctype ulong :unsigned-long)
  (defctype ushort :unsigned-short)
  (defctype uint :unsigned-int)
  (defctype u_int8_t :unsigned-char)
  (defctype u_int16_t :unsigned-short)
  (defctype u_int32_t :unsigned-int)
  (defctype u_int64_t :unsigned-long)
  (defctype register_t :long))
