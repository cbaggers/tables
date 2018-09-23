(in-package :gdenuf)

#+(and :x86-64 (or :win32 :windows))
(progn
  (defctype pointer_64_int :unsigned-long)
  (defctype int8 :int8)
  (defctype pint8 (:pointer :int8))
  (defctype int16 :short)
  (defctype pint16 (:pointer :short))
  (defctype int32 :int)
  (defctype pint32 (:pointer :int))
  (defctype int64 :long-long)
  (defctype pint64 (:pointer :long-long))
  (defctype uint8 :unsigned-char)
  (defctype puint8 (:pointer :unsigned-char))
  (defctype uint16 :unsigned-short)
  (defctype puint16 (:pointer :unsigned-short))
  (defctype uint32 :unsigned-int)
  (defctype puint32 (:pointer :unsigned-int))
  (defctype uint64 :unsigned-long-long)
  (defctype puint64 (:pointer :unsigned-long-long))
  (defctype long32 :int)
  (defctype plong32 (:pointer :int))
  (defctype ulong32 :unsigned-int)
  (defctype pulong32 (:pointer :unsigned-int))
  (defctype dword32 :unsigned-int)
  (defctype pdword32 (:pointer :unsigned-int))
  (defctype int_ptr :long-long)
  (defctype pint_ptr (:pointer :long-long))
  (defctype uint_ptr :unsigned-long-long)
  (defctype puint_ptr (:pointer :unsigned-long-long))
  (defctype long_ptr :long-long)
  (defctype plong_ptr (:pointer :long-long))
  (defctype ulong_ptr :unsigned-long-long)
  (defctype pulong_ptr (:pointer :unsigned-long-long))
  (defctype shandle_ptr :long-long)
  (defctype handle_ptr :unsigned-long-long)
  (defctype uhalf_ptr :unsigned-int)
  (defctype puhalf_ptr (:pointer :unsigned-int))
  (defctype half_ptr :int)
  (defctype phalf_ptr (:pointer :int))
  (defctype psize_t (:pointer ulong_ptr))
  (defctype ssize_t long_ptr)
  (defctype pssize_t (:pointer long_ptr))
  (defctype dword_ptr ulong_ptr)
  (defctype pdword_ptr (:pointer ulong_ptr))
  (defctype long64 :long-long)
  (defctype plong64 (:pointer :long-long))
  (defctype ulong64 :unsigned-long-long)
  (defctype pulong64 (:pointer :unsigned-long-long))
  (defctype dword64 :unsigned-long-long)
  (defctype pdword64 (:pointer :unsigned-long-long))
  (defctype kaffinity ulong_ptr)
  (defctype pkaffinity (:pointer kaffinity)))

#+(and :x86 (not :x86-64) (or :win32 :windows))
(progn
  (defctype pointer_64_int :unsigned-long)
  (defctype int8 :int8)
  (defctype pint8 (:pointer :int8))
  (defctype int16 :short)
  (defctype pint16 (:pointer :short))
  (defctype int32 :int)
  (defctype pint32 (:pointer :int))
  (defctype int64 :long-long)
  (defctype pint64 (:pointer :long-long))
  (defctype uint8 :unsigned-char)
  (defctype puint8 (:pointer :unsigned-char))
  (defctype uint16 :unsigned-short)
  (defctype puint16 (:pointer :unsigned-short))
  (defctype uint32 :unsigned-int)
  (defctype puint32 (:pointer :unsigned-int))
  (defctype uint64 :unsigned-long-long)
  (defctype puint64 (:pointer :unsigned-long-long))
  (defctype long32 :int)
  (defctype plong32 (:pointer :int))
  (defctype ulong32 :unsigned-int)
  (defctype pulong32 (:pointer :unsigned-int))
  (defctype dword32 :unsigned-int)
  (defctype pdword32 (:pointer :unsigned-int))
  (defctype int_ptr :int)
  (defctype pint_ptr (:pointer :int))
  (defctype uint_ptr :unsigned-int)
  (defctype puint_ptr (:pointer :unsigned-int))
  (defctype long_ptr :long)
  (defctype plong_ptr (:pointer :long))
  (defctype ulong_ptr :unsigned-long)
  (defctype pulong_ptr (:pointer :unsigned-long))
  (defctype uhalf_ptr :unsigned-short)
  (defctype puhalf_ptr (:pointer :unsigned-short))
  (defctype half_ptr :short)
  (defctype phalf_ptr (:pointer :short))
  (defctype shandle_ptr :long)
  (defctype handle_ptr :unsigned-long)
  (defctype psize_t (:pointer ulong_ptr))
  (defctype ssize_t long_ptr)
  (defctype pssize_t (:pointer long_ptr))
  (defctype dword_ptr ulong_ptr)
  (defctype pdword_ptr (:pointer ulong_ptr))
  (defctype long64 :long-long)
  (defctype plong64 (:pointer :long-long))
  (defctype ulong64 :unsigned-long-long)
  (defctype pulong64 (:pointer :unsigned-long-long))
  (defctype dword64 :unsigned-long-long)
  (defctype pdword64 (:pointer :unsigned-long-long))
  (defctype kaffinity ulong_ptr)
  (defctype pkaffinity (:pointer kaffinity)))
