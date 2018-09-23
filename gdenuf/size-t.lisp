(in-package #:gdenuf)

;; based on dump from autowrap, but in cffi instead
;;
;; original version
;; (autowrap:c-include
;;  "/usr/include/stdlib.h"
;;  :spec-path '(:gdenuf :autowrap-specs))

;; {TODO} would rather use size-t from impl where available,
;;        then we can keep this as a fallback.

;;------------------------------------------------------------
;; x86
;;
;; :bit-size 32 :bit-alignment 32

#+(and (not x86-64) x86 (not darwin) (or freebsd linux windows))
(defctype size-t :unsigned-int)

#+(or (and arm linux) ;; ← was what cl-autowrap gave, but is odd
      (and (not x86-64) x86 (or openbsd darwin)))
(defctype size-t :unsigned-long)


;;------------------------------------------------------------
;; x86-64
;;
;; :bit-size 64 :bit-alignment 64

#+(and x86-64 (not windows))
(defctype size-t :unsigned-long)

#+(and x86-64 windows)
(defctype size-t :unsigned-long-long)
