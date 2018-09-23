(in-package :gdenuf)

;; (define-foreign-library kernel32
;;   (:windows "C:/WINDOWS/system32/kernel32.dll"))
;;
;; (use-foreign-library kernel32)


;; sysctlbyname(const char* name,
;;              void* oldp,
;;              size_t* oldlenp,
;;              void* newp,
;;              size_t newlen)

(defcfun ("sysctlbyname" Sysctlbyname) :int
  (name :string)
  (oldp :pointer)
  (oldlenp (:pointer size-t))
  (newp :pointer)
  (newlen size-t))

;; error just means not supported, so should not freak out here ↓↓

(defun get-len (name)
  (with-foreign-string (cname name)
    (with-foreign-object (old-len 'size-t)
      (let ((err (sysctlbyname cname (null-pointer) old-len (null-pointer) 0)))
        (if (= err 0)
            (mem-aref old-len 'size-t)
            (error "failed to query size for ~s" name))))))

;; :cores   hw.physicalcpu_max  :int
;; :sockets hw.packages         :int
;; :threads hw.logicalcpu_max   :int
;;
;; hw.pagesize :int64
;;
;; hw.l1dcachesize :int64
;; hw.l1icachesize :int64
;; hw.l2cachesize :int64
;; hw.l3cachesize :int64
;; hw.cachelinesize :int64


;; sysctl.h
;; Parameters that are byte counts or frequencies are 64 bit numbers.
;; * All other parameters are 32 bit numbers.

(defun foo ()
  (let* ((name "hw.l1dcachesize")
         (len (get-len name)))
    (with-foreign-string (cname name)
      (with-foreign-objects ((old-len 'size-t)
                             (oldp :uint8 len))
        (let ((err (sysctlbyname name oldp old-len (null-pointer) 0)))
          (if (= err 0)
              (values (mem-aref oldp :int64)
                      len
                      (mem-aref old-len 'size-t))
              (error "damn ~a" err)))))))

;; cant get info on which cpus are logical and which are physical
;; https://github.com/oshi/oshi/issues/99#issuecomment-158534801

;;
;; :threads-per-cache  hw.cacheconfig      (:array :uint64 n)
;; index 0 = memory
;; index 1 and up are how many threads share the cache at level n
(defun foo ()
  (let* ((name "hw.cacheconfig")
         (len (get-len name)))
    (with-foreign-string (cname name)
      (with-foreign-objects ((old-len 'size-t)
                             (oldp :uint8 len))
        (let ((err (sysctlbyname name oldp old-len (null-pointer) 0)))
          (if (= err 0)
              (values (loop
                         :for i :below (/ (mem-aref old-len 'size-t)
                                          (foreign-type-size :uint64))
                         :collect (mem-aref oldp :uint64 i))
                      len
                      (mem-aref old-len 'size-t))
              (error "damn ~a" err)))))))
