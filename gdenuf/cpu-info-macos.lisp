(in-package :gdenuf)

;; sysctl.h
;;
;; sysctlbyname(const char* name,
;;              void* oldp,
;;              size_t* oldlenp,
;;              void* newp,
;;              size_t newlen)
;;
;; Parameters that are byte counts or frequencies are 64 bit numbers.
;; * All other parameters are 32 bit numbers.

(defcfun ("sysctlbyname" Sysctlbyname) :int
  (name :string)
  (oldp :pointer)
  (oldlenp (:pointer size-t))
  (newp :pointer)
  (newlen size-t))

(defun get-len (name)
  "nil means not supported"
  (with-foreign-string (cname name)
    (with-foreign-object (old-len 'size-t)
      (let ((err (sysctlbyname cname (null-pointer) old-len (null-pointer) 0)))
        (when (= err 0)
          (mem-aref old-len 'size-t))))))

(defun sysctl-int64 (name)
  (let ((len (get-len name)))
    (when len
      (with-foreign-string (cname name)
        (with-foreign-objects ((old-len 'size-t)
                               (val :int64))
          (let ((err (sysctlbyname name val old-len (null-pointer) 0)))
            (if (= err 0)
                (mem-aref val :int64)
                :unknown)))))))

(defun sysctl-array (name type)
  (let* ((len (get-len name)))
    (when len
      (with-foreign-string (cname name)
        (with-foreign-objects ((old-len 'size-t)
                               (oldp :uint8 len))
          (let ((err (sysctlbyname name oldp old-len (null-pointer) 0)))
            (if (= err 0)
                (values (loop
                           :for i :below (/ (mem-aref old-len 'size-t)
                                            (foreign-type-size type))
                           :collect (mem-aref oldp type i))
                        len
                        (mem-aref old-len 'size-t))
                (error "damn ~a" err))))))))

;; :cores   hw.physicalcpu_max  :int
;; :sockets hw.packages         :int
;; :threads hw.logicalcpu_max   :int


;; cant get info on which cpus are logical and which are physical
;; https://github.com/oshi/oshi/issues/99#issuecomment-158534801

;;
;; :threads-per-cache  hw.cacheconfig      (:array :uint64 n)
;; index 0 = memory
;; index 1 and up are how many threads share the cache at level n

(defun cpu-info ()
  (let* ((cache-line-size (sysctl-int64 "hw.cachelinesize"))
         (l1d-size (sysctl-int64 "hw.l1dcachesize"))
         (l1i-size (sysctl-int64 "hw.l1icachesize"))
         (l2-size (sysctl-int64 "hw.l2cachesize"))
         (l3-size (sysctl-int64 "hw.l3cachesize"))
         (cpu-count (sysctl-int64 "hw.logicalcpu_max"))
         (phys-count (sysctl-int64 "hw.physicalcpu_max"))

         (l1 (list
              (make-instance
               'cache
               :size l1i-size
               :line-size cache-line-size)
              (make-instance
               'cache
               :size l1d-size
               :line-size cache-line-size)))
         (l2 (make-instance
              'cache
              :size l2-size
              :line-size cache-line-size))
         (l3 (when (and l3-size (> l3-size 0))
               (make-instance
                'cache
                :size l3-size
                :line-size cache-line-size)))
         (cpus (loop
                  :for i :below cpu-count
                  :collect (make-instance
                            'cpu
                            :id i
                            :core :unknown
                            :socket :unknown
                            :caches (cons l1 (if l3 (list l2 l3) l2))))))
    (make-instance
     'cpu-info
     :logical-cpu-count cpu-count
     :physical-cpu-count phys-count
     :cpus cpu-objs)))
