(in-package :gdenuf)

;; all affinity 'funcs' in sched.h are just macros..which sucks ass.
;; cl-cpu-affinity certainly has the best current way of handling it
;; but requires a c compiler..ok for linux at least but wont be
;; save-lisp-and-die friendly (without code like we have in shipshape)
;;
;; hmm, given that sched_setaffinity takes an argument specifying the
;; mask size, and given that it is know that the cpu_set_t is a bitmask
;; then maybe we can just wing it and use our own mask.

;; int sched_getaffinity (__pid_t __pid,
;;                        size_t __cpusetsize,
;;                        cpu_set_t* __cpuset);

;; ok so next problems is pid_t.. we need that.

;; ok, got that, can test this soon

(defcfun ("getpid" get-pid) pid_t)

(defcfun ("sched_getaffinity" sched_getaffinity) :int
  (pid pid_t)
  (set-size size_t)
  (set :pointer))

(defun get-process-affinity ()
  (with-foreign-object (set :uint64)
    (when (= (sched_getaffinity 0 (foreign-type-size :uint64) set) 0)
      (mem-aref set :uint64))))
