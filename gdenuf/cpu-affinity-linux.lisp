(in-package :gdenuf)

;; all affinity 'funcs' in sched.h are just macros..which sucks ass.
;; cl-cpu-affinity certainly has the best current way of handling it
;; but requires a c compiler..ok for linux at least but wont be
;; save-lisp-and-die friendly (without code like we have in shipshape)
;;
;; hmm, given that sched_setaffinity takes an argument specifying the
;; mask size, and given that it is know that the cpu_set_t is a bitmask
;; then maybe we can just wing it and use our own mask.
