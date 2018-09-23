(in-package :gdenuf)

;; going to ignore processor groups as assume less that 64 cores
;; https://docs.microsoft.com/en-us/windows/desktop/ProcThread/processor-groups

;; A thread affinity mask is a bit vector in which each bit represents a
;; logical processor that a thread is allowed to run on. A thread
;; affinity mask must be a subset of the process affinity mask for the
;; containing process of a thread. A thread can only run on the
;; processors its process can run on. Therefore, the thread affinity mask
;; cannot specify a 1 bit for a processor when the process affinity mask
;; specifies a 0 bit for that processor.


(defcfun ("GetCurrentProcess" get-current-process) handle)
(defcfun ("GetCurrentThread" get-current-thread) handle)

(defcfun ("GetProcessAffinityMask" getprocessaffinitymask) :bool
  (process handle)
  (process-affinity-mask pdword_ptr)
  (system-affinity-mask pdword_ptr))

;; returns old mask
(defcfun ("SetThreadAffinityMask" setthreadaffinitymask) dword
  (thread handle)
  (thread-affinity-mask dword_ptr))

(defun get-process-affinity-mask ()
  (with-foreign-objects ((process-affinity-mask 'dword_ptr)
                         (system-affinity-mask 'dword_ptr))
    (getprocessaffinitymask (get-current-process)
                            process-affinity-mask
                            system-affinity-mask)
    (values
     (mem-aref process-affinity-mask 'dword_ptr)
     (mem-aref system-affinity-mask 'dword_ptr))))

;; no nice way to query thread affinity, couple of options here:
;; https://stackoverflow.com/questions/6601862/query-thread-not-process-processor-affinity
