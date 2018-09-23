
;; just thinking out loud

;;------------------------------------------------------------

;; ~/Code/lisp/tables/protocode $ lscpu
;; Architecture:        x86_64
;; CPU op-mode(s):      32-bit, 64-bit
;; Byte Order:          Little Endian
;; CPU(s):              8
;; On-line CPU(s) list: 0-7
;; Thread(s) per core:  2
;; Core(s) per socket:  4
;; Socket(s):           1
;; NUMA node(s):        1
;; Vendor ID:           GenuineIntel
;; CPU family:          6
;; Model:               26
;; Model name:          Intel(R) Core(TM) i7 CPU         950  @ 3.07GHz
;; Stepping:            5
;; CPU MHz:             1607.017
;; CPU max MHz:         3068.0000
;; CPU min MHz:         1600.0000
;; BogoMIPS:            6147.62
;; Virtualisation:      VT-x
;; L1d cache:           32K
;; L1i cache:           32K
;; L2 cache:            256K
;; L3 cache:            8192K
;; NUMA node0 CPU(s):   0-7
;; Flags: fpu vme de pse tsc msr pae mce cx8 apic sep mtrr pge mca
;;        cmov pat pse36 clflush dts acpi mmx fxsr sse sse2 ss ht tm
;;        pbe syscall nx rdtscp lm constant_tsc arch_perfmon pebs bts
;;        rep_good nopl xtopology nonstop_tsc cpuid aperfmperf pni
;;        dtes64 monitor ds_cpl vmx est tm2 ssse3 cx16 xtpr pdcm
;;        sse4_1 sse4_2 popcnt lahf_lm pti ssbd ibrs ibpb stibp
;;        tpr_shadow vnmi flexpriority ept vpid dtherm ida flush_l1d

;;------------------------------------------------------------

;; combining lscpu (for flags, sockets, cores & threads per core) and
;; sysconf (for cache and cache-line sizes)

;;------------------------------------------------------------
;; cpu details

;; :linux "/proc/cpuinfo" - awesome
;; :osx sysctl http://fortysomethinggeek.blogspot.com/2012/11/getting-cpu-info-from-command-line-in.html - can do this from c, use cffi
;; c lib https://msdn.microsoft.com/en-us/library/windows/desktop/ms683194(v=vs.85).aspx
;; https://unix.stackexchange.com/questions/43539/what-do-the-flags-in-proc-cpuinfo-mean - will be handy for normalizing results from other platforms

;;------------------------------------------------------------

(defclass table ()
  ((clusters)
   (indexes)
   (info)))

(defclass chunk () ())

(defclass fixed-size-chunk ()
  ((rows :initform #())
   (total-size :initform 0)))


;;------------------------------------------------------------
;; obviously use structs, bt, channels, atomics, etc
;; cl-cpu-affinity is linux only but it's a nice api, try port
;; it places. cl-cpus has start of lin, mac & win code even though
;; it's only exposing #'get-number-of-processors

(defclass gud-jerb ()
  ((workers :initform #())))

(defun get-worker-with-shortest-queue (manager)
  (with-slots (workers) manager
    (let* ((best (aref workers 0))
           (est (slot-value best 'length-estimate)))
      (loop
         :for i :from 1 :below (length workers)
         :for w := (aref workers i)
         :for l := (slot-value w 'length-estimate)
         :when (< l est)
         :do (setf best w
                   est l))
      best)))

(defun enqueue-task (manager task)
  (let ((worker (get-worker-with-shortest-queue manager)))
    (with-slots (length-estimate queue) worker
      (incf length-estimate (slot-value task 'work-estimate))
      (push task queue)
      worker)))

(defclass task ()
  ((func)
   (work-estimate)))

(defclass worker ()
  ((queue) ;; will be a channel
   (length-estimate :initform 0)
   (thread) ;; bt thread
   (keep-running :initform t)))

(defun gj-worker-loop (worker)
  (with-slots (keep-running queue length-estimate) worker
    (loop
       :while keep-running
       :do (let ((task (pop queue)))
             (funcall task)
             (decf length-estimate (slot-value task 'work-estimate))))))

;;------------------------------------------------------------
