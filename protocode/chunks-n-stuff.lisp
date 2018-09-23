
;; just thinking out loud

(defconstant _sc_arg_max 0)
(defconstant _sc_child_max 1)
(defconstant _sc_clk_tck 2)
(defconstant _sc_ngroups_max 3)
(defconstant _sc_open_max 4)
(defconstant _sc_stream_max 5)
(defconstant _sc_tzname_max 6)
(defconstant _sc_job_control 7)
(defconstant _sc_saved_ids 8)
(defconstant _sc_realtime_signals 9)
(defconstant _sc_priority_scheduling 10)
(defconstant _sc_timers 11)
(defconstant _sc_asynchronous_io 12)
(defconstant _sc_prioritized_io 13)
(defconstant _sc_synchronized_io 14)
(defconstant _sc_fsync 15)
(defconstant _sc_mapped_files 16)
(defconstant _sc_memlock 17)
(defconstant _sc_memlock_range 18)
(defconstant _sc_memory_protection 19)
(defconstant _sc_message_passing 20)
(defconstant _sc_semaphores 21)
(defconstant _sc_shared_memory_objects 22)
(defconstant _sc_aio_listio_max 23)
(defconstant _sc_aio_max 24)
(defconstant _sc_aio_prio_delta_max 25)
(defconstant _sc_delaytimer_max 26)
(defconstant _sc_mq_open_max 27)
(defconstant _sc_mq_prio_max 28)
(defconstant _sc_version 29)
(defconstant _sc_pagesize 30)
(defconstant _sc_rtsig_max 31)
(defconstant _sc_sem_nsems_max 32)
(defconstant _sc_sem_value_max 33)
(defconstant _sc_sigqueue_max 34)
(defconstant _sc_timer_max 35)
(defconstant _sc_bc_base_max 36)
(defconstant _sc_bc_dim_max 37)
(defconstant _sc_bc_scale_max 38)
(defconstant _sc_bc_string_max 39)
(defconstant _sc_coll_weights_max 40)
(defconstant _sc_equiv_class_max 41)
(defconstant _sc_expr_nest_max 42)
(defconstant _sc_line_max 43)
(defconstant _sc_re_dup_max 44)
(defconstant _sc_charclass_name_max 45)
(defconstant _sc_2_version 46)
(defconstant _sc_2_c_bind 47)
(defconstant _sc_2_c_dev 48)
(defconstant _sc_2_fort_dev 49)
(defconstant _sc_2_fort_run 50)
(defconstant _sc_2_sw_dev 51)
(defconstant _sc_2_localedef 52)
(defconstant _sc_pii 53)
(defconstant _sc_pii_xti 54)
(defconstant _sc_pii_socket 55)
(defconstant _sc_pii_internet 56)
(defconstant _sc_pii_osi 57)
(defconstant _sc_poll 58)
(defconstant _sc_select 59)
(defconstant _sc_uio_maxiov 60)
(defconstant _sc_pii_internet_stream 61)
(defconstant _sc_pii_internet_dgram 62)
(defconstant _sc_pii_osi_cots 63)
(defconstant _sc_pii_osi_clts 64)
(defconstant _sc_pii_osi_m 65)
(defconstant _sc_t_iov_max 66)
(defconstant _sc_threads 67)
(defconstant _sc_thread_safe_functions 68)
(defconstant _sc_getgr_r_size_max 69)
(defconstant _sc_getpw_r_size_max 70)
(defconstant _sc_login_name_max 71)
(defconstant _sc_tty_name_max 72)
(defconstant _sc_thread_destructor_iterations 73)
(defconstant _sc_thread_keys_max 74)
(defconstant _sc_thread_stack_min 75)
(defconstant _sc_thread_threads_max 76)
(defconstant _sc_thread_attr_stackaddr 77)
(defconstant _sc_thread_attr_stacksize 78)
(defconstant _sc_thread_priority_scheduling 79)
(defconstant _sc_thread_prio_inherit 80)
(defconstant _sc_thread_prio_protect 81)
(defconstant _sc_thread_process_shared 82)
(defconstant _sc_nprocessors_conf 83)
(defconstant _sc_nprocessors_onln 84)
(defconstant _sc_phys_pages 85)
(defconstant _sc_avphys_pages 86)
(defconstant _sc_atexit_max 87)
(defconstant _sc_pass_max 88)
(defconstant _sc_xopen_version 89)
(defconstant _sc_xopen_xcu_version 90)
(defconstant _sc_xopen_unix 91)
(defconstant _sc_xopen_crypt 92)
(defconstant _sc_xopen_enh_i18n 93)
(defconstant _sc_xopen_shm 94)
(defconstant _sc_2_char_term 95)
(defconstant _sc_2_c_version 96)
(defconstant _sc_2_upe 97)
(defconstant _sc_xopen_xpg2 98)
(defconstant _sc_xopen_xpg3 99)
(defconstant _sc_xopen_xpg4 100)
(defconstant _sc_char_bit 101)
(defconstant _sc_char_max 102)
(defconstant _sc_char_min 103)
(defconstant _sc_int_max 104)
(defconstant _sc_int_min 105)
(defconstant _sc_long_bit 106)
(defconstant _sc_word_bit 107)
(defconstant _sc_mb_len_max 108)
(defconstant _sc_nzero 109)
(defconstant _sc_ssize_max 110)
(defconstant _sc_schar_max 111)
(defconstant _sc_schar_min 112)
(defconstant _sc_shrt_max 113)
(defconstant _sc_shrt_min 114)
(defconstant _sc_uchar_max 115)
(defconstant _sc_uint_max 116)
(defconstant _sc_ulong_max 117)
(defconstant _sc_ushrt_max 118)
(defconstant _sc_nl_argmax 119)
(defconstant _sc_nl_langmax 120)
(defconstant _sc_nl_msgmax 121)
(defconstant _sc_nl_nmax 122)
(defconstant _sc_nl_setmax 123)
(defconstant _sc_nl_textmax 124)
(defconstant _sc_xbs5_ilp32_off32 125)
(defconstant _sc_xbs5_ilp32_offbig 126)
(defconstant _sc_xbs5_lp64_off64 127)
(defconstant _sc_xbs5_lpbig_offbig 128)
(defconstant _sc_xopen_legacy 129)
(defconstant _sc_xopen_realtime 130)
(defconstant _sc_xopen_realtime_threads 131)
(defconstant _sc_advisory_info 132)
(defconstant _sc_barriers 133)
(defconstant _sc_base 134)
(defconstant _sc_c_lang_support 135)
(defconstant _sc_c_lang_support_r 136)
(defconstant _sc_clock_selection 137)
(defconstant _sc_cputime 138)
(defconstant _sc_thread_cputime 139)
(defconstant _sc_device_io 140)
(defconstant _sc_device_specific 141)
(defconstant _sc_device_specific_r 142)
(defconstant _sc_fd_mgmt 143)
(defconstant _sc_fifo 144)
(defconstant _sc_pipe 145)
(defconstant _sc_file_attributes 146)
(defconstant _sc_file_locking 147)
(defconstant _sc_file_system 148)
(defconstant _sc_monotonic_clock 149)
(defconstant _sc_multi_process 150)
(defconstant _sc_single_process 151)
(defconstant _sc_networking 152)
(defconstant _sc_reader_writer_locks 153)
(defconstant _sc_spin_locks 154)
(defconstant _sc_regexp 155)
(defconstant _sc_regex_version 156)
(defconstant _sc_shell 157)
(defconstant _sc_signals 158)
(defconstant _sc_spawn 159)
(defconstant _sc_sporadic_server 160)
(defconstant _sc_thread_sporadic_server 161)
(defconstant _sc_system_database 162)
(defconstant _sc_system_database_r 163)
(defconstant _sc_timeouts 164)
(defconstant _sc_typed_memory_objects 165)
(defconstant _sc_user_groups 166)
(defconstant _sc_user_groups_r 167)
(defconstant _sc_2_pbs 168)
(defconstant _sc_2_pbs_accounting 169)
(defconstant _sc_2_pbs_locate 170)
(defconstant _sc_2_pbs_message 171)
(defconstant _sc_2_pbs_track 172)
(defconstant _sc_symloop_max 173)
(defconstant _sc_streams 174)
(defconstant _sc_2_pbs_checkpoint 175)
(defconstant _sc_v6_ilp32_off32 176)
(defconstant _sc_v6_ilp32_offbig 177)
(defconstant _sc_v6_lp64_off64 178)
(defconstant _sc_v6_lpbig_offbig 179)
(defconstant _sc_host_name_max 180)
(defconstant _sc_trace 181)
(defconstant _sc_trace_event_filter 182)
(defconstant _sc_trace_inherit 183)
(defconstant _sc_trace_log 184)
(defconstant _sc_level1_icache_size 185)
(defconstant _sc_level1_icache_assoc 186)
(defconstant _sc_level1_icache_linesize 187)
(defconstant _sc_level1_dcache_size 188)
(defconstant _sc_level1_dcache_assoc 189)
(defconstant _sc_level1_dcache_linesize 190)
(defconstant _sc_level2_cache_size 191)
(defconstant _sc_level2_cache_assoc 192)
(defconstant _sc_level2_cache_linesize 193)
(defconstant _sc_level3_cache_size 194)
(defconstant _sc_level3_cache_assoc 195)
(defconstant _sc_level3_cache_linesize 196)
(defconstant _sc_level4_cache_size 197)
(defconstant _sc_level4_cache_assoc 198)
(defconstant _sc_level4_cache_linesize 199)
(defconstant _sc_iov_max _sc_uio_maxiov)

(defconstant +sc-nprocessors-onln+ 84)

(cffi:defcfun "sysconf" :long
  (name :int))

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
