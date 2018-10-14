(in-package :gdenuf)

;;------------------------------------------------------------

(defun lscpu ()
  "(CPU Core Socket (L1d L1i L2 L3))"
  (let* ((str (with-output-to-string (s)
                (uiop:run-program "lscpu -pcpu,core,socket,cache" :output s)
                s))
         (data-strs (remove-if (lambda (s)
                                 (or (= (length s) 0)
                                     (char= (char s 0) #\#)))
                               (split-sequence #\newline str))))
    (mapcar (lambda (s)
              (let ((l (split-sequence #\, s)))
                (list (parse-integer (first l))
                      (parse-integer (second l))
                      (parse-integer (third l))
                      (mapcar #'parse-integer
                              (split-sequence #\: (fourth l))))))
            data-strs)))

;;------------------------------------------------------------

(cffi:defcfun ("sysconf" sysconf) :long
  (name :int))

(defun cache-sizes ()
  (let ((l1_icache_size 185)
        ;; (l1_icache_assoc 186)
        (l1_icache_linesize 187)

        (l1_dcache_size 188)
        ;; (l1_dcache_assoc 189)
        (l1_dcache_linesize 190)

        (l2_cache_size 191)
        ;; (l2_cache_assoc 192)
        (l2_cache_linesize 193)

        (l3_cache_size 194)
        ;; (l3_cache_assoc 195)
        (l3_cache_linesize 196)

        (l4_cache_size 197)
        ;; (l4_cache_assoc 198)
        (l4_cache_linesize 199))
    (list (list (list (sysconf l1_icache_size)
                      (sysconf l1_icache_linesize))
                (list (sysconf l1_dcache_size)
                      (sysconf l1_dcache_linesize)))
          (list (sysconf l2_cache_size)
                (sysconf l2_cache_linesize))
          (list (sysconf l3_cache_size)
                (sysconf l3_cache_linesize))
          (list (sysconf l4_cache_size)
                (sysconf l4_cache_linesize)))))


;;------------------------------------------------------------

(defun cpu-info ()
  (let* ((caches (cache-sizes))
         (cpus (lscpu))
         (cpu-objs
          (mapcar (lambda (cpu)
                    (destructuring-bind (id core socket
                                            (&optional (l1ishare-id :unknown)
                                                       (l1dshare-id :unknown)
                                                       (l2share-id :unknown)
                                                       (l3share-id :unknown)))
                        cpu
                      (destructuring-bind (((l1i-size l1i-llen) (l1d-size l1d-llen))
                                           (l2-size l2-llen)
                                           (l3-size l3-llen)
                                           (l4-size l4-llen))
                          caches
                        (let* ((l1 (list
                                    (make-instance
                                     'cache
                                     :size l1i-size
                                     :line-size l1i-llen
                                     :share-id l1ishare-id)
                                    (make-instance
                                     'cache
                                     :size l1d-size
                                     :line-size l1d-llen
                                     :share-id l1dshare-id)))
                               (l2 (make-instance
                                    'cache
                                    :size l2-size
                                    :line-size l2-llen
                                    :share-id l2share-id))
                               (l3 (when (> l3-size 0)
                                     (make-instance
                                      'cache
                                      :size l3-size
                                      :line-size l3-llen
                                      :share-id l3share-id)))
                               (l4 (when (> l4-size 0)
                                     (make-instance
                                      'cache
                                      :size l4-size
                                      :line-size l4-llen
                                      :share-id :unknown))))
                          (make-instance
                           'cpu
                           :id id
                           :core core
                           :socket socket
                           :caches (remove nil (list l1 l2 l3 l4)))))))
                  cpus)))
    (make-instance
         'cpu-info
         :logical-cpu-count (length cpu-objs)
         :physical-cpu-count (length
                              (remove-duplicates
                               (mapcar (lambda (x) (slot-value x 'core))
                                       cpu-objs)))
         :cpus cpu-objs)))

;;------------------------------------------------------------
