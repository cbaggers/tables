(in-package :gdenuf)

(define-foreign-library kernel32
  (:windows "C:/WINDOWS/system32/kernel32.dll"))

(use-foreign-library kernel32)

(defctype dword :unsigned-long)
(defctype word :unsigned-short)

(defcenum logical-processor-relationship
  :relation-processor-core
  :relation-numa-node
  :relation-cache
  :relation-processor-package
  :relation-group
  (:relation-all #xffff))

(defcstruct processor-core
  (flags :uint8))

(defcstruct numa-node
  (node-number dword))

(defcenum processor-cache-type
  :cache-unified
  :cache-instruction
  :cache-data
  :cache-trace)

(defcstruct cache-descriptor
  (level :uint8)
  (associativity :uint8)
  (line-size word)
  (size dword)
  (type processor-cache-type))

(defcunion dummy-slpi-union
  (processor-core (:struct processor-core))
  (numa-node (:struct numa-node))
  (cache (:struct cache-descriptor))
  (reserved0 (:array :unsigned-long-long 2)))


(defcstruct system-logical-processor-information
  (processor-mask (:pointer :ulong))
  (relationship logical-processor-relationship)
  (dummy-union (:union dummy-slpi-union)))

(defcfun
    ("GetLogicalProcessorInformation" get-logical-processor-information) :bool
  (buffer (:pointer (:struct system-logical-processor-information)))
  (return-length (:pointer dword)))

;; see example at link below for how to interpret data
;; https://msdn.microsoft.com/en-us/library/windows/desktop/ms683194(v=vs.85).aspx

(defun logical-cpu-info ()
  (labels ((parse-info (ptr)
             (with-foreign-slots
                 ((processor-mask relationship (:pointer dummy-union))
                  ptr (:struct system-logical-processor-information))
               (list
                relationship
                (pointer-address processor-mask)
                (case relationship
                  (:relation-processor-core
                   (mem-aref dummy-union '(:struct processor-core)))
                  (:relation-numa-node
                   (mem-aref dummy-union '(:struct numa-node)))
                  (:relation-cache
                   (mem-aref dummy-union '(:struct cache-descriptor)))
                  (:relation-processor-package)
                  (:relation-group)
                  (otherwise :invalid)))))
           (query-buf-len ()
             (with-foreign-object (len 'dword)
               (get-logical-processor-information (null-pointer) len)
               (mem-aref len 'dword))))
    (with-foreign-objects ((buffer :uint8 (query-buf-len))
                           (len 'dword))
      (when (get-logical-processor-information buffer len)
        (loop
           :for i :below (floor (mem-aref len 'dword) 32)
           :collect (parse-info (mem-aptr
                                 buffer
                                 '(:struct system-logical-processor-information)
                                 i)))))))

(defun cpu-info ()
  (let* ((info (logical-cpu-info))
         (sockets (remove :relation-processor-package info :key #'first
                          :test-not #'eq))
         (caches (remove :relation-cache info :key #'first
                         :test-not #'eq))
         (cores (remove :relation-processor-core info :key #'first
                        :test-not #'eq)))
    (labels ((caches-for (mask)
               (let* ((set (remove-if-not
                            (lambda (x) (> (logand mask (second x)) 0))
                            caches))
                      (l1i nil)
                      (l1d nil)
                      (objs (loop
                               :for c :in set
                               :for (nil
                                     type
                                     nil size nil line-size
                                     nil associativity nil level) := (third c)
                               :if (eq type :cache-data) :do
                               (setf l1d (make-instance 'cache
                                                        :size size
                                                        :line-size line-size))
                               :else :if (eq type :cache-instruction) :do
                               (setf l1i (make-instance 'cache
                                                        :size size
                                                        :line-size line-size))
                               :else :collect
                               (list level
                                     (make-instance 'cache
                                                    :size size
                                                    :line-size line-size))))
                      (objs (mapcar #'second (sort objs #'< :key #'first))))
                 (cons (list l1i l1d)
                       objs))))
      (let ((cpus (loop
                     :for core :in cores
                     :for id :from 0
                     :for (mask flags) := (rest core)
                     :collect (make-instance
                               'cpu
                               :id id ;; ← ↓ not sure about these
                               :core (floor (log mask 2))
                               :socket (position-if
                                        (lambda (x)
                                          (> (logand mask (second x)) 0))
                                        sockets)
                               :caches (caches-for mask)))))
        (make-instance
         'cpu-info
         :logical-cpu-count (length cpus)
         :physical-cpu-count (length
                              (remove-duplicates
                               (mapcar (lambda (x) (slot-value x 'core)) cpus)))
         :cpus cpus)))))
