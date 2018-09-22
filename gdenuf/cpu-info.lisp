(in-package :gdenuf)

#+linux
(defun cpu-info ()
  (with-output-to-string (s)
    (uiop:run-program "lscpu -pcpu,core,socket,cache" :output s)
    s))

#+windows

(cffi:define-foreign-library kernel32
  (:windows "C:/WINDOWS/system32/kernel32.dll"))

(cffi:use-foreign-library kernel32)

(cffi:defctype dword :unsigned-long)
(cffi:defctype word :unsigned-short)

(cffi:defcenum logical-processor-relationship
  :relation-processor-core
  :relation-numa-node
  :relation-cache
  :relation-processor-package
  :relation-group
  (:relation-all #xffff))

(cffi:defcstruct processor-core
  (flags :uint8))

(cffi:defcstruct numa-node
  (node-number dword))

(cffi:defcenum processor-cache-type
  :cache-unified
  :cache-instruction
  :cache-data
  :cache-trace)

(cffi:defcstruct cache-descriptor
  (level :uint8)
  (associativity :uint8)
  (line-size word)
  (size dword)
  (type processor-cache-type))

(cffi:defcunion dummy-slpi-union
  (processor-core (:struct processor-core))
  (numa-node (:struct numa-node))
  (cache (:struct cache-descriptor))
  (reserved0 (:array :unsigned-long-long 2)))


(cffi:defcstruct system-logical-processor-information
  (processor-mask (:pointer :ulong))
  (relationship logical-processor-relationship)
  (dummy-union (:union dummy-slpi-union)))

(cffi:defcfun
    ("GetLogicalProcessorInformation" get-logical-processor-information) :bool
  (buffer (:pointer (:struct system-logical-processor-information)))
  (return-length (:pointer dword)))

;; see example at link below for how to interpret data
;; https://msdn.microsoft.com/en-us/library/windows/desktop/ms683194(v=vs.85).aspx

(defun cpu-info ()
  (labels ((parse-info (ptr)
             (cffi:with-foreign-slots
                 ((processor-mask relationship (:pointer dummy-union))
                  ptr (:struct system-logical-processor-information))
               (list
                relationship
                processor-mask
                (case relationship
                  (:relation-processor-core
                   (cffi:mem-aref dummy-union '(:struct processor-core)))
                  (:relation-numa-node
                   (cffi:mem-aref dummy-union '(:struct numa-node)))
                  (:relation-cache
                   (cffi:mem-aref dummy-union '(:struct cache-descriptor)))
                  (:relation-processor-package)
                  (:relation-group)
                  (otherwise :invalid)))))
           (query-buf-len ()
             (cffi:with-foreign-object (len 'dword)
               (get-logical-processor-information (cffi:null-pointer) len)
               (cffi:mem-aref len 'dword))))
    (cffi:with-foreign-objects ((buffer :uint8 (query-buf-len))
                                (len 'dword))
      (when (get-logical-processor-information buffer len)
        (loop
           :for i :below (floor (cffi:mem-aref len 'dword) 32)
           :collect (parse-info (cffi:mem-aptr
                                 buffer
                                 '(:struct system-logical-processor-information)
                                 i)))))))
