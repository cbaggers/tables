(in-package :gdenuf)

(defun cpu-info ()
  (with-output-to-string (s)
    (uiop:run-program "lscpu -pcpu,core,socket,cache" :output s)
    s))
