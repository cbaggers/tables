(in-package #:tables)

;;------------------------------------------------------------

(defclass failure ()
  ((message :initarg :message :accessor message)
   (data :initarg :data :accessor data)))

(defmethod print-object ((thing failure) stream)
  (let* ((len 30)
         (msg (message thing))
         (msg (if (< (length msg) len)
                  msg
                  (format nil "~a…" (subseq msg 0 len))))
         (msg (substitute #\space #\newline msg))
         (msg (string-left-trim '(#\space) msg)))
    (format stream "#<FAILURE ~s>" msg)))

(defun fail (pattern &rest data)
  (make-instance 'failure
                 :message (apply #'format nil pattern data)
                 :data data))

(defun collate-failures (list &optional header-pattern &rest data)
  (assert (listp list))
  (let ((failures (remove-if-not (lambda (x) (typep x 'failure)) list)))
    (if failures
        (make-instance
         'failure
         :message (format nil "~@[~%~a~]~{~%~a~}"
                          (when header-pattern
                            (format nil header-pattern data))
                          (mapcar #'message failures))
         :data (cons data (reduce #'append (mapcar #'data failures))))
        list)))

(defun mapcar-collating-failures (function list
                                  &optional header-pattern &rest data)
  (collate-failures (mapcar function list) header-pattern data))

;;------------------------------------------------------------

#+nil
(defun foo (a)
  (if (symbolp a)
      (list :yay a)
      (fail "woops, not that: ~a" a)))

;;------------------------------------------------------------
