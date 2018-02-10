(in-package #:tables)

;;
;; The arbiter manages all data access and threading
;;

;;------------------------------------------------------------
;; chunks

;; global chunk pool, we gc this
(defvar *chunks*
  (make-array 100 :adjustable t :fill-pointer 0))

(defvar *gc-size* 100)
(defvar *last-gc-reached* 0)

(defun gc-chunks ()
  (let ((last *last-gc-reached*)
        (size *chunks*))
    (loop :for i :from last :below (+ last *gc-size*)
       :for index := (mod ) :do
       (format t ".")
       :finally (setf x i))
    x))

;;------------------------------------------------------------
;; tasks

(defun arbiter-enqueue-task (task)
  (declare (ignore task))
  nil)

(defun arbiter-run-tasks (&key gc)
  (declare (ignore gc))
  nil)

(defun arbiter-run-tasks-non-blocking (&key gc)
  (declare (ignore gc))
  nil)

(defun arbiter-run-dev-tasks ()
  nil)

;;------------------------------------------------------------
