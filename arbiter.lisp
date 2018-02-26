(in-package #:tables)

;;
;; The arbiter manages all data access and threading
;;

;; TODO this is gonna belong to the table manager now

;;------------------------------------------------------------
;; chunks

;; global chunk pool, we gc this
(defvar *chunks*
  (make-array 100 :adjustable t :fill-pointer 0))

(defvar *gc-size* 100)
(defvar *last-gc-reached* 0)

(defun gc-chunks ()
  (flet ((gc-chunk (chunk index)
           (declare (ignore chunk index))
           nil))
    (let ((chunks *chunks*)
          (last *last-gc-reached*)
          (len (length *chunks*)))
      (loop :for i :from last :below (+ last *gc-size*)
         :for index := (mod i len)
         :do (gc-chunk (aref chunks index) index)
         :finally (setf *last-gc-reached* index)))))

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

;;------------------------------------------------------------

(defvar *definition-queue*
  (make-array 0 :fill-pointer 0 :adjustable t))

(defun enqueue-definition (definition)
  (vector-push-extend definition *definition-queue*))

(defun arbiter-run-dev-tasks ()
  (flet ((run (definition)
           (print definition)
           (validate-definition definition)
           (update-definition definition)))
    (map nil #'run *definition-queue*)
    (setf (fill-pointer *definition-queue*) 0)))

;;------------------------------------------------------------
