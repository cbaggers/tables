(in-package :tables)

;;------------------------------------------------------------

(define-completable column-definition ()
  name
  element-type
  cluster)

(define-completable table-definition ()
  name
  columns)

(defmethod make-load-form ((obj table-definition) &optional env)
  (declare (ignore env))
  (with-contents (name columns) obj
    `(make-table-definition
      :name ',name
      :columns (list ,@columns))))

(defmethod make-load-form ((obj column-definition) &optional env)
  (declare (ignore env))
  (with-contents (name element-type cluster) obj
    `(make-column-definition
      :name ',name
      :element-type ',element-type
      :cluster ',cluster)))

(define-completable table-metadata ()
  columns)

(define-completable table ()
  name
  metadata
  join-groups)

(defun make-join-group-array ()
  (make-array 10 :adjustable t :fill-pointer 0))

;; TODO: Maybe we have joins as well a clusters. So:
;;
;; table
;;   join-groups
;;     clusters
;;       chunks

;; This is just so simplify 1-to-1 structural joins.

;;------------------------------------------------------------

(define-completable join-group-metadata ())

(define-completable join-group ()
  metadata
  clusters)

(defun make-cluster-array ()
  (make-array 10 :adjustable t :fill-pointer 0))

;;------------------------------------------------------------

(define-completable cluster-metadata ())

(define-completable cluster ()
  metadata
  chunks)

(defun make-chunk-array ()
  (make-array 10 :adjustable t :fill-pointer 0))

;;------------------------------------------------------------

(defclass chunk-metadata () ())

;; posibility:
;; removing things is a bugger as it breaks any indexes used internally
;; instead of reorganising slots in the data-ptrs array just free and leave
;; empty, always push a new entry.
;; Doesnt matter as next session each will assign new indices anyhoo.
(defclass chunk ()
  ((metadata :initarg :metadata)
   (data-ptrs :initarg :data-ptr) ;; an array of pointers
   (skip-list-ptr :initarg :skip-list-ptr)))

;;------------------------------------------------------------

(defvar *tables*
  (make-hash-table))

(defun get-table (name)
  (gethash name *tables*))

(defun set-table (table)
  (assert (name table))
  (setf (gethash (name table) *tables*) table))

(defun nuke-shit ()
  (setf *tables* (make-hash-table)))

;;------------------------------------------------------------

(defmacro define-table (name (&key) &body columns)
  (let ((definition (make-table-definition
                     :name name
                     :columns (parse-table-columns columns))))
    `(progn
       (enqueue-definition ,definition)
       ',name)))

(defun parse-table-columns (columns)
  (mapcar #'parse-table-column columns))

(defun parse-table-column (column)
  (destructuring-bind (name type &key cluster) column
    (make-column-definition
     :name name
     :element-type (parse-type-specifier type)
     :cluster cluster)))

;;------------------------------------------------------------

(defmacro define-join (&body who-knows)
  (declare (ignore who-knows))
  nil)

;;------------------------------------------------------------
