(in-package #:tables)

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

(defclass data-trait-definition ()
  ((name :initarg :name)
   (slots :initarg :slots)))

(defclass data-trait-slot-definition ()
  ((name :initarg :name)
   (type :initarg :type)))

(defmethod make-load-form ((obj data-trait-definition) &optional env)
  (declare (ignore env))
  (with-slots (name slots) obj
    `(make-instance 'data-trait-definition
                    :name ',name
                    :slots (list ,@slots))))

(defmethod make-load-form ((obj data-trait-slot-definition) &optional env)
  (declare (ignore env))
  (with-slots (name type) obj
    `(make-instance 'data-trait-slot-definition
                    :name ',name
                    :type ',type)))

;;------------------------------------------------------------

(defclass data-type-part-definition ()
  ((size :initarg :size)
   (name :initarg :name)))

(defclass data-type-definition ()
  ((name :initarg :name)
   (lisp-type :initarg :lisp-type)
   (ffi-type :initarg :ffi-type)
   (parts :initarg :parts)))

(defmethod make-load-form ((obj data-type-definition) &optional env)
  (declare (ignore env))
  (with-slots (name lisp-type ffi-type parts) obj
    `(make-instance 'data-type-definition
                    :name ',name
                    :lisp-type ',lisp-type
                    :ffi-type ',ffi-type
                    :parts (list ,@parts))))

(defmethod make-load-form ((obj data-type-part-definition) &optional env)
  (declare (ignore env))
  (with-slots (name size) obj
    `(make-instance 'data-type-part-definition
                    :size ',size
                    :name ',name)))

;;------------------------------------------------------------
