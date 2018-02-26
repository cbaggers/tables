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

(defgeneric to-specifier (ttype))

;;------------------------------------------------------------

(define-completable data-trait-definition ()
  name
  slots)

(define-completable data-trait-slot-definition ()
  name
  (ttype type-ref))

(defmethod make-load-form ((obj data-trait-definition) &optional env)
  (declare (ignore env))
  (with-contents (name slots) obj
    `(make-data-trait-definition
      :name ',name
      :slots (list ,@slots))))

(defmethod make-load-form ((obj data-trait-slot-definition) &optional env)
  (declare (ignore env))
  (with-contents (name ttype) obj
    `(make-data-trait-slot-definition
      :name ',name
      :ttype ',ttype)))

(defmethod to-specifier ((obj data-trait-definition))
  (name obj))

;;------------------------------------------------------------

(defun table-type-def-p (x)
  (or (typep x 'data-trait-definition)
      (typep x 'data-type-definition)
      (typep x 'anon-type)))

(defclass type-ref ()
  ((ttype :initarg :ttype :accessor ttype)))

(defmethod make-type-ref (&key ttype)
  (assert (table-type-def-p ttype) ()
          "Tables: Cannot make type-ref to ~a~%Value: ~a"
          (type-of ttype) ttype)
  (make-instance 'type-ref :ttype ttype))

(defmethod print-object ((obj type-ref) stream)
  (with-slots (ttype) obj
    (format stream "#<TYPE-REF ~s>" ttype)))

(defmethod make-load-form ((obj type-ref) &optional env)
  (declare (ignore env))
  (with-slots (ttype) obj
    `(parse-type-specifier ',(to-specifier ttype))))

;;------------------------------------------------------------

(define-completable anon-type ()
  size)

(defmethod make-load-form ((obj anon-type) &optional env)
  (declare (ignore env))
  (with-contents (size) obj
    `(make-anon-type :size ,size)))

(defmethod to-specifier ((obj anon-type))
  (size obj))

;;------------------------------------------------------------

(define-completable data-type-part-definition ()
  name
  (ttype type-ref)
  offset)

(define-completable data-type-definition ()
  name
  packed
  parts)

(defmethod to-specifier ((obj data-type-definition))
  (name obj))

(defmethod make-load-form ((obj data-type-definition) &optional env)
  (declare (ignore env))
  (with-contents (name packed parts) obj
    `(make-data-type-definition
      :name ',name
      :packed ,packed
      :parts (list ,@parts))))

(defmethod make-load-form ((obj data-type-part-definition) &optional env)
  (declare (ignore env))
  (with-contents (name ttype offset) obj
    `(make-data-type-part-definition
      :name ',name
      :ttype ',ttype
      :offset ',offset)))

;;------------------------------------------------------------
