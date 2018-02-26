(in-package #:tables)

;;------------------------------------------------------------

;; Definitions are what are validated and used to create a type object.
;; A specifier is the symbol/list that names a type object

;;------------------------------------------------------------

(defgeneric to-specifier (ttype))

(defgeneric update-definition (definition)
  (:method (definition)
    (error "Tables: update-definition not defined for ~a"
           definition)))

(defgeneric validate-definition (definition)
  (:method (definition)
    (error "Tables: validate-definition not defined for ~a"
           definition)))

(defgeneric init-type (definition))

;;------------------------------------------------------------

(defun table-type-def-p (x)
  (or (typep x 'data-trait)
      (typep x 'bit-type)
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
