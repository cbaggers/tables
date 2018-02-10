(in-package #:tables)

;;------------------------------------------------------------

(defclass table-spec () ())

(defclass table-metadata () ())

(defclass table ()
  ((metadata :initarg :metadata)
   (clusters :initform (make-array 10 :adjustable t :fill-pointer 0))))

;;------------------------------------------------------------

(defclass cluster-metadata () ())

(defclass cluster ()
  ((metadata :initarg :metadata)
   (chunks :initform (make-array 10 :adjustable t :fill-pointer 0))))

;;------------------------------------------------------------

(defclass chunk-metadata () ())

(defclass chunk ()
  ((metadata :initarg :metadata)
   (data-ptr :initarg :data-ptr)
   (skip-list-ptr :initarg :skip-list-ptr)))

;;------------------------------------------------------------
