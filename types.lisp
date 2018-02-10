(in-package #:tables)

;;------------------------------------------------------------

(defclass table-definition ()
  ((columns :initarg :columns)))

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
   (data-ptrs :initarg :data-ptr) ;; an array of pointers
   (skip-list-ptr :initarg :skip-list-ptr)))

;;------------------------------------------------------------

(defclass flat-type-spec ()
  ((name :initarg :name)
   (slots :initarg :slots)))

(defclass flat-type-slot-definition ()
  ((name :initarg :name)
   (type :initarg :type)))

;;------------------------------------------------------------
