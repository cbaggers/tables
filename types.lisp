(in-package #:tables)

;;------------------------------------------------------------

(defclass column-definition ()
  ())

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

(defclass packed-part ()
  ((size :initarg :size)
   (name :initarg :name)))

(defclass packed-type-spec ()
  ((name :initarg :name)
   (lisp-type :initarg :lisp-type)
   (ffi-type :initarg :ffi-type)
   (parts :initarg :parts)))

(defmethod make-load-form ((obj packed-type-spec) &optional env)
  (declare (ignore env))
  (with-slots (name lisp-type ffi-type parts) obj
    `(make-instance 'packed-type-spec
                    :name ',name
                    :lisp-type ',lisp-type
                    :ffi-type ',ffi-type
                    :parts (list ,@parts))))

(defmethod make-load-form ((obj packed-part) &optional env)
  (declare (ignore env))
  (with-slots (name size) obj
    `(make-instance 'packed-part
                    :size ',size
                    :name ',name)))

;;------------------------------------------------------------

(defclass flat-struct-spec ()
  ((name :initarg :name)
   (slots :initarg :slots)))

(defclass flat-struct-slot-definition ()
  ((name :initarg :name)
   (type :initarg :type)))

;;------------------------------------------------------------
