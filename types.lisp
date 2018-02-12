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

(defclass flat-primitive-spec ()
  ((name :initarg :name)
   (size :initarg :size)
   (ffi-type :initarg :ffi-type)
   (lisp-type :initarg :lisp-type)
   (alignment :initarg :alignment)))

(defmethod make-load-form ((obj flat-primitive-spec) &optional env)
  (declare (ignore env))
  (with-slots (name size alignment lisp-type ffi-type) obj
    `(make-instance 'flat-primitive-spec
                    :name ',name
                    :size ',size
                    :alignment ',alignment
                    :lisp-type ',lisp-type
                    :ffi-type ',ffi-type)))

(defclass flat-struct-spec ()
  ((name :initarg :name)
   (slots :initarg :slots)))

(defclass flat-struct-slot-definition ()
  ((name :initarg :name)
   (type :initarg :type)))

;;------------------------------------------------------------
