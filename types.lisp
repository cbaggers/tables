(in-package #:tables)

;;------------------------------------------------------------

(defclass prototype-table ()
  ((name :initarg :name :type symbol)
   (columns :initarg :columns :type list)))

(defclass table () ())

(defclass prototype-column ()
  ((name :initarg :name :type symbol)
   (type :initarg :type :type flat-type-specifier)))

(defclass column () ())

;;------------------------------------------------------------

(defclass prototype-expression-query () ())

(defclass expression-query () ())

;;------------------------------------------------------------

(defclass prototype-function-query () ())

(defclass function-query () ())

;;------------------------------------------------------------
