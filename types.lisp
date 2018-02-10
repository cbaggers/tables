(in-package #:tables)

;;------------------------------------------------------------

(defclass prototype-table ()
  ((name :initarg :name :accessor name)
   (columns :initarg :columns :accessor columns)))

(defclass table () ())

(defclass prototype-column ()
  ((name :initarg :name :accessor name)
   (type :initarg :type :accessor flat-type)))

(defclass column () ())

;;------------------------------------------------------------

(defclass prototype-expression-query () ())

(defclass expression-query () ())

;;------------------------------------------------------------

(defclass prototype-function-query () ())

(defclass function-query () ())

;;------------------------------------------------------------

;; root of all flat type internal representations
(defclass flat-type ()
  ((specifier :initarg :specifier :accessor specifier)))
