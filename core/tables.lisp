(in-package :tables.tables)

;;------------------------------------------------------------

(defclass column-quality () ())

(defclass column-spec ()
  ((name :initarg :name)
   (type-designator :initarg :type-designator)
   (qualities :initform nil :initarg :qualities)))

(defclass table-spec ()
  ((column-specs :initarg :column-specs)
   (validated :initform nil)))

(defclass chunk ()
  ((columns :initarg columns)))

(defclass column ()
  ((pointer :initarg :pointer)
   (first-element-pointer :initarg :first-element-pointer)
   (count :initarg :count)))

(defclass unordered-table ()
  ((chunks :initarg :chunks)))

;;------------------------------------------------------------

(defun make-column-spec (column-form)
  (destructuring-bind (name type-designator) column-form
    (make-instance 'column-spec
                   :name name
                   :type-designator type-designator)))

(defun make-table-spec (column-forms)
  (let ((column-specs (mapcar #'make-column-spec column-forms)))
    (make-instance 'table-spec :column-specs column-specs)))

;;------------------------------------------------------------

;; First version restrictions are
;; - implementing map & delete
;; - only supporting delete within chunk, no cross chunk stuff movement yet
;; - no aos columns
;; - no struct columns (we need to add output destructuring to optimizer first)
;; - no column qualities (e.g. cluster)
