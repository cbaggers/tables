(in-package :tables)

(defvar *tables*
  (make-hash-table))

(defun get-table (name)
  (gethash name *tables*))

(defun set-table (table)
  (assert (name table))
  (setf (gethash (name table) *tables*) table))

(defun nuke-shit ()
  (setf *tables* (make-hash-table)))

;;------------------------------------------------------------

(defmacro define-table (name (&key) &body columns)
  (let ((definition (make-table-definition
                     :name name
                     :columns (parse-table-columns columns))))
    `(progn
       (enqueue-definition ,definition)
       ',name)))

(defun parse-table-columns (columns)
  (mapcar #'parse-table-column columns))

(defun parse-table-column (column)
  (destructuring-bind (name type &key cluster) column
    (make-column-definition
     :name name
     :element-type (parse-type-specifier type)
     :cluster cluster)))

;;------------------------------------------------------------

(defmacro define-join (&body who-knows)
  (declare (ignore who-knows))
  nil)

;;------------------------------------------------------------
