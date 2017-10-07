(in-package #:tables)

;;------------------------------------------------------------

(defmacro define-table (name (&key &allow-other-keys)
                        &body column-definitions)
  (let* ((proto (make-proto-table name column-definitions))
         (processed (validate-prototype proto)))
    (etypecase processed
      (table (add-query processed) nil)
      (error (error processed) nil))))

;;------------------------------------------------------------

(defun make-proto-table (name column-definitions)
  (make-instance 'prototype-table
                 :name name
                 :columns (mapcar (lambda (x) (apply #'make-proto-column x))
                                  column-definitions)))

(defun make-proto-column (name type)
  (make-instance 'prototype-column
                 :name name
                 :type type))

;;------------------------------------------------------------

(defmethod validate-prototype ((proto-table prototype-table)))
