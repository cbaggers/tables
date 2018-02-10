(in-package #:tables)

;;------------------------------------------------------------

(defmacro define-table (name (&key &allow-other-keys)
                        &body column-definitions)
  (let* ((proto (make-proto-table name column-definitions))
         (processed (validate-prototype proto)))
    (if (failures processed)
        (error "Failed: ~s" processed)
        nil)))

#+null
(define-table foo ()
  (a single-float)
  (b (unsigned-byte 32)))

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

(defmethod validate-prototype ((proto-table prototype-table))
  ;; ((validate-in-issolation proto-table)
  ;;  "Failed to validate table: ~a" proto-table)
  ;; ((validate-in-context proto-table)
  ;;  "When looking at how ~a would affect the existing system we found these issues:"
  ;;  proto-table)
  )

;; (defmethod validate-in-issolation ((proto-table prototype-table))
;;   ;; check that the table prototype itself is well formed
;;   (build-report-validate :validate-table
;;     ((symbolp (name proto-table))
;;      "The name of the table was not a symbol ~a"
;;      (name proto-table))
;;     ((mapcar #'validate-in-issolation (columns proto-table))
;;      "Some of the column definitions in ~a were invalid")))

;; (defmethod validate-in-issolation ((proto-col prototype-column))
;;   ;; check that the table prototype itself is well formed
;;   (build-report-validate :validate-column
;;     ((and (symbolp (name proto-col))
;;           (flat-typep (flat-type proto-col)))
;;      "Invalid column definition: ~a"
;;      (list (name proto-col) (flat-type proto-col)))))

;; (defmethod validate-in-context ((proto-table prototype-table))
;;   t)
