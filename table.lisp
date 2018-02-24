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
     :element-type type
     :cluster cluster)))

;;------------------------------------------------------------

(defmacro define-join (&body who-knows)
  (declare (ignore who-knows))
  nil)

;;------------------------------------------------------------


#+nil
(progn
  (define-table test-table ()
    (position vec3)
    (rotation quaternion)
    (foo beans)
    (health (integer 0 100))
    (shot-recharge-time single-float))
  (arbiter-run-dev-tasks))

#+nil
(define-table metadata ()
  (flags handy-flags))

#+nil
(define-join test-table 1-1 metadata)

#+nil
(define-join
  (test-table 1)
  (metadata 1))

;; Join Strategies
;; ---------------
;; 1-to-1
;; 1-to-1-exclusive

;; hmm structural is always 1 to 1

;; Join Strategies
;; ---------------
;; structural
;; structural-exclusive
;; index

;; structural & structural-exclusive will be baked in and not replicatable via
;; user code, however i want to provide ways to have user extensible indexes
;;
;; An index can be too a table or not. The later case is for things like
;; spatial indexes.
;;
;; The datatype of an index is user defined but cannot be soa, so that means
;; it's gonna be a packed type of some kind.
;;
;; The index the system will allow will be almost idential to a query. The
;; columns required will inform the indexer when to mark the index as dirty
;; (if updating indicies lazily) or recompute (if updating is eager)
