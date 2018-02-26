(in-package :tables)

;;------------------------------------------------------------

(define-completable column-definition ()
  name
  element-type
  cluster)

(define-completable table-definition ()
  name
  columns)

(defmethod make-load-form ((obj table-definition) &optional env)
  (declare (ignore env))
  (with-contents (name columns) obj
    `(make-table-definition
      :name ',name
      :columns (list ,@columns))))

(defmethod make-load-form ((obj column-definition) &optional env)
  (declare (ignore env))
  (with-contents (name element-type cluster) obj
    `(make-column-definition
      :name ',name
      :element-type ',element-type
      :cluster ',cluster)))

(define-completable table-metadata ()
  columns)

(define-completable table ()
  name
  metadata
  join-groups)

(defun make-join-group-array ()
  (make-array 10 :adjustable t :fill-pointer 0))

;; TODO: Maybe we have joins as well a clusters. So:
;;
;; table
;;   join-groups
;;     clusters
;;       chunks

;; This is just so simplify 1-to-1 structural joins.

;;------------------------------------------------------------

(define-completable join-group-metadata ())

(define-completable join-group ()
  metadata
  clusters)

(defun make-cluster-array ()
  (make-array 10 :adjustable t :fill-pointer 0))

;;------------------------------------------------------------

(define-completable cluster-metadata ())

(define-completable cluster ()
  metadata
  chunks)

(defun make-chunk-array ()
  (make-array 10 :adjustable t :fill-pointer 0))

;;------------------------------------------------------------

(defclass chunk-metadata () ())

;; posibility:
;; removing things is a bugger as it breaks any indexes used internally
;; instead of reorganising slots in the data-ptrs array just free and leave
;; empty, always push a new entry.
;; Doesnt matter as next session each will assign new indices anyhoo.
(defclass chunk ()
  ((metadata :initarg :metadata)
   (data-ptrs :initarg :data-ptr) ;; an array of pointers
   (skip-list-ptr :initarg :skip-list-ptr)))

;;------------------------------------------------------------

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

(defmethod update-definition ((definition table-definition))
  (let ((table (get-table (name definition))))
    (if table
        (update-existing-table definition)
        (add-new-table definition))))

(defun add-new-table (definition)
  (let* ((table-name (name definition))
         (table (make-table
                 :name table-name
                 :metadata (make-table-metadata)
                 :join-groups (make-join-group-array))))
    (set-table table)
    (map nil (lambda (x) (add-column table-name x))
         (columns definition))))

(defun update-existing-table (definition)
  (let* ((starting-table (get-table (name definition)))
         (starting-meta (metadata starting-table)))
    (loop :for column :in (columns definition) :do
       (if (find (name column) (columns starting-meta) :key #'name)
           (modify-column starting-table column)
           (add-column (name starting-table) column)))))

;; We kinda want to use symbol as if we use table we need to run the next
;; iteration with the new table object
(defgeneric add-column (table column-definition)
  (:method ((table-name symbol) column-definition)
    (let ((table (get-table table-name)))
      (assert table)
      (add-column table column-definition)))
  (:method ((table table) column-definition)
    (let ((meta (metadata table)))
      (assert (not (find (name column-definition)
                         (columns meta) :key #'name)))
      (map-chunks (lambda (chunk) (add-column chunk column-definition))
                  table)
      (set-table
       (copy table
             :metadata (copy meta :columns (cons column-definition
                                                 (columns meta))))))))

(defun modify-column (table columns)
  (declare (ignore table columns))
  (print "implement modify-column"))

(defmethod add-column ((chunk chunk) column-definition)
  (print "add the actual column"))

(defgeneric map-chunks (function obj)
  (:method (function (obj table))
    (loop :for join-group :across (join-groups obj) :append
       (map-chunks function join-group)))

  (:method (function (obj join-group))
    (loop :for cluster :across (clusters obj) :append
       (map-chunks function cluster)))

  (:method (function (obj cluster))
    (loop :for chunk :across (chunks obj) :append
       (funcall function chunk))))

;;------------------------------------------------------------

#||

add column needs to be spit in 2, one for clusters and one not.

clustered add is really just adding to the metadata and then calling recluster.

non-clustered add maps over the chunks adding a column that matches the chunk
length.

chunks dont change size. chunks in the same table/cluster/etc can have
different sizes. A chunk's data can also be copied into a new chunk (of
a different size) and retired. But there is no mutating the size of an existing
chunk.

||#

#||

remove column (once validated) is also easy, map through chunks removing the
column, metadata, etc and call recluster if needed.

recluster for removal should be easy. Simply mask out the removed columns
contriution and merge all clusters with the same cluster-id.

||#

#||

Adding a cluster to existing column is always the worst case (perf wise).
We have to:
- add the metadata
- call recluster
- remove the now clustered column (as it's data is now implicit in the cluster)
- and of course purple.

..by which I mean recompile all queries

||#

#||

Removing a cluster is also a bit pricey, you need to add a column for the data
and set the values to the implicit values from that cluster. Then you can drop
the metadata and recompile all queries

||#
