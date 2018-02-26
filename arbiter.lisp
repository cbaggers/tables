(in-package #:tables)

;;
;; The arbiter manages all data access and threading
;;

;; TODO this is gonna belong to the table manager now

;;------------------------------------------------------------
;; chunks

;; global chunk pool, we gc this
(defvar *chunks*
  (make-array 100 :adjustable t :fill-pointer 0))

(defvar *gc-size* 100)
(defvar *last-gc-reached* 0)

(defun gc-chunks ()
  (flet ((gc-chunk (chunk index)
           (declare (ignore chunk index))
           nil))
    (let ((chunks *chunks*)
          (last *last-gc-reached*)
          (len (length *chunks*)))
      (loop :for i :from last :below (+ last *gc-size*)
         :for index := (mod i len)
         :do (gc-chunk (aref chunks index) index)
         :finally (setf *last-gc-reached* index)))))

;;------------------------------------------------------------
;; tasks

(defun arbiter-enqueue-task (task)
  (declare (ignore task))
  nil)

(defun arbiter-run-tasks (&key gc)
  (declare (ignore gc))
  nil)

(defun arbiter-run-tasks-non-blocking (&key gc)
  (declare (ignore gc))
  nil)

;;------------------------------------------------------------

(defvar *definition-queue*
  (make-array 0 :fill-pointer 0 :adjustable t))

(defun enqueue-definition (definition)
  (vector-push-extend definition *definition-queue*))

(defgeneric update-definition (definition)
  (:method (definition)
    (error "Tables: update-definition not defined for ~a"
           definition)))

(defgeneric validate-definition (definition)
  (:method (definition)
    (error "Tables: validate-definition not defined for ~a"
           definition)))

(defun arbiter-run-dev-tasks ()
  (map nil #'update-definition *definition-queue*)
  (setf (fill-pointer *definition-queue*) 0))

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
