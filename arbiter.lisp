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

(defun arbiter-run-dev-tasks ()
  nil)

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
