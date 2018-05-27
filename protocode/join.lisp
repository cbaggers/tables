(uiop:define-package :join (:use :cl))
(in-package :join)

(defun make-data-array ()
  (make-array 0 :fill-pointer 0 :adjustable t))

(defun make-index-array ()
  (make-array 0 :fill-pointer 0 :adjustable t))

(defun make-indexes-array ()
  (make-array 0 :fill-pointer 0 :adjustable t))

(defclass chunk ()
  ((;; Does the chunk contains columns or does the column
    ;; contain the chunks?
    )
   (indexes :initform (make-indexes-array))))

(defun make-table-chunk-array ()
  (make-array 0 :fill-pointer 0 :adjustable t))

(defun make-joins-container ()
  (make-hash-table))

(defclass join-table ()
  ((next-id :initform 0)
   (entries :initform (make-joins-container))))

(defclass join ()
  ((table :initarg :table)
   (id :initarg :id)))

(defclass table ()
  ((chunks :initform (make-table-chunk-array));; maybe?
   (columns ..)
   (clusters ..) ;; a container of chunks indexed by cluster-id
   (joins  )))

(defclass column ()
  (arr :initform (make-data-array)))

(defun make-table ()
  (make-instance 'table))

(defvar *t0* (make-table))
(defvar *t1* (make-table))

;;------------------------------------------------------------
;; The above is getting into the weeds too fast; lets think in
;; terms of api.

(defun joined-row (table-a chunk-a index-a table-b)
  "Given row from table-a, get the row from table-b"
  ;; - ideally wouldnt need table-a
  row)

;; The issue I see with this ^^ is that for some traversals you
;; can step both sides by bumping pointers, so a full lookup is
;; going to be really slow.
;; Maybe we can make it even higher level and define it in terms
;; of the kind of traversal.

(defun map-as-joined (chunk-a chunk-b &body body)
  "Assume the two chunks are joined and map across them.")

;; Hmm, what kinds of traversal are common in sql
;;
;; select - map
;; select-where - filter
;; select-accum - reduce
;;
;; These 3 come to mind immediately. We want to expose `map` &
;; `reduce` in the public api but intend to have the query compiler
;; extract extra info of how we can filter the operation.
;;
;; for example, map all entries for given cluster-id.
;;
;; I wonder what we can optimize for, might be nice to work that out.
;;
;; First one, as mentioned before, is cluster ID. We can scan chunks and
;; compare the bitmask, we can keep a map of cluster-id to array of chunks
;; (the 'map' in that case might be a hash-table in some cases)
