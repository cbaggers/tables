(in-package :tables.lang)


(defclass chunk ()
  ((subcolumns :initform (make-hash-table))))

(defclass subcolumn ()
  (pointer))

;; Subcolumn Modification Strategy
;; compacting
;; skip-sentinel
;; no deletion
;;
;; Subcolumn
;;



(defvar *test-chunk*
  (make-instance 'chunk))


;; - naive nested loops join in which case the search scans the whole table
;;   or index
;; - index nested loops join when the search can utilize an existing index
;;   to perform lookups
;; - temporary index nested loops join if the optimizer creates a temporary
;;   index as part of the query plan and destroys it after query execution
;;   completes.
;;
;; An index nested loops join performs better than a merge join or hash
;; join if a small set of rows are involved. Whereas, if a large set of
;; rows are involved the Nested Loops join might not be an optimal
;; choice. Nested Loops support almost all join types except right and
;; full outer joins, right semi-join and right anti-semi join.
;;
;; left-deep
;;     using a base table (rather than another join) as the inner
;;     operand of each join in the plan
;; right-deep
;;     using a base table as the outer operand of each join in the plan
;; bushy
;;     neither left-deep nor right-deep; both inputs to a join may
;;     themselves result from joins
;;
;; These names derive from the appearance of the query plan if drawn
;; as a tree, with the outer join relation on the left and the inner
;; relation on the right (as convention dictates).


;; Bitmap index
;; A bitmap index is a special kind of indexing that stores the bulk
;; of its data as bit arrays (bitmaps) and answers most queries by
;; performing bitwise logical operations on these bitmaps. The most
;; commonly used indexes, such as B+ trees, are most efficient if the
;; values they index do not repeat or repeat a small number of
;; times. In contrast, the bitmap index is designed for cases where
;; the values of a variable repeat very frequently. For example, the
;; sex field in a customer database usually contains at most three
;; distinct values: male, female or unknown (not recorded). For such
;; variables, the bitmap index can have a significant performance
;; advantage over the commonly used trees.
;;
;; Dense index
;; A dense index in databases is a file with pairs of keys and
;; pointers for every record in the data file. Every key in this file
;; is associated with a particular pointer to a record in the sorted
;; data file. In clustered indices with duplicate keys, the dense
;; index points to the first record with that key.[3]
;;
;; Sparse index
;; A sparse index in databases is a file with pairs of keys ands
;; pointers for every block in the data file. Every key in this file
;; is associated with a particular pointer to the block in the sorted
;; data file. In clustered indices with duplicate keys, the sparse
;; index points to the lowest search key in each block.
;;
;; Reverse index
;; A reverse-key index reverses the key value before entering it in
;; the index. E.g., the value 24538 becomes 83542 in the
;; index. Reversing the key value is particularly useful for indexing
;; data such as sequence numbers, where new key values monotonically
;; increase.
