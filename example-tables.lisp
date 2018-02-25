(in-package #:tables)

;;------------------------------------------------------------

(define-table test-table ()
  (position vec3)
  (rotation quaternion)
  (health u32)
  (shot-recharge-time f32))


;;------------------------------------------------------------

#+nil
(define-table metadata ()
  (flags handy-flags))

;;------------------------------------------------------------

#+nil
(define-join test-table 1-1 metadata)

#+nil
(define-join
  (test-table 1)
  (metadata 1))

;;------------------------------------------------------------

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
