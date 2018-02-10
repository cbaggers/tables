(in-package #:tables)

;;------------------------------------------------------------
;; query sets are a named collection of queries that are intended to be run
;; concurrently. This is where partial queries are stiched into whole queries
;; and where tje actual lisp code to run the queries is emitted.
;;
;; The reason for this seperation is that it allows table's compiler to
;; combine queries that are reading from the same tables (as we know that no
;; query in the set depends on the results of another of the other queries)

(defmacro define-query-set (name (&key) &body queries)
  (declare (ignore queries))
  `(progn
     ',name))


(define-query-set test-set ()
  query-a
  (pq-a pq-b)
  (pq-a pq-c)
  query-b)

;;------------------------------------------------------------
