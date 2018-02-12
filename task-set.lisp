(in-package #:tables)

;;------------------------------------------------------------
;; task sets are a named collection of tasks that are intended to be run
;; concurrently. This is where partial queries are stiched into whole queries
;; and where the actual lisp code to run the queries is emitted.
;;
;; The reason for this seperation is that it allows table's compiler to
;; combine queries that are reading from the same tables (as we know that no
;; task in the set depends on the results of another of the other tasks)
;;
;; Update: v1 wont have partial queries, every query will need to specify
;;         table/s to write into
(defmacro define-task-set (name (&key) &body queries)
  (declare (ignore queries))
  `(progn
     ',name))


;; Q: how do we use the results?
;; A: well they are gonna be in a table so the next set just reads from that.
;;
;; Running the primary function produced by a define-task-set (test-set in
;; this case) enqueues the task using the arbiter, if no threading is
;; set up then it might run immediately. OR we could have an enqueue function
;; and pass in the name?

#+nil
(define-task-set test-set ()
  query-a
  (pq-a pq-b)
  (pq-a pq-c)
  task-a
  query-b)

;;------------------------------------------------------------
