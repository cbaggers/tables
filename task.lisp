(in-package #:tables)

;;------------------------------------------------------------
;; Custom tasks allow the user to call a regular function with
;; certain tables (or ranges of data within a tavble) locked.
;;
;; This can be useful for tables which are usafely mutated by
;; external systems (like a C library)

;;------------------------------------------------------------
