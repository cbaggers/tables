#|| Unsorted

||#
;;============================================================
;; public api

;;------------------------------
;; hub
pump-hub

;;------------------------------
;; table

define-table

;;------------------------------
;; hub

define-hub

;;------------------------------
;; query

define-query

;;------------------------------
;; query-set

define-query-set
query-set-run ;; (run 'pass-0)

;;============================================================
;; low level api

;;------------------------------
;; hub

hub ;; type
register-job-manager

;;------------------------------
;; table

table-schema ;; type
make-table-schema
give-table-schema ;; (hub-name table-name table-schema) creates table if not present


;;------------------------------------------------------------
;; flag

flag ;; type
make-flag
flag-raised-p
cord-draw
cord-drawn-p

;;------------------------------
;; job

job
job-execute

;;============================================================
;; private api

;;------------------------------
;; hub

get-hub ;; by name
hub-version
hub-potential-future-version
hub-memory-pool
hub-request-queue
handle-request

;;------------------------------
;; request

request
make-request

;;------------------------------
;; redefinition

redefinition
make-set-table
make-remove-table
make-set-query
make-remove-query
make-set-query-set
make-remove-query-set
make-move-columns

;;------------------------------
;; job

make-job

;;------------------------------
;; memory-pool

take-block
release-block

;;------------------------------
;; misc

principle-component-p ;; maybe not
