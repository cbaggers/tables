;;;; tables.asd

(asdf:defsystem #:tables
  :description "Describe tables here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (:alexandria
               :completable-types
               :bordeaux-threads
               :cffi
               :rtg-math ;; only here for example/testing. Will remove soon
               :optima)
  :components ((:file "package")
               (:file "utils")
               (:file "errors")
               (:file "types")
               (:file "generics")
               (:file "base")
               (:file "flat-types")
               (:file "primitive-types")
               (:file "table")
               (:file "query-ops")
               (:file "query-functions")
               (:file "query")
               (:file "task")
               (:file "task-set")
               (:file "arbiter")))
