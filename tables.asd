;;;; tables.asd

(asdf:defsystem #:tables
  :description "Describe tables here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :depends-on (:alexandria)
  :components ((:file "package")
               (:file "utils")
               (:file "errors")
               (:file "types")
               (:file "generics")
               (:file "base")
               (:file "table")
               (:file "query-ops")
               (:file "query-functions")
               (:file "query")
               (:file "task")
               (:file "task-set")
               (:file "arbiter")))
