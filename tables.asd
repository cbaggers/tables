;;;; tables.asd

(asdf:defsystem #:tables
  :description "Describe tables here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :serial t
  :components ((:file "package")
               (:file "types")
               (:file "generics")
               (:file "tables")
               (:file "expression-queries")
               (:file "function-queries")))
