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
               (:file "base")))
