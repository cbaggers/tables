;;;; tables.asd

(asdf:defsystem #:tables
  :description "Foo"
  :author "Chris Bagley (Baggers) <chris.bagley@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (:checkmate)
  :components ((:file "package")
               (:file "core/lang")))
