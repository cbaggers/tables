;;;; tables.asd

(asdf:defsystem #:tables
  :description "Foo"
  :author "Chris Bagley (Baggers) <chris.bagley@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (:checkmate)
  :components ((:file "package")
               (:file "core/utils")
               (:file "core/impl")
               (:file "core/lang")
               (:file "core/stage-0")
               (:file "core/ast-to-ir")
               (:file "core/pass-2")
               (:file "core/pass-3")
               (:file "core/pass-4")
               (:file "core/compile")
               ))
