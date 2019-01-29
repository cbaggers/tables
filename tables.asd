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
               (:file "core/stage-0/ast-to-ir")
               (:file "core/stage-0/dead-binding-removal")
               (:file "core/stage-0/early-constant-folding")
               (:file "core/stage-0/inline-direct-calls")
               (:file "core/compile")))
