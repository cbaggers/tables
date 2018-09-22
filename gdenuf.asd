;;;; gdenuf.asd

(asdf:defsystem #:gdenuf
  :description "An array in foreign memory"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :depends-on (:cffi :cl-autowrap)
  :serial t
  :components ((:module #:autowrap-specs
                        :pathname "gdenuf/specs")
               (:file "gdenuf/package")
               (:file "gdenuf/size-t")
               (:file "gdenuf/gdenuf")))
