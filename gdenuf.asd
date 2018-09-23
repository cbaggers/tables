;;;; gdenuf.asd

(asdf:defsystem #:gdenuf
  :description "An array in foreign memory"
  :author "Chris Bagley <techsnuffle@gmail.com>"
  :license "BSD 2 Clause"
  :depends-on (:cffi :cl-autowrap :split-sequence)
  :serial t
  :components ((:module #:autowrap-specs
                        :pathname "gdenuf/specs")
               (:file "gdenuf/package")
               (:file "gdenuf/core-types-win" :if-feature (:or :windows :win32))
               (:file "gdenuf/core-types-posix" :if-feature (:or :linux :unix))
               (:file "gdenuf/size-t")
               (:file "gdenuf/cpu-info-common")
               (:file "gdenuf/cpu-info-linux" :if-feature :linux)
               (:file "gdenuf/cpu-info-macos" :if-feature :darwin)
               (:file "gdenuf/gdenuf")))
