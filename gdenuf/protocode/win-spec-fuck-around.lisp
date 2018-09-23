;; (autowrap:c-include
;;  "C:\\Program Files (x86)\\Windows Kits\\10\\Include\\10.0.17134.0\\um\\winnt.h"
;;  :spec-path '(:gdenuf :autowrap-specs))

;; also could try Windows.h and use precompiled headers.
;; Btw, should probably define WIN32_LEAN_AND_MEAN before doing this


(defun moo (x)
  (jonathan:parse
   (alexandria:read-file-into-string x)
   :as :hash-table))

(defun foo ()
  (labels ((blort (str)
             (let ((str (string-upcase str)))
               (if (char= (char str 0) #\:)
                   (intern (subseq str 1) :keyword)
                   (intern str))))
           (jam (type-ht)
             (let ((tag (blort (gethash "tag" type-ht))))
               (if (gethash "type" type-ht)
                   (list tag (jam (gethash "type" type-ht)))
                   tag))))
    (let ((flap '("types.i386-unknown-freebsd.spec"
                  "types.i386-unknown-openbsd.spec"
                  "types.i686-apple-darwin9.spec"
                  "types.i686-pc-linux-gnu.spec"
                  "types.x86_64-apple-darwin9.spec"
                  "types.x86_64-pc-linux-gnu.spec"
                  "types.x86_64-unknown-freebsd.spec"
                  "types.x86_64-unknown-openbsd.spec")))
      (loop
         :for f :in flap
         :do
           (print f)
           (loop
              :for entry :in (moo (format nil "~~/Code/lisp/tables/gdenuf/specs/~a" f))
              :do
                (when (equal (gethash "tag" entry) "typedef")
                  (print
                   `(defctype ,(blort (gethash "name" entry))
                        ,(jam (gethash "type" entry))))))))))
