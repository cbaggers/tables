;; (autowrap:c-include
;;  "C:\\Program Files (x86)\\Windows Kits\\10\\Include\\10.0.17134.0\\um\\winnt.h"
;;  :spec-path '(:gdenuf :autowrap-specs))

;; also could try Windows.h and use precompiled headers.
;; Btw, should probably define WIN32_LEAN_AND_MEAN before doing this


(defun moo ()
  (jonathan:parse
   (alexandria:read-file-into-string
    ;;"c:/home/quicklisp/local-projects/tables/gdenuf/basetsd.i686-pc-windows-msvc.spec"
    ;;"c:/home/quicklisp/local-projects/tables/gdenuf/basetsd.x86_64-pc-windows-msvc.spec"
    ;; "c:/home/quicklisp/local-projects/tables/gdenuf/winnt.i686-pc-windows-msvc.spec"
    ;; "c:/home/quicklisp/local-projects/tables/gdenuf/winnt.x86_64-pc-windows-msvc.spec"
    )
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
    (let ((entries (moo)))
      (loop :for entry :in entries :do
         (when (equal (gethash "tag" entry) "typedef")
           (print
            `(cffi:defctype ,(blort (gethash "name" entry))
                 ,(jam (gethash "type" entry)))))))))
