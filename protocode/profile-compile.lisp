(defun profile-all-that-shit ()
  (let (res)
    (do-all-symbols (s)
      (let* ((p (symbol-package s)))
        (when p
          (let* ((pn (package-name p)))
            (when (and pn (alexandria:starts-with-subseq
                           "TABLES.COM" pn)
                       (fboundp s))
              (push s res))))))
    (eval (cons 'sb-profile res)))
  t)
