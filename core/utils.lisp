(in-package :tables.utils)

(defun assocr (item alist &key (test #'eql))
  (cdr (assoc item alist :test test)))

(defun last1 (list) (car (last list)))

(defun gensym-named (name)
  (gensym (format nil "~a_" name)))

(defun rehome-symbol (symbol new-package-name)
  (intern (format nil "~a.~a"
                  (package-name (symbol-package symbol))
                  (symbol-name symbol))
          new-package-name))
