(in-package :tables.utils)

(defun assocr (item alist)
  (cdr (assoc item alist)))

(defun last1 (list) (car (last list)))

(defun gensym-named (name)
  (gensym (format nil "~a_" name)))
