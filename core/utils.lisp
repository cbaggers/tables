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

(defmacro vbind (vars value-form &body body)
  ;; {TODO} handle declare forms properly. It is complicated
  ;;        as the declare has to be the first thing in the scope
  ;;        but the vars are now split across multiple binds
  (let* ((list? (mapcar #'listp vars))
         (mvb-vars (mapcar (lambda (v l?) (if l? (gensym) v)) vars list?))
         (d-vars (mapcar (lambda (v l?) (when l? v)) vars list?))
         (d-forms (mapcar (lambda (mvb d)
                            (when d `(dbind ,d ,mvb)))
                          mvb-vars d-vars))
         (d-forms (remove nil d-forms)))
    `(multiple-value-bind ,mvb-vars ,value-form
       (declare (ignorable ,@mvb-vars))
       ,@(reduce (lambda (accum x)
                   (list (append x accum)))
                 (cons body d-forms)))))
