(in-package :tables.compile)

(defun macroexpand-query (form)
  (mexpand-form form))

(defun mexpand-form (form)
  (if (atom form)
      form
      (let ((head (first form)))
        (case head
          (lambda (mexpand-lambda (rest form)))
          (if (mexpand-if (rest form)))
          (let (mexpand-let (rest form)))
          (progn (mexpand-progn (rest form)))
          (funcall (mexpand-funcall (rest form)))
          (output (mexpand-output (rest form)))
          ((function :construct tables.lang::read-val) form)
          (otherwise
           (let ((macro-func (find-tables-macro-func head)))
             (if macro-func
                 (mexpand macro-func (rest form))
                 form)))))))

(defun find-tables-macro-func (potential-macro-name)
  (gethash potential-macro-name *registered-macros*))

(defun mexpand (macro-func form)
  (multiple-value-bind (new-code is-new-code)
      (funcall macro-func form :TODO_ENVIRONMENT)
    (if is-new-code
        (mexpand-form new-code)
        form)))

(defun mexpand-lambda (form)
  (destructuring-bind (args . body) form
    `(lambda ,args ,@(mapcar #'mexpand-form body))))

(defun mexpand-if (form)
  (cons 'if (mapcar #'mexpand-form form)))

(defun mexpand-let (form)
  (destructuring-bind (args . body) form
    `(let ,(loop
              :for (arg-name arg-form) :in args
              :collect (list arg-name (mexpand-form arg-form)))
       ,@(mapcar #'mexpand-form body))))

(defun mexpand-progn (form)
  (cons 'progn (mapcar #'mexpand-form form)))

(defun mexpand-funcall (form)
  (cons 'funcall (mapcar #'mexpand-form form)))

(defun mexpand-output (form)
  (cons 'output
        (loop
           :for (key val) :on form :by #'cddr
           :do (assert (keywordp key))
           :append (list key (mexpand-form val)))))
