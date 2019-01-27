(in-package :tables-lang)

(defun test ()
  (let ((res (infer (make-check-context 'tables)
                    `(funcall (lambda ((a ?a) (i i8))
                                (let ((b a))
                                  (if b
                                      i
                                      20)))
                              t
                              10))))
    ;;(print res)
    (let* ((context (make-blockify-context nil nil nil))
           (lets (blockify context res)))
      (with-slots (type name) (last1 lets)
        (make-instance 'ssad-let1
                       :bindings lets
                       :body-form name
                       :type type)))))
