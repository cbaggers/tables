(in-package :tables-lang)

(defun test-pass-1 ()
  (let ((res (infer (make-check-context 'tables)
                    `(funcall (let ((f (lambda ((a ?a) (i i8))
                                         (let ((b a))
                                           (if b
                                               i
                                               20))))
                                    (g (lambda ((a ?a) (i i8))
                                         10)))
                                (if t
                                    f
                                    g))
                              t
                              (let ((x 20))
                                20)))))
    ;;(print res)
    (let* ((context (make-blockify-context nil nil nil))
           (lets (blockify context res)))
      (with-slots (type name) (last1 lets)
        (make-instance 'ssad-let1
                       :bindings lets
                       :body-form name
                       :type type)))))

(defun test-pass-2 ()
  (pass-2 (test-pass-1)))

(defun test ()
  (test-pass-2))
