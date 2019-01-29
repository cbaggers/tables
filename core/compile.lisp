(in-package :tables.compile)

(defun test-pass-1 ()
  (let ((ast (infer (make-check-context 'tables)
                    `(funcall (let ((f (lambda ((a ?a) (i i8))
                                         (let ((b a))
                                           (if b
                                               i
                                               20))))
                                    (g (lambda ((a ?a) (i i8))
                                         10)))
                                (funcall g nil 123)
                                (if t
                                    f
                                    g))
                              t
                              (let ((x 20))
                                x)))))
    (tables.compile.stage-0.ast-to-ir:run-pass ast)))

(defun test-pass-2 ()
  (tables.compile.stage-0.early-constant-folding:run-pass
   (tables.compile.stage-0.dead-binding-removal:run-pass
    (test-pass-1))))

(defun test-pass-3 ()
  (tables.compile.stage-0.inline-direct-calls:run-pass
   (tables.compile.stage-0.dead-binding-removal:run-pass
    (test-pass-2))))

(defun test-pass-4 ()
  (tables.compile.stage-0.dead-binding-removal:run-pass
   (tables.compile.stage-0.dead-if-branch-removal:run-pass
    (test-pass-3))))

(defun test ()
  (let ((hi (tables.compile.stage-0.early-constant-folding:run-pass
             (test-pass-4))))
    (loop
       :for i :below 10
       :do (setf
            hi
            (tables.compile.stage-0.early-constant-folding:run-pass
             (tables.compile.stage-0.dead-binding-removal:run-pass
              (tables.compile.stage-0.inline-direct-calls:run-pass
               (tables.compile.stage-0.dead-if-branch-removal:run-pass                     hi))))))
    hi))
