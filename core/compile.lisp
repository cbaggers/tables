(in-package :tables.compile)

(defun first-run (code)
  (let ((ast (infer (make-check-context 'tables) code)))
    (tables.compile.stage-0.inline-top-level-functions:run-pass
     (tables.compile.stage-0.dead-binding-removal:run-pass
      (tables.compile.stage-0.dead-if-branch-removal:run-pass
       (tables.compile.stage-0.early-constant-folding:run-pass
        (tables.compile.stage-0.inline-direct-calls:run-pass
         (tables.compile.stage-0.dead-binding-removal:run-pass
          (tables.compile.stage-0.early-constant-folding:run-pass
           (tables.compile.stage-0.dead-binding-removal:run-pass
            (tables.compile.stage-0.ast-to-ir:run-pass ast)))))))))))

(defun test (&optional code)
  (let ((hi (first-run code)))
    (loop
       :for i :below 10
       :do (setf
            hi
            (tables.compile.stage-0.early-constant-folding:run-pass
             (tables.compile.stage-0.dead-binding-removal:run-pass
              (tables.compile.stage-0.inline-direct-calls:run-pass
               (tables.compile.stage-0.dead-if-branch-removal:run-pass                     hi))))))
    hi))
