(in-package :tables.compile)

(defun test-pass-1 (code)
  (let ((ast (infer (make-check-context 'tables) code)))
    (tables.compile.stage-0.ast-to-ir:run-pass ast)))

(defun test-pass-2 (code)
  (tables.compile.stage-0.early-constant-folding:run-pass
   (tables.compile.stage-0.dead-binding-removal:run-pass
    (test-pass-1 code))))

(defun test-pass-3 (code)
  (tables.compile.stage-0.inline-direct-calls:run-pass
   (tables.compile.stage-0.dead-binding-removal:run-pass
    (test-pass-2 code))))

(defun test-pass-4 (code)
  (tables.compile.stage-0.dead-binding-removal:run-pass
   (tables.compile.stage-0.dead-if-branch-removal:run-pass
    (test-pass-3 code))))

(defun test-pass-5 (code)
  (tables.compile.stage-0.inline-top-level-functions:run-pass
   (test-pass-4 code)))

(defun test (&optional code)
  (let ((hi (tables.compile.stage-0.early-constant-folding:run-pass
             (test-pass-5 code))))
    (loop
       :for i :below 10
       :do (setf
            hi
            (tables.compile.stage-0.early-constant-folding:run-pass
             (tables.compile.stage-0.dead-binding-removal:run-pass
              (tables.compile.stage-0.inline-direct-calls:run-pass
               (tables.compile.stage-0.dead-if-branch-removal:run-pass                     hi))))))
    hi))
