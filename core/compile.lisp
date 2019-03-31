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
            (tables.compile.stage-0.subexpression-elim:run-pass
             (tables.compile.stage-0.early-constant-folding:run-pass
              (tables.compile.stage-0.dead-binding-removal:run-pass
               (tables.compile.stage-0.inline-direct-calls:run-pass
                (tables.compile.stage-0.dead-if-branch-removal:run-pass
                 hi)))))))
    hi))

;; {TODO}
;;
;; - funcall sinking (push funcall of conditional result to branches)
;; - make and & or special forms?
;;
;; Amongst others..

#+nil
(defun ftest ()
  (tables.compile::test
   '(funcall (if (checkmate.lang:construct boolean ??)
                 (lambda ((x i8)) (+ x x))
                 (lambda ((x i8)) (* x x)))
     1)))
