(in-package :tables.compile)

(defun first-pass (ir)
  (tables.compile.stage-0.inline-top-level-functions:run-pass
   (tables.compile.stage-0.dead-binding-removal:run-pass
    (tables.compile.stage-0.dead-if-branch-removal:run-pass
     (tables.compile.stage-0.early-constant-folding:run-pass
      (tables.compile.stage-0.inline-direct-calls:run-pass
       (tables.compile.stage-0.dead-binding-removal:run-pass
        (tables.compile.stage-0.early-constant-folding:run-pass
         (tables.compile.stage-0.dead-binding-removal:run-pass
          ir)))))))))

(defun first-run (ctx code)
  (let ((ast (infer ctx code)))
    (first-pass
     (tables.compile.stage-0.ast-to-ir:run-pass ast))))

(defun test (&optional code uniforms)
  (let* ((ctx (make-check-context 'tables))
         (code
          `(let ,(loop
                    :for (n d) :in uniforms
                    :collect `(,n (checkmate.lang:construct ,d :arg)))
             ,code))
         (hi (first-run ctx code)))
    (loop
       :for i :below 10
       :do (setf
            hi
            (tables.compile.stage-0.compiler-macro-expand:run-pass
             (tables.compile.stage-0.uniform-local-lift:run-pass
              (tables.compile.stage-0.uniform-propagation:run-pass
               (tables.compile.stage-0.subexpression-elim:run-pass
                (tables.compile.stage-0.early-constant-folding:run-pass
                 (tables.compile.stage-0.dead-binding-removal:run-pass
                  (tables.compile.stage-0.inline-direct-calls:run-pass
                   (tables.compile.stage-0.dead-if-branch-removal:run-pass
                    hi))))))))))
    hi))

;; {TODO}
;;
;; - funcall sinking (push funcall of conditional result to branches)
;; - make and & or special forms?
;; - integer ops
;; - code hoisting
;; - add arg name to arg bindings
;;
;; Amongst others..

#+nil
(defun ftest ()
  (tables.compile::test
   '(funcall (if (checkmate.lang:construct boolean ??)
                 (lambda ((x i8)) (+ x x))
                 (lambda ((x i8)) (* x x)))
     1)))
