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
               (tables.compile.stage-0.dead-if-branch-removal:run-pass
                hi))))))
    hi))

;; {TODO}
;;
;; - common subexpression elimination
;; - record decomposition (split slots into vars)
;; - funcall sinking (push funcall of conditional result to branches)
;; - make and & or special forms?
;;
;; Amongst others..

;; == BUG ==
#+nil
(defun foo ()
  (tables.compile::test
   '(let ((a (checkmate.lang:construct i8 dang)))
     (let ((b a))
       (+ (- a 2) (- 1 2))))))


#+nil
(defun ftest ()
  (tables.compile::test
   '(funcall (if (checkmate.lang:construct boolean ??)
                 (lambda ((x i8)) (+ x x))
                 (lambda ((x i8)) (* x x)))
     1)))
