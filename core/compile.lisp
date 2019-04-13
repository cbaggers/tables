(in-package :tables.compile)

(defun first-run (ctx code)
  (let ((ast (infer ctx code)))
    (tables.compile.stage-0.inline-top-level-functions:run-pass
     (tables.compile.stage-0.dead-binding-removal:run-pass
      (tables.compile.stage-0.dead-if-branch-removal:run-pass
       (tables.compile.stage-0.early-constant-folding:run-pass
        (tables.compile.stage-0.inline-direct-calls:run-pass
         (tables.compile.stage-0.dead-binding-removal:run-pass
          (tables.compile.stage-0.early-constant-folding:run-pass
           (tables.compile.stage-0.dead-binding-removal:run-pass
            (tables.compile.stage-0.ast-to-ir:run-pass ast)))))))))))

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
            (tables.compile.stage-0.inline-conditional-call:run-pass
             (tables.compile.stage-0.compiler-macro-expand:run-pass
              (tables.compile.stage-0.uniform-local-lift:run-pass
               (tables.compile.stage-0.uniform-propagation:run-pass
                (tables.compile.stage-0.subexpression-elim:run-pass
                 (tables.compile.stage-0.early-constant-folding:run-pass
                  (tables.compile.stage-0.dead-binding-removal:run-pass
                   (tables.compile.stage-0.inline-direct-calls:run-pass
                    (tables.compile.stage-0.dead-if-branch-removal:run-pass
                     hi)))))))))))
    hi))

#+nil
;; BUG: infinite loop in print (well designator-from-type really)
;;      as the type is properly recursive! The are type is the
;;      function type. I love that the checker was happy to do this
;;      it's rather mind bending.
;;      I guess we need a *print-circle* for types XD. or actually..
;;      we just need to make the designator circular, and punt to
;;      CL for the printing shiz.
(infer 'tables
       '(lambda ((f (function ((function (?a) ?b)) ?b)))
          (funcall f f)))

;; {TODO}
;;
;; - code hoisting
;; - funcall sinking (push funcall of conditional result to branches)
;; - make and & or special forms?
;; - add arg name to arg bindings
;; - integer ops
;; - query if var/form depends on var (lets us do limited lamdba fold)
;; - track number of things depending on binding?
;;
;; Amongst others..

#+nil
(test '(let ((f (if a
                    (lambda ((x i8)) (* x x))
                    (lambda ((x i8)) (+ x x)))))
        (+ (funcall f 10)
         (funcall f 20)))
      '((a boolean)))

#||

funcalls of ifs returning lambdas is still super interesting. One thing
I need to work out is, when is there ever other bindings in an 'if' branch
which also returns a lambda?

Anything that isnt returned, or isnt depended on by the thing returned is
stripped. That means the other thing needs to be used by the lambda.

This means closures.

Now we have no mutation so once it's captured it wont change. This feels
like it gives us an opportunity for simplifying the handling here.



||#
