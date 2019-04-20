(in-package :tables.compile)

(defun type-check (code)
  (let ((ctx (make-check-context 'tables)))
    (infer ctx code)))

(defun typed-ast->ir (typed-ast)
  (tables.compile.stage-0.ast-to-ir:run-pass typed-ast))

(defun first-pass (ir ctx)
  (tables.compile.stage-0.dead-binding-removal:run-pass ir ctx)
  (tables.compile.stage-0.early-constant-folding:run-pass ir ctx)
  (tables.compile.stage-0.dead-binding-removal:run-pass ir ctx)
  (tables.compile.stage-0.inline-direct-calls:run-pass ir ctx)
  (tables.compile.stage-0.early-constant-folding:run-pass ir ctx)
  (tables.compile.stage-0.dead-if-branch-removal:run-pass ir ctx)
  (tables.compile.stage-0.dead-binding-removal:run-pass ir ctx)
  (tables.compile.stage-0.inline-top-level-functions:run-pass ir ctx)
  (values))

(defun run-passes (ir ctx)
  (tables.compile.stage-0.dead-if-branch-removal:run-pass ir ctx)
  (tables.compile.stage-0.inline-direct-calls:run-pass ir ctx)
  (tables.compile.stage-0.dead-binding-removal:run-pass ir ctx)
  (tables.compile.stage-0.early-constant-folding:run-pass ir ctx)
  (tables.compile.stage-0.subexpression-elim:run-pass ir ctx)
  (tables.compile.stage-0.uniform-propagation:run-pass ir ctx)
  (tables.compile.stage-0.uniform-local-lift:run-pass ir ctx)
  (tables.compile.stage-0.compiler-macro-expand:run-pass ir ctx)
  (tables.compile.stage-0.inline-conditional-call:run-pass ir ctx)
  (tables.compile.stage-0.inline-conditional-constants:run-pass ir ctx)
  (values))

(defun test (&optional code uniforms)
  (let* ((code
          `(let ,(loop
                    :for (n d) :in uniforms
                    :collect `(,n (checkmate.lang:construct ,d :arg)))
             ,code))
         (typed-ast (type-check code))
         (ir (typed-ast->ir typed-ast))
         (compile-ctx (make-compile-context))
         (passes 0))
    (first-pass ir compile-ctx)
    (loop
       :do
         (incf passes)
         (clear-mark compile-ctx)
         (run-passes ir compile-ctx)
       :while (marked-changed-p compile-ctx)
       :when (> passes 1000)
       :do (error "Too many passes"))
    (values ir passes)))

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
;; - make and & or special forms?
;; - add arg name to arg bindings
;; - query if var/form depends on var (lets us do limited lamdba fold)
;; - track number of things depending on binding?
;;
;; Amongst others..

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
