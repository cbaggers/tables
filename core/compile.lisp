(in-package :tables.compile)

(defun type-check (code outputs)
  (let ((ctx (make-check-context 'tables :user-data outputs)))
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

(defun run-passes-until-stablized (ir &key (max-passes 1000))
  (let ((compile-ctx (make-compile-context)))
    (first-pass ir compile-ctx)
    (prog1
        (loop
           :for passes :from 1
           :do
             (clear-mark compile-ctx)
             (run-passes ir compile-ctx)
           :while (marked-changed-p compile-ctx)
           :when (> passes max-passes)
           :do (error "Too many passes")
           :finally (return passes))
      (tables.compile.stage-0.cleanup-outputs:run-pass ir compile-ctx))))

(defun macroexpand-query (body)
  body)

(defun process-varying-declarations (varyings)
  (let (inputs outputs)
    (loop
       :for (name type direction) :in varyings
       :do
         (assert (not (find name inputs :key #'first)) ()
                 "Duplicate argument named ~a in query" name)
         (assert (not (find name outputs :key #'first)) ()
                 "Duplicate argument named ~a in query" name)
         (when (keywordp type)
           (if (and (find type #(:in :out :in/out)) (null direction))
               (error "missing type for varying ~a" name)
               (error "invalid type ~s for varying ~a" type name)))
         (if (eq direction :in/out)
             (progn
               (push (list name type) inputs)
               (push (list name type) outputs))
             (case direction
               ((nil :in) (push (list name type) inputs))
               (:out (push (list name type) outputs))
               (otherwise (error "Invalid varying direction ~a for ~a"
                                 direction name)))))
    (values
     (reverse inputs)
     (reverse outputs))))

(defun process-outputs (outputs body)
  (labels ((outputs-p (x)
             (and (listp x) (eq (first x) 'output)))
           (dispatch-err ()
             (let ((misplaced (find-in-tree-if #'outputs-p body)))
               (if misplaced
                   (error "Output form found but not in tail position")
                   (error "A query must contain an output form in tail position"))))
           (find-outputs (form)
             (unless (atom form)
               (case (first form)
                 ((let progn)
                   (find-outputs (last1 form)))
                 (output form))))
           (match-outputs (output-plist)
             (let (onames)
               (loop
                  :for (oname oval) :on output-plist :by #'cddr
                  :for (aname atype) := (find oname outputs :key #'first
                                              :test #'string-desig-and=)
                  :do (push oname onames)
                  :unless aname
                  :do (error "Unknown output ~a.~%Val: ~a" oname oval))
               (loop
                  :for (aname) :in outputs
                  :unless (find aname onames :test #'string-desig-and=)
                  :do (error "'~a' declared as output but not written to"
                             aname)))))
    (let* ((output-form (find-outputs body)))
      (if output-form
          (match-outputs (rest output-form))
          (dispatch-err))
      (values body outputs))))

(defun add-args (inputs uniforms body)
  `(let ,(loop
            :for (n d) :in (append inputs uniforms)
            :collect `(,n (tables.lang::read-val ,d ,n)))
     ,body))

;;#+nil
(defun compile-query (varyings uniforms body)
  (let ((body (macroexpand-query body)))
    (multiple-value-bind (inputs outputs)
        (process-varying-declarations varyings)
      (multiple-value-bind (body used-sorted-outputs)
          (process-outputs outputs body)
        (let* ((body (add-args inputs uniforms body))
               (typed-ast (type-check body used-sorted-outputs))
               (ir (typed-ast->ir typed-ast))
               (passes (run-passes-until-stablized ir))
               (queries
                (tables.compile.stage-0.split-vertically:run-transform
                 (tables.compile.stage-0:copy-for-inlining
                  ir (make-hash-table)))))
          (values queries passes))))))

;; {TODO}
;;
;; - code hoisting
;; - make and & or special forms?
;; - add arg name to arg bindings
;; - query if var/form depends on var (lets us do limited lamdba fold)
;; - track number of things depending on binding?
;;
;; Amongst others..
