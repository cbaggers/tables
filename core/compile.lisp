(in-package :tables.internals)

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
  (tables.compile.stage-0.record-to-slot-forms:run-pass ir ctx)
  (tables.compile.stage-0.early-constant-folding:run-pass ir ctx)
  (tables.compile.stage-0.dead-if-branch-removal:run-pass ir ctx)
  (tables.compile.stage-0.dead-binding-removal:run-pass ir ctx)
  (tables.compile.stage-0.inline-top-level-functions:run-pass ir ctx)
  (values))

(defun run-passes (ir ctx)
  (tables.compile.stage-0.dead-if-branch-removal:run-pass ir ctx)
  (tables.compile.stage-0.inline-direct-calls:run-pass ir ctx)
  (tables.compile.stage-0.record-to-slot-forms:run-pass ir ctx)
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

(defun process-varying-declarations (varyings)
  (let* ((all-varying-names (mapcar #'first (mapcar #'rest varyings)))
         (dup-names (find-duplicates all-varying-names)))
    (assert (not dup-names) ()
            "Duplicate arguments found in query:~%~{~a~^, ~}"
            dup-names)
    (loop
       :with inputs := nil
       :with outputs := nil
       :for (table-designator . vset) :in varyings
       :for table := (find-table table-designator)
       :for spec := (table-spec table)
       :for col-specs := (slot-value spec 'column-specs)
       :do
         (loop
            :for varying :in vset
            :for (var-name col-name direction)
              := (if (= (length varying) 3)
                     varying
                     (list (first varying) (first varying) (second varying)))
            :do
              (let* ((col
                      (or (find col-name col-specs
                                :key (lambda (x)
                                       (slot-value x 'name)))
                          (error "no column named ~a found in table ~a"
                                 col-name table)))
                     (type (slot-value col 'type))
                     (varying (make-instance
                               'varying
                               :name var-name
                               :table table
                               :column-name col-name
                               :type type
                               :type-designator (ttype-of type))))
                (case direction
                  (:in/out
                   (push varying inputs)
                   (push varying outputs))
                  ((nil :in) (push varying inputs))
                  (:out (push varying outputs))
                  (otherwise (error "Invalid varying direction ~a for ~a"
                                    direction var-name)))))
       :finally (return
                  (values
                   (reverse inputs)
                   (reverse outputs))))))

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
                  :for varying := (find oname outputs
                                        :key (lambda (x) (slot-value x 'name))
                                        :test #'string-desig-and=)
                  :for aname := (slot-value varying 'name)
                  :for atype := (slot-value varying 'type)
                  :do (push oname onames)
                  :unless aname
                  :do (error "Unknown output ~a.~%Val: ~a" oname oval))
               (loop
                  :for varying :in outputs
                  :for aname := (slot-value varying 'name)
                  :unless (find aname onames :test #'string-desig-and=)
                  :do (error "'~a' declared as output but not written to"
                             aname)))))
    (let* ((output-form (find-outputs body)))
      (if output-form
          (match-outputs (rest output-form))
          (dispatch-err))
      (values body outputs))))

(defun add-args (inputs uniforms body)
  `(let ,(append
          (loop
             :for (n d) :in inputs
             :collect `(,n (tables.lang::read-varying ,d ,n)))
          (loop
             :for (n d) :in uniforms
             :collect `(,n (tables.lang::read-uniform ,d ,n))))
     ,body))

(defclass varying ()
  ((name :initarg :name)
   (table :initarg :table)
   (column-name :initarg :column-name)
   (type :initarg :type)
   (type-designator :initarg :type-designator)))

(defclass uniform ()
  ((name :initarg :name)
   (type :initarg :type)
   (type-designator :initarg :type-designator)))

(defclass preprocessed-query ()
  ((varying-inputs :initarg :varying-inputs)
   (uniform-inputs :initarg :uniform-inputs)
   (varying-outputs :initarg :varying-outputs)
   (body :initarg :body)))

(defun preprocess-query (varyings uniforms body)
  (let ((body (macroexpand-query body)))
    (multiple-value-bind (inputs outputs)
        (process-varying-declarations varyings)
      (multiple-value-bind (body used-sorted-outputs)
          (process-outputs outputs body)
        (make-instance 'preprocessed-query
                       :uniform-inputs uniforms
                       :varying-inputs inputs
                       :varying-outputs used-sorted-outputs
                       :body body)))))

(defun stage-0-compile (preprocessed-query)
  (with-slots (uniform-inputs varying-inputs varying-outputs body)
      preprocessed-query
    (let* ((body (add-args varying-inputs uniform-inputs body))
           (typed-ast (type-check body varying-outputs))
           (ir (typed-ast->ir typed-ast))
           (pass-count (run-passes-until-stablized ir))
           (sub-queries
            (tables.compile.stage-0.split-vertically:run-transform
             (copy-for-inlining ir (make-hash-table)))))
      (values sub-queries pass-count))))

(defun compile-query (varyings uniforms body)
  ;;(validate-table-columns table-name inputs uniforms outputs)
  (multiple-value-bind (sub-queries pass-count)
      (stage-0-compile (preprocess-query varyings uniforms body))
    (tables.backends.fallback:transform sub-queries pass-count)))

(defun validate-table-columns (table-name inputs uniforms outputs)
  (declare (ignore table-name inputs uniforms outputs))
  ;; Here is want to extract the table info check that the required
  ;; columns exist and add any additional metadata we have on that column.
  ;; For example whether the column is clustered
  ;; We may also expand one column into many (depending on layout)
  t)

;;------------------------------------------------------------
