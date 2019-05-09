(in-package :tables.compile.stage-0.compiler-macro-expand)

;; description

(defun run-pass (ssad-let cmp-ctx)
  (let ((check-env (make-check-context 'tables)))
    (cm-expand ssad-let check-env cmp-ctx)
    (values)))

(defmethod cm-expand ((o ssad-let1) check-env cmp-ctx)
  ;;
  (with-slots (bindings body-form) o
    (setf bindings
          (loop
             :for binding :in bindings
             :for form := (slot-value binding 'form)
             :for ir := (cm-expand form check-env cmp-ctx)
             :if (eq ir form)
             :collect binding
             :else
             :append (if (typep ir 'ssad-let1)
                         (with-slots (bindings body-form type) ir
                           (with-slots (is-uniform name type form)
                               binding
                             (setf is-uniform nil
                                   name (gensym)
                                   type type
                                   form body-form))
                           (append bindings
                                   (list binding)))
                         (progn
                           (setf (slot-value binding 'form) ir)
                           (list binding)))))
    (cm-expand body-form check-env cmp-ctx)
    o))

(defmethod cm-expand ((o ssad-lambda) check-env cmp-ctx)
  (with-slots (body-form) o
    (cm-expand body-form check-env cmp-ctx)
    o))

(defmethod cm-expand ((o ssad-if) check-env cmp-ctx)
  (with-slots (test then else) o
    (cm-expand test check-env cmp-ctx)
    (cm-expand then check-env cmp-ctx)
    (cm-expand else check-env cmp-ctx)
    o))

(defmethod cm-expand ((o ssad-funcall) check-env cmp-ctx)
  (with-slots (func args) o
    (if (typep func 'ssad-constant)
        (let* ((fform (slot-value func 'form))
               (func-type (slot-value func 'type))
               (fname (second fform))
               (expander (gethash fname *registered-compiler-macros*)))
          (if expander
              (dbind-ttype (function ~ ~return-type)
                  func-type
                (multiple-value-bind (new-code is-new-code)
                    (funcall expander args :TODO_ENVIRONMENT)
                  ;; I want to do this here:
                  ;; (if (typep new-code 'ir-node)
                  ;;         new-code
                  ;;         (tables.compile.stage-0.ast-to-ir:run-pass
                  ;;          (check check-env new-code return-type)))
                  ;; but we need to check that new-code has the same type
                  ;; as the original so we'd probably need to expose unify
                  (if is-new-code
                      (progn
                        (mark-changed cmp-ctx)
                        (tables.compile.stage-0.ast-to-ir:run-pass
                        (check check-env new-code return-type)))
                      o)))
              o))
        o)))

(defmethod cm-expand ((o ssad-var) check-env cmp-ctx)
  (declare (ignore check-env))
  o)
(defmethod cm-expand ((o ssad-output) check-env cmp-ctx)
  (declare (ignore check-env))
  o)
(defmethod cm-expand ((o symbol) check-env cmp-ctx)
  (declare (ignore check-env))
  o)
(defmethod cm-expand ((o ssad-constant) check-env cmp-ctx)
  (declare (ignore check-env))
  o)
(defmethod cm-expand ((o ssad-constructed) check-env cmp-ctx)
  (declare (ignore check-env))
  o)
(defmethod cm-expand ((o ssad-read-col) check-env cmp-ctx)
  (declare (ignore check-env))
  o)
