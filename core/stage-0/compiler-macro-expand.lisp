(in-package :tables.compile.stage-0.compiler-macro-expand)

;; description

(defun run-pass (ssad-let)
  (let ((check-env (make-check-context 'tables)))
    (cm-expand ssad-let check-env)))

(defmethod cm-expand ((o ssad-let1) check-env)
  ;;
  (with-slots (bindings body-form) o
    (setf bindings
          (loop
             :for binding :in bindings
             :for form := (slot-value binding 'form)
             :for ir := (cm-expand form check-env)
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
    (cm-expand body-form check-env)
    o))

(defmethod cm-expand ((o ssad-lambda) check-env)
  (with-slots (body-form) o
    (cm-expand body-form check-env)
    o))

(defmethod cm-expand ((o ssad-if) check-env)
  (with-slots (test then else) o
    (cm-expand test check-env)
    (cm-expand then check-env)
    (cm-expand else check-env)
    o))

(defmethod cm-expand ((o ssad-funcall) check-env)
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
                    (funcall expander
                             (loop
                                :for a :in args :collect
                                  (if (typep a 'ssad-constant)
                                      (slot-value a 'form)
                                      a))
                             :TODO_ENVIRONMENT)
                  (if is-new-code
                      (tables.compile.stage-0.ast-to-ir:run-pass
                       (check check-env new-code return-type))
                      o)))
              o))
        o)))

(defmethod cm-expand ((o ssad-var) check-env)
  (declare (ignore check-env))
  o)
(defmethod cm-expand ((o symbol) check-env)
  (declare (ignore check-env))
  o)
(defmethod cm-expand ((o ssad-constant) check-env)
  (declare (ignore check-env))
  o)
(defmethod cm-expand ((o ssad-constructed) check-env)
  (declare (ignore check-env))
  o)
