(in-package :tables.compile.stage-0.inline-direct-calls)

;; directly called function inlining
;;
;; WARNING: Pass mutates graph
;;

(defun run-pass (ssad-let cmp-ctx)
  (inline-funcs ssad-let cmp-ctx))

(defmethod inline-funcs ((o ssad-let1) cmp-ctx)
  (with-slots (bindings body-form type) o
    (setf bindings
          (loop
             :for binding :in bindings
             :for old-form := (slot-value binding 'form)
             :for new-form := (inline-funcs old-form cmp-ctx)
             :do (setf (slot-value binding 'form) new-form)
             :if (typep old-form 'ssad-let1)
             :append (progn
                       (setf (slot-value binding 'form)
                             (slot-value new-form 'body-form))
                       (append (slot-value new-form 'bindings)
                               (list binding)))
             :else
             :collect binding))
    (setf body-form (inline-funcs body-form cmp-ctx))
    o))

(defmethod inline-funcs ((o ssad-funcall) cmp-ctx)
  (with-slots (func args) o
    (etypecase func
      (ssad-constant o)
      (ssad-var
       (let ((form (slot-value (slot-value func 'binding) 'form)))
         (if (typep form 'ssad-lambda)
             (with-slots ((largs args)
                          (ltype result-type))
                 form
               (mark-changed cmp-ctx)
               (let* ((lbody (slot-value form 'body-form))
                      (copy-env (make-hash-table))
                      (new-bindings
                       (mapcar
                        (lambda (arg form)
                          (let* ((arg-name (first arg))
                                 (arg-type (second arg))
                                 (b (make-instance
                                     'ssad-binding
                                     :name (gensym)
                                     :form form
                                     :type arg-type)))
                            (setf (gethash arg-name copy-env) b)))
                        largs
                        args))
                      (copied
                       (copy-for-inlining lbody copy-env)))
                 (with-slots (bindings) copied
                   (setf bindings (append new-bindings bindings)))
                 (inline-funcs copied cmp-ctx)))
             o))))))

(defmethod inline-funcs ((o ssad-output) cmp-ctx)
  (with-slots (args) o
    (setf args (mapcar (lambda (x) (inline-funcs x cmp-ctx))
                       args))
    o))

(defmethod inline-funcs ((o ssad-lambda) cmp-ctx)
  (with-slots (body-form) o
    (setf body-form (inline-funcs body-form cmp-ctx))
    o))

(defmethod inline-funcs ((o ssad-if) cmp-ctx)
  (with-slots (test then else) o
    (setf test (inline-funcs test cmp-ctx))
    (setf then (inline-funcs then cmp-ctx))
    (setf else (inline-funcs else cmp-ctx))
    o))

(defmethod inline-funcs ((o ssad-var) cmp-ctx) o)
(defmethod inline-funcs ((o symbol) cmp-ctx) o)
(defmethod inline-funcs ((o ssad-constant) cmp-ctx) o)
(defmethod inline-funcs ((o ssad-constructed) cmp-ctx) o)
