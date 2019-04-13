(in-package :tables.compile.stage-0.inline-direct-calls)

;; directly called function inlining
;;
;; WARNING: Pass mutates graph
;;

(defun run-pass (ssad-let)
  (inline-funcs ssad-let))

(defmethod inline-funcs ((o ssad-let1))
  (with-slots (bindings body-form type) o
    (setf bindings
          (loop
             :for binding :in bindings
             :for old-form := (slot-value binding 'form)
             :for new-form := (inline-funcs old-form)
             :do (setf (slot-value binding 'form) new-form)
             :if (typep old-form 'ssad-let1)
             :append (progn
                       (setf (slot-value binding 'form)
                             (slot-value new-form 'body-form))
                       (append (slot-value new-form 'bindings)
                               (list binding)))
             :else
             :collect binding))
    (setf body-form (inline-funcs body-form))
    o))

(defmethod inline-funcs ((o ssad-funcall))
  (with-slots (func args) o
    (etypecase func
      (ssad-constant o)
      (ssad-var
       (let ((form (slot-value (slot-value func 'binding) 'form)))
         (if (typep form 'ssad-lambda)
             (with-slots ((largs args)
                          (ltype result-type))
                 form
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
                 (inline-funcs copied)))
             o))))))

(defmethod inline-funcs ((o ssad-lambda))
  (with-slots (body-form) o
    (setf body-form (inline-funcs body-form))
    o))

(defmethod inline-funcs ((o ssad-if))
  (with-slots (test then else) o
    (setf test (inline-funcs test))
    (setf then (inline-funcs then))
    (setf else (inline-funcs else))
    o))

(defmethod inline-funcs ((o ssad-var)) o)
(defmethod inline-funcs ((o symbol)) o)
(defmethod inline-funcs ((o ssad-constant)) o)
(defmethod inline-funcs ((o ssad-constructed)) o)
