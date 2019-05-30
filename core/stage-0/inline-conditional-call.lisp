(in-package :tables.compile.stage-0.inline-conditional-call)

;; directly called function inlining
;;
;; WARNING: Pass mutates graph
;;

(defun run-pass (ssad-let cmp-ctx)
  (inline-cond-calls ssad-let cmp-ctx))

(defmethod inline-cond-calls ((o ssad-let1) cmp-ctx)
  (with-slots (bindings body-form type) o
    (setf bindings
          (loop
             :for binding :in bindings
             :for old-form := (slot-value binding 'form)
             :for new-form := (inline-cond-calls old-form cmp-ctx)
             :do (setf (slot-value binding 'form) new-form)
             :if (typep old-form 'ssad-let1)
             :append (progn
                       (setf (slot-value binding 'form)
                             (slot-value new-form 'body-form))
                       (append (slot-value new-form 'bindings)
                               (list binding)))
             :else
             :collect binding))
    (setf body-form (inline-cond-calls body-form cmp-ctx))
    o))

(defmethod inline-cond-calls ((o ssad-funcall) cmp-ctx)
  (labels ((inject (ir-let args)
             (with-slots ((src-bindings bindings)
                          (src-body-form body-form)
                          (src-type type))
                 ir-let
               (let* ((call
                       (make-instance 'ssad-funcall
                                      :func src-body-form
                                      :args args))
                      (result-type
                       (dbind-ttype (function ~ ~return-type) src-type
                         return-type))
                      (foop
                       (make-instance 'ssad-binding
                                      :name (gensym)
                                      :form call
                                      :type result-type)))
                 ;; {TODO} might be able to reuse original ssad-let1 node
                 (make-instance
                  'ssad-let1
                  :bindings (append src-bindings (list foop))
                  :body-form (make-instance 'ssad-var :binding foop)
                  :type result-type)))))
    (with-slots (func args) o
      (etypecase func
        (ssad-constant o)
        (ssad-var
         (let ((form (slot-value (slot-value func 'binding) 'form)))
           (if (typep form 'ssad-if)
               (with-slots (test then else) form
                 (let* ((cthen
                         (copy-for-inlining then (make-hash-table)))
                        (celse
                         (copy-for-inlining else (make-hash-table))))
                   (mark-changed cmp-ctx)
                   (make-instance
                    'ssad-if
                    :test (make-instance
                           'ssad-var :binding (slot-value test 'binding))
                    :then (inject cthen args)
                    :else (inject celse args))))
               o)))))))

(defmethod inline-cond-calls ((o ssad-lambda) cmp-ctx)
  (with-slots (body-form) o
    (setf body-form (inline-cond-calls body-form cmp-ctx))
    o))

(defmethod inline-cond-calls ((o ssad-if) cmp-ctx)
  (with-slots (test then else) o
    (setf test (inline-cond-calls test cmp-ctx))
    (setf then (inline-cond-calls then cmp-ctx))
    (setf else (inline-cond-calls else cmp-ctx))
    o))

(defmethod inline-cond-calls ((o ssad-var) cmp-ctx) o)
(defmethod inline-cond-calls ((o ssad-output) cmp-ctx) o)
(defmethod inline-cond-calls ((o symbol) cmp-ctx) o)
(defmethod inline-cond-calls ((o ssad-constant) cmp-ctx) o)
(defmethod inline-cond-calls ((o ssad-constructed) cmp-ctx) o)
(defmethod inline-cond-calls ((o ssad-read-varying) cmp-ctx) o)
(defmethod inline-cond-calls ((o ssad-read-uniform) cmp-ctx) o)
