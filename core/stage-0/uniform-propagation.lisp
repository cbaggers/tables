(in-package :tables.compile.stage-0.uniform-propagation)

;; description

(defun run-pass (ssad-let)
  (u-prop ssad-let)
  ssad-let)

(defmethod u-prop ((o ssad-let1))
  ;;
  (with-slots (bindings body-form) o
    (loop
       :for b :in bindings
       :do (u-prop-binding b))
    (u-prop body-form)
    (values)))

(defun u-prop-binding (binding)
  (labels ((arg-is-uniform-p (arg)
             (typecase arg
               (ssad-constant t)
               (ssad-var
                (slot-value (slot-value arg 'binding)
                            'is-uniform)))))
    (with-slots (form is-uniform) binding
      (setf is-uniform
            (typecase form
              (ssad-funcall
               (with-slots (func args) form
                 (and (typep func 'ssad-constant)
                      (every #'arg-is-uniform-p args))))
              (ssad-constructed
               (with-slots ((cform form)) form
                 (eq cform :arg)))
              (ssad-if
               (with-slots (test then else) form
                 (and (arg-is-uniform-p test)
                      (arg-is-uniform-p then)
                      (arg-is-uniform-p else))))))))
  (values))

(defmethod u-prop ((o ssad-lambda))
  (with-slots (body-form) o
    (u-prop body-form)
    (values)))

(defmethod u-prop ((o ssad-if))
  (with-slots (test then else) o
    (u-prop test)
    (u-prop then)
    (u-prop else)
    (values)))

(defmethod u-prop ((o ssad-funcall)) (values))
(defmethod u-prop ((o ssad-var)) (values))
(defmethod u-prop ((o symbol)) (values))
(defmethod u-prop ((o ssad-constant)) (values))
(defmethod u-prop ((o ssad-constructed)) (values))
