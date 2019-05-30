(in-package :tables.compile.stage-0.uniform-propagation)

;; description

(defun run-pass (ssad-let cmp-ctx)
  (u-prop ssad-let cmp-ctx)
  (values))

(defmethod u-prop ((o ssad-let1) cmp-ctx)
  ;;
  (with-slots (bindings body-form) o
    (loop
       :for b :in bindings
       :do (u-prop-binding b cmp-ctx))
    (u-prop body-form cmp-ctx)
    (values)))

(defun u-prop-binding (binding cmp-ctx)
  (labels ((arg-is-uniform-p (arg)
             (typecase arg
               (ssad-constant t)
               (ssad-var
                (slot-value (slot-value arg 'binding)
                            'is-uniform)))))
    (with-slots (form is-uniform) binding
      (u-prop form cmp-ctx)
      (setf is-uniform
            (typecase form
              (ssad-funcall
               (with-slots (func args) form
                 (and (typep func 'ssad-constant)
                      (every #'arg-is-uniform-p args))))
              (ssad-read-varying t)
              (ssad-read-uniform t)
              (ssad-if
               (with-slots (test then else) form
                 (and (arg-is-uniform-p test)
                      (arg-is-uniform-p then)
                      (arg-is-uniform-p else)))))))
    (values)))

(defmethod u-prop ((o ssad-lambda) cmp-ctx)
  (with-slots (body-form) o
    (u-prop body-form cmp-ctx)
    (values)))

(defmethod u-prop ((o ssad-if) cmp-ctx)
  (with-slots (test then else) o
    (u-prop test cmp-ctx)
    (u-prop then cmp-ctx)
    (u-prop else cmp-ctx)
    (values)))

(defmethod u-prop ((o ssad-funcall) cmp-ctx) (values))
(defmethod u-prop ((o ssad-slot-value) cmp-ctx) (values))
(defmethod u-prop ((o ssad-output) cmp-ctx) (values))
(defmethod u-prop ((o ssad-var) cmp-ctx) (values))
(defmethod u-prop ((o symbol) cmp-ctx) (values))
(defmethod u-prop ((o ssad-constant) cmp-ctx) (values))
(defmethod u-prop ((o ssad-constructed) cmp-ctx) (values))
(defmethod u-prop ((o ssad-read-varying) cmp-ctx) (values))
(defmethod u-prop ((o ssad-read-uniform) cmp-ctx) (values))
