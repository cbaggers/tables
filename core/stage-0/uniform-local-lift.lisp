(in-package :tables.compile.stage-0.uniform-local-lift)

;; description
;;
;; moves uniform forms to the top of the ssad-let1 they exist in
;; Does not lift as high as most things could be (globally) as we
;; need to look at data dependencies for that.
;; Once that is done this pass may be redundant.

(defun run-pass (ssad-let)
  (u-lift ssad-let)
  ssad-let)

(defmethod u-lift ((o ssad-let1))
  ;;
  (with-slots (bindings body-form) o
    (setf bindings
          (loop
             :for b :in bindings
             :if (slot-value b 'is-uniform)
             :collect b :into uniform
             :else
             :collect b :into normal
             :finally (return (append uniform normal))))
    (u-lift body-form)
    (values)))

(defmethod u-lift ((o ssad-lambda))
  (with-slots (body-form) o
    (u-lift o)
    (values)))

(defmethod u-lift ((o ssad-if))
  (with-slots (test then else) o
    (u-lift test)
    (u-lift then)
    (u-lift else)
    (values)))

(defmethod u-lift ((o ssad-funcall)) (values))
(defmethod u-lift ((o ssad-var)) (values))
(defmethod u-lift ((o symbol)) (values))
(defmethod u-lift ((o ssad-constant)) (values))
(defmethod u-lift ((o ssad-constructed)) (values))
