(in-package :tables.compile.stage-0.uniform-local-lift)

;; description
;;
;; moves uniform forms to the top of the ssad-let1 they exist in
;; Does not lift as high as most things could be (globally) as we
;; need to look at data dependencies for that.
;; Once that is done this pass may be redundant.

(defun run-pass (ssad-let cmp-ctx)
  (u-lift ssad-let cmp-ctx)
  (values))

(defmethod u-lift ((o ssad-let1) cmp-ctx)
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
    (u-lift body-form cmp-ctx)
    (values)))

(defmethod u-lift ((o ssad-lambda) cmp-ctx)
  (with-slots (body-form) o
    (u-lift o cmp-ctx)
    (values)))

(defmethod u-lift ((o ssad-if) cmp-ctx)
  (with-slots (test then else) o
    (u-lift test cmp-ctx)
    (u-lift then cmp-ctx)
    (u-lift else cmp-ctx)
    (values)))

(defmethod u-lift ((o ssad-funcall) cmp-ctx) (values))
(defmethod u-lift ((o ssad-output) cmp-ctx) (values))
(defmethod u-lift ((o ssad-var) cmp-ctx) (values))
(defmethod u-lift ((o symbol) cmp-ctx) (values))
(defmethod u-lift ((o ssad-constant) cmp-ctx) (values))
(defmethod u-lift ((o ssad-constructed) cmp-ctx) (values))
