(in-package :tables.compile.stage-0.cleanup-outputs)

(defun run-pass (ssad-let cmp-ctx)
  (with-slots (bindings body-form type) ssad-let
    (assert (eq (ttype-of type) 'tables.lang::outputs))
    (when (typep body-form 'ssad-var)
      (mark-changed cmp-ctx)
      (setf body-form (slot-value (last1 bindings) 'form)
            bindings (butlast bindings)))
    (values)))
