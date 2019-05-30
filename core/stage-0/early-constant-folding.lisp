(in-package :tables.compile.stage-0.early-constant-folding)

;; early local constant folding
;;
;; This pass is dirt simple as it's only goal is to remove some
;; cruft from the previous pass. It's main advantage is to us as
;; the reader as it makes the ssa'd code easier to read.
;;
;; As a side effect it removes unused constant bindings
;;
;; WARNING: Pass mutates graph
;;

(defun run-pass (ssad-let cmp-ctx)
  (cfold ssad-let cmp-ctx)
  (tables.compile.stage-0.vars-to-bindings:run-pass ssad-let cmp-ctx)
  (values))

(defmethod cfold ((o ssad-let1) cmp-ctx)
  ;;
  (labels ((fold (binding)
             (with-slots (name form type) binding
               (cond
                 ((foldable-constant-p form)
                  (setf name nil)
                  t)
                 ((symbolp form)
                  (setf name nil)
                  t)
                 ((typep form 'ssad-var)
                  (setf name nil)
                  (with-slots ((b binding)) form
                    (unless (slot-value b 'name)
                      (setf form (slot-value b 'form))))
                  t)
                 (t
                  (setf form (cfold form cmp-ctx))
                  nil)))))
    ;;
    (with-slots (bindings body-form type) o
      (let ((new-bindings (remove-if #'fold bindings)))
        (when (/= (length new-bindings) (length bindings))
          (mark-changed cmp-ctx)
          (setf bindings new-bindings)))
      (setf body-form (cfold body-form cmp-ctx))
      o)))

(defmethod cfold ((o ssad-lambda) cmp-ctx)
  (with-slots (body-form) o
    (setf body-form (cfold body-form cmp-ctx))
    o))

(defmethod cfold ((o ssad-if) cmp-ctx)
  (with-slots (test then else) o
    (setf test (cfold test cmp-ctx))
    (setf then (cfold then cmp-ctx))
    (setf else (cfold else cmp-ctx))
    o))

(defmethod cfold ((o ssad-funcall) cmp-ctx)
  (with-slots (func args) o
    (setf func (cfold func cmp-ctx))
    (setf args (mapcar (lambda (x) (cfold x cmp-ctx))
                       args))
    o))

(defmethod cfold ((o ssad-output) cmp-ctx)
  (with-slots (args) o
    (setf args (mapcar (lambda (x) (cfold x cmp-ctx))
                       args))
    o))

(defmethod cfold ((o ssad-var) cmp-ctx)
  (with-slots (binding) o
    (with-slots (name form type) binding
      (cond
        ((null name)
         (if (typep form 'ssad-var)
             (progn
               (mark-changed cmp-ctx)
               (setf binding (slot-value form 'binding))
               o)
             form))
        ((foldable-constant-p form)
         (mark-changed cmp-ctx)
         form)
        (t o)))))

(defmethod cfold ((o symbol) cmp-ctx) o)
(defmethod cfold ((o ssad-constant) cmp-ctx) o)
(defmethod cfold ((o ssad-constructed) cmp-ctx) o)
(defmethod cfold ((o ssad-read-varying) cmp-ctx) o)
(defmethod cfold ((o ssad-read-uniform) cmp-ctx) o)

(defun foldable-constant-p (constant)
  (typep constant 'ssad-constant))
