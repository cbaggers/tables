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

(defun run-pass (ssad-let)
  (tables.compile.stage-0.vars-to-bindings:run-pass
   (cfold ssad-let)))

(defmethod cfold ((o ssad-let1))
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
                  (setf form (cfold form))
                  nil)))))
    ;;
    (with-slots (bindings body-form type) o
      (setf bindings (remove-if #'fold bindings))
      (setf body-form (cfold body-form))
      o)))

(defmethod cfold ((o ssad-lambda))
  (with-slots (body-form) o
    (setf body-form (cfold body-form))
    o))

(defmethod cfold ((o ssad-if))
  (with-slots (test then else) o
    (setf test (cfold test))
    (setf then (cfold then))
    (setf else (cfold else))
    o))

(defmethod cfold ((o ssad-funcall))
  (with-slots (func args) o
    (setf func (cfold func))
    (setf args (mapcar #'cfold args))
    o))

(defmethod cfold ((o ssad-var))
  (with-slots (binding) o
    (with-slots (name form type) binding
      (cond
        ((null name)
         (if (typep form 'ssad-var)
             (progn
               (setf binding (slot-value form 'binding))
               o)
             form))
        ((foldable-constant-p form)
         form)
        (t o)))))

(defmethod cfold ((o symbol)) o)
(defmethod cfold ((o ssad-constant)) o)
(defmethod cfold ((o ssad-constructed)) o)

(defun foldable-constant-p (constant)
  (typep constant 'ssad-constant))
