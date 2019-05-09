(in-package :tables.compile.stage-0.split-vertically)

(defstruct dep
  (vars nil)
  (cols nil))

(defun run-transform (ir)
  (check-type ir ssad-let1)
  (with-slots (body-form) ir
    (check-type body-form ssad-output)
    (with-slots (names args) body-form
      (if (<= (length names) 1)
          (list ir)
          (split-vertically (trace-and-group names args) ir)))))

(defun trace-and-group (names args)
  (let* ((groups
          (make-array (length names) :fill-pointer 0 :adjustable t)))
    (loop
       :for name :in names
       :for arg :in args
       :for dep := (make-dep)
       :do (trace-dependencies arg dep)
         (progn
           (loop
              :for group :across groups
              :for (vars . members) := group
              :when (intersection (dep-vars dep) vars)
              :do
                (setf (cdr group) (cons name members))
                (return)
              :finally (vector-push-extend
                        (cons (dep-vars dep) (list name))
                        groups))))
    groups))

(defun split-vertically (groups ir)
  (labels ((gen-ouput (output-ir output-names)
             (with-slots (args names) output-ir
               (loop
                  :for a :in args
                  :for n :in names
                  :when (find n output-names)
                  :collect a :into new-args
                  :and
                  :collect n :into new-names
                  :finally
                    (return
                      (make-instance
                       'ssad-output
                       :names new-names
                       :args new-args)))))
           (extract-bindings (used-args)
             (with-slots (bindings) ir
               (loop
                  :for b :in bindings
                  :when (find (slot-value b 'name) used-args)
                  :collect b)))
           (extract (output-names used-args)
             (with-slots (body-form) ir
               (let ((new-output (gen-ouput body-form output-names)))
                 (make-instance
                  'ssad-let1
                  :type (slot-value ir 'type)
                  :body-form new-output
                  :bindings (extract-bindings used-args))))))
    (loop
       :for (used-args . output-names) :across groups
       :collect (extract output-names used-args))))

(defmethod trace-dependencies ((node ssad-var) (dep dep))
  (with-slots (binding) node
    (with-slots (form) binding
      (if (typep form 'ssad-read-col)
          (setf (dep-cols dep) (cons form (dep-cols dep)))
          (progn
            (setf (dep-vars dep) (cons (slot-value binding 'name)
                                       (dep-vars dep)))
            (trace-dependencies binding dep))))
    (values)))

(defmethod trace-dependencies ((binding ssad-binding) (dep dep))
  (with-slots (form) binding
    (trace-dependencies form dep)
    (values)))

(defmethod trace-dependencies ((node ssad-if) (dep dep))
  (with-slots (test then else) node
    (trace-dependencies test dep)
    (trace-dependencies then dep)
    (trace-dependencies else dep)
    (values)))

(defmethod trace-dependencies ((node ssad-let1) (dep dep))
  (with-slots (body-form) node
    (trace-dependencies body-form dep)
    (values)))

(defmethod trace-dependencies ((node ssad-funcall) (dep dep))
  (with-slots (args) node
    (loop :for arg :in args :do (trace-dependencies arg dep))
    (values)))

(defmethod trace-dependencies ((node ssad-constant) (dep dep))
  (declare (ignore node dep))
  (values))

(defmethod trace-dependencies ((node ssad-constructed) (dep dep))
  (declare (ignore node dep))
  (values))
