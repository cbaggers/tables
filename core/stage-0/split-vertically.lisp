(in-package :tables.compile.stage-0.split-vertically)

(defstruct dep
  (vars nil)
  (cols nil))

(defstruct group
  vars
  outputs
  (cols nil))

(defun run-transform (ir)
  (check-type ir ssad-let1)
  (with-slots (body-form) ir
    (check-type body-form ssad-output)
    (with-slots (names args) body-form
      (split-vertically (trace-and-group names args) ir))))

(defun trace-and-group (names args)
  (let* ((groups
          (make-array (length names) :fill-pointer 0 :adjustable t
                      :element-type 'group
                      :initial-element (make-group :vars :invalid))))
    (loop
       :for name :in names
       :for arg :in args
       :for dep := (make-dep)
       :do
         (trace-dependencies arg dep)
         (loop
            :for group :across groups
            :when (intersection (dep-vars dep) (group-vars group))
            :do
              (progn
                (push name (group-outputs group))
                (loop
                   :for v :in (dep-vars dep)
                   :do (pushnew v (group-vars group)))
                (loop
                   :for c :in (dep-cols dep)
                   :do (pushnew c (group-cols group)))
                (return))
            :finally (vector-push-extend
                      (make-group
                       :vars (dep-vars dep)
                       :outputs (list name)
                       :cols (dep-cols dep))
                      groups)))
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
           (scan-for-used-vals (node-type val-bindings)
             (reverse
              (reduce
               (lambda (a b)
                 (with-slots (form) b
                   (if (typep form node-type)
                       (with-slots (name type) form
                         (cons (list name type) a))
                       a)))
               val-bindings
               :initial-value nil)))
           (extract (output-names vars cols)
             (with-slots ((output-node body-form)) ir
               (let* ((used-args (append vars cols))
                      (new-output (gen-ouput output-node output-names))
                      (bindings (extract-bindings used-args))
                      (new-ir (make-instance
                               'ssad-let1
                               :type (slot-value ir 'type)
                               :body-form new-output
                               :bindings bindings))
                      (output-varyings
                       (loop
                          :for a :in (slot-value new-output 'args)
                          :for n :in (slot-value new-output 'names)
                          :for type :=
                            (etypecase a
                              (ssad-var
                               (slot-value (slot-value a 'binding) 'type))
                              (ssad-constant
                               (slot-value a 'type)))
                          :collect (list n type)))
                      (used-varyings
                       (scan-for-used-vals 'ssad-read-varying bindings))
                      (used-uniforms
                       (scan-for-used-vals 'ssad-read-uniform bindings)))
                 (make-instance
                  'subquery
                  :uniform-args used-uniforms
                  :input-varyings used-varyings
                  :output-varyings output-varyings
                  :ir new-ir)))))
    (loop
       :for group :across groups
       :collect (extract (group-outputs group)
                         (group-vars group)
                         (group-cols group)))))

(defmethod trace-dependencies ((node ssad-var) (dep dep))
  (with-slots (binding) node
    (with-slots (form name) binding
      (typecase form
        (ssad-read-varying
         (pushnew name (dep-cols dep)))
        (ssad-read-uniform
         (pushnew name (dep-cols dep)))
        (t
          (setf (dep-vars dep) (cons (slot-value binding 'name)
                                     (dep-vars dep)))
          (trace-dependencies binding dep))))
    (values)))

(defmethod trace-dependencies ((binding ssad-binding) (dep dep))
  (with-slots (form) binding
    (trace-dependencies form dep)
    (values)))

(defmethod trace-dependencies ((binding ssad-slot-value) (dep dep))
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
