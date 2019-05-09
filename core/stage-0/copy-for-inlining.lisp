(in-package :tables.compile.stage-0)

;; {TODO} maybe make a stage-0.utils package and put this in that

(defmethod copy-for-inlining ((o ssad-let1) env)
  (with-slots (bindings body-form type) o
    (make-instance
     'ssad-let1
     :bindings (loop
                  :for binding :in bindings
                  :collect
                    (with-slots (name form type is-uniform) binding
                      (setf (gethash name env)
                            (make-instance
                             'ssad-binding
                             :name name
                             :form (copy-for-inlining form env)
                             :type type
                             :is-uniform is-uniform))))
     :body-form (copy-for-inlining body-form env)
     :type type)))

(defmethod copy-for-inlining ((o ssad-funcall) env)
  (with-slots (func args) o
    (make-instance
     'ssad-funcall
     :func (copy-for-inlining func env)
     :args (loop :for a :in args :collect (copy-for-inlining a env)))))

(defmethod copy-for-inlining ((o ssad-output) env)
  (with-slots (names args) o
    (make-instance
     'ssad-output
     :names names
     :args (loop :for a :in args :collect (copy-for-inlining a env)))))

(defmethod copy-for-inlining ((o ssad-lambda) env)
  (with-slots (args body-form result-type) o
    (make-instance
     'ssad-lambda
     :args args
     :body-form (copy-for-inlining body-form env)
     :result-type result-type)))

(defmethod copy-for-inlining ((o ssad-if) env)
  (with-slots (test then else) o
    (make-instance
     'ssad-if
     :test (copy-for-inlining test env)
     :then (copy-for-inlining then env)
     :else (copy-for-inlining else env))))

(defmethod copy-for-inlining ((o ssad-var) env)
  (with-slots (binding) o
    (make-instance
     'ssad-var
     :binding (let* ((name (slot-value binding 'name)))
                (or (gethash name env) binding)))))

(defmethod copy-for-inlining ((o ssad-constructed) env)
  (with-slots (form type) o
    (make-instance
     'ssad-constructed
     :form form
     :type type)))

(defmethod copy-for-inlining ((o ssad-read-col) env)
  (with-slots (name type) o
    (make-instance
     'ssad-read-col
     :name name
     :type type)))

(defmethod copy-for-inlining ((o ssad-constant) env)
  (with-slots (form type) o
    (make-instance
     'ssad-constant
     :form form
     :type type)))

(defmethod copy-for-inlining ((o symbol) env) o)
