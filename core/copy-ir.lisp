(in-package :tables.internals)

;; {TODO} maybe make a stage-0.utils package and put this in that

(defmethod copy-ir ((o ssad-let1) env)
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
                             :form (copy-ir form env)
                             :type type
                             :is-uniform is-uniform))))
     :body-form (copy-ir body-form env)
     :type type)))

(defmethod copy-ir ((o ssad-funcall) env)
  (with-slots (func args) o
    (make-instance
     'ssad-funcall
     :func (copy-ir func env)
     :args (loop :for a :in args :collect (copy-ir a env)))))

(defmethod copy-ir ((o ssad-write-varying) env)
  (with-slots (form name slot-id) o
    (make-instance
     'ssad-write-varying
     :form (copy-ir form env)
     :name name
     :slot-id slot-id)))

(defmethod copy-ir ((o ssad-slot-value) env)
  (with-slots (form name type) o
    (make-instance
     'ssad-slot-value
     :form (copy-ir form env)
     :name name
     :type type)))

(defmethod copy-ir ((o ssad-output) env)
  (with-slots (names args) o
    (make-instance
     'ssad-output
     :names names
     :args (loop :for a :in args :collect (copy-ir a env)))))

(defmethod copy-ir ((o ssad-lambda) env)
  (with-slots (args body-form result-type) o
    (make-instance
     'ssad-lambda
     :args args
     :body-form (copy-ir body-form env)
     :result-type result-type)))

(defmethod copy-ir ((o ssad-if) env)
  (with-slots (test then else) o
    (make-instance
     'ssad-if
     :test (copy-ir test env)
     :then (copy-ir then env)
     :else (copy-ir else env))))

(defmethod copy-ir ((o ssad-var) env)
  (with-slots (binding) o
    (make-instance
     'ssad-var
     :binding (let* ((name (slot-value binding 'name)))
                (or (gethash name env) binding)))))

(defmethod copy-ir ((o ssad-constructed) env)
  (with-slots (form type) o
    (make-instance
     'ssad-constructed
     :form form
     :type type)))

(defmethod copy-ir ((o ssad-read-varying) env)
  (with-slots (name type) o
    (make-instance
     'ssad-read-varying
     :name name
     :type type)))

(defmethod copy-ir ((o ssad-read-uniform) env)
  (with-slots (name type) o
    (make-instance
     'ssad-read-uniform
     :name name
     :type type)))

(defmethod copy-ir ((o ssad-constant) env)
  (with-slots (form type) o
    (make-instance
     'ssad-constant
     :form form
     :type type)))

(defmethod copy-ir ((o symbol) env) o)
