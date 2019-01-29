(in-package :tables.compile.stage-0.dead-binding-removal)

;;------------------------------------------------------------

(defun run-pass (ssad-let)
  (let* ((live-set (make-hash-table)))
    (find-live ssad-let live-set)
    (remove-dead ssad-let live-set)))

;;------------------------------------------------------------

(defmethod find-live ((o ssad-let1) live)
  (with-slots (bindings body-form) o
    (mapcar (lambda (b) (find-live (slot-value b 'form) live))
            bindings)
    (find-live body-form live)))

(defmethod find-live ((o ssad-lambda) live)
  (with-slots (body-form) o
    (find-live body-form live)))

(defmethod find-live ((o ssad-if) live)
  (with-slots (test then else) o
    (find-live test live)
    (find-live then live)
    (find-live else live)))

(defmethod find-live ((o ssad-funcall) live)
  (with-slots (func args) o
    (find-live func live)
    (map nil (lambda (a) (find-live a live)) args)))

(defmethod find-live ((o symbol) live)
  (setf (gethash o live) t))

(defmethod find-live ((o number) live))

;;------------------------------------------------------------

(defmethod remove-dead ((o ssad-let1) live)
  (with-slots (bindings body-form type) o
    (let* ((pruned
            (remove-if-not (lambda (b)
                             (gethash (slot-value b 'name) live))
                           bindings))
           (new-bindings
            (mapcar (lambda (b)
                      (with-slots (name form type) b
                        (make-instance 'ssad-binding
                                       :name name
                                       :form (remove-dead form live)
                                       :type type)))
                    pruned)))
      (make-instance 'ssad-let1
                     :bindings new-bindings
                     :body-form (remove-dead body-form live)
                     :type type))))

(defmethod remove-dead ((o ssad-lambda) live)
  (with-slots (args body-form result-type) o
    (make-instance 'ssad-lambda
                   :args args
                   :body-form (remove-dead body-form live)
                   :result-type result-type)))

(defmethod remove-dead ((o ssad-if) live)
  (with-slots (test then else) o
    (make-instance 'ssad-if
                   :test (remove-dead test live)
                   :then (remove-dead then live)
                   :else (remove-dead else live))))

(defmethod remove-dead ((o ssad-funcall) live)
  (with-slots (func args) o
    (make-instance 'ssad-funcall
                   :func (remove-dead func live)
                   :args (mapcar (lambda (a) (remove-dead a live))
                                 args))))

(defmethod remove-dead ((o symbol) live) o)
(defmethod remove-dead ((o number) live) o)

;;------------------------------------------------------------
