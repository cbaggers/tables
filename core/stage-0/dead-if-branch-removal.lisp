(in-package :tables.compile.stage-0.dead-if-branch-removal)

;;------------------------------------------------------------

(defun run-pass (ssad-let)
  (remove-dead ssad-let))

;;------------------------------------------------------------

(defmethod remove-dead ((o ssad-let1))
  (with-slots (bindings body-form type) o
    (let* ((new-bindings
            (mapcar (lambda (b)
                      (with-slots (name form type) b
                        (let* ((new-form (remove-dead form))
                               (type (if (typep new-form 'ssad-let1)
                                         (slot-value new-form 'type)
                                         type))
                               (merge (and (typep new-form 'ssad-let1)
                                           (null (slot-value new-form
                                                             'bindings))))
                               (new-form (if merge
                                             (slot-value new-form 'body-form)
                                             new-form)))
                          (make-instance 'ssad-binding
                                         :name name
                                         :form new-form
                                         :type type))))
                    bindings)))
      (let* ((new-form (remove-dead body-form))
             (type (if (typep new-form 'ssad-let1)
                       (slot-value new-form 'type)
                       type)))
        (make-instance 'ssad-let1
                       :bindings new-bindings
                       :body-form new-form
                       :type type)))))

(defmethod remove-dead ((o ssad-lambda))
  (with-slots (args body-form result-type) o
    (let* ((new-form (remove-dead body-form))
           (type (if (typep new-form 'ssad-let1)
                     (slot-value new-form 'type)
                     result-type)))
      (make-instance 'ssad-lambda
                     :args args
                     :body-form new-form
                     :result-type type))))

(defmethod remove-dead ((o ssad-if))
  (with-slots (test then else) o
    (assert (atom test))
    (cond
      ((null test) else)
      ((eq test t) then)
      (t (make-instance 'ssad-if
                        :test test
                        :then (remove-dead then)
                        :else (remove-dead else))))))

(defmethod remove-dead ((o ssad-funcall))
  (with-slots (func args) o
    (make-instance 'ssad-funcall
                   :func (remove-dead func)
                   :args (mapcar (lambda (a) (remove-dead a))
                                 args))))

(defmethod remove-dead ((o symbol)) o)
(defmethod remove-dead ((o number)) o)

;;------------------------------------------------------------
