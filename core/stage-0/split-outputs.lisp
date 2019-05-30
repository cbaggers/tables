(in-package :tables.compile.stage-0.split-outputs)

;;------------------------------------------------------------

(defun run-transform (ssad-let)
  (split-outputs ssad-let)
  (values))

;;------------------------------------------------------------

(defmethod split-outputs ((o ssad-let1))
  (with-slots (bindings body-form type) o
    (loop
       :for b :in bindings
       :do (split-outputs (slot-value b 'form)))
    (let ((additional-bindings (split-outputs body-form)))
      (when additional-bindings
        (setf bindings
              (append bindings additional-bindings))))
    nil))

(defmethod split-outputs ((o ssad-lambda))
  (with-slots (body-form) o
    (split-outputs body-form)))

(defmethod split-outputs ((o ssad-slot-value))
  (with-slots (form) o
    (split-outputs form)))

(defmethod split-outputs ((o ssad-if))
  (with-slots (test then else) o
    (append
     (split-outputs test)
     (split-outputs then)
     (split-outputs else))))

(defmethod split-outputs ((o ssad-funcall))
  (with-slots (func args) o
    (split-outputs func)
    (mapcan (lambda (a) (split-outputs a)) args)))

(defmethod split-outputs ((o ssad-output))
  (labels ((split-arg (output-name arg)
             (etypecase arg
               (ssad-var
                (with-slots (name form type) (slot-value arg 'binding)
                  (if (record-type-p type)
                      (let ((info (ttype-aggregate-info type)))
                        (with-slots (slots) info
                          (loop :for slot :in slots :collect
                               (with-slots ((slot-name name)
                                            (slot-type type))
                                   slot
                                 (let* ((binding-name (gensym))
                                        (binding
                                         (make-instance
                                          'ssad-binding
                                          :name binding-name
                                          :form (make-instance
                                                 'ssad-slot-value
                                                 :form arg
                                                 :name slot-name
                                                 :type slot-type)
                                          :type slot-type
                                          :is-uniform nil)))
                                   (list
                                    binding
                                    (make-instance
                                     'ssad-write-varying
                                     :name output-name
                                     :form (make-instance
                                            'ssad-var
                                            :binding binding)
                                     :slot-id slot-name)))))))
                      (let ((foo (make-instance
                                  'ssad-write-varying
                                  :name output-name
                                  :form arg
                                  :slot-id nil)))
                        (list (list nil foo))))))
               (ssad-constant
                (let ((foo (make-instance
                                  'ssad-write-varying
                                  :name output-name
                                  :form arg
                                  :slot-id nil)))
                  (list (list nil foo)))))))
    (with-slots (names args) o
      ;; all args will be constants or vars
      (loop
         :for name :in names
         :for arg :in args
         :for new := (split-arg name arg)
         :for new-bindings := (remove nil (mapcar #'first new))
         :for new-args := (mapcar #'second new)
         :append new-args :into final-args
         :append (make-list (length new-args)
                            :initial-element name) :into final-names
         :append new-bindings :into final-bindings
         :finally (progn
                    (setf names final-names
                          args final-args)
                    (return final-bindings))))))

(defmethod split-outputs ((o ssad-var)) nil)
(defmethod split-outputs ((o ssad-write-varying)) nil)
(defmethod split-outputs ((o symbol)) nil)
(defmethod split-outputs ((o ssad-constant)) nil)
(defmethod split-outputs ((o ssad-constructed)) nil)
(defmethod split-outputs ((o ssad-read-varying)) nil)
(defmethod split-outputs ((o ssad-read-uniform)) nil)

;;------------------------------------------------------------
