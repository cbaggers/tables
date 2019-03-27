(in-package :tables.compile.stage-0.subexpression-elim)

;; description

(defun run-pass (ssad-let)
  (s-elim ssad-let nil)
  ssad-let)

(defmethod s-elim ((o ssad-let1) env)
  ;;
  (with-slots (bindings body-form) o
    (loop
       :for b :in bindings
       :for new-pair := (s-elim-binding b env)
       :when new-pair
       :do (push new-pair env))
    (s-elim body-form env)
    (values)))

(defun form-key (form)
  (typecase form
    (ssad-funcall
     (labels ((arg-key (arg)
                (etypecase arg
                  (ssad-var (slot-value (slot-value arg 'binding) 'name))
                  (ssad-constant (slot-value arg 'form)))))
       (with-slots (func args) form
         (append (list 'funcall (slot-value func 'form))
                 (mapcar #'arg-key args)))))))

(defun s-elim-binding (o env)
  (with-slots (form) o
    (let ((key (form-key form)))
      ;;(print (list :> form key))
      (when key
        (let ((existing-form (when key (assocr key env :test #'equal))))
          (if existing-form
              (progn
                (print "yay!")
                (setf form (make-instance 'ssad-var :binding existing-form))
                nil)
              (cons key o)))))))

(defmethod s-elim ((o ssad-lambda) env)
  (with-slots (body-form) o
    (s-elim o env)
    (values)))

(defmethod s-elim ((o ssad-if) env)
  (with-slots (test then else) o
    (s-elim test env)
    (s-elim then env)
    (s-elim else env)
    (values)))

(defmethod s-elim ((o ssad-funcall) env) (values))
(defmethod s-elim ((o ssad-var) env) (values))
(defmethod s-elim ((o symbol) env) (values))
(defmethod s-elim ((o ssad-constant) env) (values))
(defmethod s-elim ((o ssad-constructed) env) (values))
