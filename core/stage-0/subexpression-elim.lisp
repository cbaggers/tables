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
       :for new-pairs := (s-elim-binding b env)
       :when new-pairs
       :do (setf env (append new-pairs env)))
    (s-elim body-form env)
    (values)))

(defun form-key (binding-name form)
  (typecase form
    (ssad-funcall
     (labels ((arg-key (arg)
                (etypecase arg
                  (ssad-var (slot-value (slot-value arg 'binding) 'name))
                  (ssad-constant (slot-value arg 'form)))))
       (with-slots (func args) form
         (when (typep func 'ssad-constant)
           (let* ((func-name
                   (second (slot-value func 'form)))
                  (func-info
                   (gethash func-name *registered-top-level-functions*)))
             (values
              `(funcall ,func-name ,@(mapcar #'arg-key args))
              ;; if it's a record constructor then the values in the
              ;; slots are the arguments to the function
              (when func-info
                (loop
                   :for s :in (record-ctor-slots func-info)
                   :for a :in args
                   :for elim := (typecase a
                                  (ssad-var (slot-value a 'binding))
                                  (ssad-constant a))
                   :when elim
                   :collect (cons `(funcall ,s ,binding-name)
                                  elim)))))))))))

(defun s-elim-binding (o env)
  (with-slots (form name) o
    (multiple-value-bind (key record-accessor-pairs)
        (form-key name form)
      (when key
        (let ((existing-form (assocr key env :test #'equal)))
          (if existing-form
              (etypecase existing-form
                (ssad-binding
                 (setf form (make-instance
                             'ssad-var :binding existing-form))
                 nil)
                (ssad-constant
                 (setf form existing-form)
                 nil))
              (cons (cons key o) record-accessor-pairs)))))))

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
