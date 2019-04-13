(in-package :tables.compile.stage-0.vars-to-bindings)

;;
;; This pass replaces all variables that dont reference func args
;; with a var object that directly points to the binding
;;
;; This turns the code into a more direct graph so we dont have to
;; pass environments around in some passes
;;
;;
;; WARNING: Pass mutates graph
;;

(defun run-pass (ssad-let)
  (vars-to-bindings ssad-let nil))

(defun push-into-env (bindings env)
  (labels ((proc (env binding)
             (with-slots (name form) binding
               (setf form (vars-to-bindings form env))
               (acons name binding env))))
    (reduce #'proc bindings :initial-value env)))

(defmethod vars-to-bindings ((o ssad-let1) env)
  (with-slots (bindings body-form) o
    (let ((env (push-into-env bindings env)))
      (setf body-form (vars-to-bindings body-form env))
      o)))

(defmethod vars-to-bindings ((o ssad-lambda) env)
  (with-slots (args body-form result-type) o
    (let* ((bindings
            (loop
               :for (name type) :in args
               :collect (make-instance
                         'ssad-binding
                         :name name
                         :form (make-instance 'ssad-constructed
                                              :type type
                                              :form :arg)
                         :type type)))
           (env (push-into-env bindings env)))
      (setf body-form (vars-to-bindings body-form env))
      o)))

(defmethod vars-to-bindings ((o ssad-if) env)
  (with-slots (test then else) o
    (setf test (vars-to-bindings test env)
          then (vars-to-bindings then env)
          else (vars-to-bindings else env))
    o))

(defmethod vars-to-bindings ((o ssad-funcall) env)
  (with-slots (func args) o
    (setf func (vars-to-bindings func env))
    (setf args (mapcar (lambda (a) (vars-to-bindings a env)) args))
    o))

(defmethod vars-to-bindings ((o symbol) env)
  (let ((binding (assocr o env)))
    (if binding
        (make-instance 'ssad-var :binding binding)
        o)))

(defmethod vars-to-bindings ((o ssad-constant) env)
  (declare (ignore env))
  o)

(defmethod vars-to-bindings ((o ssad-var) env)
  (declare (ignore env))
  o)

(defmethod vars-to-bindings ((o ssad-constructed) env)
  (declare (ignore env))
  o)
