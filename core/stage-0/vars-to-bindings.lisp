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

(defun run-pass (ssad-let cmp-ctx)
  (vars-to-bindings ssad-let nil cmp-ctx)
  (values))

(defun push-into-env (bindings env cmp-ctx)
  (labels ((proc (env binding)
             (with-slots (name form) binding
               (setf form (vars-to-bindings form env cmp-ctx))
               (acons name binding env))))
    (reduce #'proc bindings :initial-value env)))

(defmethod vars-to-bindings ((o ssad-let1) env cmp-ctx)
  (with-slots (bindings body-form) o
    (let ((env (push-into-env bindings env cmp-ctx)))
      (setf body-form (vars-to-bindings body-form env cmp-ctx))
      o)))

(defmethod vars-to-bindings ((o ssad-lambda) env cmp-ctx)
  (with-slots (args body-form result-type) o
    (let* ((bindings
            (loop
               :for (name type) :in args
               :collect (make-instance
                         'ssad-binding
                         :name name
                         :form (make-instance 'ssad-read-varying
                                              :type type
                                              :name name)
                         :type type)))
           (env (push-into-env bindings env cmp-ctx)))
      (setf body-form (vars-to-bindings body-form env cmp-ctx))
      o)))

(defmethod vars-to-bindings ((o ssad-if) env cmp-ctx)
  (with-slots (test then else) o
    (setf test (vars-to-bindings test env cmp-ctx)
          then (vars-to-bindings then env cmp-ctx)
          else (vars-to-bindings else env cmp-ctx))
    o))

(defmethod vars-to-bindings ((o ssad-funcall) env cmp-ctx)
  (with-slots (func args) o
    (setf func (vars-to-bindings func env cmp-ctx))
    (setf args (mapcar (lambda (a) (vars-to-bindings a env cmp-ctx))
                       args))
    o))

(defmethod vars-to-bindings ((o ssad-output) env cmp-ctx)
  (with-slots (args) o
    (setf args (mapcar (lambda (a) (vars-to-bindings a env cmp-ctx))
                       args))
    o))

(defmethod vars-to-bindings ((o symbol) env cmp-ctx)
  (let ((binding (assocr o env)))
    (if binding
        (make-instance 'ssad-var :binding binding)
        o)))

(defmethod vars-to-bindings ((o ssad-constant) env cmp-ctx)
  (declare (ignore env))
  o)

(defmethod vars-to-bindings ((o ssad-var) env cmp-ctx)
  (declare (ignore env))
  o)

(defmethod vars-to-bindings ((o ssad-constructed) env cmp-ctx)
  (declare (ignore env))
  o)

(defmethod vars-to-bindings ((o ssad-read-varying) env cmp-ctx)
  (declare (ignore env))
  o)

(defmethod vars-to-bindings ((o ssad-read-uniform) env cmp-ctx)
  (declare (ignore env))
  o)
