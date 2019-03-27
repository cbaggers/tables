(in-package :tables.compile.stage-0)

;;
;; IR nodes for stage-0
;;
;;------------------------------------------------------------

(defclass ssad-let1 ()
  ((bindings :initarg :bindings :initform nil)
   (body-form :initarg :body-form :initform nil)
   (type :initarg :type)))

(defclass ssad-binding ()
  ((name :initarg :name)
   (form :initarg :form)
   (type :initarg :type)))

(defclass ssad-var ()
  ((binding :initarg :binding)))

(defclass ssad-lambda ()
  ((args :initarg :args)
   (body-form :initarg :body-form) ;; always a ssad-let1
   (result-type :initarg :result-type)))

(defclass ssad-if ()
  ((test :initarg :test)
   (then :initarg :then)   ;; always a ssad-let1
   (else :initarg :else))) ;; always a ssad-let1

(defclass ssad-funcall ()
  ((func :initarg :func)
   (args :initarg :args)))

(defclass ssad-constant ()
  ((form :initarg :form)
   (type :initarg :type)))

(defclass ssad-constructed ()
  ((form :initarg :form)
   (type :initarg :type)))

;;------------------------------------------------------------

(defgeneric as-debug-form (o))

(defmethod as-debug-form ((o ssad-let1))
  (with-slots (bindings body-form) o
    `(ssad-let1 ,(mapcar #'as-debug-form bindings)
                ,(as-debug-form body-form))))

(defmethod as-debug-form ((o ssad-binding))
  (with-slots (name form) o
    (list name (as-debug-form form))))

(defmethod as-debug-form ((o ssad-var))
  (with-slots (binding) o
    (with-slots (name) binding
      (list :var name))))

(defmethod as-debug-form ((o ssad-lambda))
  (with-slots (args body-form) o
    (list 'ssad-lambda args
          (as-debug-form body-form))))

(defmethod as-debug-form ((o ssad-if))
  (with-slots (test then else) o
    (list 'ssad-if
          (as-debug-form test)
          (as-debug-form then)
          (as-debug-form else))))

(defmethod as-debug-form ((o ssad-funcall))
  (with-slots (func args) o
    `(ssad-funcall ,(as-debug-form func)
                   ,@(mapcar #'as-debug-form args))))

(defmethod as-debug-form ((o ssad-constant))
  (with-slots (form) o
    (list :constant form)))

(defmethod as-debug-form ((o symbol))
  o)

(defmethod as-debug-form ((o ssad-constructed))
  (with-slots (type form) o
    (list :construct type form)))

;;------------------------------------------------------------

(defmethod print-object ((o ssad-let1) stream)
  (format stream "#~a" (as-debug-form o)))

(defmethod print-object ((o ssad-binding) stream)
  (format stream "#~a" (as-debug-form o)))

(defmethod print-object ((o ssad-lambda) stream)
  (format stream "#~a" (as-debug-form o)))

(defmethod print-object ((o ssad-if) stream)
  (format stream "#~a" (as-debug-form o)))

(defmethod print-object ((o ssad-funcall) stream)
  (format stream "#~a" (as-debug-form o)))

;;------------------------------------------------------------
