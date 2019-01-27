(in-package :tables-lang)

(defun last1 (list) (car (last list)))

(defun gensym-named (name)
  (gensym (format nil "~a_" name)))

;;------------------------------------------------------------
;; Acronyms
;;
;; ssad <- ssa'd <- single statically assigned <- single static assignment'ed
;;
;;------------------------------------------------------------

(defclass blockify-context ()
  ((parent :initarg :parent)
   (bindings :initform nil :initarg :bindings)
   (functions :initform nil :initarg :functions)))

(defun make-blockify-context (parent new-bindings new-functions)
  (let ((res (make-instance
              'blockify-context
              :parent parent
              :bindings (when parent
                          (slot-value parent 'bindings))
              :functions (when parent
                           (slot-value parent 'functions)))))
    (with-slots (bindings) res
      (loop
         :for binding :in new-bindings
         :do (setf bindings (cons binding bindings))))
    (with-slots (functions) res
      (loop
         :for function :in new-functions
         :do (setf functions (cons function functions))))
    res))

(defclass ssad-let1 ()
  ((bindings :initarg :bindings :initform nil)
   (body-form :initarg :body-form :initform nil)
   (type :initarg :type)))

(defclass ssad-binding ()
  ((name :initarg :name)
   (form :initarg :form)
   (type :initarg :type)))

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

(defgeneric as-debug-form (o)
  (:method (o) o))

(defmethod as-debug-form ((o ssad-let1))
  (with-slots (bindings body-form) o
    `(ssad-let1 ,(mapcar #'as-debug-form bindings)
                ,(as-debug-form body-form))))

(defmethod as-debug-form ((o ssad-binding))
  (with-slots (name form) o
    (list name (as-debug-form form))))

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

(defun blockify (context ast)
  (assert (eq (first ast) 'truly-the))
  (let* ((form (third ast))
         (blockified (blockify-form context form)))
    (typecase blockified
      ((or ssad-lambda ssad-if ssad-funcall)
       (list (make-instance 'ssad-binding
                            :name (gensym)
                            :form blockified
                            :type (second ast))))
      (ssad-let1
       (with-slots (bindings body-form) blockified
           (append
            bindings
            (list (make-instance 'ssad-binding
                                 :name (gensym)
                                 :form body-form
                                 :type (second ast))))))
      (ssad-binding
       (error "naked binding in blockify"))
      (t
       (list (make-instance 'ssad-binding
                            :name (gensym)
                            :form form
                            :type (second ast)))))))

(defun blockify-form (context form)
  (typecase form
    (list
     (case (first form)
       (lambda (blockify-lambda-form context form))
       (if (blockify-if-form context form))
       (let (blockify-let-form context form))
       (progn (blockify-progn-form context form))
       (funcall (blockify-funcall-form context form))
       (otherwise (error "not sure what to do with ~s" (first form)))))
    (symbol
     (if (or (eq form t) (null form))
         (make-instance 'ssad-let1
                        :body-form form
                        :type 'boolean)
         (blockify-var-access context form)))
    (otherwise
     form)))

(defun blockify-var-access (context symbol)
  (or (make-instance
       'ssad-let1
       :body-form (cdr (assoc symbol (slot-value context 'bindings))))
      (error "bug: ~s" symbol)))

(defun blockify-if-form (context form)
  (let* ((test (blockify context (second form)))
         (test-last (last1 test))
         (then (blockify context (third form)))
         (else (blockify context (fourth form))))
    (make-instance
     'ssad-let1
     :bindings test
     :body-form (make-instance
                 'ssad-if
                 :test (slot-value test-last 'name)
                 :then (with-slots (name type) (last1 then)
                         (make-instance
                          'ssad-let1
                          :bindings then
                          :body-form name
                          :type type))
                 :else (with-slots (name type) (last1 else)
                         (make-instance
                          'ssad-let1
                          :bindings else
                          :body-form name
                          :type type))))))

(defun blockify-let-form (context form)
  ;; note: this function is for let, not let*
  (let* ((renamed-args (loop
                          :for (name val) :in (second form)
                          :collect (list (gensym-named (symbol-name name))
                                         val)))
         (decls (loop
                   :for (name val) :in renamed-args
                   :for bindings := (blockify context val)
                   :append bindings
                   :collect (with-slots ((lname name) (ltype type))
                                (last1 bindings)
                              (make-instance 'ssad-binding
                                             :name name
                                             :form lname
                                             :type ltype))))
         (context (make-blockify-context
                   context
                   (loop
                      :for (old-name) :in (second form)
                      :for (new-name) :in renamed-args
                      :collect (cons old-name new-name))
                   nil))
         (body (third form))
         (bindings (blockify context body)))
    (with-slots (name type) (last1 bindings)
      (make-instance 'ssad-let1
                     :bindings (append decls bindings)
                     :body-form name
                     :type type))))

(defun blockify-progn-form (context form)
  (let* ((bindings (loop :for x :in (rest form)
                      :append (blockify context x))))
    (with-slots (name type) (last1 bindings)
      (make-instance 'ssad-let1
                     :bindings bindings
                     :body-form name
                     :type type))))

(defun blockify-lambda-form (context form)
  (let* ((renamed-args (loop
                          :for (name type) :in (second form)
                          :collect (list (gensym-named (symbol-name name))
                                         type)))
         (context (make-blockify-context
                   context
                   (loop
                      :for (old-name) :in (second form)
                      :for (new-name) :in renamed-args
                      :collect (cons old-name new-name))
                   nil))
         (body (third form))
         (bindings (blockify context body))
         (ssad-name (slot-value (last1 bindings) 'name)))
    (make-instance
     'ssad-lambda
     :args renamed-args
     :body-form (make-instance 'ssad-let1
                               :bindings bindings
                               :body-form ssad-name
                               :type (second body))
     :result-type (second body))))

(defun blockify-funcall-form (context expr-ast)
  (assert (eq (first expr-ast) 'funcall))
  (destructuring-bind (ssad-names prior-lets)
      (loop
         :for arg :in (rest expr-ast)
         :for blocked-arg := (blockify context arg)
         :for ssad-name := (slot-value (last1 blocked-arg) 'name)
         :collect ssad-name :into names
         :append blocked-arg :into bindings
         :finally (return (list names bindings)))
    (make-instance 'ssad-let1
                   :bindings prior-lets
                   :body-form (make-instance 'ssad-funcall
                                             :func (first ssad-names)
                                             :args (rest ssad-names)))))
