(uiop:define-package :ttype (:use :cl))
(uiop:define-package :ttype-classes (:use))
(in-package :ttype)

;; designator->type
;; type->designator
;; unify
;; (defmethod no-applicable-method ((method (eql #'foo)) &rest args)
;;   (format t "no applicable method for (foo ~{~s~^ ~})~%" args))

;; This first version will have no multiple value return and
;; will use void as it's (values) type.

;;------------------------------------------------------------

(defclass check-context ()
  ((function-types :initform nil :initarg :function-types)
   (variable-bindings :initform nil :initarg :variable-bindings)
   (parent :initform nil :initarg :parent)))

;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
;; Next issue is how to handle the relationship between contexts.
;; We want to make context's often, but we want to be able to traverse
;; in terms of scope too (to find bindings etc)
;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^


(defun make-check-context ()
  (make-instance 'check-context))

(defun get-function-type (context name)
  (labels ((inner (context)
             (with-slots (function-types parent) context
               (or (when function-types (gethash name function-types))
                   (when parent (inner parent))))))
    (inner context)))

(defun get-binding (context name)
  (labels ((inner (context)
             (with-slots (variable-bindings parent) context
               (or (when variable-bindings (gethash name variable-bindings))
                   (when parent (inner parent))))))
    (inner context)))

(defun add-binding (context name type)
  (assert (type-ref-p type))
  (let ((bindings (make-hash-table)))
    (setf (gethash name bindings) type)
    (make-instance 'check-context
                   :variable-bindings bindings
                   :parent context)))

(defun add-bindings (context name-type-pairs)
  (let ((bindings (make-hash-table)))
    (loop
       :for (name type) :in name-type-pairs
       :do
         (assert (type-ref-p type))
         (setf (gethash name bindings) type))
    (make-instance 'check-context
                   :variable-bindings bindings
                   :parent context)))

;;------------------------------------------------------------

;; {TODO} ttype needs to have a load-form, which implies all
;;        elements in the designator need to have that too

(defclass ttype ()
  ((refs :initform nil)
   (known-complete :initform nil)))

(defclass constraint ()
  ((spec :initarg :spec)
   (name :initarg :name)
   (arg-vals :initarg :arg-vals)))

(defclass constraint-ref ()
  ((target :initarg :spec)
   (designator :initarg :designator)))

(defclass type-ref ()
  ((target :initarg :target)))

(defun naked-type-p (x)
  (typep x 'ttype))

(defclass unknown (ttype)
  ((name :initform (gensym "?UT"))
   (constraints :initform nil :initarg :constraints)))

(defclass tfunction (ttype)
  ((arg-types :initform nil :initarg :arg-types)
   (return-type :initform nil :initarg :return-type)))

(defmacro ttype (designator)
  (designator->type designator))

(defun unknown-designator-name-p (name)
  (and (symbolp name)
       (not (keywordp name))
       (let ((sname (symbol-name name)))
         (and (> (length sname ) 1)
              (char= (char sname 0) #\?)))))

;;------------------------------------------------------------

(defclass ttype-parameter ()
  ((name :initarg :name)
   (spec :initarg :spec)
   (value :initarg :value)
   (refs :initform nil)))

(defclass unknown-param (ttype-parameter)
  ((name :initform (gensym "?UP"))
   (value :initform nil)))

(defclass ttype-parameter-spec ()
  ((name :initarg :name)
   (unify :initarg :unify)
   (to-param :initarg :to-param)))

(defclass param-ref ()
  ((target :initarg :target)))

(defun tparam-val (param-ref)
  (check-type param-ref param-ref)
  (slot-value (deref param-ref)
              'value))

(defun naked-param-p (x)
  (typep x 'ttype-parameter))

(defun naked-constraint-p (x)
  (typep x 'constraint))

;;------------------------------------------------------------

(defclass constraint-spec ()
  ((name :initarg :name)
   (init :initarg :init)
   (satisfies :initarg :satisfies)
   (desig-to-constraint :initarg :desig-to-constraint)
   (arg-param-specs :initarg :arg-param-specs)
   (custom-data :initarg :custom-data :initform nil)))

(defclass user-ttype (ttype)
  ((spec :initarg :spec)
   (name :initarg :name)
   (arg-vals :initarg :arg-vals)
   (known-complete :initarg :known-complete :initform nil)))

(defclass user-ttype-spec ()
  ((name :initarg :name)
   (init :initarg :init)
   (arg-param-specs :initarg :arg-param-specs)
   (desig-to-type :initarg :desig-to-type)
   (custom-data :initarg :custom-data :initform nil)))

(defgeneric complete-p (type)
  (:method ((type unknown))
    (declare (ignore type))
    nil)
  (:method ((type tfunction))
    (calc-complete-p type))
  (:method ((type user-ttype))
    (calc-complete-p type))
  (:method ((param ttype-parameter))
    (declare (ignore param))
    t))

(defun calc-complete-p (type)
  (with-slots (known-complete) type
    (or known-complete
        (let ((complete (check-user-type-complete type)))
          (when complete
            (setf known-complete t))
          complete))))

(defun check-user-type-complete (type)
  (etypecase type
    (unknown nil)
    (unknown-param nil)
    (tfunction
     (with-slots (arg-types return-type) type
       (and (every (lambda (x) (complete-p (deref x))) arg-types)
            (complete-p (deref return-type)))))
    (user-ttype
     (let ((arg-vals (slot-value type 'arg-vals)))
       (assert (> (length arg-vals) 0) ()
               "BUG: this type should have been known-complete ~a"
               type)
       (every (lambda (x) (complete-p (deref x)))
              arg-vals)))
    (ttype-parameter t) ;; type params become type-refs not param-refs
    (ttype t)))

(defun tspec (type)
  (slot-value type 'spec))

(defvar *registered-user-types*
  (make-hash-table :test #'eq))

(defvar *registered-parameter-types*
  (make-hash-table :test #'eq))

(defvar *registered-constraints*
  (make-hash-table :test #'eq))

(defun get-parameter-type-spec (name)
  (or (gethash name *registered-parameter-types*)
      (error
       "define-ttype: ~a is not valid designator arg type.~%valid:~a"
       name (alexandria:hash-table-keys *registered-parameter-types*))))

(defun parse-ttype-lambda-list (lambda-list)
  (multiple-value-bind (required-parameters
                        optional-parameters
                        rest-parameters-name
                        keyword-parameters
                        has-allow-other-keys-p
                        aux-parameter)
      (alexandria:parse-ordinary-lambda-list lambda-list)
    (assert (not aux-parameter))
    (assert (not rest-parameters-name))
    (assert (not optional-parameters))
    (assert (not has-allow-other-keys-p))
    (assert (not (has-duplicates-p (append required-parameters
                                           keyword-parameters))))
    (list required-parameters
          (sort keyword-parameters #'string< :key #'first))))

(defvar *last-dropped-type* nil)
(defvar *last-dropped-constraint* nil)
(defvar *last-dropped-parameter-type* nil)

(defgeneric register-type (type-spec)
  (:method (spec)
    ;; this ↓ is just for debugging
    (setf *last-dropped-type* spec)
    (warn "register-type is not implemented")))

(defgeneric register-constraint (type-spec)
  (:method (spec)
    ;; this ↓ is just for debugging
    (setf *last-dropped-constraint* spec)
    (warn "register-type is not implemented")))

(defgeneric register-parameter-type (parameter-type-spec)
  (:method (spec)
    ;; this ↓ is just for debugging
    (setf *last-dropped-parameter-type* spec)
    (warn "register-parameter-type is not implemented")))

;;------------------------------------------------------------

;; Test impls. remove later
;; Later we want users to control when types are register. hmm maybe
;; we allow them to say when but not how.
(defmethod register-type (spec)
  (with-slots (name) spec
    (format t "~%;; Registered type ~a" name)
    (setf (gethash name *registered-user-types*) spec)))

(defmethod register-constraint (spec)
  (with-slots (name) spec
    (format t "~%;; Registered constraint ~a" name)
    (setf (gethash name *registered-constraints*) spec)))

(defmethod register-parameter-type (spec)
  (with-slots (name) spec
    (format t "~%;; Registered param type ~a" name)
    (setf (gethash name *registered-parameter-types*) spec)))

;;------------------------------------------------------------

(defmacro define-parameter-type (name
                                 &body rest
                                 &key valid-p equal)
  (declare (ignore rest))
  (assert (not (eq name 'ttype)))
  (assert (and (symbolp name) (not (keywordp name))))
  `(let ((valid-p (or ,valid-p #'identity))
         (param-equal-p ,equal))
     (labels ((to-param (spec val)
                (assert (eq (slot-value spec 'name) ',name))
                (assert (funcall valid-p val) ()
                        "~a is not a valid value to make a ~a type parameter"
                        val ',name)
                (take-ref
                 (make-instance 'ttype-parameter
                                :name ',name
                                :spec spec
                                :value val))))
       (register-parameter-type
        (make-instance 'ttype-parameter-spec
                       :name ',name
                       :unify (lambda (a b mut-p)
                                (declare (ignore mut-p))
                                (funcall param-equal-p a b))
                       :to-param #'to-param))
       ',name)))

;;------------------------------------------------------------

(defun ttype-p (ttype designator)
  (handler-case
      (progn
        (unify ttype (designator->type designator) nil)
        t)
    (error () nil)))

(defun ttype-of (type-ref)
  (with-slots (target) type-ref
    (with-slots (spec) target
      (designator-from-type target))))

(defun ttype-custom-data (type-ref)
  (with-slots (target) type-ref
    (with-slots (spec) target
      (slot-value spec 'custom-data))))

(defun designator-from-type (type)
  (check-type type ttype)
  (flet ((desig (p)
           (if (typep p 'type-ref)
               (designator-from-type (deref p))
               (let ((naked-param (deref p))
                     (value (tparam-val p)))
                 (if (typep naked-param 'unknown-param)
                     (slot-value naked-param 'name)
                     value)))))
    (etypecase type
      (unknown
       (slot-value type 'name))
      (tfunction
       (with-slots (arg-types return-type) type
         `(function ,(mapcar #'ttype-of arg-types)
                    ,(ttype-of return-type))))
      (user-ttype
       (with-slots (name arg-vals) type
         (if (> (length arg-vals) 0)
             (cons name (map 'list #'desig arg-vals))
             name))))))

;; {TODO} I'd like the user to have to specify the type of the designator
;;        argument. I think to start we will restrict it to types, symbols
;;        and numbers.
;; {TODO} where could be a a regular function rather than be generated by
;;        this macro
(defmacro define-ttype (designator
                        &body rest
                        &key where init custom-spec-data)
  (declare (ignore rest))
  (destructuring-bind (name . designator-args)
      (uiop:ensure-list designator)
    (destructuring-bind (req-args key-forms)
        (parse-ttype-lambda-list designator-args)
      (let* ((req-len (length req-args))
             (key-len (length key-forms))
             (args-len (+ req-len key-len))
             (key-args (mapcar #'first key-forms))
             (args (append req-args key-args))
             (where (loop :for arg :in args
                       :collect (or (find arg where :key #'first)
                                    (list arg 'ttype))))
             (arg-param-types (mapcar #'second where)))
        (alexandria:with-gensyms (gtype-spec)
          `(let ((init (or ,init #'identity)))
             (labels ((destructure-args (args)
                        (destructuring-bind (,@req-args &key ,@key-forms)
                            args
                          (list ,@req-args ,@key-args)))
                      (to-type (,gtype-spec
                                named-unknowns
                                constraints
                                args)
                        (assert (eq (slot-value ,gtype-spec 'name)
                                    ',name))
                        (let ((args (destructure-args args)))
                          (let* ((vals
                                  (make-array
                                   ,args-len
                                   :initial-contents
                                   (construct-designator-args ,gtype-spec
                                                              named-unknowns
                                                              constraints
                                                              args)))
                                 (is-complete
                                  (or (= (length vals) 0)
                                      (every (lambda (x)
                                               (complete-p (deref x)))
                                             vals))))
                            (take-ref
                             (make-instance 'user-ttype
                                            :spec ,gtype-spec
                                            :name ',name
                                            :arg-vals vals
                                            :known-complete is-complete))))))
               (register-type
                (let ((arg-param-specs
                       (make-array ,(length arg-param-types)
                                   :initial-contents
                                   (mapcar #'get-parameter-type-spec
                                           ',arg-param-types))))
                  (make-instance 'user-ttype-spec
                                 :name ',name
                                 :init init
                                 :arg-param-specs arg-param-specs
                                 :desig-to-type #'to-type
                                 :custom-data ',custom-spec-data)))
               ',name)))))))
;; hmm, we really need a way to define a type-system.

(defmacro define-constraint (designator
                             &body rest
                             &key where satifies-this-p
                               custom-spec-data)
  (declare (ignore rest))
  (destructuring-bind (name . designator-args)
      (uiop:ensure-list designator)
    (destructuring-bind (req-args key-forms)
        (parse-ttype-lambda-list designator-args)
      (let* ((req-len (length req-args))
             (key-len (length key-forms))
             (args-len (+ req-len key-len))
             (key-args (mapcar #'first key-forms))
             (args (append req-args key-args))
             (where (loop :for arg :in args
                       :collect (or (find arg where :key #'first)
                                    (list arg 'ttype))))
             (arg-param-types (mapcar #'second where)))
        (alexandria:with-gensyms (gconstraint-spec)
          `(let ((satisfies ,satifies-this-p))
             (labels ((destructure-args (args)
                        (destructuring-bind (,@req-args &key ,@key-forms)
                            args
                          (list ,@req-args ,@key-args)))
                      (to-constraint (,gconstraint-spec
                                      named-unknowns
                                      args)
                        (assert (eq (slot-value ,gconstraint-spec 'name)
                                    ',name))
                        (let ((args (destructure-args args)))
                          (let ((vals (make-array
                                       ,args-len
                                       :initial-contents
                                       (construct-designator-args ,gconstraint-spec
                                                                 named-unknowns
                                                                 nil
                                                                 args))))
                            (make-instance 'constraint
                                           :spec ,gconstraint-spec
                                           :name ',name
                                           :arg-vals vals)))))
               (register-constraint
                (let ((arg-param-specs
                       (make-array ,(length arg-param-types)
                                   :initial-contents
                                   (mapcar #'get-parameter-type-spec
                                           ',arg-param-types))))
                  (make-instance 'constraint-spec
                                 :name ',name
                                 :satisfies satisfies
                                 :arg-param-specs arg-param-specs
                                 :desig-to-constraint #'to-constraint
                                 :custom-data ',custom-spec-data)))
               ',name)))))))

;;------------------------------------------------------------

(defun type-ref-p (x)
  (typep x 'type-ref))

(defun param-ref-p (x)
  (typep x 'param-ref))

(defun make-unknown-param ()
  (take-ref (make-instance 'unknown-param)))

(defun take-ref (type/param)
  (let ((ref
         (cond
           ((naked-type-p type/param)
            (make-instance 'type-ref :target type/param))
           ((naked-param-p type/param)
            (make-instance 'param-ref :target type/param))
           (t (error "BUG: cant take ref to ~a" type/param)))))
    (with-slots (refs) type/param
      (pushnew ref refs)
      ref)))

(defun retarget-ref (x-ref new)
  (let* ((old (deref x-ref)))
    (with-slots (refs) old
      (loop :for ref :in refs :do
           (setf (deref ref) new)
           (pushnew ref (slot-value new 'refs)))
      (setf refs nil))
    x-ref))

(defun deref (ref)
  (slot-value ref 'target))

(defun (setf deref) (value ref)
  (setf (slot-value ref 'target) value))

;;------------------------------------------------------------

(defgeneric print-type (type stream))

(defmethod print-object ((obj ttype) stream)
  (format stream "#<NAKED-TYPE ~a>" (type-of obj)))

(defmethod print-object ((obj ttype-parameter) stream)
  (format stream "#<NAKED-PARAMETER ~a>" (slot-value obj 'name)))

(defmethod print-object ((obj type-ref) stream)
  (print-type (deref obj) stream))

(defmethod print-type ((obj ttype) stream)
  (format stream "#T~a"
          (etypecase obj
            (unknown
             (slot-value obj 'name))
            (tfunction
             (designator-from-type obj))
            (user-ttype
             (designator-from-type obj)))))

(defmethod print-type ((obj ttype-parameter) stream)
  (if (eq (slot-value obj 'name) 'ttype)
      (print-type (deref (slot-value obj 'value)))
      (format stream "#P~a"
              (if (typep obj 'unknown-param)
                  (slot-value obj 'name)
                  (slot-value obj 'value)))))

;;------------------------------------------------------------

(defun type-of-typed-expression (expression)
  (assert (and (listp expression)
               (eq (first expression) 'truly-the)
               (type-ref-p (second expression)))
          ()
          "The following is not a typed expression:~%~s"
          expression)
  (second expression))

;;------------------------------------------------------------

(defun make-unknown (&optional constraints)
  (take-ref (make-naked-unknown constraints)))

(defun make-naked-unknown (constraints)
  (make-instance 'unknown :constraints constraints))

(defun designator->type (type-designator)
  (internal-designator-to-type nil nil type-designator))

(defun internal-designator-to-type (named-unknowns constraints designator)
  (destructuring-bind (principle-name . args)
      (uiop:ensure-list designator)
    (case principle-name
      ;;
      ;; always use make-unknown
      (unknown
       (error "BUG: Attempt to make unknown type via designator"))
      ;;
      ;; non user type
      (function
       (assert (= (length designator) 3))
       (take-ref (make-instance
                  'tfunction
                  :arg-types (mapcar #'designator->type
                                     (second designator))
                  :return-type (designator->type
                                (third designator)))))
      ;;
      ;; is a user type or unknown
      (otherwise
       ;;
       ;; dont allow possibility of creating unknown if named-unknowns is nil
       ;; as this means you got here from a public facing method
       (if (and named-unknowns
                (unknown-designator-name-p designator))
           ;;
           ;; unknown
           (or (gethash designator named-unknowns)
               (setf (gethash designator named-unknowns)
                     (make-unknown (gethash designator constraints))))
           ;;
           ;; user type
           (let ((type-spec (gethash principle-name
                                     *registered-user-types*)))
             (if type-spec
                 ;;
                 ;; note: desig-to-type returns a ref
                 (funcall (slot-value type-spec 'desig-to-type)
                          type-spec
                          named-unknowns
                          constraints
                          args)
                 (error "Could not identify type for designator: ~a"
                        designator))))))))

(defun construct-designator-args (type-spec named-unknowns constraints vals)
  (with-slots (name arg-param-specs) type-spec
    (loop
       :for val :in vals
       :for param-spec :across arg-param-specs
       :for i :from 0
       :collect
       ;; dont need to handle function etc as that is covered by the to-param
       ;; of ttype parameters
       ;;
       ;; dont allow possibility of creating unknown if named-unknowns is nil
       ;; as this means you got here from a public facing method
         (if (and named-unknowns
                  (unknown-designator-name-p val))
             ;;
             ;; unknown
             ;;
             ;; may seem odd to use val here but ?x is also used for unknown
             ;; param designators
             (let ((already-seen (gethash val named-unknowns)))
               ;;
               ;; {TODO} clean up when combine unknowns
               (if (eq (slot-value param-spec 'name) 'ttype)
                   ;;
                   ;; type param
                   (progn
                     (when already-seen
                       (assert (typep already-seen 'type-ref) ()
                               "Argument ~a to ~a must be a type"
                               i name))
                     (or already-seen
                         (setf (gethash val named-unknowns)
                               (make-unknown (gethash val constraints)))))
                   ;;
                   ;; value param
                   (progn
                     (when already-seen
                       (assert (not (typep already-seen 'param-ref)) ()
                               "Argument ~a to ~a must be a type"
                               i name))
                     (or already-seen
                         (setf (gethash val named-unknowns)
                               (make-unknown-param))))))
             ;;
             ;; param type
             (funcall (slot-value param-spec 'to-param)
                       param-spec
                       val)))))

(defun populate-constraint (constraint-ref named-unknowns)
  (let ((designator (slot-value constraint-ref 'designator)))
    (destructuring-bind (principle-name . args)
        (uiop:ensure-list designator)
      (assert (not (unknown-designator-name-p designator)) ()
              "Constraint cannot be unknown")
      (let ((spec (gethash principle-name
                           *registered-constraints*)))
        (if spec
            (setf (deref constraint-ref)
                  (funcall (slot-value spec 'desig-to-constraint)
                           spec
                           named-unknowns
                           args))
            (error "Could not identify constraint for designator: ~a"
                   designator))))))

;;------------------------------------------------------------

(defun unify (type-a type-b mutate-p)
  ;; The only case when mutate-p is nil is when you are trying
  ;; to check constraints as there you dont want the type to aquire
  ;; information from the type
  (check-type type-a type-ref)
  (check-type type-b type-ref)
  (let* ((a (deref type-a))
         (b (deref type-b))
         (a-is-user-type-p (typep a 'user-ttype))
         (b-is-user-type-p (typep b 'user-ttype)))
    (unless (eq type-a type-b)
      (cond
        ((and a-is-user-type-p
              b-is-user-type-p
              (eq (slot-value a 'name)
                  (slot-value b 'name))
              (unify-user-type type-a type-b mutate-p))
         t)
        ((and (typep a 'tfunction) (typep b 'tfunction))
         (mapcar (lambda (x y) (unify x y mutate-p))
                 (slot-value a 'arg-types)
                 (slot-value b 'arg-types))
         (unify (slot-value a 'return-type)
                (slot-value b 'return-type)
                mutate-p))
        (t
         (let* ((a-unknown (typep a 'unknown))
                (b-unknown (typep b 'unknown))
                (a-constraints
                 (when a-unknown
                   (slot-value a 'constraints)))
                (b-constraints
                 (when b-unknown
                   (slot-value b 'constraints))))
           (cond
             ((and a-unknown b-unknown)
              (let ((new (make-naked-unknown
                          (append a-constraints
                                  b-constraints))))
                (retarget-ref type-a new)
                (retarget-ref type-b new)))
             (a-unknown
              (check-constraints type-b a-constraints)
              (when mutate-p
                (retarget-ref type-a b)))
             (b-unknown
              (check-constraints type-a b-constraints)
              (when mutate-p
                (retarget-ref type-b a)))
             (t (error "No way to unify ~a and ~a" type-a type-b))))))))
  (values))


(defun check-constraints (type-ref constraints)
  ;; We can safely use unify here as we tell it not to mutate the types.
  ;; This means we check everything can work but we dont allow any
  ;; modification to a type's references.
  (check-type type-ref type-ref)
  (when constraints
    (let ((type (deref type-ref)))
      (labels ((unifies-with-constraint (constraint)
                 (with-slots (satisfies)
                     (slot-value (deref constraint) 'spec)
                   (unless(typep type 'unknown)
                     (handler-case
                         (funcall satisfies constraint type-ref)
                       (error () nil))))))
        (let ((failed
               (loop
                  :for constraint :in constraints
                  :unless (unifies-with-constraint constraint)
                  :collect (slot-value constraint 'name))))
          (when failed
            (error "Type ~a failed to satisfy the following constraints:~%~{~a~}"
                   type-ref failed))))))
  t)

(defun unify-user-type (type-a type-b mutate-p)
  (let* ((a (deref type-a))
         (b (deref type-b)))
    (assert (eq (slot-value a 'name)
                (slot-value b 'name)))
    (loop
       :for aparam :across (slot-value a 'arg-vals)
       :for bparam :across (slot-value b 'arg-vals)
       :do (if (typep aparam 'type-ref)
               (progn
                 (assert (typep bparam 'type-ref))
                 (unify aparam bparam mutate-p))
               (unify-params aparam bparam t)))
    t))

(defun unify-params (param-a param-b mutate-p)
  (check-type param-a param-ref)
  (check-type param-b param-ref)
  (let* ((a (deref param-a))
         (b (deref param-b))
         (a-unknown (typep a 'unknown-param))
         (b-unknown (typep b 'unknown-param))
         (primary-name-matches (eq (slot-value a 'name)
                                   (slot-value b 'name))))
    (cond
      ((and primary-name-matches
            (funcall (slot-value (slot-value a 'spec) 'unify)
                     (slot-value a 'value)
                     (slot-value b 'value)
                     mutate-p))
       t)
      (a-unknown
       (when mutate-p
         (retarget-ref param-a b)))
      (b-unknown
       (when mutate-p
         (retarget-ref param-b a)))
      (t (error "No way to unify params:~%~a: ~a~%~a: ~a"
                (slot-value a 'name)
                (slot-value a 'value)
                (slot-value b 'name)
                (slot-value b 'value)))))
  (values))

;;------------------------------------------------------------

(defgeneric infer-literal (context expression)
  (:method (c e)
    (error "Could not infer a type for literal ~a given env ~a" c e)))

(defgeneric infer-form (context name args)
  (:method (context name args)
    (case name
      (fake-instance-of
       (assert (= (length args) 1))
       (infer-fake-instance-of (first args)))
      (if
       (assert (= (length args) 3))
       (infer-if context (first args) (second args) (third args)))
      (construct
       (assert (= (length args) 2))
       (infer-construct context (first args) (second args)))
      (truly-the
       (assert (= (length args) 2))
       (infer-truly-the context (first args) (second args)))
      (the
       (assert (= (length args) 2))
       (infer-the context (first args) (second args)))
      (progn (infer-progn context args))
      (let (infer-let-form context (first args) (rest args)))
      (lambda (infer-lambda-form context (first args) (rest args)))
      (funcall (infer-funcall context (first args) (rest args)))
      (function
       (assert (= (length args) 1))
       (infer-function-form context (first args)))
      (quote
       (assert (= (length args) 1))
       (infer-quote-form context (first args)))
      (infer-function-call context name args)
      (otherwise (error "Could not infer a type for: ~a~%given context:~a"
                        `(,name ,@args)
                        context)))))

(defun infer-fake-instance-of (designator)
  `(truly-the ,(designator->type designator) :FAKE))

(defun infer-quote-form (context quoted-expression)
  (error "Quoted expressions not implemented yet~%expression:~s~%context:~s"
         `(quote ,quoted-expression)
         context))

(defun infer-function-form (context function-designator)
  (let ((ftype (get-function-type context function-designator)))
    (assert ftype () "TType: No function named ~a found in current scope"
            function-designator)
    `(truly-the ,ftype (function ,function-designator))))

(defun infer-if (context test then else)
  (let* (;; {TODO} support any object in test
         (typed-test (check context test (designator->type 'tboolean)))
         ;; {TODO} can we support 'or' types here?
         (typed-then (infer context then))
         (let-type (type-of-typed-expression typed-then))
         (typed-else (check context else let-type)))
    `(truly-the ,let-type
                (if ,typed-test
                    ,typed-then
                    ,typed-else))))

(defun infer-construct (context designator form)
  ;; Acts as no-op. The form is correctly types so return as is
  (declare (ignore context))
  (let ((type (designator->type designator)))
    `(truly-the ,type ,form)))

(defun infer-truly-the (context type form)
  ;; Acts as no-op. The form is correctly types so return as is
  (declare (ignore context))
  (assert (type-ref-p type))
  `(truly-the ,type ,form))

(defun infer-the (context type-designator form)
  (let* ((type (designator->type type-designator))
         (typed-form (check context form type)))
    (assert (eq 'truly-the (first typed-form)))
    `(truly-the ,type ,(third typed-form))))

(defun infer-progn (context body)
  (let* ((butlast
          (loop
             :for form :in (butlast body)
             :collect (infer context form)))
         (last1 (infer context (car (last body)))))
    `(truly-the ,(type-of-typed-expression last1)
                (progn
                  ,@butlast
                  ,last1))))

(defun infer-let-form (context declarations body)
  (destructuring-bind (inferred-decls type-pairs)
      (loop
         :for (decl-name decl-form) :in declarations
         :for typed-form := (infer context decl-form)
         :for type := (type-of-typed-expression typed-form)
         :collect `(,decl-name ,typed-form) :into typed-decls
         :collect `(,decl-name ,type) :into type-pairs
         :finally (return (list typed-decls type-pairs)))
    (let* ((body-context (add-bindings context type-pairs))
           (typed-body (infer body-context `(progn ,@body))))
      `(truly-the ,(type-of-typed-expression typed-body)
                  (let ,inferred-decls
                    ,typed-body)))))


(defun process-function-arg-specs (arg-specs constraints named-unknowns)
  (loop
     :for spec :in arg-specs
     :for (name type) := spec
     :collect (list name
                    (if type
                        (internal-designator-to-type named-unknowns
                                                     constraints
                                                     type)
                        (let ((constraints-for-this
                               (gethash type constraints)))
                          (make-unknown constraints-for-this))))))

;; {TODO} handle AND types
;; {TODO} this assumes only regular args (no &key &optional etc)
(defun parse-declarations (declaration-forms args)
  (let* ((flat (alexandria:flatten (mapcar #'second args)))
         (arg-unknowns (remove-duplicates
                        (remove-if-not #'unknown-designator-name-p flat)))
         (merged (mapcar #'second declaration-forms))
         (constraints nil)
         (constraints-lookup (make-hash-table)))
    (loop
       :for decl :in merged
       :do (ecase (first decl)
             (satisfies
              (let* ((spec (second decl))
                     (targets (cddr decl))
                     (constraint
                      (make-instance 'constraint-ref :designator spec)))
                (push constraint constraints)
                (loop
                   :for target :in targets
                   :do (assert (unknown-designator-name-p target)
                               () "Cannot constrain known type ~a"
                               (second target))
                   :do (assert (find target arg-unknowns))
                   :do (setf (gethash target constraints-lookup)
                             (cons constraint
                                   (gethash target
                                            constraints-lookup))))))))
    (values constraints-lookup constraints)))

(defun infer-lambda-form (context args body)
  (multiple-value-bind (body declarations doc-string)
      (alexandria:parse-body body :documentation t)
    (let* ((args (mapcar #'alexandria:ensure-list args))
           (named-unknowns
            (make-hash-table)))
      (multiple-value-bind (constraints-lookup constraints)
          (parse-declarations declarations args)
        (let ((processed-args
               (process-function-arg-specs
                args constraints-lookup named-unknowns)))
          (loop
             :for constraint :in constraints
             :do (populate-constraint constraint named-unknowns))
          (let* ((body-context (add-bindings context processed-args))
                 (typed-body (infer body-context `(progn ,@body)))
                 (arg-types (mapcar #'second processed-args))
                 (return-type (type-of-typed-expression typed-body)))
            `(truly-the
              ,(take-ref (make-instance 'tfunction
                                        :arg-types arg-types
                                        :return-type return-type))
              (lambda ,args
                ,@(when doc-string (list doc-string))
                ,@declarations
                ,typed-body))))))))

(defun infer (context expression)
  "The type-system equivalent of eval.
   Assumes the expression is macroexpanded"
  (cond
    ((or (eq expression t)
         (eq expression nil))
     (infer-literal context expression))
    ((symbolp expression)
     (infer-variable context expression))
    ((listp expression)
     (infer-form context
                 (first expression)
                 (rest expression)))
    (t
     (infer-literal context expression))))

(defun infer-variable (context expression)
  (let ((type (or (get-binding context expression)
                  (error "Variable ~s is not in scope" expression))))
    `(truly-the ,type ,expression)))

(defun infer-function-call (context name arg-forms)
  (format t "~%infer func ~a" name)
  (infer-funcall context `(function ,name) arg-forms))

(defun infer-funcall (context func-form arg-forms)
  (let* ((arg-len (length arg-forms))
         (arg-types (loop
                       :repeat arg-len
                       :collect (make-unknown)))
         (ret-type (make-unknown))
         (check-type (take-ref (make-instance
                                'tfunction
                                :arg-types arg-types
                                :return-type ret-type)))
         (typed-func-form (check context func-form check-type))
         (typed-arg-forms
          (loop
             :for arg-form :in arg-forms
             :for arg-type :in arg-types
             :collect (check context arg-form arg-type))))
    (assert (= (length arg-forms)
               (length (slot-value
                        (deref (type-of-typed-expression typed-func-form))
                        'arg-types)))
            () "Incorrect number of args in funcall~%~s"
            `(funcall ,func-form ,@arg-forms))
    `(truly-the ,ret-type
                (funcall ,typed-func-form
                         ,@typed-arg-forms))))

;;------------------------------------------------------------

(defmethod infer-literal (context (expression symbol))
  (declare (ignore context))
  (assert (or (eq expression t)
              (eq expression nil)))
  `(truly-the ,(designator->type 'boolean) ,expression))

(defmethod infer-literal (context (expression integer))
  (declare (ignore context))
  `(truly-the ,(designator->type 'integer) ,expression))

;;------------------------------------------------------------

(defun check (context expression type)
  "Returns typed-expression or errors"
  (let ((typed-expression (infer context expression)))
    (unify (type-of-typed-expression typed-expression)
           type
           t)
    typed-expression))

;;------------------------------------------------------------

(defun has-duplicates-p (list)
  (loop :for (val . rest) :on list
     :when (find val rest)
     :do (return t)))

;;------------------------------------------------------------

;; TYPES

;; You can have data-types, traits, 'and' types & 'or' types
;; 'or' types can contain traits and data-types
;; 'and' types can only contain traits
;; functions can be polymorphic
;; functions can only have one signature

;; We can be polymorphic over trait argument pretty easily by
;; just having an underspecified trait in a function
;; hmm
;; do we want that?
;;
;; (defn foo ((i integer) (c collection))
;;   ..)
;;
;; -v-
;;
;; (defn foo ((i integer) (c (collection ?)))
;;   ..)
;;
;; there is at least noise in the first one, but it is down to the
;; user to know that it is a trait rather than a data-type. One nice
;; thing is that it does minimize paren overload in cases where it's
;; going to be infered anyway
;; I think I like it.
;;
;; Oh yeah, back to the planning.. I was wondering about non-type args
;; to a trait (or data-type? hmm not sure what that would mean)
;;
;; The classic example is (array single-float (5)) which stores the size
;; of the array in the type. We can either dictate the inference for certain
;; types or open this up to the user.
;;
;; What do we need to infer the length? Well lets take this func
;;
;; (defn foo ((arr (array single-float (?))))
;;   (setf (aref arr 5) 5)
;;   (setf (aref arr 10) 10)
;;   arr)
;;
;; the array needs to have a length of at least 10 otherwise this code will
;; fail. We cant infer length from first form as otherwise second will fail
;; or the second as any length >= 10 is valid.
;; Also, when constructed we cant allow non integer lengths.. maybe this is
;; controlled by the constructor function itself, you cant pass a float to
;; the :count arg so its no worry.
;;
;; For the array case we need a range [10 -> ∞]
;; what does that mean in practice?
;; array could have min-length & max-length slots, there could be an 'infer'
;; macro defined for (aref array (constant ?)) which would then set min-length
;; to (max min-length 10) or whatever.
;;
;; infer-macros risk clashes, do they just stack? What if they come up with
;; different inferences for the same piece of data? Maybe we limit it to only 1
;; per signature... we need to detect clashing signatures in that case.. which
;; may be interesting anyway.
;;
;; I think infer-macros will be like compiler-macros in that they run after
;; standard inference and are used to augment the type, they will handle the
;; non-type arguments
;;
;; (define-trait (array ?elem !) ()
;;   (aref array integer)
;;   (setf (aref (array ?elem !) integer) ?elem))
;;
;; where ? indicates a type argument and ! indicates a dependent argument.
;; Dependent args will always be handled by user defined methods
;;
;; This brings up the issue of unification. How do you unify these user
;; dependent arguments? Gonna have to be a user api for this.
;;
;; The good news though is we can get going without this and add them later,
;; we just need to be mindful of what we are doing when writing the base api.
;;
;; Actually we could put in placeholders so we can have sized arrays, the
;; default unify would just be #'eql. There would be no inference for these
;; args so the funcs would be polymorphic over length, which works for aref and
;; such. This lets us have funcs taking sized arrays too. Just gotta be careful
;; not to paint ourselves into a corner.
;;
;; Regarding the ?0 notation, maybe we could use (type 0) and thus some reader
;; macro ala quote. Oriignally thought #t but this sucks e.g. #telem so #? or
;; something? egh, it's hard as reader macros are not the most loved things.
;; Also we need something else for dependent args..hmm. Lets revisit this after
;; working with ?foo and !bar for a bit.
