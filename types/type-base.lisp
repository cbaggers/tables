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

(defclass ttype ()
  ((refs :initform nil)))

(defun ttype-p (x)
  (typep x 'ttype))

(defclass unknown (ttype)
  ((name :initform (gensym))))

(defclass data-type (ttype) ())

(defclass void (data-type) ())

(defclass tinteger (data-type) ())

(defclass tboolean (data-type) ())

(defclass tfunction (data-type)
  ((arg-types :initform nil :initarg :arg-types)
   (return-type :initform nil :initarg :return-type)))

(defun ttype-class-name (type-principle-name)
  (let ((name (symbol-name type-principle-name))
        (pkg (symbol-package type-principle-name)))
    (intern
     (format nil "~@[~a.~]~a"
             (unless (eq pkg #.(find-package :cl))
               (package-name pkg))
             name)
     :ttype-classes)))

(defun parse-ttype-lambda-list ()
  (multiple-value-bind (required-parameters
                        optional-parameters
                        rest-parameters-name
                        keyword-parameters
                        has-allow-other-keys-p
                        aux-parameter)
      (alexandria:parse-ordinary-lambda-list '(a (b) &key c))
    (assert (not aux-parameter))
    (assert (not rest-parameters-name))
    (assert (not optional-parameters))
    (assert (not has-allow-other-keys-p))
    (list required-parameters keyword-parameters)))

#+nil
(defmacro define-ttype (name &rest designator-args)
  (declare (ignore args))
  (destructuring-bind (req-args key-args) (parse-ttype-lambda-list)
    (let* ((class-name (ttype-class-name name)))
      `(progn
         (defclass ,class-name (ttype) ())
         (defmethod %designator->type ((name (eql ',name)) &rest args)
           (destructuring-bind ,designator-args args
             ))))))

#+nil
(define-ttype boolean)

;;------------------------------------------------------------

(defclass type-ref ()
  ((target :initarg :target)))

(defun type-ref-p (x)
  (typep x 'type-ref))

(defun take-ref (type)
  (assert (ttype-p type))
  (let ((ref (make-instance 'type-ref :target type)))
    (with-slots (refs) type
      (pushnew ref refs)
      ref)))

(defun retarget-ref (type-ref new-type)
  (let* ((old-type (deref type-ref)))
    (with-slots (refs) old-type
      (loop :for ref :in refs :do
           (setf (deref ref) new-type)
           (pushnew ref (slot-value new-type 'refs)))
      (setf refs nil))
    type-ref))

(defun deref (type-ref)
  (slot-value type-ref 'target))

(defun (setf deref) (value type-ref)
  (setf (slot-value type-ref 'target) value))

;;------------------------------------------------------------

(defgeneric print-type (type stream))

(defmethod print-object ((obj ttype) stream)
  (format stream "#<NAKED-TYPE ~a>" (type-of obj)))

(defmethod print-object ((obj type-ref) stream)
  (print-type (deref obj) stream))

(defmethod print-type ((obj ttype) stream)
  (format stream "#T~a"
          (typecase obj
            (unknown (list (slot-value obj 'name)))
            (tfunction (with-slots (arg-types return-type) obj
                         `(tfunction ,arg-types ,return-type)))
            (t (list (type-of obj))))))

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

(defun designator->type (type-designator)
  (or (if (symbolp type-designator)
          (case type-designator
            (unknown (take-ref (make-instance 'unknown)))
            (tboolean (take-ref (make-instance 'tboolean)))
            (tinteger (take-ref (make-instance 'tinteger))))
          (case (first type-designator)
            (function
             (assert (= (length type-designator) 3))
             (take-ref (make-instance
                        'tfunction
                        :arg-types (mapcar #'designator->type (second type-designator))
                        :return-type (designator->type (third type-designator)))))))
      (error "designator->type not implemented for ~a"
             type-designator)))

;;------------------------------------------------------------

(defun unify (type-a type-b)
  (check-type type-a type-ref)
  (check-type type-b type-ref)
  (let* ((a (deref type-a))
         (b (deref type-b)))
    (unless (equal type-a type-b)
      (cond
        ((and (typep a 'tinteger) (typep b 'tinteger)))
        ((and (typep a 'tboolean) (typep b 'tboolean)))
        ((and (typep a 'tfunction) (typep b 'tfunction))
         (mapcar #'unify
                 (slot-value a 'arg-types)
                 (slot-value b 'arg-types))
         (unify (slot-value a 'return-type)
                (slot-value b 'return-type)))
        ((typep a 'unknown)
         (retarget-ref type-a b)
         type-b)
        ((typep b 'unknown)
         (retarget-ref type-b a)
         type-a)
        (t (error "No way to unify ~a and ~a" type-a type-b)))))
  (values))


;;------------------------------------------------------------

(defgeneric infer-literal (context expression)
  (:method (c e)
    (error "Could not infer a type for literal ~a given env ~a" c e)))

(defgeneric infer-form (context name args)
  (:method (context name args)
    (case name
      (if
       (assert (= (length args) 3))
       (infer-if context (first args) (second args) (third args)))
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

(defun infer-lambda-form (context args body)
  (let* ((arg-unknown-vars
          (mapcar (lambda (arg)
                    (destructuring-bind (name &optional type)
                        (if (listp arg) arg (list arg))
                      (list name
                            (if type
                                (designator->type type)
                                (designator->type 'unknown)))))
                  args))
         (body-context (add-bindings context arg-unknown-vars))
         (typed-body (infer body-context `(progn ,@body))))
    `(truly-the
      ,(take-ref (make-instance
                  'tfunction
                  :arg-types (mapcar #'second arg-unknown-vars)
                  :return-type (type-of-typed-expression typed-body)))
      (lambda ,args ,typed-body))))

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
                       :collect (designator->type 'unknown)))
         (ret-type (designator->type 'unknown))
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
    `(truly-the ,ret-type
                (funcall ,typed-func-form
                         ,@typed-arg-forms))))

;;------------------------------------------------------------

(defmethod infer-literal (context (expression symbol))
  (declare (ignore context))
  (assert (or (eq expression t)
              (eq expression nil)))
  `(truly-the ,(designator->type 'tboolean) ,expression))

(defmethod infer-literal (context (expression integer))
  (declare (ignore context))
  `(truly-the ,(designator->type 'tinteger) ,expression))

;;------------------------------------------------------------

(defun check (context expression type)
  "Returns typed-expression or errors"
  (let ((typed-expression (infer context expression)))
    (unify (type-of-typed-expression typed-expression)
           type)
    typed-expression))

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
