(in-package :tables-lang)

;;------------------------------------------------------------

(defvar *registered-user-types* (make-hash-table :test #'eq))
(defvar *registered-parameter-types* (make-hash-table :test #'eq))
(defvar *registered-constraints* (make-hash-table :test #'eq))
(defvar *registered-top-level-functions* (make-hash-table :test #'eq))

;;------------------------------------------------------------

(defun register-type (spec)
  (let ((name (spec-name spec)))
    (format t "~%;; Registered type ~a" name)
    (setf (gethash name *registered-user-types*) spec)))

(defun register-constraint (spec)
  (let ((name (spec-name spec)))
    (format t "~%;; Registered constraint ~a" name)
    (setf (gethash name *registered-constraints*) spec)))

(defun register-parameter-type (spec)
  (let ((name (spec-name spec)))
    (format t "~%;; Registered param type ~a" name)
    (setf (gethash name *registered-parameter-types*) spec)))

(defun register-top-level-function (func-name type)
  (format t "~%;; Registered function ~a" func-name)
  (setf (gethash func-name *registered-top-level-functions*)
        (generalize type)))

;;------------------------------------------------------------

(define-type-system tables)

(defmethod get-type-spec ((type-system tables) designator)
  (let ((principle-name (first (alexandria:ensure-list designator))))
    (or (gethash principle-name *registered-user-types*)
        (error "Could not identify type for designator: ~a"
               designator))))

(defmethod get-parameter-spec ((type-system tables) name)
  (or (gethash name *registered-parameter-types*)
      (error
       "define-ttype: ~a is not valid designator arg type.~%valid:~a"
       name (alexandria:hash-table-keys *registered-parameter-types*))))

(defmethod get-constraint-spec ((type-system tables) designator)
  (let ((principle-name (first (alexandria:ensure-list designator))))
    (or (gethash principle-name
                 *registered-constraints*)
        (error "Could not identify constraint for designator: ~a"
               designator))))

(defmethod get-top-level-function-type ((type-system tables) name)
  (or (gethash name *registered-top-level-functions*)
      (error "Could not function for name: ~a" name)))

;;------------------------------------------------------------
;; these will be removed, jsut for testing

(defmacro define-ttype (designator
                        &body rest
                        &key where custom-spec-data)
  (declare (ignore rest))
  (destructuring-bind (name . rest) (uiop:ensure-list designator)
    (declare (ignore rest))
    (let ((spec (register-type
                 (make-ttype-spec (find-type-system 'tables)
                                  designator
                                  where
                                  custom-spec-data))))
      `(progn
         (register-type ,spec)
         ',name))))

(defmacro define-constraint (designator
                             &body rest
                             &key where satifies-this-p
                               custom-spec-data)
  (declare (ignore rest))
  (destructuring-bind (name . rest) (uiop:ensure-list designator)
    (declare (ignore rest))
    (let ((spec (register-constraint
                 (make-constraint-spec (find-type-system 'tables)
                                       designator
                                       where
                                       satifies-this-p
                                       custom-spec-data))))
      `(progn
         (register-constraint ,spec)
         ',name))))

(defmacro define-parameter-type (name
                                 &body rest
                                 &key valid-p equal)
  (declare (ignore rest))
  (let ((spec (register-parameter-type
               (make-parameter-spec (find-type-system 'tables)
                                    name
                                    valid-p
                                    equal))))
    `(progn
       (register-parameter-type ,spec)
       ',name)))

;;------------------------------------------------------------

(define-parameter-type integer
  :valid-p integerp
  :equal =)

(define-ttype boolean)

(define-ttype (unordered-set type size)
  :where ((size integer)))

(define-ttype integer)

;;------------------------------------------------------------

(defclass blockify-context ()
  ((parent :initarg :parent)
   (bindings :initform nil :initarg :bindings)))

(defun make-blockify-context (parent new-bindings)
  (let ((res (make-instance 'blockify-context
                            :parent parent
                            :bindings (when parent
                                        (slot-value parent 'bindings)))))
    (with-slots (bindings) res
      (loop
         :for binding :in new-bindings
         :do (setf bindings (cons binding bindings))))
    res))

(defun test ()
  (let ((res (infer (make-check-context 'tables)
                    `(funcall (lambda ((a ?a))
                                (let ((b a))
                                  b))
                              t))))
    (print res)
    (let* ((context (make-blockify-context nil nil))
           (lets (blockify context res))
           (last (first (last lets))))
      `(let* ,lets
         (truly-the ,(second (second last)) ,(first last))))))

(defun blockify (context ast)
  (assert (eq (first ast) 'truly-the))
  (multiple-value-bind (ssad-expr prior-lets)
      (blockify-form context (third ast))
    (let ((expr-name (gensym)))
      (append
       prior-lets
       `((,expr-name (truly-the ,(second ast) ,ssad-expr)))))))

(defun blockify-form (context form)
  (typecase form
    (list
     (case (first form)
       (lambda (blockify-lambda-form context form))
       (let (blockify-let-form context form))
       (progn (blockify-progn-form context form))
       (funcall (blockify-funcall-form context form))
       (otherwise (error "not sure what to do with ~s" (first form)))))
    (symbol
     (if (or (eq form t) (null form))
         form
         (blockify-var-access context form)))
    (otherwise
     form)))

(defun blockify-var-access (context symbol)
  (or (cdr (assoc symbol (slot-value context 'bindings)))
      (error "bug: ~s" symbol)))

(defun gensym-named (name)
  (gensym (format nil "~a_" name)))

(defun blockify-let-form (context form)
  ;; note this is let, not let*
  (let* ((renamed-args (loop
                          :for (name val) :in (second form)
                          :collect (list (gensym-named (symbol-name name))
                                         val)))
         (decls (loop
                   :for (name val) :in renamed-args

                   :for blocked := (blockify context val)
                   :for ssad-name := (first (first (last blocked)))
                   :append blocked
                   :collect (list name ssad-name)))
         (context (make-blockify-context
                   context
                   (loop
                      :for (old-name) :in (second form)
                      :for (new-name) :in renamed-args
                      :collect (cons old-name new-name))))
         (body (third form))
         (lets (blockify context body))
         (ssad-name (first (first (last lets)))))
    (values ssad-name
            (append decls lets))))

(defun blockify-progn-form (context form)
  (let* ((lets (loop :for x :in (rest form)
                  :append (blockify context x)))
         (last (first lets)))
    (values (first last)
            lets)))

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
                      :collect (cons old-name new-name))))
         (body (third form))
         (lets (blockify context body))
         (ssad-name (first (first (last lets)))))
    (values `(lambda ,renamed-args
               (let* ,lets
                 (truly-the ,(second body) ,ssad-name)))
            nil)))

(defun blockify-funcall-form (context expr-ast)
  (destructuring-bind (ssad-names prior-lets)
      (loop
         :for arg :in (rest expr-ast)
         :for blocked-arg := (blockify context arg)
         :for ssad-name := (first (first (last blocked-arg)))
         :collect ssad-name :into names
         :append blocked-arg :into lets
         :finally (return (list names lets)))
    (values
     (cons (first expr-ast) ssad-names)
     prior-lets)))
