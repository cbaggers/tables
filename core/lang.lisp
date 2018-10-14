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

(defun test ()
  (let ((res (infer (make-check-context 'tables)
                    `(funcall (lambda ((a ?a))
                                a)
                              t))))
    (print res)
    (let* ((lets (blockify res))
           (last (first (last lets))))
      `(let ,lets
         (truly-the ,(second (second last)) ,(first last))))))

(defun blockify (ast)
  (assert (eq (first ast) 'truly-the))
  (multiple-value-bind (ssad-expr prior-lets)
      (blockify-form (third ast))
    (let ((expr-name (gensym)))
      (append
       prior-lets
       `((,expr-name (truly-the ,(second ast) ,ssad-expr)))))))

(defun blockify-form (form)
  (if (listp form)
      (case (first form)
        (lambda (blockify-lambda-form form))
        (progn (blockify-progn-form form))
        (funcall (blockify-funcall-form form))
        (otherwise (error "not sure what to do with ~s" (first form))))
      form))

(defun blockify-progn-form (form)
  (let* ((lets (apply #'append (mapcar #'blockify (rest form))))
         (last (first lets)))
    (values (first last)
            lets)))

(defun blockify-lambda-form (form)
  (let* ((body (third form))
         (lets (blockify body))
         (ssad-name (first (first (last lets)))))
    (values `(lambda ,(second form)
               (let ,lets
                 (truly-the ,(second body) ,ssad-name)))
            nil)))

(defun blockify-funcall-form (expr-ast)
  (destructuring-bind (ssad-names prior-lets)
      (loop
         :for arg :in (rest expr-ast)
         :for blocked-arg := (blockify arg)
         :for ssad-name := (first (first (last blocked-arg)))
         :collect ssad-name :into names
         :append blocked-arg :into lets
         :finally (return (list names lets)))
    (values
     (cons (first expr-ast) ssad-names)
     prior-lets)))
