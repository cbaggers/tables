(in-package :tables.lang)

;;------------------------------------------------------------

(defclass aggregate-spec ()
  ((name :initarg :name :reader aggregate-name)
   (slots :initarg :slots :reader aggregate-slots)))

(defclass value-type-spec (aggregate-spec)
  ((size :initarg :size :reader value-size)))

(defclass aggregate-slot ()
  ((name :initarg :name :reader slot-name)
   (type :initarg :type :reader slot-type)))

(defmethod make-load-form ((obj aggregate-spec)
                           &optional environment)
  (declare (ignore environment))
  (with-slots (name slots) obj
    `(make-instance 'aggregate-spec
                    :name ',name
                    :slots ,(cons 'list slots))))

(defmethod make-load-form ((obj value-type-spec)
                           &optional environment)
  (declare (ignore environment))
  (with-slots (name size slots) obj
    `(make-instance 'value-type-spec
                    :name ',name
                    :size ,size
                    :slots ,(cons 'list slots))))

(defmethod make-load-form ((obj aggregate-slot)
                           &optional environment)
  (declare (ignore environment))
  (with-slots (name type) obj
    `(make-instance 'aggregate-slot
                    :name ',name
                    :type ,type)))

;;------------------------------------------------------------

(defclass function-info ()
  ((type :initarg :type :reader function-type)
   (is-trait-func-p :initarg :is-trait-func-p :reader is-trait-func-p)
   (ast :initarg :ast :initform nil :reader function-ast)
   (record-ctor-slots :initarg :record-ctor-slots
                      :reader record-ctor-slots)
   (trait-impls :initarg :trait-impls :initform (make-hash-table)
                :reader trait-impls)))

;;------------------------------------------------------------

(defvar *registered-user-types* (make-hash-table :test #'eq))
(defvar *registered-parameter-types* (make-hash-table :test #'eq))
(defvar *registered-constraints* (make-hash-table :test #'eq))
(defvar *registered-top-level-functions* (make-hash-table :test #'eq))
(defvar *registered-records* (make-hash-table :test #'eq))
(defvar *registered-value-types* (make-hash-table :test #'eq))
(defvar *registered-traits* (make-hash-table :test #'eq))
(defvar *registered-compiler-macros* (make-hash-table :test #'eq))
(defvar *registered-macros* (make-hash-table :test #'eq))

;; {TODO} This is temporary, will be replaced with something more
;;        general
(defvar *registered-constant-folds* (make-hash-table :test #'eq))


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

(defun register-tlf-from-code (func-name code is-trait-func)
  (let* ((ast (infer 'tables code))
         (type (type-of-typed-expression ast)))
    (format t "~%;; Registered function ~a" func-name)
    (setf (gethash func-name *registered-top-level-functions*)
          (make-instance 'function-info
                         :type (generalize type)
                         :is-trait-func-p is-trait-func
                         :ast ast))))

(defun register-tlf-from-type (func-name type-designator is-trait-func
                               record-ctor-slots unknowns declarations)
  (destructuring-bind (f args ret) type-designator
    (assert (eq f 'function))
    (let* ((unknowns (or unknowns (make-hash-table)))
           (type (make-function-ttype (make-check-context 'tables)
                                      args ret :unknowns unknowns
                                      :declarations declarations)))
      (format t "~%;; Registered function ~a" func-name)
      (setf (gethash func-name *registered-top-level-functions*)
            (make-instance 'function-info
                           :type (generalize type)
                           :is-trait-func-p is-trait-func
                           :record-ctor-slots record-ctor-slots
                           :ast nil)))))

(defun register-record (spec)
  (let ((name (aggregate-name spec)))
    (format t "~%;; Registered record ~a" name)
    (setf (gethash name *registered-records*) spec)))

(defun register-value-type (spec)
  (let ((name (aggregate-name spec)))
    (format t "~%;; Registered value-type ~a" name)
    (setf (gethash name *registered-value-types*) spec)))

;;------------------------------------------------------------

(defun infer-atom (context expression)
  (if (symbolp expression)
      (if (or (null expression) (eq expression t))
          `(truly-the ,(find-ttype context 'boolean)
                      ,expression)
          (infer-variable context expression))
      (infer-literal context expression)))

(defun infer-literal (context expression)
  ;; {TODO} this is wrong as we wont ever be able to get unsigned 8-32
  ;;        we need to special case 'the' for literals so we dont need
  ;;        to add casting
  (let ((ttype
         (typecase expression
           ((signed-byte 8) (find-ttype context 'i8))
           ((signed-byte 16) (find-ttype context 'i16))
           ((signed-byte 32) (find-ttype context 'i32))
           ((signed-byte 64) (find-ttype context 'i64))
           ;; ((unsigned-byte 8) (find-ttype context 'u8))
           ;; ((unsigned-byte 16) (find-ttype context 'u16))
           ;; ((unsigned-byte 32) (find-ttype context 'u32))
           ((unsigned-byte 64) (find-ttype context 'u64))
           (single-float (find-ttype context 'f32))
           (tables.compile.stage-0:ssad-var
            (slot-value
             (slot-value expression 'tables.compile.stage-0:binding)
             'tables.compile.stage-0:type))
           (tables.compile.stage-0:ssad-constant
            (slot-value expression 'tables.compile.stage-0:type)))))
    (when ttype
      `(truly-the ,ttype ,expression))))

(defun infer-special-form (context name args)
  (cond
    ((eq name 'if)
     (assert (= (length args) 3))
     (infer-if context (first args) (second args) (third args)))
    ((eq name 'read-val)
     (infer-read-val context name args))
    ((string= name 'output)
     (infer-outputs context name args))))

(defun infer-outputs (context name args)
  ;; {TODO} make user-data a hash-table
  (let* ((outputs (check-context-user-data context))
         (typed-args
          (loop
             :for (key val) :on args :by #'cddr
             :for (aname type-desig) :=
               (find key outputs :test #'string-desig-and= :key #'first)
             :append (list key (infer context `(the ,type-desig ,val))))))
    `(truly-the ,(find-ttype context 'tables.lang::outputs)
                ,(cons name typed-args))))

(defun infer-read-val (context name args)
  (destructuring-bind (type column-name) args
    (let ((type (find-ttype context type)))
      `(truly-the ,type ,(list name type column-name)))))

(defun infer-if (context test then else)
  (let* (;; {TODO} support any object in test
         (typed-test
          (check context test (find-ttype context 'boolean)))
         ;; {TODO} can we support 'or' types here?
         (typed-then (infer context then))
         (let-type (type-of-typed-expression typed-then))
         (typed-else (check context else let-type)))
    `(truly-the ,let-type
                (if ,typed-test
                    ,typed-then
                    ,typed-else))))

;;------------------------------------------------------------

(defvar *registered-type-macro-functions*
  (make-hash-table :test #'eq))

;; {TODO} pass type objects as arguments rather than symbols
(defun expand-type-designator (context designator)
  (when designator
    (if (integerp designator)
        (list 'bits designator)
        (destructuring-bind (principle-name . args)
            (alexandria:ensure-list designator)
          (let ((macro (type-macro-function principle-name)))
            (if macro
                (expand-type-designator
                 context
                 (apply macro context args))
                designator))))))

(defun type-macro-function (name)
  (gethash name *registered-type-macro-functions*))

(defun register-type-macro (name func)
  (setf (gethash name *registered-type-macro-functions*)
        func))

(defmacro define-type-macro (designator
                             (context-var)
                             &body body)
  (when (listp designator)
    (assert (> (length designator) 1)))
  (destructuring-bind (name &rest args)
      (alexandria:ensure-list designator)
    (let ((mangled-name (rehome-symbol name :tables.macros)))
      (register-type-macro name mangled-name)
      `(progn
         (defun ,mangled-name (,context-var ,@args)
           (declare (ignorable ,context-var))
           ,@body)
         (register-type-macro ',name #',mangled-name)
         ',name))))

#+nil
(define-type-macro (foop x) (context)
  (print x)
  'f32)

;;------------------------------------------------------------

(defun get-type-spec (context designator)
  (declare (ignore context))
  (let ((principle-name (first (alexandria:ensure-list designator))))
    (or (gethash principle-name *registered-user-types*)
        (error "Could not identify type for designator: ~a"
               designator))))

(defun get-parameter-spec (context name)
  (declare (ignore context))
  (or (gethash name *registered-parameter-types*)
      (error
       "define-ttype: ~a is not valid designator arg type.~%valid:~a"
       name (alexandria:hash-table-keys *registered-parameter-types*))))

(defun get-constraint-spec (context designator)
  (declare (ignore context))
  (let ((principle-name (first (alexandria:ensure-list designator))))
    (or (gethash principle-name
                 *registered-constraints*)
        (error "Could not identify constraint for designator: ~a"
               designator))))

(defun get-top-level-function-type (context name arg-types-provided-p
                                    arg-types)
  (declare (ignore context))
  (let ((func-info (gethash name *registered-top-level-functions*)))
    (if func-info
        (with-slots (is-trait-func-p trait-impls type) func-info
          (if is-trait-func-p
              (if arg-types-provided-p
                  (let* ((impl-name
                          (gethash
                           (ttype-principle-name (first arg-types))
                           trait-impls))
                         (func-info
                          (gethash impl-name
                                   *registered-top-level-functions*)))
                    (if func-info
                        (values
                         (slot-value func-info 'type)
                         impl-name)
                        (error "Could not identify traint function implementation for ~a ~%when passed ~a" name (mapcar #'ttype-of arg-types))))
                  (error "Cannot find a trait function without knowing the argument types:~%~a" name))
              type))
        (error "Could not identify function for name: ~a" name))))

;;------------------------------------------------------------

(define-type-system tables
    :infer-atom infer-atom
    :infer-special-form infer-special-form

    :type-expander expand-type-designator
    :get-type-spec get-type-spec
    :get-constraint-spec get-constraint-spec
    :get-parameter-spec get-parameter-spec
    :get-top-level-function-type get-top-level-function-type)

(defvar *type-system* (find-type-system 'tables))

;;------------------------------------------------------------

(defclass spec-data ()
  ((traits :initform (make-hash-table) :initarg :traits)
   (aggregate-info :initarg :aggregate-info)))

(defun make-spec-data (aggregate-info)
  (assert (or (null aggregate-info)
              (typep aggregate-info 'aggregate-spec)))
  (make-instance 'spec-data :aggregate-info aggregate-info))

(defmethod make-load-form ((obj spec-data)
                           &optional environment)
  (declare (ignore environment))
  (with-slots (traits aggregate-info) obj
    (let ((ht-data (unless (null traits)
                     (alexandria:hash-table-alist traits))))
      `(make-instance 'spec-data
                      :traits (alexandria:alist-hash-table ',ht-data)
                      :aggregate-info ',aggregate-info))))

;;------------------------------------------------------------
;; these will be removed, jsut for testing

;; {TODO} look up the spec rather than storing it, load forms seem to be
;;        slow.
(defmacro define-ttype (designator
                        &body rest
                        &key where aggregate-info)
  (declare (ignore rest))
  (destructuring-bind (name . rest) (uiop:ensure-list designator)
    (declare (ignore rest))
    (let ((spec (register-type
                 (make-ttype-spec (make-check-context
                                   (find-type-system 'tables))
                                  designator
                                  where
                                  (make-spec-data aggregate-info)))))
      `(progn
         (register-type ,spec)
         ',name))))

(defmacro define-constraint (designator
                             &body rest
                             &key where satisfies-this-p)
  (declare (ignore rest))
  (destructuring-bind (name . rest) (uiop:ensure-list designator)
    (declare (ignore rest))
    (let ((spec (register-constraint
                 (make-constraint-spec (find-type-system 'tables)
                                       designator
                                       where
                                       satisfies-this-p
                                       (make-spec-data nil)))))
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

(defmacro defn-host-func (name arg-types return-type)
  (declare (ignore arg-types return-type))
  `(progn
     ',name))

;;------------------------------------------------------------

(defun parse-slot (definition)
  (destructuring-bind (name type)
      (if (numberp definition)
          (list nil definition)
          definition)
    ;; {TODO} proper checks
    (assert (symbolp name))
    (assert (symbol-package name))
    (let ((type-obj (find-ttype *type-system* type)))
      (make-instance 'aggregate-slot
                     :name name
                     :type type-obj))))

(defmacro define-record (name &body slots)
  (let* ((slot-specs (mapcar #'parse-slot slots))
         (spec (make-instance
                'aggregate-spec
                :name name
                :slots slot-specs))
         (constructor-name name)
         (slot-type-desigs
          (mapcar (lambda (x) (ttype-of (slot-type x)))
                  slot-specs))
         (slot-funcs
          (loop
             :for (slot-name slot-type-desig) :in slots
             :for acc-name := (intern
                               (format nil "~a-~a" name slot-name)
                               (symbol-package name))
             :collect `(define-dummy-func ,acc-name (,name)
                         ,slot-type-desig))))
    (register-record spec)
    `(progn
       (register-record ,spec)
       (define-ttype ,name :aggregate-info ,spec)
       (define-record-ctor-func ,constructor-name ,slot-type-desigs ,name
                                ,(mapcar #'second slot-funcs))
       ,@slot-funcs
       ',name)))

;;------------------------------------------------------------

(defmacro define-value-type (name (size) &body slots)
  (assert (and (> size 0) (<= size 64)))
  (let ((spec (make-instance
               'value-type-spec
               :name name
               :size size
               :slots (mapcar #'parse-slot slots))))
    (register-value-type spec)
    `(progn
       (register-value-type ,spec)
       (define-ttype ,name :aggregate-info ,spec)
       ',name)))

;;------------------------------------------------------------

;; Defining a layout is simply defining a function that takes
;; a struct definition and returns a new one, potentially with
;; additional padding slots

(defmacro define-layout (name (aggregate-arg) &body body)
  (declare (ignore aggregate-arg body))
  ;; (let ((layout-name (intern )))
  ;;   `(defun ,name () ))
  `',name)

;;------------------------------------------------------------

(defmacro define-dummy-func (name args return)
  (let ((type `(function ,args ,return)))
    (register-tlf-from-type name type nil nil nil nil)
    `(register-tlf-from-type ',name ',type nil nil nil nil)))

(defmacro define-record-ctor-func (name args return slot-func-names)
  (let ((type `(function ,args ,return)))
    (register-tlf-from-type name type nil t nil nil)
    `(register-tlf-from-type ',name ',type nil',slot-func-names nil nil)))

;;------------------------------------------------------------

(defmacro define-trait (trait-designator associated-type funcs)
  (assert (or (symbolp trait-designator)
              (every #'symbolp trait-designator)))
  (assert (or (null associated-type)
              (checkmate::unknown-designator-name-p associated-type)))
  (register-trait trait-designator associated-type funcs)
  `(progn
     (register-trait ',trait-designator ',associated-type ',funcs)
     ',trait-designator))

(defun register-trait (trait-designator associated-type funcs)
  (let* ((designator-as-list
          (alexandria:ensure-list trait-designator))
         (trait-info
          (list (rest designator-as-list) associated-type funcs))
         (principle-name
          (first designator-as-list))
         (unknowns (make-hash-table)))
    (when associated-type
      (find-ttype 'tables associated-type :unknowns unknowns))
    (when (listp trait-designator)
      (reduce
       (lambda (u d)
         (find-ttype 'tables d :unknowns u)
         u)
       (remove-duplicates
        (remove-if-not #'checkmate::unknown-designator-name-p
                       (rest trait-designator)))
       :initial-value unknowns))
    (setf (gethash principle-name *registered-traits*)
        trait-info)
    (loop
       :for fspec :in funcs
       :do (gen-and-register-trait-func fspec unknowns))
    (values)))

(defun gen-and-register-trait-func (spec unknowns)
  (destructuring-bind (name (d-type d-args d-ret) &key satisfies)
      spec
    (assert (eq d-type 'function))
    (let* ((decls (loop :for s :in satisfies :collect `(satisfies ,@s)))
           (ftype `(function ,d-args ,d-ret)))
      (register-tlf-from-type name ftype t nil unknowns decls)
      (values))))

(defmacro define-trait-impl (trait-name (&optional associated-type) type
                             &body func-specs)
  (register-trait-impl (find-type-system 'tables)
                       trait-name associated-type type func-specs)
  `(register-trait-impl (find-type-system 'tables)
                        ',trait-name
                        ',associated-type
                        ',type
                        ',func-specs))


(defun register-trait-impl (type-system trait-designator associated-type
                            type-designator func-specs)
  (let* ((trait-desig-as-list
          (alexandria:ensure-list trait-designator))
         (trait-principle-name
          (first trait-desig-as-list))
         (impl-principle-name
          (first (alexandria:ensure-list type-designator)))
         (trait-impl-args (rest trait-desig-as-list))
         (trait-info (gethash trait-principle-name *registered-traits*)))
    (assert trait-info () "Unknown trait ~a" trait-principle-name)
    (destructuring-bind (trait-args t-associated-type funcs-needed)
        trait-info
      (assert (= (length trait-impl-args) (length trait-args)))
      (assert (= (length func-specs) (length funcs-needed)))
      (assert (every (lambda (x) (find (first x) func-specs :key #'first))
                     funcs-needed))
      (when t-associated-type
        (assert associated-type))
      (let* ((funcs-needed
              (if t-associated-type
                  (subst associated-type t-associated-type funcs-needed)
                  funcs-needed))
             (funcs-needed
              (reduce (lambda (a x)
                        (destructuring-bind (targ . iarg) x
                          (subst iarg targ a :test #'eq)))
                      (mapcar #'cons trait-args trait-impl-args)
                      :initial-value funcs-needed))
             (unknowns0
              (make-hash-table))
             (unknowns1
              (make-hash-table))
             (unknowns0
              (if associated-type
                  (progn
                    (find-ttype 'tables associated-type
                                :unknowns unknowns1)
                    unknowns0)
                  unknowns0))
             (funcs-needed
              (mapcar (lambda (x)
                        (destructuring-bind (name spec) x
                          (list name
                                (gethash
                                 name
                                 *registered-top-level-functions*)
                                (find-ttype type-system spec
                                            :unknowns unknowns0))))
                      funcs-needed))
             (funcs-provided
              (mapcar
               (lambda (x)
                 (destructuring-bind (name impl-func-name) x
                   (let* ((z (instantiate-function-type
                              (slot-value
                               (gethash impl-func-name
                                        *registered-top-level-functions*)
                               'type)
                              :named-unknowns unknowns1)))
                     (list name impl-func-name z))))
               func-specs))
             (impl-type (find-ttype type-system type-designator
                                    :unknowns unknowns1)))
        (loop
           :for (needed-name info needed-type) :in funcs-needed
           :for (impl-func-name provided-type)
             := (assocr needed-name funcs-provided)
           :do (dbind-ttype (function ~args ~) provided-type
                 ;;    vvvv this should be a type-eql or something
                 (if (unifies-p (aref args 0) impl-type)
                     (if (unifies-p provided-type
                                    needed-type
                                    :named-unknowns unknowns1)
                         (setf (gethash impl-principle-name
                                        (slot-value info 'trait-impls))
                               impl-func-name)
                         (error "~a does not unify with ~a for ~a"
                                provided-type needed-type needed-name))
                     (error "first arg of ~a is not ~a"
                            impl-func-name
                            type-designator))))
        trait-designator))))

;;------------------------------------------------------------

(defmacro defn (name args &body body)
  (let* ((code `(lambda ,args ,@body)))
    (register-tlf-from-code name code nil)
    `(progn
       (register-tlf-from-code ',name ',code nil)
       ',name)))

#+nil
(defn horse ()
  1)

;;------------------------------------------------------------

(defmacro define-optimize-macro (func-name args &body body)
  (let ((func (gen-macro-function-code func-name args body)))
    `(progn
       (setf (gethash ',func-name *registered-compiler-macros*)
             ,func)
       ',func-name)))

(defmacro define-tables-macro (name lambda-list &body body)
  (let ((func (gen-macro-function-code name lambda-list body)))
    `(progn
       (setf (gethash ',name *registered-macros*)
             ,func)
       ',name)))

;;------------------------------------------------------------

(defmacro define-constant-folder (func-name args &body body)
  `(progn
     (setf (gethash ',func-name *registered-constant-folds*)
           (lambda ,args ,@body))
     ',func-name))
