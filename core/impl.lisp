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

(defvar *registered-user-types* (make-hash-table :test #'eq))
(defvar *registered-parameter-types* (make-hash-table :test #'eq))
(defvar *registered-constraints* (make-hash-table :test #'eq))
(defvar *registered-top-level-functions* (make-hash-table :test #'eq))
(defvar *registered-records* (make-hash-table :test #'eq))
(defvar *registered-value-types* (make-hash-table :test #'eq))

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
           ((unsigned-byte 64) (find-ttype context 'u64)))))
    `(truly-the ,ttype ,expression)))

(defun infer-special-form (context name args)
  (when (eq name 'if)
    (assert (= (length args) 3))
    (infer-if context (first args) (second args) (third args))))

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
  (if (integerp designator)
      (list 'bits designator)
      (destructuring-bind (principle-name . args)
          (alexandria:ensure-list designator)
        (let ((macro (type-macro-function principle-name)))
          (if macro
              (expand-type-designator
               context
               (apply macro context args))
              designator)))))

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

(defun get-top-level-function-type (context name)
  (declare (ignore context))
  (or (gethash name *registered-top-level-functions*)
      (error "Could not identify function for name: ~a" name)))

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
  (let ((spec (find-ttype 'tables `(function ,arg-types ,return-type))))
    (register-top-level-function name spec)
    `(progn
       (register-top-level-function ',name ,spec))))

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
  (let ((spec (make-instance
               'aggregate-spec
               :name name
               :slots (mapcar #'parse-slot slots))))
    (register-record spec)
    `(progn
       (register-record ,spec)
       (define-ttype ,name :aggregate-info ,spec)
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
  (let ((type (make-function-ttype (make-check-context 'tables)
                                   args return nil)))
    (register-top-level-function name type)
    `(register-top-level-function ',name ,type)))

;;------------------------------------------------------------

(defvar *pending-new-trait-impl-type* nil)

(defun implements-trait-p (trait-name type-ref)
  (with-slots (traits) (spec-custom-data type-ref)
    (when (gethash trait-name traits)
      t)))

(defun trait-constraint-checker (this type-ref)
  (let ((name (slot-value (checkmate::deref this)
                          'checkmate::name)))
    (or (implements-trait-p name type-ref)
        (let ((wip *pending-new-trait-impl-type*))
          (when wip
            (destructuring-bind (wip-trait-name . impl-type) wip
              (when (eq name wip-trait-name)
                (unify type-ref impl-type nil)
                t)))))))

(defmacro define-trait (trait-name funcs &key where)
  (check-type trait-name symbol)
  (let* ((ts (find-type-system 'tables))
         (context (make-check-context ts))
         (func-names (mapcar #'first funcs))
         (spec
          (register-constraint
           (make-constraint-spec context
                                 trait-name
                                 where
                                 'trait-constraint-checker
                                 func-names)))
         (trait-funcs
          (loop
             :for fspec :in funcs
             :collect (gen-and-register-trait-func
                       trait-name context fspec))))
    `(progn
       (register-constraint ,spec)
       ,@trait-funcs
       ',trait-name)))

(defun gen-and-register-trait-func (trait-name context spec)
  (destructuring-bind (name (d-type d-args d-ret) &key satisfies)
      spec
    (assert (eq d-type 'function))
    (let* ((satisfies
            (remove-duplicates
             (cons (list trait-name (first d-args))
                   satisfies)
             :test #'equal))
           (decls (loop :for s :in satisfies :collect `(satisfies ,@s)))
           (ftype (make-function-ttype context d-args d-ret decls)))
      (register-top-level-function name ftype)
      `(register-top-level-function ',name ,ftype))))

(defmacro define-trait-impl (trait-name type &body func-specs)
  (register-trait-impl (find-type-system 'tables)
                       trait-name type func-specs)
  `(register-trait-impl (find-type-system 'tables)
                        ',trait-name
                        ',type
                        ',func-specs))


(defun register-trait-impl (type-system trait-name type-principle-name
                            func-specs)
  (check-type type-principle-name symbol)
  (let* ((constraint-spec (get-constraint-spec nil trait-name))
         (type (find-ttype-by-principle-name
                type-system type-principle-name))
         ;;              {TODO} make this vv work
         (funcs-needed ;;(spec-custom-data constraint-spec)
          (slot-value constraint-spec 'checkmate::custom-data)))
    (assert (= (length func-specs) (length funcs-needed)))
    (assert (every (lambda (x) (find x func-specs :key #'first))
                   funcs-needed))
    (let ((*pending-new-trait-impl-type* (cons trait-name type)))
      (loop
         :for (trait-func-name impl-func-name) :in func-specs
         :for trait-func-type := (get-top-level-function-type
                                  type-system trait-func-name)
         :for impl-func-type := (get-top-level-function-type
                                 type-system impl-func-name)
         :for gfunc-type := (instantiate-function-type trait-func-type)
         :for gimpl-type := (instantiate-function-type impl-func-type)
         :do (dbind-ttype (function ~args ~) gimpl-type
               (if (eq (ttype-principle-name (aref args 0))
                       type-principle-name)
                   (unify gimpl-type gfunc-type nil)
                   (error "first arg of ~a is not ~a"
                          impl-func-name
                          type-principle-name)))))
    (with-slots (traits) (spec-custom-data type)
      (setf (gethash trait-name traits) func-specs)
      trait-name)))

;;------------------------------------------------------------
