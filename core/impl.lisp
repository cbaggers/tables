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
                    :size size
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
    (format t "~%;; Registered layout ~a" name)
    (setf (gethash name *registered-records*) spec)))

(defun register-value-type (spec)
  (let ((name (aggregate-name spec)))
    (format t "~%;; Registered layout ~a" name)
    (setf (gethash name *registered-value-types*) spec)))

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
      (error "Could not identify function for name: ~a" name)))

;;------------------------------------------------------------

(defclass spec-data ()
  ((traits :initform (make-hash-table) :initarg :traits)
   (layout :initarg :layout)))

(defun make-spec-data (layout)
  (assert (or (null layout) (typep layout 'aggregate-spec)))
  (make-instance 'spec-data :layout layout))

(defmethod make-load-form ((obj spec-data)
                           &optional environment)
  (declare (ignore environment))
  (with-slots (traits) obj
    (let ((ht-data (alexandria:hash-table-alist traits)))
      `(make-instance 'spec-data :traits ',ht-data))))

;;------------------------------------------------------------
;; these will be removed, jsut for testing

(defmacro define-ttype (designator
                        &body rest
                        &key where aggregate-info)
  (declare (ignore rest))
  (destructuring-bind (name . rest) (uiop:ensure-list designator)
    (declare (ignore rest))
    (let ((spec (register-type
                 (make-ttype-spec (find-type-system 'tables)
                                  designator
                                  where
                                  (make-spec-data aggregate-info)))))
      `(progn
         (register-type ,spec)
         ',name))))

(defmacro define-constraint (designator
                             &body rest
                             &key where satifies-this-p)
  (declare (ignore rest))
  (destructuring-bind (name . rest) (uiop:ensure-list designator)
    (declare (ignore rest))
    (let ((spec (register-constraint
                 (make-constraint-spec (find-type-system 'tables)
                                       designator
                                       where
                                       satifies-this-p
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

(defmacro defn-host (name arg-types return-type)
  (let ((spec (checkmate::designator->type
               (find-type-system 'tables)
               `(function ,arg-types ,return-type))))
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
    (make-instance 'aggregate-slot
                   :name name
                   :type type)))

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

#+nil
(define-record ff32
  (sign 1)
  (mantissa 7)
  (exponent 23))

;;------------------------------------------------------------

(defmacro define-value-type (name (size) &body slots)
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
