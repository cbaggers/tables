(in-package #:tables)

;;------------------------------------------------------------

(define-completable data-trait-slot-definition ()
  (name symbol)
  ttype)

(define-completable data-trait-definition ()
  (name symbol)
  (slots list))

(define-completable data-trait-slot ()
  (name symbol)
  (ttype type-ref))

(define-completable data-trait ()
  (name symbol)
  (slots list))

(defun valid-data-trait-slot-type-p (x)
  (or (typep x 'data-trait)
      (typep x 'bit-type)
      (typep x 'lisp-type)))

(defmethod make-load-form ((obj data-trait-definition) &optional env)
  (declare (ignore env))
  (with-contents (name slots) obj
    `(make-data-trait-definition
      :name ',name
      :slots (list ,@slots))))

(defmethod make-load-form ((obj data-trait-slot-definition) &optional env)
  (declare (ignore env))
  (with-contents (name ttype) obj
    `(make-data-trait-slot-definition
      :name ',name
      :ttype ',ttype)))

(defmethod to-specifier ((obj data-trait-definition))
  (name obj))

(defmethod to-specifier ((obj data-trait))
  (name obj))

(defmethod make-type-ref ((ttype data-trait))
  (make-instance 'type-ref :ttype ttype))

;;------------------------------------------------------------

(defvar *data-traits* (make-hash-table))

(defun data-trait-type-name-p (name)
  (not (null (gethash name *data-traits*))))

;; stride & alignment kinda seem like column/sequence parameters. an f32 is
;; still an f32 if packed inside an i64.. it's just not a very accessible one.
;; In functions the accessors should abstract the read/write and any alignment
;; involved.
;; due to this size & alignment were removed from these macros

(defun get-trait (name &key (error t))
  (assert (symbolp name))
  (or (gethash name *data-traits*)
      (when error
        (error "Tables: Unknown trait ~a" name))))

;;------------------------------------------------------------

(defmacro define-data-trait (name (&key) &body slots)
  (assert (not (get-lisp-type name :error nil)))
  (flet ((parse-slot (slot)
           (destructuring-bind (name type) slot
             (make-data-trait-slot-definition
              :name name
              :ttype type))))
    `(enqueue-definition
      ,(make-data-trait-definition
        :name name
        :slots (mapcar #'parse-slot slots)))))

;; TODO: Check all tables/queries/etc for conflicts or other potential
;;       issues
(defmethod validate-definition ((obj data-trait-definition))
  (loop :for slot :in (slots obj) :do
     (assert (or (get-trait (ttype slot) :error nil)
                 (get-bit-type (ttype slot) :error nil)
                 (get-lisp-type (ttype slot) :error nil)))))

(defmethod init-type ((definition data-trait-definition))
  (make-data-trait
   :name (name definition)
   :slots (loop :for slot :in (slots definition) :collect
             (make-data-trait-slot
              :name (name slot)
              :ttype (parse-type-specifier (ttype slot))))))

;; TODO: propegate change to all tables/queries/etc
;;       this should have been check to be safe by validate-definition
;;       so it's ok to assume it will succeed
(defmethod update-definition ((definition data-trait-definition))
  (let ((new-trait (init-type definition))
        (current (gethash (name definition) *data-traits*)))
    (if current
        (setf (ttype current) new-trait)
        (setf (gethash (name new-trait) *data-traits*)
              (make-type-ref new-trait)))))

;;------------------------------------------------------------


;; you use this to define how a given type (lisp type or tables type) satifies
;; a given data trait.
(defmacro define-trait-impl (lisp/tables-type satisfies &body accessors)
  (declare (ignore lisp/tables-type satisfies accessors))
  nil)

;;------------------------------------------------------------
