(in-package #:tables)

;;------------------------------------------------------------

(define-completable data-trait-slot-definition ()
  (name symbol)
  (ttype #'allowed-lisp-type-p))

(define-completable data-trait-definition ()
  (name symbol)
  (slots list))

(define-completable data-trait-slot ()
  (name symbol)
  (ttype type-ref))

(define-completable data-trait ()
  (name symbol)
  (slots list))

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

;;------------------------------------------------------------
