(in-package #:tables)

;;------------------------------------------------------------

(define-completable anon-type ()
  (size unsigned-byte))

(defmethod make-type-ref ((ttype anon-type))
  (make-instance 'type-ref :ttype ttype))

(defmethod to-specifier ((obj anon-type))
  (size obj))

;;------------------------------------------------------------
