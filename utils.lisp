(in-package #:tables)

;;------------------------------------------------------------

(defun intern-f (pattern &rest parts)
  (intern (apply #'format nil pattern parts)))

(defun intern-pf (package pattern &rest parts)
  (intern (apply #'format nil pattern parts) package))

(defun symb (&rest parts)
  (intern (format nil "~{~a~}" parts)))

(defun p-symb (package &rest parts)
  (intern (format nil "~{~a~}" parts) package))

(defun kwd (&rest parts)
  (intern (format nil "~{~a~}" parts) :keyword))

;;------------------------------------------------------------
