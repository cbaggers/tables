(defpackage :foo (:use :cl))
(in-package :foo)

;;------------------------------------------------------------
;; non trait types (defining no functions)
(defun int8 ())
(defun int16 ())

;;------------------------------------------------------------

(defun collection (x)
  `((cons (function (,x (collection ,x)) (collection ,x)))
    (first (function ((collection ,x)) ,x))
    (second (function ((collection ,x)) ,x))))

;; would need a form of pattern matching, essentially culminating
;; in a logic language. So why not just do it properly..eh lets keep thinking

;;------------------------------------------------------------

(defmacro define-trait (name args &body functions &key &allow-other-keys)
  nil)

(define-trait int8 ())

(define-trait int16 ())

(define-trait collection (x)
  (cons (x (collection x)) (collection x))
  (first ((collection x)) x)
  (second ((collection x)) x)
  (reduce ((function (a (collection x)) a) (collection x) a)
          x))

;;------------------------------------------------------------

(defmacro define-trait-impl (name type args &body body))

(define-trait-impl cons list ((elem x)
                              (list (list x)))
  (list-cons elem list))

(define-trait-impl reduce list ((f (function (x (list x)) x))
                                x
                                x))

;; eh, should be able to just specify existing funcs

;;------------------------------------------------------------

(defmacro satisfy-trait (trait &body funcs &key &allow-other-keys))

(satisfy-trait collection
  :cons #'list-cons
  :first #'list-first
  :second #'list-second
  :reduce #'list-reduce)

;; or something.. brain is dead.

;;------------------------------------------------------------

;; I need to look again at the mechanics and concerns of the bidirectional
;; typechecking so I can understand what is expected and what is missing.
