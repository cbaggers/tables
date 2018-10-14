;;;; package.lisp

(defpackage #:tables-lang
  (:use #:cl #:checkmate))

(defpackage #:tables
  (:use)
  (:import-from :tables-lang))
