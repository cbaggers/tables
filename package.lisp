;;;; package.lisp

(uiop:define-package #:tables
  (:use #:cl)
  (:import-from #:alexandria
                :with-gensyms))
