;;;; package.lisp

(uiop:define-package #:tables
    (:use :cl :cffi :bordeaux-threads)
  (:import-from :alexandria
                :with-gensyms))
