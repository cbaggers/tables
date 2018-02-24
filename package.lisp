;;;; package.lisp

(uiop:define-package #:tables
    (:use :cl :cffi :bordeaux-threads
          :completable-types)
  (:import-from :alexandria
                :with-gensyms
                :ensure-list))
