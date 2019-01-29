;;;; package.lisp

(uiop:define-package #:tables.utils
    (:use #:cl)
  (:export :assocr
           :last1
           :gensym-named))

(uiop:define-package #:tables.lang
    (:use #:cl #:checkmate #:tables.utils)
  (:export :tables
           ;;
           :integer
           :boolean
           :unordered-set
           :i8
           :i16
           :i32
           :i64
           :u8
           :u16
           :u32
           :u64))

(uiop:define-package #:tables.compile
    (:use #:cl #:checkmate #:tables.utils #:tables.lang)
  (:reexport #:tables.lang))

(uiop:define-package #:tables.compile.stage-0
    (:use #:cl #:checkmate #:tables.utils #:tables.compile)
  (:reexport #:tables.lang)
  (:export :ssad-let1
           :ssad-binding
           :ssad-lambda
           :ssad-if
           :ssad-funcall
           ;;
           :args
           :bindings
           :body
           :body-form
           :else
           :form
           :func
           :name
           :result
           :result-type
           :test
           :then
           :type
           ;;
           :as-debug-form))

(uiop:define-package #:tables.compile.stage-0.ast-to-ir
    (:use #:cl #:checkmate #:tables.utils #:tables.compile.stage-0)
  (:export :run-pass))

(uiop:define-package #:tables.compile.stage-0.dead-binding-removal
    (:use #:cl #:checkmate #:tables.utils #:tables.compile.stage-0)
  (:export :run-pass))

(uiop:define-package #:tables.compile.stage-0.early-constant-folding
    (:use #:cl #:checkmate #:tables.utils #:tables.compile.stage-0)
  (:export :run-pass))

(uiop:define-package #:tables.compile.stage-0.inline-direct-calls
    (:use #:cl #:checkmate #:tables.utils #:tables.compile.stage-0)
  (:export :run-pass))

(uiop:define-package #:tables.compile.stage-0.dead-if-branch-removal
    (:use #:cl #:checkmate #:tables.utils #:tables.compile.stage-0)
  (:export :run-pass))

(uiop:define-package #:tables
    (:use)
  (:import-from :tables.lang))
