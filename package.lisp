;;;; package.lisp

(uiop:define-package #:tables.utils
    (:use #:cl)
  (:export :assocr
           :last1
           :gensym-named
           :rehome-symbol))

(uiop:define-package #:tables.macros (:use))

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
           :u64
           :f32
           :vec3
           ;;
           :*registered-top-level-functions*
           :record-ctor-slots))

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
           :ssad-var
           :ssad-constant
           :ssad-constructed
           ;;
           :args
           :binding
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

(uiop:define-package #:tables.compile.stage-0.vars-to-bindings
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

(uiop:define-package #:tables.compile.stage-0.inline-top-level-functions
    (:use #:cl #:checkmate #:tables.utils #:tables.compile.stage-0)
  (:export :run-pass))

(uiop:define-package #:tables.compile.stage-0.subexpression-elim
    (:use #:cl #:checkmate #:tables.utils #:tables.compile.stage-0)
  (:export :run-pass))

(uiop:define-package #:tables
    (:use)
  (:import-from :tables.lang))
