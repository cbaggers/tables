;;;; package.lisp

(uiop:define-package #:tables.utils
    (:use #:cl)
  (:export :assocr
           :last1
           :gensym-named
           :rehome-symbol
           :vbind
           :find-in-tree-if
           :string-desig-and=))

(uiop:define-package #:tables.macros (:use))

(uiop:define-package #:tables.lang
    (:use #:cl #:checkmate #:tables.utils)
  (:intern :outputs)
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
           :output
           ;;
           :*registered-top-level-functions*
           :*registered-constant-folds*
           :*registered-compiler-macros*
           :*registered-macros*
           :record-ctor-slots
           :record-type-p
           :value-type-p
           :value-type-size))

(uiop:define-package #:tables.tables
    (:use #:cl #:checkmate #:tables.utils #:tables.lang)
  (:export))

(uiop:define-package #:tables.compile
    (:use #:cl #:checkmate #:tables.utils #:tables.lang)
  (:reexport #:tables.lang)
  (:export :make-compile-context
           :mark-changed
           :marked-changed-p
           :clear-mark))

(uiop:define-package #:tables.compile.stage-0
    (:use #:cl #:checkmate #:tables.utils #:tables.compile)
  (:reexport #:tables.lang)
  (:reexport #:tables.compile)
  (:export :ir-node
           :ssad-let1
           :ssad-binding
           :ssad-lambda
           :ssad-if
           :ssad-funcall
           :ssad-var
           :ssad-constant
           :ssad-constructed
           :ssad-output
           :ssad-read-val
           ;;
           :args
           :arg-bindings
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
           :is-uniform
           :names
           ;;
           :as-debug-form
           :var-eq
           :match-ir*
           ;;
           :copy-for-inlining))

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

(uiop:define-package #:tables.compile.stage-0.uniform-propagation
    (:use #:cl #:checkmate #:tables.utils #:tables.compile.stage-0)
  (:export :run-pass))

(uiop:define-package #:tables.compile.stage-0.uniform-local-lift
    (:use #:cl #:checkmate #:tables.utils #:tables.compile.stage-0)
  (:export :run-pass))

(uiop:define-package #:tables.compile.stage-0.compiler-macro-expand
    (:use #:cl #:checkmate #:tables.utils #:tables.compile.stage-0)
  (:export :run-pass))

(uiop:define-package #:tables.compile.stage-0.inline-conditional-call
    (:use #:cl #:checkmate #:tables.utils #:tables.compile.stage-0)
  (:export :run-pass))

(uiop:define-package #:tables.compile.stage-0.inline-conditional-constants
    (:use #:cl #:checkmate #:tables.utils #:tables.compile.stage-0)
  (:export :run-pass))

(uiop:define-package #:tables.compile.stage-0.cleanup-outputs
    (:use #:cl #:checkmate #:tables.utils #:tables.compile.stage-0)
  (:export :run-pass))

(uiop:define-package #:tables.compile.stage-0.split-vertically
    (:use #:cl #:checkmate #:tables.utils #:tables.compile.stage-0)
  (:export :run-transform))

(uiop:define-package #:tables
    (:use)
  (:import-from :tables.lang)
  (:export :tmem
           :tmem-ptr))
