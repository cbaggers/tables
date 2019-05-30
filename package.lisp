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
           :or
           :and
           :bits
           :boolean
           :outputs
           ;;
           :b1
           :b8
           :i16
           :i32
           :i64
           :i8
           :u16
           :u32
           :u64
           :u8
           :f32
           ;;
           :vec3
           ;;
           :addable
           :subtractable
           :dividable
           :multiplyable
           :partial-numeric-equality
           :zeroable-numeric
           :negatable
           :absolutable
           ;;
           :f32+
           :f32-
           :f32*
           :f32/
           :f32=
           :f32-negate
           :f32-sqrt
           :f32-abs
           :i8+
           :i8-
           :i8*
           :i8/
           :i16+
           :i16-
           :i16*
           :i16/
           :i32+
           :i32-
           :i32*
           :i32/
           :i64+
           :i64-
           :i64*
           :i64/
           :u8+
           :u8-
           :u8*
           :u8/
           :u16+
           :u16-
           :u16*
           :u16/
           :u32+
           :u32-
           :u32*
           :u32/
           :u64+
           :u64-
           :u64*
           :u64/
           :i8=
           :i16=
           :i32=
           :i64=
           :u8=
           :u16=
           :u32=
           :u64=
           :i8-negate
           :i16-negate
           :i32-negate
           :i64-negate
           ;;
           :vec3+
           :vec3-
           :vec3*
           :vec3/
           :vec3-zerop
           :vec3+s
           :vec3-s
           :vec3*s
           :vec3/s
           :vec3-negate
           :vec3-dot
           :vec3-cross
           :vec3-length-squared
           :vec3-length
           :vec3-distance-squared
           :vec3-distance
           :vec3-abs
           ;;
           :output
           :slots
           :name
           :type
           ;;
           :*registered-top-level-functions*
           :*registered-constant-folds*
           :*registered-compiler-macros*
           :*registered-macros*
           :record-ctor-slots
           :get-top-level-function-purpose
           :purpose-name
           :purpose-target
           :record-type-p
           :value-type-p
           :value-type-size
           :ttype-aggregate-info
           ;;
           :define-backend
           :define-op-func
           :define-op-emitter
           :find-backend
           :find-op-emitter-function
           ;;
           :define-value-type
           :define-value-rw-emitters
           :find-value-rw-emitters))

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
           :ssad-read-varying
           :ssad-read-uniform
           :ssad-write-varying
           :ssad-slot-value
           :slot-id
           :subquery
           :ir
           :input-varyings
           :output-varyings
           :uniform-args
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

(uiop:define-package #:tables.compile.stage-0.record-to-slot-forms
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

(uiop:define-package #:tables.compile.stage-0.split-outputs
    (:use #:cl #:checkmate #:tables.utils #:tables.compile.stage-0)
  (:export :run-transform))

(uiop:define-package #:tables.backends.fallback
    (:use #:cl
          #:tables.utils #:tables.lang #:tables.compile.stage-0
          #:wrap-sized)
  (:import-from :alexandria
                :compose)
  (:export :emit))

(uiop:define-package #:tables
    (:use)
  (:import-from :tables.lang)
  (:export :tmem
           :tmem-ptr))
