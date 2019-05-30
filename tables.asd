;;;; tables.asd

(asdf:defsystem #:tables
  :description "Foo"
  :author "Chris Bagley (Baggers) <chris.bagley@gmail.com>"
  :license "BSD 2 Clause"
  :serial t
  :depends-on (:checkmate :wrap-sized :cffi)
  :components ((:file "package")
               (:file "core/utils")
               ;;
               ;; language impl
               (:file "core/compiler-macros-helpers")
               (:file "core/impl")
               (:file "core/stage-0")
               (:file "core/match-ir")
               ;;
               ;; standard lib
               (:file "std-lib/outputs")
               (:file "std-lib/early-macros")
               (:file "std-lib/type-parameters")
               (:file "std-lib/logical-types")
               (:file "std-lib/early-traits")
               (:file "std-lib/bits")
               (:file "std-lib/sized-integers")
               (:file "std-lib/floating-point")
               (:file "std-lib/vectors/vec3")
               ;;
               ;; compilation
               (:file "core/stage-0/copy-for-inlining")
               (:file "core/stage-0/ast-to-ir")
               (:file "core/stage-0/vars-to-bindings")
               (:file "core/stage-0/dead-binding-removal")
               (:file "core/stage-0/early-constant-folding")
               (:file "core/stage-0/inline-direct-calls")
               (:file "core/stage-0/dead-if-branch-removal")
               (:file "core/stage-0/inline-top-level-funcs")
               (:file "core/stage-0/subexpression-elim")
               (:file "core/stage-0/uniform-propagation")
               (:file "core/stage-0/uniform-local-lift")
               (:file "core/stage-0/compiler-macro-expand")
               (:file "core/stage-0/inline-conditional-call")
               (:file "core/stage-0/inline-cond-constants")
               (:file "core/stage-0/cleanup-outputs")
               (:file "core/stage-0/split-vertically")
               ;;
               ;;
               (:file "core/tables")
               (:file "core/macroexpand")
               ;;
               ;; backends
               (:file "core/backends/fallback/definition")
               (:file "core/backends/fallback/ops")
               (:file "core/backends/fallback/rw")
               (:file "core/backends/fallback/emit")
               ;;
               ;;
               (:file "core/compile")))
