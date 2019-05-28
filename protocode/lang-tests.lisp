
;;------------------------------------------------------------

;; dont eval type macros until all types are known
;; if reach end of function and some have not expanded
;; that is an error. Therefore type eval is lazy
;;
;; (defn foo ((x ?b) (z (bar i8 ?a 1 2 3)) (y ?a))
;;   ..)

;; (or i8 boolean)
;;
;; (let ((a (if something
;;              1
;;              t)))
;;   (typecase a
;;     (boolean
;;      (let ((a (truly-the a boolean)))
;;        ..))
;;     (i8
;;      (let ((a (truly-the a i8)))
;;        ..))))

;;------------------------------------------------------------

(define-op-func i8foo (i8 i32) i32)
(define-op-func i8foop (i8 i32) i8)

(define-trait (fooable ?a) ()
  ((foo (function (?b ?c) ?c))
   (foop (function (?b ?c) ?a))))

(define-trait-impl (fooable i8) () i8
  (foo i8foo)
  (foop i8foop))


(define-trait iter ?item
  ((next (function (?a) ?item))))

(define-ttype (unordered-set ?x))

(define-op-func u-set-next ((unordered-set ?x)) ?x)

(define-trait-impl iter (?b) (unordered-set ?b)
  (next u-set-next))
