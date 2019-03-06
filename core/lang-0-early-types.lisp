(in-package :tables.lang)

;;------------------------------------------------------------
;; Type Parameter Types

(define-parameter-type integer
  :valid-p integerp
  :equal =)

;;------------------------------------------------------------
;; Types

(define-ttype boolean)

(define-ttype (unordered-set type size)
  :where ((size integer)))

(define-ttype i8)
(define-ttype i16)
(define-ttype i32)
(define-ttype i64)

(define-ttype u8)
(define-ttype u16)
(define-ttype u32)
(define-ttype u64)

(define-ttype (or type-a type-b))
(define-ttype (and type-a type-b))

(define-ttype (bits size)
  :where ((size integer)))

;;------------------------------------------------------------
;; Traits

(define-trait addable ()
  ((+ (function (?a ?a) ?a))))

(define-dummy-func i8+ (i8 i8) i8)

#+nil
(define-trait-impl addable i8
  (+ i8+))

;;------------------------------------------------------------
;; Value types

(define-value-type b1 (1))
(define-value-type b8 (8))

(define-value-type f32 (32)
  (sign 1)
  (exponent 8)
  (mantissa 23))

(define-dummy-func f32+ (f32 f32) f32)

#+nil
(define-trait-impl addable f32
  (+ f32+))

;;------------------------------------------------------------

(define-record vec3
  (x f32)
  (y f32)
  (z f32))

#+nil
(defn vec3+ ((a vec3) (b vec3))
  (vec3 (+ (vec3-x a) (vec3-x b))
        (+ (vec3-y a) (vec3-z b))
        (+ (vec3-y a) (vec3-z b))))

#+nil
(define-trait-impl addable vec3
  (+ vec3+))

#+nil
(defn boop ((x vec3))
  (+ x x))

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

;; should we allow unknowns that arent in the trait-designator
;; or associated-type? probably not right?
(define-trait (fooable ?a) ()
  ((foo (function (?b ?c) ?c))
   (foop (function (?b) ?a))))

(define-dummy-func i8foo (i8 i32) i32)
(define-dummy-func i8foop (i8) u8)

;; INCORRECT, i8foop's u8 is not ?a
#+nil
(define-trait-impl (fooable i8) i8
  (foo i8foo)
  (foop i8foop))

;;------------------------------------------------------------
;; Ok so here I want to see how constraints with args can unify
;; their type args.
;;
;; I think that before 'check' is run we need to unify the
;; ttype args. We must do something like this for regular ttypes right?

(progn

  (defun is-breenable (this type-ref)
    (break "ho ~a ~a" this type-ref)
    (print (list :ohmy this type-ref))
    (list type-ref))

  (define-constraint (breenable ?x)
    :satisfies-this-p is-breenable))

(defun test ()
  (let* ((u (make-hash-table))
         (a (find-ttype
             'tables '(or ?b ?a) :unknowns u
             :declarations '((satisfies (breenable ?b) ?a))))
         (b (find-ttype 'tables '(or i8 i32) :unknowns u)))
    (or (checkmate::unify a b)
        (list a b))))
