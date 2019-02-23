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

(define-trait addable
    ((+ (function (?a ?a) ?a)))
  ;; vv- could be omitted as ttype is the default
  :where ((type ttype)))

(define-dummy-func i8+ (i8 i8) i8)

(define-trait-impl addable i8
  (+ i8+))

(define-dummy-func f32+ (f32 f32) f32)

(define-trait-impl addable f32
  (+ f32+))

;;------------------------------------------------------------
;; Value types

(define-value-type b1 (1))
(define-value-type b8 (8))

(define-value-type f32 (32)
  (sign 1)
  (exponent 8)
  (mantissa 23))

;;------------------------------------------------------------

(define-record vec3
  (x f32)
  (y f32)
  (z f32))

(defn vec3+ ((a vec3) (b vec3))
  (vec3 (+ (vec3-x a) (vec3-x b))
        (+ (vec3-y a) (vec3-z b))
        (+ (vec3-y a) (vec3-z b))))

(define-trait-impl addable vec3
  (+ vec3+))

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
