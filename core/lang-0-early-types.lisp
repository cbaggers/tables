(in-package :tables.lang)

;;------------------------------------------------------------
;; Type Parameter Types

(define-parameter-type integer
  :valid-p integerp
  :equal =)

;;------------------------------------------------------------

(define-ttype (bits size)
  :where ((size integer)))

(define-value-type b1 (1))
(define-value-type b8 (8))

(define-value-type f32 (32)
  (sign 1)
  (exponent 8)
  (mantissa 23))

;;------------------------------------------------------------
