(in-package :tables.lang)

;;------------------------------------------------------------
;; Type Parameter Types

(define-parameter-type integer
  :valid-p integerp
  :equal =)
