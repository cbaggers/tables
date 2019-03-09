(in-package :tables.lang)

;;------------------------------------------------------------

;; If overflow is possible then types satisfying these traits should wrap
;; on overflow

(define-trait addable ()
  ((+ (function (?a ?a) ?a))))

(define-trait subtractable ()
  ((- (function (?a ?a) ?a))))

(define-trait dividable ()
  ((/ (function (?a ?a) ?a))))

(define-trait multiplyable ()
  ((* (function (?a ?a) ?a))))

;;------------------------------------------------------------

(define-trait partial-numeric-equality ()
  ((= (function (?a ?a) boolean))))

(define-trait zeroable-numeric ()
  ((zerop (function (?a) boolean))))

(define-trait negatable ()
  ((negate (function (?a) ?a))))

(define-trait absolutable ()
  ((abs (function (?a) ?a))))

;;------------------------------------------------------------
