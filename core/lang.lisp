(in-package :tables.lang)

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

;; {TODO} add 'or designator' arg to define-type-system. If set
;;        the 'if' will return an 'or' type in the case that
;;        the branch types dont unify

;;------------------------------------------------------------
;; Infer
;;
;; Infer for boolean is handled by checkmate because of defining
;; the bool type as part of defining the type system



;;------------------------------------------------------------

;; This doesnt work as we only allow one func binding per name
;;
;; (defn-host + (i8 i8) i8)
;; (defn-host + (i16 i16) i16)
;; (defn-host + (i32 i32) i32)
;; (defn-host + (i64 i64) i64)
;; (defn-host + (u8 u8) u8)
;; (defn-host + (u16 u16) u16)
;; (defn-host + (u32 u32) u32)
;; (defn-host + (u64 u64) u64)

(defun implements-trait-p (trait-name type-ref)
  (let ((implements
         (cdr (assoc 'implements
                     (spec-custom-data type-ref)))))
    (find trait-name implements)))

(defmacro define-trait (designator funcs &key where)
  (destructuring-bind (name . rest) (uiop:ensure-list designator)
    (declare (ignore rest))
    (let* ((checker-name (intern (format nil "CHECK-~s" name)))
           (ts (find-type-system 'tables))
           (spec
            (register-constraint
             (make-constraint-spec ts
                                   designator
                                   where
                                   'early-check
                                   nil))))
      `(progn
         (defun ,checker-name (this type-ref)
           (declare (ignore this))
           (implements-trait-p ',name type-ref))
         (register-constraint ,spec)
         ,@(loop
              :for fspec :in funcs
              :collect (gen-trait-func ts fspec))
         ',name))))

(defun gen-trait-func (ts spec)
  (destructuring-bind (name type-designator &key satisfies)
      spec
    (assert (eq (first type-designator) 'function))
    (let ((ftype (checkmate::designator->type ts type-designator)))
      `())))

(defmacro define-trait-impl (trait type &body funcs)
  (declare (ignore trait type funcs)))

#+nil
(progn
  (define-trait (addable type)
      ((+ (function (?a ?a) ?a)
          :satisfies ((addable ?a) ?a)))
    :where ((type ttype))) ;; could be omitted as ttype is the default

  (define-trait-impl (addable i8) i8
    (+ (:host +)))

  (define-trait-impl (addable i16) i16
    (+ (:host +)))

  (define-trait-impl (addable i32) i32
    (+ (:host +))))
