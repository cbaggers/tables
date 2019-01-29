(in-package :tables.lang)

;;------------------------------------------------------------
;; Types

(define-parameter-type integer
  :valid-p integerp
  :equal =)

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

;;------------------------------------------------------------
;; Infer
;;
;; Infer for boolean is handled by checkmate because of defining
;; the bool type as part of defining the type system

(defmethod infer-literal ((type-system tables) (expression integer))
  ;; {TODO} this is wrong as we wont ever be able to get unsigned 8-32
  ;;        we need to special case 'the' for literals so we dont need
  ;;        to add casting
  (let ((ttype
         (typecase expression
           ((signed-byte 8) (ttype tables i8))
           ((signed-byte 16) (ttype tables i16))
           ((signed-byte 32) (ttype tables i32))
           ((signed-byte 64) (ttype tables i64))
           ((unsigned-byte 8) (ttype tables u8))
           ((unsigned-byte 16) (ttype tables u16))
           ((unsigned-byte 32) (ttype tables u32))
           ((unsigned-byte 64) (ttype tables u64)))))
    `(truly-the ,ttype ,expression)))

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
    (let* ((check-name (intern (format nil "CHECK-~s" name)))
           (ts (find-type-system 'tables))
           (spec
            (register-constraint
             (make-constraint-spec ts
                                   designator
                                   where
                                   'early-check
                                   nil))))
      `(progn
         (defun ,check-name (this type-ref)
           (declare (ignore this))
           (implements-trait-p ',name type-ref))
         (register-constraint ,spec)
         ,@(loop
              :for fspec :in funcs
              :collect (gen-trait-func ts fspec))
         ',name))))

(defun gen-trait-func (ts spec)
  (destructuring-bind (name type-designator &key satifies)
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
    (+ (:host + i8)))

  (define-trait-impl (addable i16) i16
    (+ (:host + i16)))

  (define-trait-impl (addable i32) i32
    (+ (:host + i32))))
