(in-package #:tables)
(in-readtable :fn.reader)

;;------------------------------------------------------------

;; Definitions are what are validated and used to create a type object.
;; A specifier is the symbol/list that names a type object

;;------------------------------------------------------------

(defgeneric init-type (definition))


(defgeneric update-definition (definition)
  (:method (definition)
    (error "Tables: update-definition not defined for ~a"
           definition)))


(defgeneric validate-definition (definition)
  (:method (definition)
    (error "Tables: validate-definition not defined for ~a"
           definition)))


(defgeneric to-specifier (ttype))

(defun parse-type-specifier (specifier)
  (let ((type (cond
                ((listp specifier)
                 (parse-type-specifier-form (first specifier)
                                            (rest specifier)))
                ((numberp specifier)
                 (make-type-ref (make-anon-type :size specifier)))
                (t (or (get-lisp-type specifier :error nil)
                       (get-trait specifier :error nil)
                       (get-bit-type specifier :error nil))))))
    (assert type () "Tables: Unknown type specifier ~a" specifier)
    (assert (typep type 'type-ref))
    type))

(defgeneric parse-type-specifier-form (name args)
  (:method (name args)
    (error "Tables: Unknown type specifier (~a ~{~a~^ ~}"
           name args)))

;;------------------------------------------------------------

(defclass type-ref ()
  ((ttype :initarg :ttype :accessor ttype)))

(defgeneric make-type-ref (ttype)
  (:method (ttype)
    (error "Tables: Cannot make type-ref to ~a~%Value: ~a"
           (type-of ttype) ttype)))

(defmethod print-object ((obj type-ref) stream)
  (with-slots (ttype) obj
    (format stream "#<TYPE-REF ~s>" ttype)))

;;------------------------------------------------------------

(defclass lisp-type ()
  ((ttype :initarg :ttype :reader ttype)))

(defun make-lisp-type (specifier)
  (make-instance 'lisp-type :ttype specifier))

(defmethod make-type-ref ((ttype lisp-type))
  (make-instance 'type-ref :ttype ttype))

(defvar *allowed-lisp-types*
  (alexandria:alist-hash-table
   (list (cons 'single-float
               (make-type-ref (make-lisp-type 'single-float)))
         (cons 'double-float
               (make-type-ref (make-lisp-type 'double-float)))
         (cons '(unsigned-byte 8)
               (make-type-ref (make-lisp-type '(unsigned-byte 8))))
         (cons '(unsigned-byte 16)
               (make-type-ref (make-lisp-type '(unsigned-byte 16))))
         (cons '(unsigned-byte 32)
               (make-type-ref (make-lisp-type '(unsigned-byte 32))))
         (cons '(unsigned-byte 64)
               (make-type-ref (make-lisp-type '(unsigned-byte 64))))
         (cons '(signed-byte 8)
               (make-type-ref (make-lisp-type '(signed-byte 8))))
         (cons '(signed-byte 16)
               (make-type-ref (make-lisp-type '(signed-byte 16))))
         (cons '(signed-byte 32)
               (make-type-ref (make-lisp-type '(signed-byte 32))))
         (cons '(signed-byte 64)
               (make-type-ref (make-lisp-type '(signed-byte 64)))))
   :test #'equal))

(defun get-lisp-type (name &key (error t))
  (or (gethash name *allowed-lisp-types*)
      (when error
        (error "Tables: Invalid type ~a" name))))

;;------------------------------------------------------------
;; it's a mask, we will try to pack this with other sub-word
;; sized data. tables can be clustered on enum values.
;;
;; define-enum should, when recompiled, keep current values valid
;; this means not reassigning all the values. If a value is removed
;; keep track of this value so it can be reused before extending
;; the bit-length of the type.
;;

(defmacro define-enum (name &body constants)
  (declare (ignore name constants))
  nil)

;; same syntax as enum but each entry can have multiple flags
;; so flag values increase by power of 2 (1,2,4,8,etc) whereas
;; enums increase by 1
;;
;; Also it is valid to a row to have 0 for flags (no flags) but
;; that is not legal for enums. Enum column entries must always
;; be one of the entries.
(defmacro define-flags (name &body constants)
  (declare (ignore name constants))
  nil)

;;------------------------------------------------------------

#+nil
(define-enum entity-kinds
  player
  bat-enemy
  turtle-enemy)

;;------------------------------------------------------------

#||

What data operations does a type need to provide? We need accessors for sure.
with those we can make swap, so nothing else needed there. hmm

||#

;;------------------------------------------------------------

;; TODO: gotta think about endianess and unions

;;------------------------------------------------------------
