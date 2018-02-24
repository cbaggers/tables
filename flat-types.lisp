(in-package :tables)
(in-readtable :fn.reader)

;;------------------------------------------------------------

;; what to do about quintword?
(defmacro declare-machine-type-size (size)
  (declare (ignore size))
  nil)

;;------------------------------------------------------------

(defvar *data-traits* (make-hash-table))

;; stride & alignment kinda seem like column/sequence parameters. an f32 is
;; still an f32 if packed inside an i64.. it's just not a very accessible one.
;; In functions the accessors should abstract the read/write and any alignment
;; involved.
;; due to this size & alignment were removed from these macros

(defun get-trait (name &key (error t))
  (assert (symbolp name))
  (or (gethash name *data-traits*)
      (when error
        (error "Tables: Unknown trait ~a" name))))

(defun parse-type-specifier (specifier)
  (or (if (listp specifier)
          (parse-type-specifier-form (first specifier) (rest specifier))
          (or (get-trait specifier :error nil)
              (get-data-type specifier)))
      (error "Tables: Unknown type specifier ~a" specifier)))

(defun get-data-type (specifier)
  (print "get-data-type not implemented yet")
  specifier)

(defgeneric parse-type-specifier-form (name args)
  (:method (name args)
    (error "Tables: Unknown type specifier (~a ~{~a~^ ~}"
           name args)))

(defmacro define-data-trait (name (&key) &body slots)
  (flet ((parse-slot (slot)
           (destructuring-bind (name type) slot
             (make-instance 'data-trait-slot-definition
                            :name name
                            :type (parse-type-specifier type)))))
    `(enqueue-definition
      ,(make-instance 'data-trait-definition
                      :name name
                      :slots (mapcar #'parse-slot slots)))))

;;------------------------------------------------------------

;; in these data types maybe the name is really a formality, all layouts with
;; the same offset, alignment, etc values are the same. (maybe)
;;
;; TODO: Should we add jai's 'using' to this? Would be nice.
(defmacro define-data-type (name (&key packed) &body parts)
  (declare (ignore name))
  (if packed
      (gen-packed-data-type name parts)
      (gen-regular-data-type name parts)))

(defun gen-packed-data-type (name parts)
  (make-instance '))

(defun gen-regular-data-type (name parts)
  )


;;------------------------------------------------------------

;; you use this to define how a given type (lisp type or tables type) satifies
;; a given data trait.
(defmacro define-trait-impl (lisp/tables-type satisfies &body accessors)
  (declare (ignore lisp/tables-type satisfies accessors))
  nil)

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

(defun type-spec->type-definiton (spec)
  spec)

;;------------------------------------------------------------

;; TODO: gotta think about endianess and unions
