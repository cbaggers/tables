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

(defmacro define-data-trait (name (&key) &body slots)
  (flet ((parse-slot (slot)
           (destructuring-bind (name type) slot
             (make-data-trait-slot-definition
              :name name
              :ttype (parse-type-specifier type)))))
    `(enqueue-definition
      ,(make-data-trait-definition
        :name name
        :slots (mapcar #'parse-slot slots)))))

;; TODO: propegate change to all tables/queries/etc
(defmethod update-definition ((definition data-trait-definition))
  (let ((current (gethash (name definition) *data-traits*)))
    (if current
        (setf (ttype current) definition)
        (setf (gethash (name definition) *data-traits*)
              (make-type-ref :ttype definition)))))

;;------------------------------------------------------------

(defvar *bit-types* (make-hash-table))


(defun bit-type-name-p (name)
  (not (null (gethash name *bit-types*))))

(defun get-bit-type (name &key (error t))
  (assert (symbolp name))
  (or (gethash name *bit-types*)
      (when error
        (error "Tables: Unknown type ~a" name))))

;; in these data types maybe the name is really a formality, all layouts with
;; the same offset, alignment, etc values are the same. (maybe)
;;
;; TODO: Should we add jai's 'using' to this? Would be nice.
(defmacro define-bit-type (name (&key packed) &body parts)
  (assert (member packed '(t nil)))
  (flet ((parse-part (part)
           (destructuring-bind (name type/size &key offset)
               (if (numberp part)
                    (list nil part)
                   part)
             (when (and packed (null name))
               (assert
                (numberp type/size) ()
                "Tables: if packed slot is anonomous the type must be a bit size"))
             (when (and (not packed) (numberp type/size))
               (assert
                (null name) ()
                "Tables: if unpacked slot is padding the name must be nil"))
             (make-bit-type-part-definition
              :name name
              :ttype (parse-type-specifier type/size)
              :offset offset))))
    `(enqueue-definition
      ,(make-bit-type-definition
        :name name
        :packed packed
        :parts (mapcar #'parse-part parts)))))

;; TODO: propegate change to all tables/queries/etc
(defmethod update-definition ((definition bit-type-definition))
  (let ((current (gethash (name definition) *bit-types*)))
    (if current
        (setf (ttype current) definition)
        (setf (gethash (name definition) *bit-types*)
              (make-type-ref :ttype definition)))))

;;------------------------------------------------------------

(defun parse-type-specifier (specifier)
  (let ((type (cond
                ((listp specifier)
                 (parse-type-specifier-form (first specifier)
                                            (rest specifier)))
                ((numberp specifier)
                 (make-type-ref
                  :ttype (make-anon-type :size specifier)))
                (t (or (get-trait specifier :error nil)
                       (get-bit-type specifier :error nil))))))
    (assert type () "Tables: Unknown type specifier ~a" specifier)
    (assert (typep type 'type-ref))
    type))

(defgeneric parse-type-specifier-form (name args)
  (:method (name args)
    (error "Tables: Unknown type specifier (~a ~{~a~^ ~}"
           name args)))

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

#||

What data operations does a type need to provide? We need accessors for sure.
with those we can make swap, so nothing else needed there. hmm

||#

;;------------------------------------------------------------

;; TODO: gotta think about endianess and unions
