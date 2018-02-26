(in-package #:tables)

;; TODO: Should we add jai's 'using' to this? Would be nice.

;;------------------------------------------------------------

(defun bit-type-specifier-p (x)
  (or (symbolp x) (numberp x)))

(define-completable bit-type-slot-definition ()
  (name symbol)
  (ttype #'bit-type-specifier-p)
  (offset unsigned-byte))

(define-completable bit-type-definition ()
  (name symbol)
  (packed boolean)
  (slots list))

(define-completable bit-type-slot ()
  (name symbol)
  (ttype type-ref)
  (offset unsigned-byte))

(define-completable bit-type ()
  (name symbol)
  (packed boolean)
  (slots list))

(defmethod to-specifier ((obj bit-type-definition))
  (name obj))

(defmethod to-specifier ((obj bit-type))
  (name obj))

(defmethod make-load-form ((obj bit-type-definition) &optional env)
  (declare (ignore env))
  (with-contents (name packed slots) obj
    `(make-bit-type-definition
      :name ',name
      :packed ,packed
      :slots (list ,@slots))))

(defmethod make-load-form ((obj bit-type-slot-definition) &optional env)
  (declare (ignore env))
  (with-contents (name ttype offset) obj
    `(make-bit-type-slot-definition
      :name ',name
      :ttype ',ttype
      :offset ,offset)))

;;------------------------------------------------------------

(defmethod to-specifier ((obj anon-type))
  (size obj))

;;------------------------------------------------------------

(defvar *bit-types* (make-hash-table))

(defun bit-type-name-p (name)
  (not (null (gethash name *bit-types*))))

(defun get-bit-type (name &key (error t))
  (assert (symbolp name))
  (or (gethash name *bit-types*)
      (when error
        (error "Tables: Unknown type ~a" name))))

;;------------------------------------------------------------

(defmethod init-type ((definition bit-type-definition))
  (make-bit-type
   :name (name definition)
   :packed (packed definition)
   :slots (loop :for slot :in (slots definition) :collect
             (make-bit-type-slot
              :name (name slot)
              :ttype (parse-type-specifier (ttype slot))
              :offset (offset slot)))))

;;------------------------------------------------------------

;; in these data types maybe the name is really a formality, all layouts with
;; the same offset, alignment, etc values are the same. (maybe)
;;
;; TODO: Should check for types in the arbiter queue as some types might not be
;;       defined yet. The validate-definition for the type will handle if the
;;       type really exists.
;;
;; TODO: FIX OFFSET
;;
(defmacro define-bit-type (name (&key packed) &body slots)
  (assert (member packed '(t nil)))
  (flet ((parse-slot (slot)
           (destructuring-bind (name type/size &key offset)
               (if (numberp slot)
                    (list nil slot)
                    slot)
             (when (and packed (null name))
               (assert
                (numberp type/size) ()
                "Tables: if packed slot is anonomous the type must be a bit size"))
             (when (and (not packed) (numberp type/size))
               (assert
                (null name) ()
                "Tables: if unpacked slot is padding the name must be nil"))
             (make-bit-type-slot-definition
              :name name
              :ttype type/size
              :offset (or offset 0)))))
    `(enqueue-definition
      ,(make-bit-type-definition
        :name name
        :packed packed
        :slots (mapcar #'parse-slot slots)))))

(defmethod validate-definition ((definition bit-type-definition))
  (loop :for slot :in (slots definition) :do
     (unless (numberp (ttype slot))
       (assert
        (bit-type-name-p (ttype slot))))))

(defmethod update-definition ((definition bit-type-definition))
  (let* ((new-type (init-type definition))
         (current (gethash (name new-type) *bit-types*)))
    ;; TODO: propegate change to all tables/queries/etc
    ;; TODO: Doesnt this potentially add columns? why not use
    ;;       add-column in that case?
    (if current
        (setf (ttype current) new-type)
        (setf (gethash (name new-type) *bit-types*)
              (make-type-ref :ttype new-type)))))

;;------------------------------------------------------------
