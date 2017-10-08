(in-package #:tables)

;;------------------------------------------------------------

(defvar *registered-flat-types*
  (make-hash-table))

(defun register-flat-type (flat-type &key allow-replace)
  (assert (typep flat-type 'flat-type))
  (assert (specifier flat-type))
  (let ((flat-type-already-exists
         (gethash (specifier flat-type) *registered-flat-types*)))
    (if (and allow-replace flat-type-already-exists)
        ;; not sure how valuable this is as type doesnt yet exist in image
        (announce-flat-type-replacement flat-type-already-exists flat-type)
        (error "WAHHH ~a" flat-type-already-exists)))
  (setf (gethash (specifier flat-type) *registered-flat-types*)
        flat-type))

(defun flat-typep (type/type-specifier)
  (or (typep type/type-specifier 'flat-type)
      (flat-type-specifier-p type/type-specifier)))

(defun flat-type-specifier-p (type/type-specifier)
  (member type/type-specifier (hash-table-keys *registered-flat-types*p)))

(defun announce-flat-type-replacement (flat-type-already-exists flat-type)
  (format t "~%Whatevs ~s ~s" flat-type-already-exists flat-type))
