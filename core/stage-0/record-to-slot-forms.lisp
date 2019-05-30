(in-package :tables.compile.stage-0.record-to-slot-forms)

;; directly called function inlining
;;
;; WARNING: Pass mutates graph
;;

(defun run-pass (ssad-let cmp-ctx)
  (transform-calls ssad-let cmp-ctx))

(defmethod transform-calls ((o ssad-let1) cmp-ctx)
  (with-slots (bindings body-form type) o
    (loop
       :for binding :in bindings
       :do (setf (slot-value binding 'form)
                 (transform-calls (slot-value binding 'form) cmp-ctx)))
    (setf body-form (transform-calls body-form cmp-ctx))
    o))

(defmethod transform-calls ((o ssad-funcall) cmp-ctx)
  (with-slots (func args) o
    (etypecase func
      (ssad-constant
       (with-slots (form type) func
         (dbind-ttype (function ~ ~return-type) type
           (let ((purpose (get-top-level-function-purpose
                           cmp-ctx
                           (second form))))
             (if (and purpose
                      (eq (purpose-name purpose) :record-accessor))
                 (progn
                   (assert (= (length args) 1))
                   (make-instance 'ssad-slot-value
                                  :form (first args)
                                  :name (purpose-target purpose)
                                  :type return-type))
                 o)))))
      (ssad-var o))))

(defmethod transform-calls ((o ssad-output) cmp-ctx)
  (with-slots (args) o
    (setf args (mapcar (lambda (x) (transform-calls x cmp-ctx))
                       args))
    o))

(defmethod transform-calls ((o ssad-lambda) cmp-ctx)
  (with-slots (body-form) o
    (setf body-form (transform-calls body-form cmp-ctx))
    o))

(defmethod transform-calls ((o ssad-if) cmp-ctx)
  (with-slots (test then else) o
    (setf test (transform-calls test cmp-ctx))
    (setf then (transform-calls then cmp-ctx))
    (setf else (transform-calls else cmp-ctx))
    o))

(defmethod transform-calls ((o ssad-slot-value) cmp-ctx)
  (with-slots (form) o
    (setf form (transform-calls form cmp-ctx)))
  o)

(defmethod transform-calls ((o ssad-write-varying) cmp-ctx)
  (with-slots (form) o
    (setf form (transform-calls form cmp-ctx)))
  o)

(defmethod transform-calls ((o ssad-var) cmp-ctx) o)
(defmethod transform-calls ((o symbol) cmp-ctx) o)
(defmethod transform-calls ((o ssad-constant) cmp-ctx) o)
(defmethod transform-calls ((o ssad-constructed) cmp-ctx) o)
(defmethod transform-calls ((o ssad-read-varying) cmp-ctx) o)
(defmethod transform-calls ((o ssad-read-uniform) cmp-ctx) o)
