(in-package :tables.compile.stage-0.user-constant-folds)

;; description

(defun run-pass (ssad-let)
  (u-fold ssad-let)
  ssad-let)

(defmethod u-fold ((o ssad-let1))
  ;;
  (with-slots (bindings body-form) o
    (loop
       :for b :in bindings
       :do (u-fold-binding b))
    (u-fold body-form)
    o))

(defun u-fold-binding (binding)
  (with-slots (form) binding
    (setf form (u-fold form)))
  (values))

(defmethod u-fold ((o ssad-lambda))
  (with-slots (body-form) o
    (u-fold body-form)
    o))

(defmethod u-fold ((o ssad-if))
  (with-slots (test then else) o
    (u-fold test)
    (u-fold then)
    (u-fold else)
    o))

(defmethod u-fold ((o ssad-funcall))
  (labels ((const-p (x) (typep x 'ssad-constant)))
    (with-slots (func args) o
      (if (and (const-p func) (every #'const-p args))
          (let* ((fform (slot-value func 'form))
                 (fname (second fform))
                 (folder (gethash fname *registered-constant-folds*)))
            (if folder
                (dbind-ttype (function ~ ~return-type)
                    (find-ttype 'tables '(function (i8 i16) boolean))
                  (make-instance
                   'ssad-constant
                   :type return-type
                   :form (apply folder
                                (loop :for a :in args :collect
                                     (slot-value a 'form)))))
                o))
          o))))



(defmethod u-fold ((o ssad-var)) o)
(defmethod u-fold ((o symbol)) o)
(defmethod u-fold ((o ssad-constant)) o)
(defmethod u-fold ((o ssad-constructed)) o)
