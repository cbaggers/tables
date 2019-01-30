(in-package :tables.compile.stage-0.dead-if-branch-removal)

;;
;; WARNING: Pass mutates graph
;;

;;------------------------------------------------------------

(defun run-pass (ssad-let)
  (remove-dead ssad-let))

;;------------------------------------------------------------

(defmethod remove-dead ((o ssad-let1))
  (with-slots (bindings body-form type) o
    ;;
    (setf bindings
          (loop
             :for b :in bindings
             :for new-form := (with-slots (form) b (remove-dead form))
             :for merge := (typep new-form 'ssad-let1)
             :if merge
             :append
               (with-slots (form type) b
                 (setf form (slot-value new-form 'body-form))
                 (setf type (slot-value new-form 'type))
                 (append (slot-value new-form 'bindings)
                         (list b)))
             :else
             :collect b))
    ;;
    (setf body-form (remove-dead body-form))
    (setf type (if (typep body-form 'ssad-let1)
                   (slot-value body-form 'type)
                   type))
    o))

(defmethod remove-dead ((o ssad-lambda))
  (with-slots (args body-form result-type) o
    (setf body-form (remove-dead body-form))
    (setf result-type (if (typep body-form 'ssad-let1)
                          (slot-value body-form 'type)
                          result-type))
    o))

(defmethod remove-dead ((o ssad-if))
  (with-slots (test then else) o
    (assert (atom test))
    (if (typep test 'ssad-constant)
        (with-slots (form) test
          (cond
            ((null form) else)
            ((eq form t) then)
            (t (error "bug: ~a" form))))
        (progn
          (remove-dead then)
          (remove-dead else)
          o))))

(defmethod remove-dead ((o ssad-funcall))
  (with-slots (func args) o
    (remove-dead func)
    (map nil #'remove-dead args)
    o))

(defmethod remove-dead ((o ssad-var)) o)
(defmethod remove-dead ((o symbol)) o)
(defmethod remove-dead ((o ssad-constant)) o)

;;------------------------------------------------------------
