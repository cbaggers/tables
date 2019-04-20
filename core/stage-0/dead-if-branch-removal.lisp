(in-package :tables.compile.stage-0.dead-if-branch-removal)

;;
;; WARNING: Pass mutates graph
;;

;;------------------------------------------------------------

(defun run-pass (ssad-let cmp-ctx)
  (remove-dead ssad-let cmp-ctx)
  (values))

;;------------------------------------------------------------

(defmethod remove-dead ((o ssad-let1) cmp-ctx)
  (with-slots (bindings body-form type) o
    ;;
    (setf bindings
          (loop
             :for b :in bindings
             :for new-form := (with-slots (form) b
                                (remove-dead form cmp-ctx))
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
    (setf body-form (remove-dead body-form cmp-ctx))
    (setf type (if (typep body-form 'ssad-let1)
                   (slot-value body-form 'type)
                   type))
    o))

(defmethod remove-dead ((o ssad-lambda) cmp-ctx)
  (with-slots (args body-form result-type) o
    (setf body-form (remove-dead body-form cmp-ctx))
    (setf result-type (if (typep body-form 'ssad-let1)
                          (slot-value body-form 'type)
                          result-type))
    o))

(defmethod remove-dead ((o ssad-if) cmp-ctx)
  (with-slots (test then else) o
    (assert (atom test))
    (if (typep test 'ssad-constant)
        (with-slots (form) test
          (mark-changed cmp-ctx)
          (cond
            ((null form) else)
            ((eq form t) then)
            (t (error "bug: ~a" form))))
        (progn
          (remove-dead then cmp-ctx)
          (remove-dead else cmp-ctx)
          o))))

(defmethod remove-dead ((o ssad-funcall) cmp-ctx)
  (with-slots (func args) o
    (remove-dead func cmp-ctx)
    (map nil (lambda (a) (remove-dead a cmp-ctx)) args)
    o))

(defmethod remove-dead ((o ssad-var) cmp-ctx) o)
(defmethod remove-dead ((o symbol) cmp-ctx) o)
(defmethod remove-dead ((o ssad-constant) cmp-ctx) o)
(defmethod remove-dead ((o ssad-constructed) cmp-ctx) o)

;;------------------------------------------------------------
