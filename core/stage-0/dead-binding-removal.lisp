(in-package :tables.compile.stage-0.dead-binding-removal)

;;
;; WARNING: Pass mutates graph
;;

;;------------------------------------------------------------

(defun run-pass (ssad-let)
  (let* ((live-set (make-hash-table)))
    (find-live ssad-let live-set)
    (remove-dead ssad-let live-set)
    ssad-let))

;;------------------------------------------------------------

(defmethod find-live ((o ssad-let1) live)
  (with-slots (bindings body-form) o
    (loop :for binding :in bindings :do
         (find-live (slot-value binding 'form) live))
    (find-live body-form live)
    (values)))

(defmethod find-live ((o ssad-lambda) live)
  (with-slots (body-form) o
    (find-live body-form live)
    (values)))

(defmethod find-live ((o ssad-if) live)
  (with-slots (test then else) o
    (find-live test live)
    (find-live then live)
    (find-live else live)
    (values)))

(defmethod find-live ((o ssad-funcall) live)
  (with-slots (func args) o
    (find-live func live)
    (loop :for a :in args :do (find-live a live))
    (values)))

(defmethod find-live ((o ssad-var) live)
  (with-slots (binding) o
    (setf (gethash binding live) t)))

(defmethod find-live ((o symbol) live))
(defmethod find-live ((o ssad-constant) live))

;;------------------------------------------------------------

(defmethod remove-dead ((o ssad-let1) live)
  (with-slots (bindings body-form type) o
    (setf bindings
          (remove-if-not (lambda (b) (gethash b live))
                         bindings))
    (remove-dead body-form live)
    (values)))

(defmethod remove-dead ((o ssad-lambda) live)
  (with-slots (body-form) o
    (remove-dead body-form live)
    (values)))

(defmethod remove-dead ((o ssad-if) live)
  (with-slots (test then else) o
    (remove-dead test live)
    (remove-dead then live)
    (remove-dead else live)
    (values)))

(defmethod remove-dead ((o ssad-funcall) live)
  (with-slots (func args) o
    (remove-dead func live)
    (map nil (lambda (a) (remove-dead a live)) args)
    (values)))

(defmethod remove-dead ((o ssad-var) live) (values))
(defmethod remove-dead ((o symbol) live) (values))
(defmethod remove-dead ((o ssad-constant) live) (values))

;;------------------------------------------------------------
