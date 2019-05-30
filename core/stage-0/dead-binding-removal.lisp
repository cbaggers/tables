(in-package :tables.compile.stage-0.dead-binding-removal)

;;
;; WARNING: Pass mutates graph
;;

;;------------------------------------------------------------

(defun run-pass (ssad-let cmp-ctx)
  (let* ((live-set (make-hash-table)))
    (find-live ssad-let live-set)
    (remove-dead ssad-let live-set cmp-ctx)
    (values)))

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

(defmethod find-live ((o ssad-output) live)
  (with-slots (args) o
    (loop :for a :in args :do (find-live a live))
    (values)))

(defmethod find-live ((o ssad-var) live)
  (with-slots (binding) o
    (setf (gethash binding live) t)))

(defmethod find-live ((o symbol) live))
(defmethod find-live ((o ssad-constant) live))
(defmethod find-live ((o ssad-constructed) live))
(defmethod find-live ((o ssad-read-varying) live))
(defmethod find-live ((o ssad-read-uniform) live))

;;------------------------------------------------------------

(defmethod remove-dead ((o ssad-let1) live cmp-ctx)
  (with-slots (bindings body-form type) o
    (let ((new-bindings (remove-if-not (lambda (b) (gethash b live))
                                       bindings)))
      (when (/= (length bindings) (length new-bindings))
        (mark-changed cmp-ctx))
      (setf bindings new-bindings))
    (map nil (lambda (b) (remove-dead (slot-value b 'form) live cmp-ctx))
         bindings)
    (remove-dead body-form live cmp-ctx)
    (values)))

(defmethod remove-dead ((o ssad-lambda) live cmp-ctx)
  (with-slots (body-form) o
    (remove-dead body-form live cmp-ctx)
    (values)))

(defmethod remove-dead ((o ssad-if) live cmp-ctx)
  (with-slots (test then else) o
    (remove-dead test live cmp-ctx)
    (remove-dead then live cmp-ctx)
    (remove-dead else live cmp-ctx)
    (values)))

(defmethod remove-dead ((o ssad-funcall) live cmp-ctx)
  (with-slots (func args) o
    (remove-dead func live cmp-ctx)
    (map nil (lambda (a) (remove-dead a live cmp-ctx)) args)
    (values)))

(defmethod remove-dead ((o ssad-output) live cmp-ctx)
  (with-slots (args) o
    (map nil (lambda (a) (remove-dead a live cmp-ctx)) args)
    (values)))

(defmethod remove-dead ((o ssad-var) live cmp-ctx) (values))
(defmethod remove-dead ((o symbol) live cmp-ctx) (values))
(defmethod remove-dead ((o ssad-constant) live cmp-ctx) (values))
(defmethod remove-dead ((o ssad-constructed) live cmp-ctx) (values))
(defmethod remove-dead ((o ssad-read-varying) live cmp-ctx) (values))
(defmethod remove-dead ((o ssad-read-uniform) live cmp-ctx) (values))

;;------------------------------------------------------------
