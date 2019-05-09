(in-package :tables.compile.stage-0.inline-top-level-functions)

;;------------------------------------------------------------

(defun run-pass (ssad-let cmp-ctx)
  (let* ((ht (make-hash-table)))
    (with-slots (bindings)
        (find-top-level-funcs ssad-let ht cmp-ctx)
      (setf bindings
            (append (alexandria:hash-table-values ht)
                    bindings)))
    (values)))

;;------------------------------------------------------------

(defmethod find-top-level-funcs ((o ssad-let1) ht cmp-ctx)
  (with-slots (bindings body-form) o
    (setf bindings
          (loop
             :for binding :in bindings
             :collect
               (with-slots (form) binding
                 (setf form (find-top-level-funcs form ht cmp-ctx))
                 binding)))
    (setf body-form (find-top-level-funcs body-form ht cmp-ctx))
    o))

(defmethod find-top-level-funcs ((o ssad-funcall) ht cmp-ctx)
  (with-slots (func args) o
    (setf func (find-top-level-funcs func ht cmp-ctx)
          args (mapcar (lambda (arg)
                         (find-top-level-funcs arg ht cmp-ctx))
                       args))
    o))

(defmethod find-top-level-funcs ((o ssad-output) ht cmp-ctx)
  (with-slots (args) o
    (setf args (mapcar (lambda (arg)
                         (find-top-level-funcs arg ht cmp-ctx))
                       args))
    o))

(defmethod find-top-level-funcs ((o ssad-lambda) ht cmp-ctx)
  (with-slots (body-form) o
    (setf body-form (find-top-level-funcs body-form ht cmp-ctx))
    o))

(defmethod find-top-level-funcs ((o ssad-if) ht cmp-ctx)
  (with-slots (test then else) o
    (setf test (find-top-level-funcs test ht cmp-ctx)
          then (find-top-level-funcs then ht cmp-ctx)
          else (find-top-level-funcs else ht cmp-ctx))
    o))

(defmethod find-top-level-funcs ((o ssad-constant) ht cmp-ctx)
  (with-slots (form) o
    (if (and (listp form) (eq (first form) 'function))
        (let* ((full-name (second form))
               (name
                (if (atom full-name)
                    full-name
                    (first full-name)))
               (seen-binding (gethash name ht)))
          (if seen-binding
              (make-instance 'ssad-var :binding seen-binding)
              (let* ((info
                      (gethash name tables.lang::*registered-top-level-functions*))
                     (ast
                      (slot-value info 'tables.lang::ast)))
                (if ast
                    (let* ((new-ssad-let
                            (find-top-level-funcs
                             (tables.compile.stage-0.ast-to-ir:run-pass
                              ast)
                             ht
                             cmp-ctx))
                           (binding
                            (first
                             (slot-value
                              new-ssad-let
                              'tables.compile.stage-0:bindings))))
                      (mark-changed cmp-ctx)
                      (setf (gethash name ht) binding)
                      (make-instance 'ssad-var :binding binding))
                    o))))
        o)))

(defmethod find-top-level-funcs ((o ssad-var) ht cmp-ctx)
  (declare (ignore ht))
  o)

(defmethod find-top-level-funcs ((o symbol) ht cmp-ctx)
  (declare (ignore ht))
  o)

(defmethod find-top-level-funcs ((o ssad-constructed) ht cmp-ctx)
  (declare (ignore ht))
  o)

(defmethod find-top-level-funcs ((o ssad-read-col) ht cmp-ctx)
  (declare (ignore ht))
  o)

;;------------------------------------------------------------
