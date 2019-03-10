(in-package :tables.compile.stage-0.inline-top-level-functions)

;;------------------------------------------------------------

(defun run-pass (ssad-let)
  (let* ((ht (make-hash-table)))
    (with-slots (bindings)
        (find-top-level-funcs ssad-let ht)
      (setf bindings
            (append (alexandria:hash-table-values ht)
                    bindings)))
    ssad-let))

;;------------------------------------------------------------

(defmethod find-top-level-funcs ((o ssad-let1) ht)
  (with-slots (bindings body-form) o
    (setf bindings
          (loop
             :for binding :in bindings
             :collect
               (with-slots (form) binding
                 (setf form (find-top-level-funcs form ht))
                 binding)))
    (setf body-form (find-top-level-funcs body-form ht))
    o))

(defmethod find-top-level-funcs ((o ssad-funcall) ht)
  (with-slots (func args) o
    (setf func (find-top-level-funcs func ht)
          args (mapcar (lambda (arg) (find-top-level-funcs arg ht))
                       args))
    o))

(defmethod find-top-level-funcs ((o ssad-lambda) ht)
  (with-slots (body-form) o
    (setf body-form (find-top-level-funcs body-form ht))
    o))

(defmethod find-top-level-funcs ((o ssad-if) ht)
  (with-slots (test then else) o
    (setf test (find-top-level-funcs test ht)
          then (find-top-level-funcs then ht)
          else (find-top-level-funcs else ht))
    o))

(defmethod find-top-level-funcs ((o ssad-constant) ht)
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
                             ht))
                           (binding
                            (first
                             (slot-value
                              new-ssad-let
                              'tables.compile.stage-0:bindings))))
                      (setf (gethash name ht) binding)
                      (make-instance 'ssad-var :binding binding))
                    o))))
        o)))

(defmethod find-top-level-funcs ((o ssad-var) ht)
  (declare (ignore ht))
  o)

(defmethod find-top-level-funcs ((o symbol) ht)
  (declare (ignore ht))
  o)

;;------------------------------------------------------------
