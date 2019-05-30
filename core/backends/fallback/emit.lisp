(in-package :tables.backends.fallback)

;;------------------------------------------------------------

;; - if we knew which columns could be mutate here we could avoid extra
;;   arguments to the function
;; - we need a type to use for lisp type-decls. We can attach to the
;;   backend for now but I'm not sure if there is any use for other
;;   backends. I guess we can say which backends use them. Yeah, do this
;;   for now. The best solution will reveal itself in time.

(defun emit (subquery)
  (with-slots (ir varying-args uniform-args) subquery
    (let* ((backend (find-backend 'fallback))
           (body (simple-emit ir backend)))
      `(lambda ,(append varying-args uniform-args)
         (declare (type cffi:foreign-pointer ,@varying-args))
         ,body))))

(defmethod simple-emit ((o ssad-let1) backend)
  (with-slots (bindings body-form) o
    (if bindings
        `(let* ,(mapcar (lambda (x) (simple-emit x backend)) bindings)
           ,(simple-emit body-form backend))
        (simple-emit body-form backend))))

(defmethod simple-emit ((o ssad-binding) backend)
  (with-slots (name form) o
    (list name (simple-emit form backend))))

(defmethod simple-emit ((o ssad-var) backend)
  (with-slots (binding) o
    ;; {TODO} type decl here
    (slot-value binding 'name)))

(defmethod simple-emit ((o ssad-lambda) backend)
  (with-slots (args body-form) o
    ;; {TODO} type decl here
    `(lambda ,(mapcar #'first args)
       ;; {TODO} type decl here
       ,(simple-emit body-form backend))))

(defmethod simple-emit ((o ssad-if) backend)
  (with-slots (test then else) o
    ;; {TODO} type decl here
    (list 'if
          (simple-emit test backend) ;; {TODO} type decl here
          (simple-emit then backend) ;; {TODO} type decl here
          (simple-emit else backend) ;; {TODO} type decl here
          )))

(defmethod simple-emit ((o ssad-funcall) backend)
  (with-slots (func args) o
    (if (typep func 'ssad-constant)
        (with-slots (form) func
          ;; form must be of the form (function op-name)
          (funcall (find-op-emitter-function (second form) backend)
                   (mapcar (lambda (x) (simple-emit x backend)) args)))
        `(funcall ,(simple-emit func backend)
                  ,@(mapcar (lambda (x) (simple-emit x backend)) args)))))

(defmethod simple-emit ((o ssad-output) backend)
  (with-slots (names args) o
    `(output ,@(loop
                  :for n :in names :for a :in args
                  :append (list n (simple-emit a backend))))))

(defmethod simple-emit ((o ssad-constant) backend)
  (with-slots (form) o
    ;; {TODO} type decl here
    form))

(defmethod simple-emit ((o ssad-constructed) backend)
  (with-slots (form) o
    ;; {TODO} type decl here
    form))

(defmethod simple-emit ((o ssad-read-varying) backend)
  (with-slots (type name) o
    ;; {TODO} type decl here
    (multiple-value-bind (read-emitter)
        (find-value-rw-emitters (checkmate:ttype-of type) backend)
      (funcall read-emitter name))))

(defmethod simple-emit ((o ssad-read-uniform) backend)
  (with-slots (type name) o
    ;; {TODO} type decl here
    name))

(defmethod simple-emit ((o symbol) backend)
  (error "unprocessed symbol in emit stream: ~a" o))

;;------------------------------------------------------------
