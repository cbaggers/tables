(in-package :tables.backends.fallback)

;;------------------------------------------------------------

;; - if we knew which columns could be mutate here we could avoid extra
;;   arguments to the function
;; - we need a type to use for lisp type-decls. We can attach to the
;;   backend for now but I'm not sure if there is any use for other
;;   backends. I guess we can say which backends use them. Yeah, do this
;;   for now. The best solution will reveal itself in time.

(defun emit (subquery optimize)
  (with-slots (ir uniform-args input-varyings output-varyings) subquery
    (let* ((backend (find-backend 'fallback))
           (varying-names-in (mapcar #'first input-varyings))
           (uniform-names (mapcar #'first uniform-args))
           (outputs (loop
                       :for (name type) :in output-varyings
                       :collect (list name
                                      (gensym (symbol-name name))
                                      type)))
           (varying-names-out (mapcar #'second outputs))
           (body (simple-emit ir backend outputs)))
      `(lambda ,(append varying-names-in varying-names-out uniform-names)
         (declare (optimize ,@optimize)
                  (type cffi:foreign-pointer
                        ,@varying-names-in
                        ,@varying-names-out)
                  ,@(loop
                       :for (name type) :in uniform-args
                       :collect `(type ,(ttype->lisp-type type) ,name)))
         ,body))))

(defmethod simple-emit ((o ssad-let1) backend output-varyings)
  (with-slots (bindings body-form) o
    (if bindings
        `(let* ,(mapcar
                 (lambda (x) (simple-emit x backend output-varyings))
                 bindings)
           ,(simple-emit body-form backend output-varyings))
        (simple-emit body-form backend output-varyings))))

(defmethod simple-emit ((o ssad-binding) backend output-varyings)
  (with-slots (name form) o
    (list name (simple-emit form backend output-varyings))))

(defmethod simple-emit ((o ssad-var) backend output-varyings)
  (with-slots (binding) o
    ;; {TODO} type decl here
    (slot-value binding 'name)))

(defmethod simple-emit ((o ssad-lambda) backend output-varyings)
  (with-slots (args body-form) o
    ;; {TODO} type decl here
    `(lambda ,(mapcar #'first args)
       ;; {TODO} type decl here
       ,(simple-emit body-form backend output-varyings))))

(defmethod simple-emit ((o ssad-if) backend output-varyings)
  (with-slots (test then else) o
    ;; {TODO} type decl here
    (list 'if
          (simple-emit test backend output-varyings) ;; {TODO} type decl here
          (simple-emit then backend output-varyings) ;; {TODO} type decl here
          (simple-emit else backend output-varyings) ;; {TODO} type decl here
          )))

(defmethod simple-emit ((o ssad-funcall) backend output-varyings)
  (with-slots (func args) o
    (if (typep func 'ssad-constant)
        (with-slots (form) func
          ;; form must be of the form (function op-name)
          (funcall (find-op-emitter-function (second form) backend)
                   (mapcar
                    (lambda (x) (simple-emit x backend output-varyings))
                    args)))
        `(funcall ,(simple-emit func backend output-varyings)
                  ,@(mapcar
                     (lambda (x) (simple-emit x backend output-varyings))
                     args)))))

(defmethod simple-emit ((o ssad-output) backend output-varyings)
  (with-slots (names args) o
    `(progn
       ,@(loop
            :for n :in names
            :for raw-a :in args
            :for a := (simple-emit raw-a backend output-varyings)
            :collect
              (let ((destination (rest (find n output-varyings
                                             :key #'first
                                             :test #'string=))))
                (assert destination ()
                        "Could not find a valid destination for varying ~s~%~s"
                        n output-varyings)
                (destructuring-bind (dest-name dest-type) destination
                  (multiple-value-bind (read-emitter write-emitter)
                      (find-value-rw-emitters
                       (checkmate:ttype-of dest-type) backend)
                    (declare (ignore read-emitter))
                    (funcall write-emitter dest-name a)))))
       (values))
    ))

(defmethod simple-emit ((o ssad-constant) backend output-varyings)
  (with-slots (form) o
    ;; {TODO} type decl here
    form))

(defmethod simple-emit ((o ssad-constructed) backend output-varyings)
  (with-slots (form) o
    ;; {TODO} type decl here
    form))

(defmethod simple-emit ((o ssad-read-varying) backend output-varyings)
  (with-slots (type name) o
    (if (record-type-p type)
        name
        ;; {TODO} type decl here
        (multiple-value-bind (read-emitter)
            (find-value-rw-emitters (checkmate:ttype-of type) backend)
          (funcall read-emitter name)))))

(defmethod simple-emit ((o ssad-read-uniform) backend output-varyings)
  (with-slots (type name) o
    ;; {TODO} type decl here
    name))

(defmethod simple-emit ((o symbol) backend output-varyings)
  (error "unprocessed symbol in emit stream: ~a" o))

;;------------------------------------------------------------
