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

(defun find-slot (type-ref name)
  (let* ((aggregate-info (ttype-aggregate-info type-ref)))
    (with-slots (slots) aggregate-info
      (let* ((offset 0)
             (slot
              (loop
                 :for slot :in slots
                 :for slot-name := (slot-value slot 'name)
                 :for slot-type := (slot-value slot 'type)
                 :if (string= name slot-name)
                 :return slot
                 :else
                 :do (let* ((l-type (ttype->cffi-type slot-type))
                            (size (cffi:foreign-type-size l-type)))
                       (incf offset size)
                       nil))))
        (when (null slot)
          (break "dang? ~a ~a" aggregate-info slots))
        (values slot offset)))))

(defmethod simple-emit ((o ssad-slot-value) backend output-varyings)
  (with-slots (form name) o
    (assert (typep form 'ssad-var))
    (let* ((record-type (slot-value (slot-value form 'binding) 'type)))
      (multiple-value-bind (slot offset) (find-slot record-type name)
        (with-slots (type) slot
          (multiple-value-bind (read-emitter)
              (find-value-rw-emitters (checkmate:ttype-of type) backend)
            (funcall read-emitter
                     (simple-emit form backend output-varyings)
                     offset)))))))

(defmethod simple-emit ((o ssad-output) backend output-varyings)
  (with-slots (names args) o
    `(progn
       ,@(loop
            :for name :in names
            :for arg :in args
            :collect (emit-write-varying
                      name arg backend output-varyings))
       (values))))

(defun emit-write-varying (name node backend output-varyings)
  (check-type node ssad-write-varying)
  (let ((destination (rest (find name output-varyings
                                 :key #'first
                                 :test #'string=))))
    (assert destination ()
            "Could not find a valid destination for varying ~s~%~s"
            name output-varyings)
    (destructuring-bind (dest-name dest-type) destination
      (with-slots ((node-name name) form slot-id) node
        (if slot-id
            (multiple-value-bind (slot offset) (find-slot dest-type slot-id)
              (multiple-value-bind (read-emitter write-emitter)
                  (find-value-rw-emitters
                   (checkmate:ttype-of (slot-value slot 'type)) backend)
                (declare (ignore read-emitter))
                (funcall write-emitter
                         dest-name
                         offset
                         (simple-emit form backend output-varyings))))
            (let ((var-type
                   (etypecase form
                     (ssad-constant
                      (slot-value form 'type))
                     (ssad-var
                      (slot-value (slot-value form 'binding) 'type)))))
              (multiple-value-bind (read-emitter write-emitter)
                  (find-value-rw-emitters
                   (checkmate:ttype-of var-type) backend)
                (declare (ignore read-emitter))
                (funcall write-emitter
                         dest-name
                         0
                         (simple-emit form backend output-varyings)))))))))

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
          (funcall read-emitter name 0)))))

(defmethod simple-emit ((o ssad-read-uniform) backend output-varyings)
  (with-slots (type name) o
    ;; {TODO} type decl here
    name))

(defmethod simple-emit ((o symbol) backend output-varyings)
  (error "unprocessed symbol in emit stream: ~a" o))

;;------------------------------------------------------------
