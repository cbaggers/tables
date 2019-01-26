(in-package :tables-lang)

;;------------------------------------------------------------
;; Acronyms
;;
;; ssad <- ssa'd <- single statically assigned <- single static assignment'ed
;;
;;------------------------------------------------------------

(defclass blockify-context ()
  ((parent :initarg :parent)
   (bindings :initform nil :initarg :bindings)
   (functions :initform nil :initarg :functions)))

(defun make-blockify-context (parent new-bindings new-functions)
  (let ((res (make-instance
              'blockify-context
              :parent parent
              :bindings (when parent
                          (slot-value parent 'bindings))
              :functions (when parent
                           (slot-value parent 'functions)))))
    (with-slots (bindings) res
      (loop
         :for binding :in new-bindings
         :do (setf bindings (cons binding bindings))))
    (with-slots (functions) res
      (loop
         :for function :in new-functions
         :do (setf functions (cons function functions))))
    res))

(defun test ()
  (let ((res (infer (make-check-context 'tables)
                    `(funcall (lambda ((a ?a) (i integer))
                                (let ((b a))
                                  (if b
                                      i
                                      20)))
                              t
                              10))))
    (print res)
    (let* ((context (make-blockify-context nil nil nil))
           (lets (blockify context res))
           (last (first (last lets))))
      `(let* ,lets
         (truly-the ,(second (second last)) ,(first last))))))

(defun blockify (context ast)
  (assert (eq (first ast) 'truly-the))
  (multiple-value-bind (ssad-expr prior-lets)
      (blockify-form context (third ast))
    (let ((expr-name (gensym)))
      (append
       prior-lets
       `((,expr-name (truly-the ,(second ast) ,ssad-expr)))))))

(defun blockify-form (context form)
  (typecase form
    (list
     (case (first form)
       (lambda (blockify-lambda-form context form))
       (if (blockify-if-form context form))
       (let (blockify-let-form context form))
       (progn (blockify-progn-form context form))
       (funcall (blockify-funcall-form context form))
       (otherwise (error "not sure what to do with ~s" (first form)))))
    (symbol
     (if (or (eq form t) (null form))
         form
         (blockify-var-access context form)))
    (otherwise
     form)))

(defun blockify-var-access (context symbol)
  (or (cdr (assoc symbol (slot-value context 'bindings)))
      (error "bug: ~s" symbol)))

(defun gensym-named (name)
  (gensym (format nil "~a_" name)))

(defun blockify-if-form (context form)
  (let* ((test (blockify context (second form)))
         (then (blockify context (third form)))
         (then-last (first (last then)))
         (else (blockify context (fourth form)))
         (else-last (first (last else))))
    (values `(if ,(first (last test))
                 (let* ,then
                   (truly-the ,(second (second then-last))
                              ,(first then-last)))
                 (let* ,else
                   (truly-the ,(second (second else-last))
                              ,(first else-last))))
            (butlast test))))

(defun blockify-let-form (context form)
  ;; note: this function is for let, not let*
  (let* ((renamed-args (loop
                          :for (name val) :in (second form)
                          :collect (list (gensym-named (symbol-name name))
                                         val)))
         (decls (loop
                   :for (name val) :in renamed-args

                   :for blocked := (blockify context val)
                   :for last := (first (last blocked))
                   :for ssad-name := (first last)
                   :append blocked
                   :collect (list name `(truly-the ,(second (second last))
                                                   ,ssad-name))))
         (context (make-blockify-context
                   context
                   (loop
                      :for (old-name) :in (second form)
                      :for (new-name) :in renamed-args
                      :collect (cons old-name new-name))
                   nil))
         (body (third form))
         (lets (blockify context body))
         (ssad-name (first (first (last lets)))))
    (values ssad-name
            (append decls lets))))

(defun blockify-progn-form (context form)
  (let* ((lets (loop :for x :in (rest form)
                  :append (blockify context x)))
         (last (first lets)))
    (values (first last)
            lets)))

(defun blockify-lambda-form (context form)
  (let* ((renamed-args (loop
                          :for (name type) :in (second form)
                          :collect (list (gensym-named (symbol-name name))
                                         type)))
         (context (make-blockify-context
                   context
                   (loop
                      :for (old-name) :in (second form)
                      :for (new-name) :in renamed-args
                      :collect (cons old-name new-name))
                   nil))
         (body (third form))
         (lets (blockify context body))
         (ssad-name (first (first (last lets)))))
    (values `(lambda ,renamed-args
               (let* ,lets
                 (truly-the ,(second body) ,ssad-name)))
            nil)))

(defun blockify-funcall-form (context expr-ast)
  (destructuring-bind (ssad-names prior-lets)
      (loop
         :for arg :in (rest expr-ast)
         :for blocked-arg := (blockify context arg)
         :for ssad-name := (first (first (last blocked-arg)))
         :collect ssad-name :into names
         :append blocked-arg :into lets
         :finally (return (list names lets)))
    (values
     (cons (first expr-ast) ssad-names)
     prior-lets)))

;; (let* ((#:g1172
;;         (truly-the #t(function (boolean) boolean)
;;                    (lambda ((#:a_1162 #tboolean))
;;                      (let* ((#:g1164 (truly-the #tboolean #:a_1162))
;;                             (#:b_1163 #:g1164)
;;                             (#:g1168
;;                              (truly-the #tboolean
;;                                         (if (#:g1165 (truly-the #tboolean #:b_1163))
;;                                             (let* ((#:g1166 (truly-the #tboolean #:b_1163)))
;;                                               (truly-the #tboolean #:g1166))
;;                                             (let* ((#:g1167 (truly-the #tboolean #:b_1163)))
;;                                               (truly-the #tboolean #:g1167)))))
;;                             (#:g1169 (truly-the #tboolean #:g1168))
;;                             (#:g1170 (truly-the #tboolean #:g1169))
;;                             (#:g1171 (truly-the #tboolean #:g1164)))
;;                        (truly-the #tboolean #:g1171)))))
;;        (#:g1173 (truly-the #tboolean t))
;;        (#:g1174 (truly-the #tboolean (funcall #:g1172 #:g1173))))
;;   (truly-the #tboolean #:g1174))

;; (let* ((#:g1173 (truly-the #tboolean t))
;;        (#:a_1162 #:g1173)
;;        (#:g1164 (truly-the #tboolean #:a_1162))
;;        (#:b_1163 #:g1164)
;;        (#:g1168
;;         (truly-the #tboolean
;;                    (if (#:g1165 (truly-the #tboolean #:b_1163))
;;                        (let* ((#:g1166 (truly-the #tboolean #:b_1163)))
;;                          (truly-the #tboolean #:g1166))
;;                        (let* ((#:g1167 (truly-the #tboolean #:b_1163)))
;;                          (truly-the #tboolean #:g1167)))))
;;        (#:g1169 (truly-the #tboolean #:g1168))
;;        (#:g1170 (truly-the #tboolean #:g1169))
;;        (#:g1171 (truly-the #tboolean #:g1164))
;;        (#:g1174 (truly-the #tboolean #:g1171)))
;;   (truly-the #tboolean #:g1174))

;; (labels ((a ()
;;            (let* ((#:g1173 (truly-the #tboolean t))
;;                   (#:a_1162 #:g1173)
;;                   (#:g1164 (truly-the #tboolean #:a_1162))
;;                   (#:b_1163 #:g1164))
;;              (if (truly-the #tboolean #:b_1163)
;;                  (b #:b_1163)
;;                  (c #:b_1163))))
;;          (b ((#:g1166 #tboolean))
;;            (d (truly-the #tboolean #:g1166)))
;;          (c ((#:g1167 #tboolean))
;;            (d (truly-the #tboolean #:g1167)))
;;          (d (#:g1168 #tboolean)
;;            (let* ((#:g1169 (truly-the #tboolean #:g1168))
;;                   (#:g1170 (truly-the #tboolean #:g1169))
;;                   (#:g1171 (truly-the #tboolean #:g1164))
;;                   (#:g1174 (truly-the #tboolean #:g1171)))
;;              (truly-the #tboolean #:g1174))))
;;   (a))

;; (labels ((a ()
;;            (let* ((#:b_1163 (truly-the #tboolean t)))
;;              (if #:b_1163
;;                  (b #:b_1163)
;;                  (c #:b_1163))))
;;          (b ((#:g1166 #tboolean))
;;            (d (truly-the #tboolean #:g1166)))
;;          (c ((#:g1167 #tboolean))
;;            (d (truly-the #tboolean #:g1167)))
;;          (d (#:g1168 #tboolean)
;;            (let* ((#:g1174 :g1168))
;;              (truly-the #tboolean #:g1174))))
;;   (a))

;; (labels ((a ()
;;            (if (truly-the #tboolean t)
;;                (b (truly-the #tboolean t))
;;                (c (truly-the #tboolean t))))
;;          (b ((#:g1166 #tboolean))
;;            (d (truly-the #tboolean #:g1166)))
;;          (c ((#:g1167 #tboolean))
;;            (d (truly-the #tboolean #:g1167)))
;;          (d (#:g1168 #tboolean)
;;            (truly-the #tboolean #:g1168)))
;;   (a))

;; (labels ((a ()
;;            (b (truly-the #tboolean t)))
;;          (b ((#:g1166 #tboolean))
;;            (d (truly-the #tboolean #:g1166)))
;;          (c ((#:g1167 #tboolean))
;;            (d (truly-the #tboolean #:g1167)))
;;          (d (#:g1168 #tboolean)
;;            (truly-the #tboolean #:g1168)))
;;   (a))

;; (labels ((b ((#:g1166 #tboolean))
;;            (d (truly-the #tboolean #:g1166)))
;;          (d (#:g1168 #tboolean)
;;            (truly-the #tboolean #:g1168)))
;;   (b (truly-the #tboolean t)))

;; (labels ((d (#:g1168 #tboolean)
;;            (truly-the #tboolean #:g1168)))
;;   (d (truly-the #tboolean t)))

;; (labels ()
;;   (truly-the #tboolean t))
