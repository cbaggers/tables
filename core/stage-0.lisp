(in-package :tables.compile.stage-0)
;; {TODO} rename this file to ir-types

;;
;; IR nodes for stage-0
;;
;;------------------------------------------------------------

(defclass ssad-let1 ()
  ((bindings :initarg :bindings :initform nil)
   (body-form :initarg :body-form :initform nil)
   (type :initarg :type)))

(defclass ssad-binding ()
  ((name :initarg :name)
   (form :initarg :form)
   (type :initarg :type)
   (is-uniform :initarg :is-uniform :initform nil)))

(defclass ssad-var ()
  ((binding :initarg :binding)))

(defclass ssad-lambda ()
  ((args :initarg :args)
   (body-form :initarg :body-form) ;; always a ssad-let1
   (result-type :initarg :result-type)
   (arg-bindings :initform nil)))

(defclass ssad-if ()
  ((test :initarg :test)
   (then :initarg :then)   ;; always a ssad-let1
   (else :initarg :else))) ;; always a ssad-let1

(defclass ssad-funcall ()
  ((func :initarg :func)
   (args :initarg :args)))

(defclass ssad-constant ()
  ((form :initarg :form)
   (type :initarg :type)))

(defclass ssad-constructed ()
  ((form :initarg :form)
   (type :initarg :type)))

;;------------------------------------------------------------

(defgeneric as-debug-form (o))

(defmethod as-debug-form ((o ssad-let1))
  (with-slots (bindings body-form) o
    `(ssad-let1 ,(mapcar #'as-debug-form bindings)
                ,(as-debug-form body-form))))

(defmethod as-debug-form ((o ssad-binding))
  (with-slots (name form) o
    (list name (as-debug-form form))))

(defmethod as-debug-form ((o ssad-var))
  (with-slots (binding) o
    (with-slots (name) binding
      (list :var name))))

(defmethod as-debug-form ((o ssad-lambda))
  (with-slots (args body-form) o
    (list 'ssad-lambda args
          (as-debug-form body-form))))

(defmethod as-debug-form ((o ssad-if))
  (with-slots (test then else) o
    (list 'ssad-if
          (as-debug-form test)
          (as-debug-form then)
          (as-debug-form else))))

(defmethod as-debug-form ((o ssad-funcall))
  (with-slots (func args) o
    `(ssad-funcall ,(as-debug-form func)
                   ,@(mapcar #'as-debug-form args))))

(defmethod as-debug-form ((o ssad-constant))
  (with-slots (form) o
    (list :constant form)))

(defmethod as-debug-form ((o symbol))
  o)

(defmethod as-debug-form ((o ssad-constructed))
  (with-slots (type form) o
    (list :construct type form)))

;;------------------------------------------------------------

(defmethod print-object ((o ssad-let1) stream)
  (format stream "#~a" (as-debug-form o)))

(defmethod print-object ((o ssad-binding) stream)
  (format stream "#~a" (as-debug-form o)))

(defmethod print-object ((o ssad-lambda) stream)
  (format stream "#~a" (as-debug-form o)))

(defmethod print-object ((o ssad-if) stream)
  (format stream "#~a" (as-debug-form o)))

(defmethod print-object ((o ssad-funcall) stream)
  (format stream "#~a" (as-debug-form o)))

;;------------------------------------------------------------

(defun var-eq (a b)
  (and (typep a 'ssad-var)
       (typep b 'ssad-var)
       (eq (slot-value a 'binding)
           (slot-value b 'binding))))

(defmacro match-ir-1 (form &body cases)
  ;; Patterns
  ;;
  ;; <some constant>
  ;; (:constant x)
  ;; (:form x)
  ;; (fn-name [:constant|:form]*)
  (let ((gform (gensym "form"))
        (gbinding (gensym "binding"))
        (ginner-form (gensym "inner-form"))
        (iargs (gensym "iargs")))
    (labels ((otherwise-case-p (case)
               (eq (first case) 'otherwise))

             (process-case-arg (arg arg-pattern)
               (destructuring-bind (pattern binding)
                   (alexandria:ensure-list arg-pattern)
                 (cond
                   ((numberp pattern)
                    (assert (null binding) ()
                            "Explicit constant subpatterns ins match-ir-1 cannot introduce bindings: ~a"
                            arg-pattern)
                    (list
                     `(and (typep ,arg 'ssad-constant)
                           (eql (slot-value ,arg 'form)
                                ,pattern))
                     (when binding
                       `(,binding
                         (slot-value ,arg 'form)))))
                   ((eq pattern :constant)
                    (list
                     `(typep ,arg 'ssad-constant)
                     (when binding
                       `(,binding
                         (slot-value ,arg 'form)))))
                   ((eq pattern :form)
                    (list
                     t
                     (when binding
                       `(,binding ,arg))))
                   (t (error "Invalid subpattern form match-ir-1: ~a"
                             arg-pattern)))))

             (process-case-common (case)
               (destructuring-bind (pattern . body) case
                 (cond
                   ((eq pattern :constant)
                    `((constantp ,gform) ,@body))
                   ((eq pattern :form)
                    `(t
                      ,@body))
                   ((and (listp pattern) (symbolp (first pattern)))
                    (let* ((arg-symbs
                            (loop
                               :for i :below (length (rest pattern))
                               :collect (gensym (format nil "arg~a" i))))
                           (test-bindings
                            (loop
                               :for a :in arg-symbs
                               :for i :from 0
                               :collect `(,a (elt ,iargs ,i))))
                           (pairs
                            (mapcar (lambda (a p) (process-case-arg a p))
                                    arg-symbs
                                    (rest pattern)))
                           (tests
                            (mapcar #'first pairs))
                           (bindings
                            (mapcar #'second pairs)))
                      `((and (typep ,gform 'ssad-var)
                             (let ((iform ,ginner-form))
                               (and (typep iform 'ssad-funcall)
                                    (typep (slot-value iform 'func)
                                           'ssad-constant)
                                    (eq (second
                                         (slot-value
                                          (slot-value iform 'func)
                                          'form))
                                        ',(first pattern))
                                    (let ((,iargs (slot-value iform 'args)))
                                      (and (= (length ,iargs) ,(length pairs))
                                           ,@(loop
                                                :for b :in test-bindings
                                                :for test :in tests
                                                :collect
                                                  (if (eq test t)
                                                      t
                                                      `(let (,b) ,test))))))))
                        (let* ((,iargs (slot-value ,ginner-form 'args))
                               ,@test-bindings
                               ,@bindings)
                          (declare (ignorable ,iargs ,@arg-symbs))
                          ,@body))))
                   (t (error "Invalid pattern for match-ir-1: ~a"
                             pattern)))))
             (process-case (case)
               (assert (not (otherwise-case-p case)))
               (process-case-common case))
             (process-last-case (case)
               (if (otherwise-case-p case)
                   `(t ,(second case))
                   (process-case-common case))))
      (let* ((cases-butlast
              (mapcar #'process-case (butlast cases)))
             (cases-last
              (process-last-case (last1 cases))))
        `(let* ((,gform ,form))
           (with-slots ((,gbinding binding)) ,gform
             (with-slots ((,ginner-form form)) ,gbinding
               (cond
                 ,@cases-butlast
                 ,cases-last))))))))

(defmacro match-ir* ((&rest forms) &body cases)
  ;; Patterns
  ;;
  ;; <some constant>
  ;; (:constant x)
  ;; (:form x)
  ;; (fn-name [:constant|:form]*)
  (let ((gforms (loop
                   :for i :below (length forms)
                   :collect (gensym (format nil "form~a" i))))
        (iargs-set
         (loop
            :for i :below (length forms)
            :collect (gensym (format nil "iargs~a" i)))))
    (labels ((otherwise-case-p (case-n)
               (eq (first case-n) 'otherwise))

             (process-case-arg (arg arg-pattern)
               (destructuring-bind (pattern binding)
                   (alexandria:ensure-list arg-pattern)
                 (cond
                   ((numberp pattern)
                    (assert (null binding) ()
                            "Explicit constant subpatterns ins match-ir-1 cannot introduce bindings: ~a"
                            arg-pattern)
                    (list
                     `(and (typep ,arg 'ssad-constant)
                           (eql (slot-value ,arg 'form)
                                ,pattern))
                     (when binding
                       `(,binding
                         (slot-value ,arg 'form)))))
                   ((eq pattern :constant)
                    (list
                     `(typep ,arg 'ssad-constant)
                     (when binding
                       `(,binding
                         (slot-value ,arg 'form)))))
                   ((eq pattern :form)
                    (list
                     t
                     (when binding
                       `(,binding ,arg))))
                   (t (error "Invalid subpattern form match-ir-1: ~a"
                             arg-pattern)))))

             (process-one-case (pattern gform ginner-form iargs)
               ;; returns a list containing
               ;; (the-test bindings ignorables)
               (cond
                 ((numberp pattern)
                  `((eql ,gform ,pattern) nil nil))
                 ((eq pattern :constant)
                  `((constantp ,gform) nil nil))
                 ((eq pattern :form)
                  `(t
                    nil
                    nil))
                 ((and (listp pattern) (eq (first pattern) :constant))
                  (assert (symbolp (second pattern)))
                  `((typep ,gform 'ssad-constant)
                    ((,(second pattern) (slot-value ,gform 'form)))
                    nil))
                 ((and (listp pattern) (eq (first pattern) :form))
                  (assert (symbolp (second pattern)))
                  `(t
                    ((,(second pattern) ,gform))
                    nil))
                 ((and (listp pattern) (symbolp (first pattern)))
                  (let* ((arg-symbs
                          (loop
                             :for i :below (length (rest pattern))
                             :collect (gensym (format nil "arg~a" i))))
                         (test-bindings
                          (loop
                             :for a :in arg-symbs
                             :for i :from 0
                             :collect `(,a (elt ,iargs ,i))))
                         (pairs
                          (mapcar (lambda (a p) (process-case-arg a p))
                                  arg-symbs
                                  (rest pattern)))
                         (tests
                          (mapcar #'first pairs))
                         (bindings
                          (mapcar #'second pairs)))
                    (list
                     `(and (typep ,gform 'ssad-var)
                           (let ((iform ,ginner-form))
                             (and (typep iform 'ssad-funcall)
                                  (typep (slot-value iform 'func)
                                         'ssad-constant)
                                  (eq (second
                                       (slot-value
                                        (slot-value iform 'func)
                                        'form))
                                      ',(first pattern))
                                  (let ((,iargs (slot-value iform 'args)))
                                    (and (= (length ,iargs) ,(length pairs))
                                         ,@(loop
                                              :for b :in test-bindings
                                              :for test :in tests
                                              :collect
                                                (if (eq test t)
                                                    t
                                                    `(let (,b) ,test))))))))
                     `((,iargs (slot-value ,ginner-form 'args))
                       ,@test-bindings
                       ,@bindings)
                     `(,iargs ,@arg-symbs))))
                 (t (error "Invalid pattern for match-ir-1: ~a"
                           pattern))))

             (process-case-common (case-n i)
               (destructuring-bind ((and . patterns) . body) case-n
                 (assert (eq and :>))
                 ;; with-slots ((,gbinding binding)) ,gform
                 ;; with-slots ((,ginner-form form)) ,gbinding
                 (loop
                    :for j :from 0
                    :for pattern :in patterns
                    :for gform := (elt gforms j)
                    :for inner-form :=
                      `(slot-value (slot-value ,gform 'binding) 'form)
                    :for (test bindings ignorables)
                    := (process-one-case pattern
                                         gform
                                         inner-form
                                         (elt iargs-set i))
                    :collect test :into all-tests
                    :append bindings :into all-bindings
                    :append ignorables :into all-ignorables
                    :finally
                      (return
                        `((and ,@all-tests)
                          (let* ,all-bindings
                            (declare (ignorable ,@all-ignorables))
                            ,@body))))))

             (process-case (case-n i)
               (assert (not (otherwise-case-p case-n)))
               (process-case-common case-n i))
             (process-last-case (case-n i)
               (if (otherwise-case-p case-n)
                   `(t ,(second case-n))
                   (process-case-common case-n i))))
      (let* ((cases-butlast
              (loop
                 :for case-n :in (butlast cases)
                 :for i :from 0
                 :collect (process-case case-n i)))
             (cases-last
              (process-last-case (last1 cases) (- (length cases) 1))))
        `(let* ,(loop
                   :for gform :in gforms
                   :for form :in forms
                   :collect (list gform form))
           (cond
             ,@cases-butlast
             ,cases-last))))))

;;------------------------------------------------------------
