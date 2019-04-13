(in-package :tables.compile.stage-0.inline-conditional-constants)

;; if a function call has args which are 'if's which have the same
;; test and constant branches then move the if around the function call

(defun run-pass (ssad-let)
  (inline-cond-const ssad-let nil))

(defmethod inline-cond-const ((o ssad-let1) ftype)
  (declare (ignore ftype))
  (with-slots (bindings body-form type) o
    (setf bindings
          (loop
             :for binding :in bindings
             :for type := (slot-value binding 'type)
             :for old-form := (slot-value binding 'form)
             :for new-form := (inline-cond-const old-form type)
             :do (setf (slot-value binding 'form) new-form)
             :if (typep old-form 'ssad-let1)
             :append (progn
                       (setf (slot-value binding 'form)
                             (slot-value new-form 'body-form))
                       (append (slot-value new-form 'bindings)
                               (list binding)))
             :else
             :collect binding))
    (setf body-form (inline-cond-const body-form type))
    o))

(defmethod inline-cond-const ((o ssad-funcall) return-type)
  (declare (optimize debug))
  (with-slots (func args) o
    (if (typep func 'ssad-constant)
        (labels ((test-binding (ir-if)
                   (slot-value
                    (slot-value ir-if 'test)
                    'binding))
                 (test-matches-binding (ir-if test-to-match)
                   (and (eq test-to-match (test-binding ir-if))
                        (typep (slot-value
                                (slot-value ir-if 'then)
                                'body-form)
                               'ssad-constant)
                        (typep (slot-value
                                (slot-value ir-if 'else)
                                'body-form)
                               'ssad-constant)))
                 (constant-if (ir)
                   (when (typep ir 'ssad-var)
                     (let ((form
                            (slot-value
                             (slot-value ir 'binding)
                             'form)))
                       (when (typep form 'ssad-if)
                         form))))
                 (inject (args procd-args slot)
                   (let* ((fc (make-instance
                               'ssad-funcall
                               :func func
                               :args (loop
                                        :for a :in args
                                        :for p :in procd-args
                                        :collect
                                          (if p
                                              (slot-value
                                               (slot-value p slot)
                                               'body-form)
                                              a))))
                          (b (make-instance
                              'ssad-binding
                              :name (gensym)
                              :form fc
                              :type return-type)))
                     (make-instance
                      'ssad-let1
                      :bindings (list b)
                      :body-form (make-instance 'ssad-var :binding b)
                      :type return-type))))
          (with-slots (func args) o
            (let* ((procd-args (mapcar #'constant-if args))
                   (cond-arg-forms (remove nil procd-args))
                   (first-if-form (first cond-arg-forms)))
              (if (and cond-arg-forms
                       (let ((first-test (test-binding first-if-form)))
                         (every (lambda (x)
                                  (test-matches-binding x first-test))
                                cond-arg-forms)))
                  (let ((test (make-instance
                               'ssad-var
                               :binding (test-binding first-if-form))))
                    (make-instance
                     'ssad-if
                     :test test
                     :then (inject args procd-args 'then)
                     :else (inject args procd-args 'else)))
                  o))))
        o)))

(defmethod inline-cond-const ((o ssad-lambda) type)
  (with-slots (body-form result-type) o
    (setf body-form (inline-cond-const body-form result-type))
    o))

(defmethod inline-cond-const ((o ssad-if) type)
  (declare (ignore type))
  (with-slots (test then else) o
    (setf test (inline-cond-const test nil))
    (setf then (inline-cond-const then nil))
    (setf else (inline-cond-const else nil))
    o))

(defmethod inline-cond-const ((o ssad-var) type)
  (declare (ignore type))
  o)

(defmethod inline-cond-const ((o symbol) type)
  (declare (ignore type))
  o)

(defmethod inline-cond-const ((o ssad-constant) type)
  (declare (ignore type))
  o)

(defmethod inline-cond-const ((o ssad-constructed) type)
  (declare (ignore type))
  o)
