(in-package :tables.compile.stage-0.subexpression-elim)

;; description

(defun run-pass (ssad-let cmp-ctx)
  (s-elim ssad-let nil cmp-ctx)
  (values))

(defmethod s-elim ((o ssad-let1) env cmp-ctx)
  ;; returns a list of keys
  (with-slots (bindings body-form) o
    (let* ((keys
            (loop
               :for b :in bindings
               :for key :=
                 (multiple-value-bind (new-pairs key)
                     (s-elim-binding b env cmp-ctx)
                   (when new-pairs
                     (setf env (append new-pairs env))
                     key))
               :collect key))
           (new-bindings
            (loop :for binding :in bindings
               :append (multiple-value-bind (ikeys new-bindings)
                           (s-elim (slot-value binding 'form) env cmp-ctx)
                         (declare (ignore ikeys))
                         (remove nil new-bindings))
               :collect binding)))
      (when (/= (length new-bindings) (length bindings))
        (mark-changed cmp-ctx))
      (setf bindings new-bindings)
      (s-elim body-form env cmp-ctx)
      (values keys nil))))

(defun form-key (binding-name form)
  (typecase form
    (ssad-funcall
     (labels ((arg-key (arg)
                (etypecase arg
                  (ssad-var (slot-value (slot-value arg 'binding) 'name))
                  (ssad-constant (slot-value arg 'form)))))
       (with-slots (func args) form
         (when (typep func 'ssad-constant)
           (let* ((func-name
                   (second (slot-value func 'form)))
                  (func-info
                   (gethash func-name *registered-top-level-functions*)))
             (values
              `(funcall ,func-name ,@(mapcar #'arg-key args))
              ;; if it's a record constructor then the values in the
              ;; slots are the arguments to the function
              (when func-info
                (loop
                   :for s :in (record-ctor-slots func-info)
                   :for a :in args
                   :for elim := (typecase a
                                  (ssad-var (slot-value a 'binding))
                                  (ssad-constant a))
                   :when elim
                   :collect (cons `(funcall ,s ,binding-name)
                                  elim)))))))))))

(defun s-elim-binding (o env cmp-ctx)
  (with-slots (form name) o
    (multiple-value-bind (key record-accessor-pairs)
        (form-key name form)
      (if key
          (let ((existing-form (assocr key env :test #'equal)))
            (if existing-form
                (progn
                  (mark-changed cmp-ctx)
                  (etypecase existing-form
                    (ssad-binding
                     (setf form (make-instance
                                 'ssad-var :binding existing-form))
                     (values nil key))
                    (ssad-constant
                     (setf form existing-form)
                     (values nil key))))
                (values (cons (cons key o) record-accessor-pairs)
                        key)))
          (values nil key)))))

(defmethod s-elim ((o ssad-lambda) env cmp-ctx)
  (with-slots (body-form) o
    (s-elim body-form env cmp-ctx)
    nil))

(defmethod s-elim ((o ssad-if) env cmp-ctx)
  ;; returns bindings to inject into parent
  (with-slots (test then else) o
    (s-elim test env cmp-ctx)
    (let* ((then-keys (s-elim then env cmp-ctx))
           (else-keys (s-elim else env cmp-ctx)))
      (values
       nil
       (loop
          :for tpos :from 0
          :for tkey :in then-keys
          :for tbinding :in (slot-value then 'bindings)
          :for ebindings := (slot-value else 'bindings)
          :for epos := (when tkey (position tkey else-keys :test #'equal))
          :when epos
          :collect
            (let* ((tform (slot-value tbinding 'form))
                   (ttype (slot-value tbinding 'type))
                   (tunif (slot-value tbinding 'is-uniform))
                   (new-binding
                    (make-instance 'ssad-binding
                                   :name (gensym)
                                   :form tform
                                   :type ttype
                                   :is-uniform tunif))
                   (ebinding (nth epos ebindings)))
              (mark-changed cmp-ctx)
              (setf (slot-value tbinding 'form)
                    (make-instance 'ssad-var :binding new-binding))
              (setf (slot-value ebinding 'form)
                    (make-instance 'ssad-var :binding new-binding))
              new-binding))))))

(defmethod s-elim ((o ssad-funcall) env cmp-ctx) nil)
(defmethod s-elim ((o ssad-output) env cmp-ctx) nil)
(defmethod s-elim ((o ssad-var) env cmp-ctx) nil)
(defmethod s-elim ((o symbol) env cmp-ctx) nil)
(defmethod s-elim ((o ssad-constant) env cmp-ctx) nil)
(defmethod s-elim ((o ssad-constructed) env cmp-ctx) nil)
(defmethod s-elim ((o ssad-read-varying) env cmp-ctx) nil)
(defmethod s-elim ((o ssad-read-uniform) env cmp-ctx) nil)
