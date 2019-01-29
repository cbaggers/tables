(in-package :tables.compile.stage-0.early-constant-folding)

;; early local constant folding
;;
;; This pass is dirt simple as it's only goal is to remove some
;; cruft from the previous pass. It's main advantage is to us as
;; the reader as it makes the ssa'd code easier to read.
;;
;; As a side effect it removes unused constant bindings

(defun run-pass (ssad-let)
  (cfold ssad-let nil))

(defmethod cfold ((o ssad-let1) to-replace)
  ;;
  (labels ((fold (accum binding)
             (destructuring-bind (to-fold new-bindings) accum
               (with-slots (name form type) binding
                 (if (foldable-constant-p form)
                     (list (acons name form to-fold)
                           new-bindings)
                     (let ((match (assocr form to-fold)))
                       (if match
                           (list (acons name match to-fold)
                                 new-bindings)
                           (list (acons name name to-fold)
                                 (cons (make-instance
                                        'ssad-binding
                                        :name name
                                        :form (cfold form to-fold)
                                        :type type)
                                       new-bindings)))))))))
    ;;
    (with-slots (bindings body-form type) o
      (destructuring-bind (to-fold new-bindings-reversed)
          (reduce #'fold bindings :initial-value (list to-replace nil))
        (make-instance 'ssad-let1
                       :bindings (reverse new-bindings-reversed)
                       :body-form (cfold body-form to-fold)
                       :type type)))))

(defmethod cfold ((o ssad-lambda) to-replace)
  (with-slots (args body-form result-type) o
    (let ((to-replace (reduce (lambda (a x)
                                (let ((arg-name (first x)))
                                  (acons arg-name arg-name a)))
                              args
                              :initial-value to-replace)))
      (make-instance 'ssad-lambda
                     :args args
                     :body-form (cfold body-form to-replace)
                     :result-type result-type))))

(defmethod cfold ((o ssad-if) to-replace)
  (with-slots (test then else) o
    (make-instance 'ssad-if
                   :test (cfold test to-replace)
                   :then (cfold then to-replace)
                   :else (cfold else to-replace))))

(defmethod cfold ((o ssad-funcall) to-replace)
  (with-slots (func args) o
    (make-instance 'ssad-funcall
                   :func (cfold func to-replace)
                   :args (mapcar (lambda (a) (cfold a to-replace))
                                 args))))

(defmethod cfold ((o symbol) to-replace)
  (or (assocr o to-replace) o))

(defmethod cfold ((o number) to-replace)
  o)

(defun foldable-constant-p (constant)
  (or (numberp constant)
      (eq constant t)
      (eq constant nil)))
