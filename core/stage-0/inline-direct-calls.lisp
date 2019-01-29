(in-package :tables.compile.stage-0.inline-direct-calls)

;; directly called function inlining

(defun run-pass (ssad-let)
  (inline-funcs ssad-let nil))

(defmethod inline-funcs ((o ssad-let1) func-bindings)
  ;;
  (labels ((inline (accum binding)
             (with-slots (name form) binding
               (destructuring-bind (func-bindings new-bindings) accum
                 (if (typep form 'ssad-lambda)
                     (list (acons name form func-bindings)
                           (cons binding new-bindings))
                     (with-slots ((bname name) (bform form) (btype type))
                         binding
                       (let* ((expanded (inline-funcs form func-bindings))
                              (is-let (typep expanded 'ssad-let1))
                              (ebindings (when is-let
                                           (reverse
                                            (slot-value expanded 'bindings))))
                              (eform (if is-let
                                         (slot-value expanded 'body-form)
                                         expanded)))
                         (list func-bindings
                               (cons (make-instance 'ssad-binding
                                                    :name bname
                                                    :form eform
                                                    :type btype)
                                     (append ebindings
                                             new-bindings))))))))))
    (with-slots (bindings body-form type) o
      (destructuring-bind (to-inline new-bindings-reversed)
          (reduce #'inline bindings :initial-value (list func-bindings nil))
        (make-instance 'ssad-let1
                       :bindings (reverse new-bindings-reversed)
                       :body-form (inline-funcs body-form to-inline)
                       :type type)))))

(defmethod inline-funcs ((o ssad-lambda) func-bindings)
  (with-slots (args body-form result-type) o
    (make-instance 'ssad-lambda
                   :args args
                   :body-form (inline-funcs body-form func-bindings)
                   :result-type result-type)))

(defmethod inline-funcs ((o ssad-if) func-bindings)
  (with-slots (test then else) o
    (make-instance 'ssad-if
                   :test (inline-funcs test func-bindings)
                   :then (inline-funcs then func-bindings)
                   :else (inline-funcs else func-bindings))))

(defmethod inline-funcs ((o ssad-funcall) func-bindings)
  (with-slots (func args) o
    (assert (symbolp func))
    (let ((match (assocr func func-bindings)))
      (if match
          (with-slots ((largs args) (lbody body-form) (ltype result-type))
              match
            (let* ((f-is-let (typep lbody 'ssad-let1))
                   (fbindings (when f-is-let (slot-value lbody 'bindings)))
                   (fbody (if f-is-let
                              (slot-value lbody 'body-form)
                              lbody)))
              (inline-funcs
               (make-instance
                'ssad-let1
                :bindings (append (mapcar (lambda (arg form)
                                            (let ((arg-name (first arg)))
                                              (make-instance 'ssad-binding
                                                             :name arg-name
                                                             :form form
                                                             :type '???)))
                                          largs
                                          args)
                                  fbindings)
                :body-form fbody
                :type ltype)
               func-bindings)))
          o))))

(defmethod inline-funcs ((o symbol) func-bindings)
  o)

(defmethod inline-funcs ((o number) func-bindings)
  o)


#||

Well this one is interesting. As functions are values we need to be able to
trace what goes where.

#(ssad-let1
  ((g649
    (ssad-lambda ((a_638 #tboolean) (i_639 #ti8))
     (ssad-let1
      ((g645 (ssad-if a_638 (ssad-let1 nil i_639) (ssad-let1 nil 20)))) g645)))
   (g654 (ssad-lambda ((a_650 #tboolean) (i_651 #ti8)) (ssad-let1 nil 10)))
   (g658 (ssad-if t (ssad-let1 nil g649) (ssad-let1 nil g654)))
   (g663 (ssad-funcall g658 t 10)))
  g663)

there is nothing wrong with the code but it does confound inlining.

We could make ssad-lambda a foldable-constant which gets rid of graph walking
when finding the function.. however that doesnt fix the big issue.

look like we will be addressing full calls sooner than expected

hmm if this is width 16 (or whatever) then we still have the divergence issue

man i'm tired :D

well you cant avoid the issue. it's the same as in shaders. So we have to trust
people wont do it. So do we migrate the if to the call site? For inline I guess
we'd have to. We do want to do the mask thing. Actually yeah that makes sense,
there is no branch and the test is already in a variable. This can work.

Now do we do it early or late in compile passes? In my mind it makes sense to
try and strip as much dead code as possible first.

I think this pass should just inline functions that are directly referenced
from a binding (no conditionals involved) and then go from there.

||#
