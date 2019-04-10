(in-package :tables.compile.stage-0.inline-direct-calls)

;; directly called function inlining
;;
;; WARNING: Pass mutates graph
;;

(defun run-pass (ssad-let)
  (inline-funcs ssad-let))

(defmethod inline-funcs ((o ssad-let1))
  (with-slots (bindings body-form type) o
    (setf bindings
          (loop
             :for binding :in bindings
             :for old-form := (slot-value binding 'form)
             :for new-form := (inline-funcs old-form)
             :do (setf (slot-value binding 'form) new-form)
             :if (typep old-form 'ssad-let1)
             :append (progn
                       (setf (slot-value binding 'form)
                             (slot-value new-form 'body-form))
                       (append (slot-value new-form 'bindings)
                               (list binding)))
             :else
             :collect binding))
    (setf body-form (inline-funcs body-form))
    o))

(defmethod inline-funcs ((o ssad-funcall))
  (with-slots (func args) o
    (etypecase func
      (ssad-constant o)
      (ssad-var
       (let ((form (slot-value (slot-value func 'binding) 'form)))
         (if (typep form 'ssad-lambda)
             (with-slots ((largs args)
                          (lbody body-form)
                          (ltype result-type))
                 form
               (let* ((f-is-let
                       (typep lbody 'ssad-let1))
                      ;; {TODO} when is this ever NIL?
                      (fbindings
                       (when f-is-let (slot-value lbody 'bindings)))
                      (fbody
                       (if f-is-let
                           (slot-value lbody 'body-form)
                           lbody))
                      (new (make-instance
                            'ssad-let1
                            :bindings (append
                                       (mapcar (lambda (arg form)
                                                 (let ((arg-name (first arg))
                                                       (arg-type (second arg)))
                                                   (make-instance
                                                    'ssad-binding
                                                    :name arg-name
                                                    :form form
                                                    :type arg-type)))
                                               largs
                                               args)
                                       fbindings)
                            :body-form fbody
                            :type ltype)))
                 (patch-bindings new (make-hash-table))
                 (inline-funcs new)))
             o))))))

(defmethod inline-funcs ((o ssad-lambda))
  (with-slots (body-form) o
    (setf body-form (inline-funcs body-form))
    o))

(defmethod inline-funcs ((o ssad-if))
  (with-slots (test then else) o
    (setf test (inline-funcs test))
    (setf then (inline-funcs then))
    (setf else (inline-funcs else))
    o))

(defmethod inline-funcs ((o ssad-var)) o)
(defmethod inline-funcs ((o symbol)) o)
(defmethod inline-funcs ((o ssad-constant)) o)
(defmethod inline-funcs ((o ssad-constructed)) o)


(defmethod patch-bindings ((o ssad-let1) env)
  (with-slots (bindings body-form) o
    (loop
       :for binding :in bindings
       :do
         (with-slots (name form) binding
           (patch-bindings form env)
           (setf (gethash name env) binding)))
    (patch-bindings body-form env)
    (values)))

(defmethod patch-bindings ((o ssad-funcall) env)
  (with-slots (func args) o
    (patch-bindings func env)
    (loop :for a :in args :do (patch-bindings a env))
    (values)))

(defmethod patch-bindings ((o ssad-lambda) env)
  (with-slots (body-form) o
    (patch-bindings body-form env)
    (values)))

(defmethod patch-bindings ((o ssad-if) env)
  (with-slots (test then else) o
    (patch-bindings test env)
    (patch-bindings then env)
    (patch-bindings else env)
    (values)))

(defmethod patch-bindings ((o ssad-var) env)
  (with-slots (binding) o
    (let* ((name
            (slot-value binding 'name))
           (new-binding
            (or (gethash name env) binding)))
      (setf binding new-binding)
      (values))))

(defmethod patch-bindings ((o symbol) env) (values))
(defmethod patch-bindings ((o ssad-constant) env) (values))
(defmethod patch-bindings ((o ssad-constructed) env) (values))


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
