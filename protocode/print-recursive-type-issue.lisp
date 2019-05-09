
#+nil
;; BUG: infinite loop in print (well designator-from-type really)
;;      as the type is properly recursive! The are type is the
;;      function type. I love that the checker was happy to do this
;;      it's rather mind bending.
;;      I guess we need a *print-circle* for types XD. or actually..
;;      we just need to make the designator circular, and punt to
;;      CL for the printing shiz.
(infer 'tables
       '(lambda ((f (function ((function (?a) ?b)) ?b)))
          (funcall f f)))
