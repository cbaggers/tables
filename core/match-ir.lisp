(in-package :tables.compile.stage-0)

;;------------------------------------------------------------

(declaim (inline ir-constant-p))
(defun ir-constant-p (x)
  (or (numberp x)
      (eq x t)
      (null x)
      (typep x 'ssad-constant)))

(declaim (inline ir-expr-p))
(defun ir-expr-p (x)
  (typep x 'ir-node))

(defun ir-funcall-of-p (func-name arg-count x)
  (and (typep x 'ssad-var)
       (with-slots (form) (slot-value x 'binding)
         (and (typep form 'ssad-funcall)
              (with-slots (func args) form
                (and (typep func 'ssad-constant)
                     (eq (second (slot-value func 'form)) func-name)
                     (= (length args) arg-count)))))))

;; <a literal value> - must be constant and must match by eql
;; :constant - any constant
;; :form - any form (constant or ir-node)
;; :expr - any non-constant (ir-node)
;; (:constant a) - same as :constant but binds val to 'a'
;; (:form a) - same as :form but binds val/node to 'a'
;; (:expr a) - same as :form but binds node to 'a'
;; (+ _ _) - a function call to + (the func must be constant)

(defun valid-case-pattern (case count)
  (let ((p (first case)))
    (or (eq p 'otherwise)
        (and (listp p)
             (eq (first p) :>)
             (= (length (rest p)) count)))))

(defmacro match-ir* (forms &body cases)
  (let ((symbs (loop :for i :below (length forms) :collect (gensym i)))
        (form-count (length forms)))
    (assert (every (lambda (case) (valid-case-pattern case form-count))
                   cases))
    (labels ((reducer (accum case)
               (destructuring-bind (patterns . body) case
                 (let* ((seen-args-ht (make-hash-table))
                        (processed
                         (loop
                            :for p :in (rest patterns)
                            :for s :in symbs
                            :collect (gen-match-check p s seen-args-ht)))
                        (matchers (mapcar #'car processed))
                        (vars (mapcar #'cdr processed)))
                   `(let ,(alexandria:flatten vars)
                      (if (and ,@matchers)
                          (progn ,@body)
                          ,accum))))))
      (let* ((rcases (reverse cases))
             (butlast (rest rcases))
             (last (first rcases))
             (ival (if (eq (first last) 'otherwise)
                       (cons 'progn (rest last))
                       (reducer nil last))))
        `(let ,(mapcar #'list symbs forms)
           ,(reduce #'reducer butlast :initial-value ival))))))

(defun gen-match-check (pattern symb seen-args)
  (if (listp pattern)
      (gen-list-match-check pattern symb seen-args)
      (cons (gen-straight-match-check pattern symb) nil)))

(defun gen-list-match-check (pattern symb seen-args)
  (let ((focus (first pattern)))
    (if (find focus '(:constant :form :expr))
        (let ((var (second pattern)))
          (assert (= (length pattern) 2))
          (assert (symbolp var))
          (let ((test (gen-straight-match-check focus symb))
                (seen (gethash var seen-args)))
            (if seen
                (cons
                 (if (eq test t)
                     `(or (eql ,var ,symb)
                          (var-eq ,var ,symb))
                     `(and ,test
                           (or (eql ,var ,symb)
                               (var-eq ,var ,symb))))
                 nil)
                (progn
                  (setf (gethash var seen-args) t)
                  (cons
                   (if (eq test t)
                       `(progn
                          (setf ,var ,symb)
                          t)
                       `(when ,test
                          (setf ,var ,symb)
                          t))
                   var)))))
        (destructuring-bind (matches . vars)
            (loop
               :for subpat :in (rest pattern)
               :for i :from 0
               :for (match . var) :=
                 (gen-match-check subpat 'arg seen-args)
               :when var
               :collect var :into vars
               :unless (eq match t)
               :collect
                 `(let ((arg (elt args ,i)))
                    ,match)
               :into matches
               :finally (return (cons matches vars)))
          (cons `(and (ir-funcall-of-p
                       ',focus ,(length (rest pattern)) ,symb)
                      (with-slots (args)
                          (slot-value (slot-value ,symb 'binding) 'form)
                        (and ,@matches)))
                vars)))))

(defun gen-straight-match-check (pattern symb)
  (cond
    ((ir-constant-p pattern) `(or (eql ,symb ,pattern)
                                  (when (typep ,symb 'ssad-constant)
                                    (with-slots (form) ,symb
                                      (eql form ,pattern)))))
    ((eq pattern :constant) `(ir-constant-p ,symb))
    ((eq pattern :form) t)
    ((eq pattern :expr) `(ir-expr-p ,symb))
    (t (error "unknown pattern ~a" pattern))))

;;------------------------------------------------------------
