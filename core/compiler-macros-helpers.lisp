(in-package :tables.lang)

(defun extract-declares-and-doc-string (body &optional full-form)
  (labels ((declp (x) (and (listp x) (eq (first x) 'declare))))
    (if (= (length body) 1)
        (values body nil nil)
        (let (doc-string declarations)
          (loop :for form :in body :for i :from 0
             :while
               (cond
                 ((declp form)
                  (push form declarations))
                 ((stringp form)
                  (if doc-string
                      (error 'duplicate-varjo-doc-string
                             :dup form :form (or full-form body))
                      (setf doc-string form))))
             :finally (return (values (subseq body i)
                                      declarations
                                      doc-string)))))))

(defun extract-arg-pair (lambda-list key)
  (labels ((forgiving-name-equal (x y)
             (when (and (symbolp x) (symbolp y))
               (string= x y))))
    (let* ((key-pos (position key lambda-list :test #'forgiving-name-equal))
           (value (when key-pos
                    (first (subseq lambda-list (1+ key-pos)))))
           (cleaned (if key-pos
                        (append (subseq lambda-list 0 key-pos)
                                (subseq lambda-list (+ 2 key-pos)))
                        lambda-list)))
      (values value cleaned))))

(defun gen-macro-function-code (name lambda-list body)
  (alexandria:with-gensyms (form-var g-env result)
    (vbind (context lambda-list) (extract-arg-pair lambda-list :&context)
      (vbind (env-var lambda-list) (extract-arg-pair lambda-list :&environment)
        (let* ((whole-var (extract-arg-pair lambda-list :&whole))
               (whole-check (if whole-var `(not (equal ,whole-var ,result)) t))
               (whole-rebind (when whole-var
                               `((,whole-var (cons ',name ,whole-var))))))
          (let* ((env-var (or env-var g-env)))
            (vbind (body declarations) (extract-declares-and-doc-string body)
              (values
               `(lambda (,form-var ,env-var)
                  (declare (ignorable ,env-var))
                  (destructuring-bind ,lambda-list ,form-var
                    ,@declarations
                    (let* (,@whole-rebind
                           (,result (progn ,@body))
                           (same-form ,whole-check))
                      (values ,result same-form))))
               context))))))))
