(in-package #:tables)

;;------------------------------------------------------------

(defmacro build-report-validate (name &body validation-forms)
  (with-gensyms (report)
    `(let ((,report (make-instance 'build-report :name ',name)))
       ,@(loop :for (test err-template . args) :in validation-forms
            :for i :from 0
            :for gtest := (gsymb "TEST-" i)
            :for gtest-code := (gsymb "TEST-CODE-" i)
            :do (assert (stringp err-template))
            :collect `(process-build-report-result
                       ,test ',test ,report ,err-template ,@args))
       ,report)))

(defun process-build-report-result (gtest
                                    gtest-code
                                    report
                                    err-template
                                    &rest args)
  (labels ((report-p (x) (typep x 'build-report)))
    (print gtest)
    (cond
      ((report-p gtest)
       (merge-subreport report gtest gtest-code err-template args))
      ((and gtest (listp gtest) (every #'report-p gtest))
       (merge-subreports report gtest gtest-code err-template args))
      (gtest (add-success report gtest-code))
      (t (add-failure report gtest-code err-template args nil)))))

(defun add-success (report test-code)
  (push test-code (slot-value report 'successes)))

(defun add-failure (report test-code error-template args sub-report)
  (push (append (list test-code
                      (apply #'format nil error-template args)
                      args)
                (when sub-report
                  (list :subreport sub-report)))
        (slot-value report 'failures)))

(defun merge-subreport (report subreport test-code error-template &rest args)
  (if (slot-value subreport 'failures)
      (add-failure report test-code error-template args subreport)
      (add-success report test-code)))

(defun merge-subreports (report subreports test-code error-template &rest args)
  (let ((failures (remove-if-not (lambda (s) (slot-value s 'failures))
                                 subreports)))
    (if failures
        (add-failure report test-code error-template args failures)
        (add-success report test-code))))


#+nil
(defun test (a b)
  (build-report-validate :test-some-stuff
    ((< a 10)
     "Expected a to be less than 10, instead recieved ~a" a)
    ((symbolp b)
     "~a is not a symbol" b)))
