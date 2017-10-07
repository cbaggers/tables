(in-package #:tables)

;;------------------------------------------------------------

;;[TODO] need better arg test
(defmacro defcondition (name (&key (condition-type 'error) prefix
                                   (print-circle nil print-circle?)
                                   (print-escape nil print-escape?)
                                   (print-length nil print-length?)
                                   (print-level nil print-level?)
                                   (print-lines nil print-lines?)
                                   (print-right-margin nil print-right-margin?))
                                (&rest args) error-string &body body)
  (assert condition-type () "DEFCONDITION: condition-type is a mandatory argument")
  (unless (every #'symbolp args) (error "can only take simple args"))
  (let ((control-str (format nil "~@[~a: ~]~a" prefix error-string)))
    `(define-condition ,name (,condition-type)
       ,(mapcar (lambda (arg) `(,arg :initarg ,(kwd arg))) args)
       (:report (lambda (condition stream)
                  (declare (ignorable condition))
                  (let ((*print-circle* (if ,print-circle?
                                            ,print-circle
                                            *print-circle*))
                        (*print-escape* (if ,print-escape?
                                            ,print-escape
                                            *print-escape*))
                        (*print-length* (if ,print-length?
                                            ,print-length
                                            *print-length*))
                        (*print-level* (if ,print-level?
                                           ,print-level
                                           *print-level*))
                        (*print-lines* (if ,print-lines?
                                           ,print-lines
                                           *print-lines*))
                        (*print-right-margin* (if ,print-right-margin?
                                                  ,print-right-margin
                                                  *print-right-margin*)))
                    (with-slots ,args condition
                      (format stream ,control-str ,@body))))))))

(defmacro define-error (name (&key (error-type 'error) prefix
                               (print-circle nil print-circle?)
                               (print-escape nil print-escape?)
                               (print-length nil print-length?)
                               (print-level nil print-level?)
                               (print-lines nil print-lines?)
                               (print-right-margin nil print-right-margin?))
                            (&rest args) error-string &body body)
  `(defcondition ,name
       (:condition-type ,error-type :prefix ,prefix
                        ,@(when print-circle? `(:print-circle ,print-circle))
                        ,@(when print-escape? `(:print-escape ,print-escape))
                        ,@(when print-length? `(:print-length ,print-length))
                        ,@(when print-level? `(:print-level ,print-level))
                        ,@(when print-lines? `(:print-lines ,print-lines))
                        ,@(when print-right-margin?
                                `(:print-right-margin ,print-right-margin)))
       ,args
       ,error-string ,@body))

(defmacro define-warning (name (&key (warning-type 'warning) prefix
                                 (print-circle nil print-circle?)
                                 (print-escape nil print-escape?)
                                 (print-length nil print-length?)
                                 (print-level nil print-level?)
                                 (print-lines nil print-lines?)
                                 (print-right-margin nil print-right-margin?))
                              (&rest args) warning-string &body body)
  `(defcondition ,name
       (:condition-type ,warning-type :prefix ,prefix
                        ,@(when print-circle? `(:print-circle ,print-circle))
                        ,@(when print-escape? `(:print-escape ,print-escape))
                        ,@(when print-length? `(:print-length ,print-length))
                        ,@(when print-level? `(:print-level ,print-level))
                        ,@(when print-lines? `(:print-lines ,print-lines))
                        ,@(when print-right-margin?
                                `(:print-right-margin ,print-right-margin)))
       ,args
       ,warning-string ,@body))

;; ------------------------------------------------------------
