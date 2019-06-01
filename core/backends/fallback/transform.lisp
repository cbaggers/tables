(in-package #:tables.backends.fallback)

(defun transform (sub-queries pass-count)
  (loop
     :for sub-query :in sub-queries
     :do (tables.compile.stage-0.split-outputs:run-transform
          (slot-value sub-query 'ir)))
  (let ((query-passes
         (loop
            :for sub-query :in sub-queries
            :collect (+ pass-count
                        (run-passes-until-stablized
                         (slot-value sub-query 'ir))))))
    (values sub-queries query-passes)))
