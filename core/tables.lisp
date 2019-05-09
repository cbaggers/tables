(in-package :tables.tables)

;;------------------------------------------------------------

(defclass column-quality () ())

(defclass column-spec ()
  ((name :initarg :name)
   (type-designator :initarg :type-designator)
   (qualities :initform nil :initarg :qualities)))

(defclass table-spec ()
  ((column-specs :initarg :column-specs)))

(defclass but-chunk ()
  ((columns :initarg columns)))

(defclass but-column ()
  ((pointer :initarg :pointer)
   (count :initarg :count)))

(defclass basic-unsorted-table ()
  ((chunks :initarg :chunks)))

;; First version restrictions are
;; - not implement filter yet, only map
;; - no aos columns
;; - no struct columns (we need to add output destructuring to optimizer first)
;; - no column qualities (e.g. cluster)


;; We will come back to these when we understand how to the the above stuff.
#+nil
(progn
  (defclass cluster (column-quality) ())
  (defclass but-cluster ()
    ((chunks :initarg :chunks)))
  (defclass basic-unsorted-clustered-table ()
    ((clusters :initarg :clusters))))


;;------------------------------------------------------------
