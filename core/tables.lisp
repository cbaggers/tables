(in-package :tables.tables)

;;------------------------------------------------------------

(defclass column-quality () ())

(defclass column-spec ()
  ((name :initarg :name)
   (type-designator :initarg :type-designator)
   (qualities :initform nil :initarg :qualities)))

(defclass table-spec ()
  ((column-specs :initarg :column-specs)))

(defclass validated-column-spec ()
  ((name :initarg :name)
   (type :initarg :type)
   (element-size :initarg :element-size)
   (qualities :initform nil :initarg :qualities)))

(defclass validated-table-spec ()
  ((column-specs :initarg :column-specs)
   (validated-against :initarg :validated-against)))

(defclass chunk ()
  ((columns :initarg :columns)))

(defclass column ()
  ((data :initarg :data) ;; :type tmem
   (count :initarg :count)))

(defclass table-info ()
  ((chunk-size :initform 1000)
   (min-reserved-size :initform 1000)))

(defclass unordered-table ()
  ((info :initarg :info)
   (chunks :initarg :chunks)))

(defun unordered-table-chunks (table)
  (slot-value table 'chunks))

(defstruct tmem
  (raw (error "") :type cffi:foreign-pointer)
  (ptr (error "") :type cffi:foreign-pointer))

;;------------------------------------------------------------

(defun make-column-spec (column-form)
  (destructuring-bind (name type-designator) column-form
    (make-instance 'column-spec
                   :name name
                   :type-designator type-designator)))

(defun make-table-spec (column-forms)
  (let ((column-specs (mapcar #'make-column-spec column-forms)))
    (make-instance 'table-spec
                   :column-specs column-specs)))

;;------------------------------------------------------------
;; seperate step as soon the validate will be done on the thread
;; running the hub
;;
;; First version restrictions are
;; - implementing map & delete
;; - only supporting delete within chunk, no cross chunk stuff movement yet
;; - no aos columns
;; - no struct columns (we need to add output destructuring to optimizer first)
;; - no column qualities (e.g. cluster)

(defun calc-element-size (context type qualities)
  (declare (ignore qualities))
  (assert (value-type-p type)) ;; {TODO} handle structs and layouts
  (ceiling (value-type-size context type) 8))

(defun validate-table-spec (spec)
  (check-type spec table-spec)
  (let (;; {TODO} pass in the context
        (context (checkmate:make-check-context 'tables)))
    (labels ((validate-column (col)
               (with-slots (name type-designator qualities) col
                 (assert (null qualities))
                 (let ((type (find-ttype context type-designator)))
                   (assert (complete-p type))
                   (assert (not (record-type-p type)))
                   (make-instance
                    'validated-column-spec
                    :name name
                    :type type
                    :element-size (calc-element-size
                                   context type qualities))))))
      (let ((column-specs
             (mapcar #'validate-column (slot-value spec 'column-specs))))
        (make-instance 'validated-table-spec
                       :column-specs column-specs
                       :validated-against context)))))

;;------------------------------------------------------------

(defun init-chunk (context column-specs table-info)
  (declare (ignore context))
  (with-slots (chunk-size) table-info
    (labels ((spec-to-column (spec)
               (with-slots (name type element-size) spec
                 (let ((data (t-alloc (* element-size chunk-size))))
                   (make-instance 'column :data data :count 0)))))
      (let ((columns (mapcar #'spec-to-column column-specs)))
        (make-instance 'chunk :columns columns)))))

(defun init-chunks (type-system column-specs table-info)
  (with-slots (chunk-size min-reserved-size) table-info
    (loop
       :for i :below (ceiling min-reserved-size chunk-size)
       :collect (init-chunk type-system column-specs table-info))))

(defun make-table (table-spec)
  (check-type table-spec validated-table-spec)
  (with-slots (validated-against column-specs) table-spec
    (let ((info (make-instance 'table-info)))
      (make-instance
       'unordered-table
       :info info
       :chunks (init-chunks validated-against column-specs info)))))

;;------------------------------------------------------------
;; This will probably move to a memory management section in
;; due course

(defun t-alloc (byte-size)
  (let ((ptr (cffi:foreign-alloc :uint8 :count byte-size)))
    ;; would be different when raw is not correctly aligned
    (make-tmem :raw ptr :ptr ptr)))
