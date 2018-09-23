(in-package :gdenuf)

;;------------------------------------------------------------

(defclass cpu-info ()
  ((physical-cpu-count :initarg :physical-cpu-count)
   (logical-cpu-count :initarg :logical-cpu-count)
   (cpus :initarg :cpus)))

(defclass cpu ()
  ((id :initarg :id :initform :unknown)
   (core :initarg :core :initform :unknown)
   (socket :initarg :socket :initform :unknown)
   (caches :initarg :caches :initform :unknown)))

(defclass cache ()
  ((size :initarg :size)
   (line-size :initarg :line-size)
   (share-id :initarg :share-id :initform :unknown)
   (shared-with :initarg :share-id :initform :unknown)))

;;------------------------------------------------------------
