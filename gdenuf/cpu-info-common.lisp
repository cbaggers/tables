(in-package :gdenuf)

;;------------------------------------------------------------

(defclass cpu ()
  ((id :initarg :id :initform :unknown)
   (core :initarg :core :initform :unknown)
   (socket :initarg :socket :initform :unknown)
   (caches :initarg :caches :initarg :unknown)))

(defclass cache ()
  ((size :initarg :size)
   (line-size :initarg :line-size)
   (share-id :initarg :share-id :initform :unknown)
   (shared-with :initarg :share-id :initform :unknown)))

;;------------------------------------------------------------
