(in-package #:tables)

;;------------------------------------------------------------

(defgeneric validate-prototype (prototype))
(defgeneric validate-in-issolation (prototype))
(defgeneric validate-in-context (prototype))

(defgeneric add-query (query))

(defgeneric name (obj))
(defgeneric uniforms (obj))
(defgeneric columns (obj))

;;------------------------------------------------------------
