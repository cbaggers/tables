;;------------------------------------------------------------
;;

(defmacro define-hub (name
                      &body args
                      &key static &allow-other-keys))

(define-hub test
  :static nil)

;;------------------------------------------------------------

(defmacro define-table (name
                        (hub &rest args &key static &allow-other-keys)
                        &body columns))

(define-table actors (test) (:static nil)
  (pos vec3)
  (rot vec3))

;;------------------------------------------------------------

(defmacro define-query (name args
                        (hub &rest args &key static &allow-other-keys)
                        &body body))

(define-query update-actors ((a vec3) (z single-float))
    (test :static nil)
  (* a z))

;;------------------------------------------------------------

(defmacro define-query-set (name
                            (hub &rest args &key static &allow-other-keys)
                            &body query-names))

(define-query-set pass-0 (test :static nil)
  update-actors)

;;------------------------------------------------------------
