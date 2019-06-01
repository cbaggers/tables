(in-package :tables.internals)

;; hacky tests, will have proper ones once this is all a bit more worked
;; out

(defun test0 (&optional (optimize '((speed 3) (safety 1) (debug 1))))
  (let ((sub-queries
         (compile-query
          '((a i8 :in/out) (b i8 :in/out) (c i8 :in/out))
          '((d i8))
          '(let* ((x (+ c (* a d)))
                  (z (* b (+ d x)))
                  (y (funcall (lambda ((g i8)) (+ g 1))
                              x)))
            (output :a y :b z :c a)))))
    (mapcar (lambda (x) (tables.backends.fallback:emit x optimize))
            sub-queries)))

(defun test2 (&optional (optimize '((speed 3) (safety 1) (debug 1))))
  (mapcar (lambda (x) (tables.backends.fallback:emit x optimize))
          (compile-query
           '((a vec3 :in/out) (b vec3 :in))
           '()
           '(output :a (vec3*s (+ a (* b b)) 2.0)))))

(defun test3 (&optional (optimize '((speed 3) (safety 1) (debug 1))))
  (mapcar (lambda (x) (tables.backends.fallback:emit x optimize))
          (compile-query
           '((a vec3 :out))
           '()
           '(output :a (vec3 0.0 1.0 2.0)))))

(defun test4 (&optional (optimize '((speed 3) (safety 1) (debug 1))))
  (mapcar (lambda (x) (tables.backends.fallback:emit x optimize))
          (compile-query
           '((a f32 :out))
           '()
           '(output :a 0.0))))

(defun test ()
  (let* ((uspec (make-table-spec '((start-val i8)
                                   (current-val i8)
                                   (rate i8))))
         (spec (validate-table-spec uspec))
         (table (make-table spec))
         (query-varyings `(,table (start-val :in)
                                  (current-val :in/out)
                                  (rate :in)))
         (query-code '(output :current-val (+ current-val rate))))
    (values table spec query-varyings query-code)))
