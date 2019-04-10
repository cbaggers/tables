(in-package :tables.compile)

(define-optimize-macro i8+ (&whole whole arg-0 arg-1)
  (declare (ignorable some-node))
  (match-ir ((a arg-0) (b arg-1))
    ;; constant optimizations
    ((:constant :constant)
     (- (mod (+ 127 (* a b)) 255) 127))
    ((:form 0)
     a)
    ((0 :form)
     b)
    ;; first reorderings perms
    ((:constant (i8+ (:constant c) (:form d)))
     `(i8+ ,d (i8+ ,a ,c)))
    ((:constant (i8+ (:form c) (:constant d)))
     `(i8+ ,d (i8+ ,c ,a)))
    ((:constant (i8+ (:form c) (:form d)))
     `(i8+ ,d (i8+ ,c ,a)))
    ;; same as previous 3 but for lhs
    (((i8+ (:constant c) (:form d)) :constant)
     `(i8+ (i8+ ,a ,c) ,d))
    (((i8+ (:form c) (:constant d)) :constant)
     `(i8+ (i8+ ,c ,a) ,d))
    (((i8+ (:form c) (:form d)) :constant)
     `(i8+ (i8+ ,c ,a) ,d))
    (otherwise whole)))


(defmacro match-ir (bindings &body cases)
  (labels ((ensure-binding (binding)
             (if (symbolp binding)
                 (list binding binding)
                 binding))
           (otherwise-case-p (case)
             (eq (first case) 'otherwise))

           (process-case-common (case)
             )

           (process-case (case)
             (assert (not (otherwise-case-p case)))
             (process-case-common case))
           (process-last-case (case)
             (if (otherwise-case-p case)
                 case
                 (process-case-common case))))
    (let* ((bindings
            (mapcar #'ensure-binding bindings))
           (cases-butlast
            (mapcar #'process-case (butlast cases)))
           (cases-last
            (process-last-case (last1 cases))))
      `(let* ,bindings
         (cond
           ,@cases-butlast
           ,cases-last)))))

#+nil
'(;;
  ;; all reduce
  (+ 1 2)
  (+ (+ 1 3) 2)
  (+ 2 (+ 1 3))
  ;;
  ;; cant do anything here
  (+ a (+ b c))
  (+ a 1)
  (+ 1 a)
  (+ a (+ b 2))
  (+ a (+ 2 b))
  ;;
  ;;
  (+ 1 (+ 2 b)) -> (+ b (+ 2 1))
  (+ 1 (+ b 2)) -> (+ b (+ 1 2))
  (+ 1 (+ a b)) -> (+ a (+ b 1))

  (+ (+ a b) 1) -> (+ (+ b 1) a)
  (+ (+ 2 b) 1) -> (+ (+ 2 1) b)
  (+ (+ b 2) 1) -> (+ (+ 1 2) b))



(defun ir-p (pattern node)
  )

;; (ir-p '(i8+ :constant :form) node)

(define-optimize-macro i8+ (&whole whole arg-0 arg-1)
  (declare (ignorable some-node))
  ;;
  (match-ir ((a arg-0) (b arg-1))
    ;; constant optimizations
    ((:constant :constant)
     (- (mod (+ 127 (* a b)) 255) 127))
    ((:form 0)
     a)
    ((0 :form)
     b)
    ;; first reorderings perms
    ((:constant (i8+ (:constant c) (:form d)))
     `(i8+ ,d (i8+ ,a ,c)))
    ((:constant (i8+ (:form c) (:constant d)))
     `(i8+ ,d (i8+ ,c ,a)))
    ((:constant (i8+ (:form c) (:form d)))
     `(i8+ ,d (i8+ ,c ,a)))
    ;; same as previous 3 but for lhs
    (((i8+ (:constant c) (:form d)) :constant)
     `(i8+ (i8+ ,a ,c) ,d))
    (((i8+ (:form c) (:constant d)) :constant)
     `(i8+ (i8+ ,c ,a) ,d))
    (((i8+ (:form c) (:form d)) :constant)
     `(i8+ (i8+ ,c ,a) ,d))
    (otherwise whole)))
