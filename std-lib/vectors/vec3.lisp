(in-package :tables.lang)

;;------------------------------------------------------------

(define-record vec3
  (x f32)
  (y f32)
  (z f32))

;;------------------------------------------------------------

(defn vec3+ ((a vec3) (b vec3))
  (vec3 (+ (vec3-x a) (vec3-x b))
        (+ (vec3-y a) (vec3-y b))
        (+ (vec3-y a) (vec3-z b))))

(defn vec3- ((a vec3) (b vec3))
  (vec3 (- (vec3-x a) (vec3-x b))
        (- (vec3-y a) (vec3-y b))
        (- (vec3-y a) (vec3-z b))))

(defn vec3* ((a vec3) (b vec3))
  (vec3 (* (vec3-x a) (vec3-x b))
        (* (vec3-y a) (vec3-y b))
        (* (vec3-y a) (vec3-z b))))

(defn vec3/ ((a vec3) (b vec3))
  (vec3 (/ (vec3-x a) (vec3-x b))
        (/ (vec3-y a) (vec3-y b))
        (/ (vec3-y a) (vec3-z b))))

(define-trait-impl addable () vec3
  (+ vec3+))

(define-trait-impl subtractable () vec3
  (- vec3-))

(define-trait-impl multiplyable () vec3
  (* vec3*))

(define-trait-impl dividable () vec3
  (/ vec3/))

;;------------------------------------------------------------

#+nil ;; need to implement 'and' before coming back to this
(progn
  (defn vec3-zerop ((v vec3))
    (and (= (vec3-x v) 0f0)
         (= (vec3-y v) 0f0)
         (= (vec3-z v) 0f0)))

  (define-trait-impl zeroable-numeric () vec3
    (zerop vec3-zerop)))

;;------------------------------------------------------------

(defn vec3+s ((a vec3) (b f32))
  (vec3 (+ (vec3-x a) b)
        (+ (vec3-y a) b)
        (+ (vec3-z a) b)))

(defn vec3-s ((a vec3) (b f32))
  (vec3 (- (vec3-x a) b)
        (- (vec3-y a) b)
        (- (vec3-z a) b)))

(defn vec3*s ((a vec3) (b f32))
  (vec3 (* (vec3-x a) b)
        (* (vec3-y a) b)
        (* (vec3-z a) b)))

(defn vec3/s ((a vec3) (b f32))
  (vec3 (/ (vec3-x a) b)
        (/ (vec3-y a) b)
        (/ (vec3-z a) b)))

;;------------------------------------------------------------

(defn vec3-negate ((a vec3))
  (vec3 (negate (vec3-x a))
        (negate (vec3-y a))
        (negate (vec3-z a))))

(define-trait-impl negatable () vec3
  (negate vec3-negate))

;;------------------------------------------------------------

(defn vec3-dot ((a vec3) (b vec3))
  (+ (* (vec3-x a) (vec3-x b))
     (+ (* (vec3-y a) (vec3-y b))
        (* (vec3-z a) (vec3-z b)))))

;;------------------------------------------------------------

(defn vec3-cross ((a vec3) (b vec3))
  (vec3 (- (* (vec3-y a) (vec3-z b)) (* (vec3-z a) (vec3-y b)))
        (- (* (vec3-z a) (vec3-x b)) (* (vec3-x a) (vec3-z b)))
        (- (* (vec3-x a) (vec3-y b)) (* (vec3-y a) (vec3-x b)))))

;;------------------------------------------------------------

(defn vec3-length-squared ((a vec3))
  (+ (* (vec3-x a) (vec3-x a))
     (+ (* (vec3-y a) (vec3-y a))
        (* (vec3-z a) (vec3-z a)))))

;;------------------------------------------------------------

(defn vec3-length ((a vec3))
  (f32-sqrt (vec3-length-squared a)))

;;------------------------------------------------------------

(defn vec3-distance-squared ((a vec3) (b vec3))
  (vec3-length-squared (- b a)))

;;------------------------------------------------------------

(defn vec3-distance ((a vec3) (b vec3))
  (vec3-length (- b a)))

;;------------------------------------------------------------

(defn vec3-abs ((a vec3))
  (vec3 (abs (vec3-x a))
        (abs (vec3-y a))
        (abs (vec3-z a))))

;;------------------------------------------------------------
