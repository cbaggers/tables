(in-package :tables.lang)

;;------------------------------------------------------------

(define-ttype i8)
(define-ttype i16)
(define-ttype i32)
(define-ttype i64)

(define-ttype u8)
(define-ttype u16)
(define-ttype u32)
(define-ttype u64)

;;------------------------------------------------------------

(define-dummy-func i8+ (i8 i8) i8)
(define-dummy-func i8- (i8 i8) i8)
(define-dummy-func i8* (i8 i8) i8)
(define-dummy-func i8/ (i8 i8) i8)

(define-optimize-macro i8+ (&whole whole arg-0 arg-1)
  (labels ((refactor (form-arg constant-arg)
             (if (= constant-arg 0)
                 form-arg
                 (tables.compile.stage-0:match-ir-1 form-arg
                   ((i8+ (:constant c) (:form f))
                    `(i8+ ,f (i8+ ,c ,constant-arg)))
                   ((i8+ (:form f) (:constant c))
                    `(i8+ ,f (i8+ ,constant-arg ,c)))
                   ((i8+ (:form f0) (:form f1))
                    `(i8+ ,f0 (i8+ ,f1 ,constant-arg)))
                   (otherwise whole)))))
    (let* ((a0-nump (numberp arg-0))
           (a1-nump (numberp arg-1)))
      (cond
        ((and a0-nump a1-nump) (- (mod (+ 127 (+ arg-0 arg-1)) 255) 127))
        (a0-nump (refactor arg-1 arg-0))
        (a1-nump (refactor arg-0 arg-1))
        (t
         (tables.compile.stage-0:match-ir* (arg-0 arg-1)
           ((:> (i8+ (:form a) (:form b))
                (i8+ (:form c) (:form d)))
            `(i8+ ,a (i8+ ,b (i8+ ,c ,d))))

           ;; {TODO}
           ;; This requires match-ir* to be able to bind at any depth
           ;; and to support walking deeper into the ir
           ;;
           ;; ((:> (:form a)
           ;;      (i8+ (i8* (:form b) (:constant c))
           ;;           (:form d)))
           ;;  :mooooo)
           (otherwise
            whole)))))))

(define-optimize-macro i8- (&whole whole a b)
  (cond
    ((and (numberp a) (numberp b)) (- (mod (+ 127 (- a b)) 255) 127))
    ((tables.compile.stage-0:var-eq a b) 0)
    (t whole)))

(define-optimize-macro i8* (&whole whole arg-0 arg-1)
  (labels ((refactor (form-arg constant-arg)
             (if (= constant-arg 0)
                 form-arg
                 (tables.compile.stage-0:match-ir-1 form-arg
                   ((i8* (:constant c) (:form f))
                    `(i8* ,f (i8* ,c ,constant-arg)))
                   ((i8* (:form f) (:constant c))
                    `(i8* ,f (i8* ,constant-arg ,c)))
                   ((i8* (:form f0) (:form f1))
                    `(i8* ,f0 (i8* ,f1 ,constant-arg)))
                   ((i8+ (:form f) (:constant c))
                    `(i8+ (i8* ,c ,constant-arg)
                          (i8* ,f ,constant-arg)))
                   ((i8+ (:constant c) (:form f))
                    `(i8+ (i8* ,f ,constant-arg)
                          (i8* ,c ,constant-arg)))
                   (otherwise whole)))))
    (let* ((a0-nump (numberp arg-0))
           (a1-nump (numberp arg-1)))
      (cond
        ((and a0-nump a1-nump) (- (mod (+ 127 (* arg-0 arg-1)) 255) 127))
        ((eql arg-0 0) 0)
        ((eql arg-1 0) 0)
        ((eql arg-0 1) arg-1)
        ((eql arg-1 1) arg-0)
        (a0-nump (refactor arg-1 arg-0))
        (a1-nump (refactor arg-0 arg-1))
        (t
         (tables.compile.stage-0:match-ir* (arg-0 arg-1)
           ((:> (i8* (:form a) (:form b))
                (i8* (:form c) (:form d)))
            `(i8* ,a (i8* ,b (i8* ,c ,d))))
           (otherwise
            whole)))))))

(define-optimize-macro i8/ (&whole whole a b)
  (cond
    ((eql a 0) 0)
    ((tables.compile.stage-0:var-eq a b) 1)
    (t whole)))

;;------------------------------------------------------------

(define-dummy-func i16+ (i16 i16) i16)
(define-dummy-func i16- (i16 i16) i16)
(define-dummy-func i16* (i16 i16) i16)
(define-dummy-func i16/ (i16 i16) i16)

;;------------------------------------------------------------

(define-dummy-func i32+ (i32 i32) i32)
(define-dummy-func i32- (i32 i32) i32)
(define-dummy-func i32* (i32 i32) i32)
(define-dummy-func i32/ (i32 i32) i32)

;;------------------------------------------------------------

(define-dummy-func i64+ (i64 i64) i64)
(define-dummy-func i64- (i64 i64) i64)
(define-dummy-func i64* (i64 i64) i64)
(define-dummy-func i64/ (i64 i64) i64)

;;------------------------------------------------------------

(define-dummy-func u8+ (u8 u8) u8)
(define-dummy-func u8- (u8 u8) u8)
(define-dummy-func u8* (u8 u8) u8)
(define-dummy-func u8/ (u8 u8) u8)

;;------------------------------------------------------------

(define-dummy-func u16+ (u16 u16) u16)
(define-dummy-func u16- (u16 u16) u16)
(define-dummy-func u16* (u16 u16) u16)
(define-dummy-func u16/ (u16 u16) u16)

;;------------------------------------------------------------

(define-dummy-func u32+ (u32 u32) u32)
(define-dummy-func u32- (u32 u32) u32)
(define-dummy-func u32* (u32 u32) u32)
(define-dummy-func u32/ (u32 u32) u32)

;;------------------------------------------------------------

(define-dummy-func u64+ (u64 u64) u64)
(define-dummy-func u64- (u64 u64) u64)
(define-dummy-func u64* (u64 u64) u64)
(define-dummy-func u64/ (u64 u64) u64)

;;------------------------------------------------------------

(define-dummy-func i8= (i8 i8) boolean)
(define-dummy-func i16= (i16 i16) boolean)
(define-dummy-func i32= (i32 i32) boolean)
(define-dummy-func i64= (i64 i64) boolean)

(define-dummy-func u8= (u8 u8) boolean)
(define-dummy-func u16= (u16 u16) boolean)
(define-dummy-func u32= (u32 u32) boolean)
(define-dummy-func u64= (u64 u64) boolean)

(define-optimize-macro i8= (&whole whole a b)
  (if (and (numberp a) (numberp b))
      (= a b)
      whole))

;;------------------------------------------------------------

(define-dummy-func i8-negate (i8) i8)
(define-dummy-func i16-negate (i16) i16)
(define-dummy-func i32-negate (i32) i32)
(define-dummy-func i64-negate (i64) i64)

(define-dummy-func u8-negate (u8) u8)
(define-dummy-func u16-negate (u16) u16)
(define-dummy-func u32-negate (u32) u32)
(define-dummy-func u64-negate (u64) u64)

;;------------------------------------------------------------

(define-trait-impl addable () i8
  (+ i8+))

(define-trait-impl addable () i16
  (+ i16+))

(define-trait-impl addable () i32
  (+ i32+))

(define-trait-impl addable () i64
  (+ i64+))

(define-trait-impl addable () u8
  (+ u8+))

(define-trait-impl addable () u16
  (+ u16+))

(define-trait-impl addable () u32
  (+ u32+))

(define-trait-impl addable () u64
  (+ u64+))

;;------------------------------------------------------------

(define-trait-impl subtractable () i8
  (- i8-))

(define-trait-impl subtractable () i16
  (- i16-))

(define-trait-impl subtractable () i32
  (- i32-))

(define-trait-impl subtractable () i64
  (- i64-))

(define-trait-impl subtractable () u8
  (- u8-))

(define-trait-impl subtractable () u16
  (- u16-))

(define-trait-impl subtractable () u32
  (- u32-))

(define-trait-impl subtractable () u64
  (- u64-))

;;------------------------------------------------------------

(define-trait-impl multiplyable () i8
  (* i8*))

(define-trait-impl multiplyable () i16
  (* i16*))

(define-trait-impl multiplyable () i32
  (* i32*))

(define-trait-impl multiplyable () i64
  (* i64*))

(define-trait-impl multiplyable () u8
  (* u8*))

(define-trait-impl multiplyable () u16
  (* u16*))

(define-trait-impl multiplyable () u32
  (* u32*))

(define-trait-impl multiplyable () u64
  (* u64*))

;;------------------------------------------------------------

(define-trait-impl dividable () i8
  (/ i8/))

(define-trait-impl dividable () i16
  (/ i16/))

(define-trait-impl dividable () i32
  (/ i32/))

(define-trait-impl dividable () i64
  (/ i64/))

(define-trait-impl dividable () u8
  (/ u8/))

(define-trait-impl dividable () u16
  (/ u16/))

(define-trait-impl dividable () u32
  (/ u32/))

(define-trait-impl dividable () u64
  (/ u64/))

;;------------------------------------------------------------

(define-trait-impl partial-numeric-equality () i8
  (= i8=))

(define-trait-impl partial-numeric-equality () i16
  (= i16=))

(define-trait-impl partial-numeric-equality () i32
  (= i32=))

(define-trait-impl partial-numeric-equality () i64
  (= i64=))

(define-trait-impl partial-numeric-equality () u8
  (= u8=))

(define-trait-impl partial-numeric-equality () u16
  (= u16=))

(define-trait-impl partial-numeric-equality () u32
  (= u32=))

(define-trait-impl partial-numeric-equality () u64
  (= u64=))

;;------------------------------------------------------------

(define-trait-impl negatable () i8
  (negate i8-negate))

(define-trait-impl negatable () i16
  (negate i16-negate))

(define-trait-impl negatable () i32
  (negate i32-negate))

(define-trait-impl negatable () i64
  (negate i64-negate))

(define-trait-impl negatable () u8
  (negate u8-negate))

(define-trait-impl negatable () u16
  (negate u16-negate))

(define-trait-impl negatable () u32
  (negate u32-negate))

(define-trait-impl negatable () u64
  (negate u64-negate))

;;------------------------------------------------------------
