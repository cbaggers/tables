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

(define-constant-folder i8+ (a b)
  (- (mod (+ 127 (+ a b)) 255) 127))

(define-constant-folder i8- (a b)
  (- (mod (+ 127 (- a b)) 255) 127))

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
