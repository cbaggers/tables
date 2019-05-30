(in-package :tables.backends.fallback)

(define-op-emitter (u8* x y) fallback
  `(uop 8 (* ,x ,y)))
(define-op-emitter (u8+ x y) fallback
  `(uop 8 (+ ,x ,y)))
(define-op-emitter (u8- x y) fallback
  `(uop 8 (- ,x ,y)))
(define-op-emitter (u8/ x y) fallback
  `(uop 8 (/ ,x ,y)))
(define-op-emitter (u8= x y) fallback
  `(= ,x ,y))

(define-op-emitter (i8* x y) fallback
  `(iop 8 (* ,x ,y)))
(define-op-emitter (i8+ x y) fallback
  `(iop 8 (+ ,x ,y)))
(define-op-emitter (i8- x y) fallback
  `(iop 8 (- ,x ,y)))
(define-op-emitter (i8/ x y) fallback
  `(iop 8 (/ ,x ,y)))
(define-op-emitter (i8-negate x) fallback
  `(the (signed-byte 8) (- ,x)))
(define-op-emitter (i8= x y) fallback
  `(= ,x ,y))

(define-op-emitter (u16* x y) fallback
  `(uop 16 (* ,x ,y)))
(define-op-emitter (u16+ x y) fallback
  `(uop 16 (+ ,x ,y)))
(define-op-emitter (u16- x y) fallback
  `(uop 16 (- ,x ,y)))
(define-op-emitter (u16/ x y) fallback
  `(uop 16 (/ ,x ,y)))
(define-op-emitter (u16= x y) fallback
  `(= ,x ,y))

(define-op-emitter (i16* x y) fallback
  `(iop 16 (* ,x ,y)))
(define-op-emitter (i16+ x y) fallback
  `(iop 16 (+ ,x ,y)))
(define-op-emitter (i16- x y) fallback
  `(iop 16 (- ,x ,y)))
(define-op-emitter (i16/ x y) fallback
  `(iop 16 (/ ,x ,y)))
(define-op-emitter (i16-negate x) fallback
  `(the (signed-byte 16) (- ,x)))
(define-op-emitter (i16= x y) fallback
  `(= ,x ,y))

(define-op-emitter (u32* x y) fallback
  `(uop 32 (* ,x ,y)))
(define-op-emitter (u32+ x y) fallback
  `(uop 32 (+ ,x ,y)))
(define-op-emitter (u32- x y) fallback
  `(uop 32 (- ,x ,y)))
(define-op-emitter (u32/ x y) fallback
  `(uop 32 (/ ,x ,y)))
(define-op-emitter (u32= x y) fallback
  `(= ,x ,y))

(define-op-emitter (i32* x y) fallback
  `(iop 32 (* ,x ,y)))
(define-op-emitter (i32+ x y) fallback
  `(iop 32 (+ ,x ,y)))
(define-op-emitter (i32- x y) fallback
  `(iop 32 (- ,x ,y)))
(define-op-emitter (i32/ x y) fallback
  `(iop 32 (/ ,x ,y)))
(define-op-emitter (i32-negate x) fallback
  `(the (signed-byte 32) (- ,x)))
(define-op-emitter (i32= x y) fallback
  `(= ,x ,y))

(define-op-emitter (u64* x y) fallback
  `(uop 64 (* ,x ,y)))
(define-op-emitter (u64+ x y) fallback
  `(uop 64 (+ ,x ,y)))
(define-op-emitter (u64- x y) fallback
  `(uop 64 (- ,x ,y)))
(define-op-emitter (u64/ x y) fallback
  `(uop 64 (/ ,x ,y)))
(define-op-emitter (u64= x y) fallback
  `(= ,x ,y))

(define-op-emitter (i64* x y) fallback
  `(iop 64 (* ,x ,y)))
(define-op-emitter (i64+ x y) fallback
  `(iop 64 (+ ,x ,y)))
(define-op-emitter (i64- x y) fallback
  `(iop 64 (- ,x ,y)))
(define-op-emitter (i64/ x y) fallback
  `(iop 64 (/ ,x ,y)))
(define-op-emitter (i64-negate x) fallback
  `(the (signed-byte 64) (- ,x)))
(define-op-emitter (i64= x y) fallback
  `(= ,x ,y))

(define-op-emitter (f32* x y) fallback
  `(* ,x ,y))
(define-op-emitter (f32+ x y) fallback
  `(+ ,x ,y))
(define-op-emitter (f32- x y) fallback
  `(- ,x ,y))
(define-op-emitter (f32/ x y) fallback
  `(/ ,x ,y))
(define-op-emitter (f32= x y) fallback
  `(= ,x ,y))

:f32-abs
:f32-negate
:f32-sqrt
