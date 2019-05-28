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
:i16*
:i16+
:i16-
:i16-negate
:i16/
:i16=
:i32*
:i32+
:i32-
:i32-negate
:i32/
:i32=
:i64*
:i64+
:i64-
:i64-negate
:i64/
:i64=
:u16*
:u16+
:u16-
:u16/
:u16=
:u32*
:u32+
:u32-
:u32/
:u32=
:u64*
:u64+
:u64-
:u64/
:u64=
