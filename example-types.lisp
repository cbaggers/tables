(in-package :tables)

#+nil
(define-data-trait vec2 ()
  (x f32)
  (y f32))

#+nil
(define-data-trait vec3 ()
  (x f32)
  (y f32)
  (z f32))

#+nil
(define-data-trait vec4 ()
  (x f32)
  (y f32)
  (z f32)
  (w f32))

#+nil
(define-data-lisp-impl rtg-math:vec2 vec2
  (x v:x)
  (y v:y))

#+nil
(define-data-lisp-impl rtg-math:vec3 vec3
  (x v:x)
  (y v:y)
  (z v:z))

#+nil
(define-data-lisp-impl rtg-math:vec4 vec4
  (x v:x)
  (y v:y)
  (z v:z)
  (w v:w))

#+nil
(define-data-ffi-impl flat-vec2 (vec2)
  (x :float 0)
  (y :float 32))

#+nil
(define-data-ffi-impl flat-vec3 (vec3)
  (x :float 0)
  (y :float 32)
  (z :float 64))

#+nil
(define-data-ffi-impl flat-vec4 (vec4)
  (x :float 0)
  (y :float 32)
  (z :float 64)
  (w :float 96))
