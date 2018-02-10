(in-package :cepl)

(define-flat-type foo ()
  (a (unsigned-byte 8))
  (b single-float))

(define-flat-type vec2 ()
  (x single-float)
  (y single-float))

(define-table basics ()
  (position vec2)
  (metadata foo))

;; explicit declaration of what is changed
(define-query poke (:map (basics position))
  (update :position (+ position (vec2 1 0))))

;; everything read in is written back, hmm that assumes that the destination
;; starts out with the same data as the source. This might not be so maybe
;; always copy everything..or hmm what is best to do kinda depends on what
;; this query is used for, if the goal is to update the original then copying
;; everything allows a pointer swap at the end, if the result is just to be
;; passed to some other query then we should only store the ..wait which data
;; from this goes in the result?
(define-query poke (:map (basics position))
  (setf position (+ position (vec2 1 0))))
