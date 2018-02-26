(in-package :tables)


;;------------------------------------------------------------


#+nil
(progn
  (define-data-type 3bit (nil)
    3)

  (define-data-type foo ((unsigned-byte 64)
                           :ffi-type :uint64)
    5
    (3bit threefiddy)
    (56 body))

  (unpack-as (unsigned-byte 8) some-foo '3bit)
  (unpack-into place some-foo '3bit))
