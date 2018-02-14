#||

We provide some types that map directly to lisp types (or just used lisp types)
and then provide palletes of wordily named (potentially target specific) types.

ieee754-32bit-floating-point           <- usually single-float
two-complement-wrapping-32bit-signed-integer  <- wrapping signed integer
two-complement-undefined-out-of-bounds-32bit-signed-integer  <- non-wrapping signed integer
two-complement-wrapping-32bit-unsigned-integer  <- wrapping signed integer
two-complement-undefined-out-of-bounds-32bit-unsigned-integer  <- non-wrapping signed integer

It is then up to the user to define aliases to those types for their projects
(we can obviously make libraries for the common ones too)

On the platforms where ieee754-32bit-floating-point & single-float are
equivalent then all functions for single-float will work with
ieee754-32bit-floating-point too (and obviously visa versa) but on platforms
where they arent they wont. This allows for picking a different add when
requiring wrapping on platforms that dont have it.

OR

We dont 'provide some types that map directly to lisp types' we provide the
tables types and then allow the definition of coversion operators on those.

1 set of ptr<->val conversion operators to get to/from list & 1 set to get
to/from the ffi.


||#

#||

what is type of int literal? smallest type? most generic? both seem to have big
impacts on inference.

||#
