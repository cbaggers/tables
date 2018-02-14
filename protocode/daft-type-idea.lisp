#||

Data has no type. By passing it to a function the data is blessed with all the
type qualities that the function argument specified. This then impacts what you
pass it to next. Future function calls result in the type being the
intersection of the previous types (for some definition of type intersection).

This might result in an intersection that has nearly no info (so cant be passed
to much) however it will never become untyped again. In short untyped != types
with no traits

So you can allocate a 64bit thing, and then it accumulates qualities from how
it is used.

||#
