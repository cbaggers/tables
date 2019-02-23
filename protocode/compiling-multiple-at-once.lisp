
#||

I think that when we submit a code change to the table instance we should
pass along the *compile-file-pathname* as the key.

Then at the next frame/point we compile we group and sort everything with
the same key. This will allow for using 'C-c C-k' during interactive dev
and get the same effect as everything getting registered for compile on
startup.

'C-c C-c' would also have a key but, as it would be the only thing in the
file, it would have order dependent behaviour, just like CL usually does.

||#

(defmacro wat ()
  (print (list :> *compile-file-pathname* 	*compile-file-truename*))
  (print (alexandria:read-file-into-string *compile-file-pathname*))
  nil)

(wat)
