
(defun super-duper-looper (ptr)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (cffi:foreign-pointer ptr))
  (tagbody
   top
     (unless (cffi:pointer-eq ptr (cffi:null-pointer))
       (cffi:incf-pointer ptr -1)
       (go top)))
  (values))

(defun super-duper-twoper (ptr)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (cffi:foreign-pointer ptr))
  (loop
     :until (cffi:pointer-eq ptr (cffi:null-pointer))
     :do (cffi:incf-pointer ptr -1))
  (values))



(defun super-duper-looper (ptr n)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (cffi:foreign-pointer ptr)
           (type fixnum n))
  (tagbody
   top
     (let ((tmp n))
       (incf n 1)
       (unless (= tmp 1000)
         (go top))))
  (values))
