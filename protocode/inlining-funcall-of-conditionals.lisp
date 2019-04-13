(LET* ((G8046 :ARG)
       (G8047 :ARG)
       (G8052 (I8+ G8047 11))
       (G8056 (I8= G8046 12))
       (G8084 (I8+ G8046 G8047))
       (G8078
        (IF G8056
            (LET* ((G8061 (I8* G8047 10))
                   (G8068
                    (LAMBDA (X_8062)
                      (LET* ((G8066 (I8* X_8062 G8061)))
                        G8066))))
              G8068)
            (LET* ((G8077
                    (LAMBDA (X_8071)
                      (LET* ((G8075 (I8* X_8071 G8052)))
                        G8075))))
              G8077)))
       (G8085 (FUNCALL G8078 G8084)))
  G8085)


(LET* ((G8046 :ARG)
       (G8047 :ARG)
       (G8052 (I8+ G8047 11))
       (G8056 (I8= G8046 12))
       (G8084 (I8+ G8046 G8047))
       (G8085 (IF G8056
                  (LET* ((G8061 (I8* G8047 10))
                         (G8066 (I8* G8084 G8061)))
                    G8066)
                  (let* ((G8075 (I8* G8084 G8052)))
                    G8075))))
  G8085)

(funcall 
 (let ((z (+ b 11)))
   (if (= a 12)
       (let ((y (* b 10)))
         (lambda ((x i8)) (* x y)))
       (lambda ((x i8)) (* x z))))
 (+ a b))

(defun foo (a b)
  (let* ((g8052 (i8+ b 11))
         (g8084 (i8+ a b)))
    (if (i8= a 12)
        (let ((g8061 (i8* b 10)))
          (i8* g8084 g8061))
        (i8* g8084 g8052))))




(defun foo (G8094 G8095)
  (LET* ((G8101 (TABLES.LANG::I8+ G8095 11))
         (G8105 (TABLES.LANG::I8= G8094 12))
         (G8135 (TABLES.LANG::I8+ G8094 G8095))
         (G8141 (TABLES.LANG::I8* G8094 G8095))
         (G8136 (IF G8105
                    (LET* ((G8110 (TABLES.LANG::I8* G8095 10))
                           (G8115 (TABLES.LANG::I8* G8135 G8110)))
                      G8115)
                    (LET* ((G8124 (TABLES.LANG::I8* G81350 G8101)))
                      G8124)))
         (G8142 (IF G8105
                    (LET* ((G8110 (TABLES.LANG::I8* G8095 10))
                           (G8115 (TABLES.LANG::I8* X_8111 G8110)))
                      G8115)
                    (LET* ((G8124 (TABLES.LANG::I8* G8141 G8101)))
                      G8124)))
         (G8143 (TABLES.LANG::I8+ G8136 G8142)))
    G8143))

;; with multiple values we could combine ifs to change ^^ into vv
(defun foo (G8094 G8095)
  (LET* ((G8101 (TABLES.LANG::I8+ G8095 11))
         (G8105 (TABLES.LANG::I8= G8094 12))
         (G8135 (TABLES.LANG::I8+ G8094 G8095))
         (G8141 (TABLES.LANG::I8* G8094 G8095))
         ((G8136 G8142)
          (IF G8105
              (LET* ((G8110 (TABLES.LANG::I8* G8095 10))
                     (G8115 (TABLES.LANG::I8* G8135 G8110))
                     (G9115 (TABLES.LANG::I8* X_8111 G8110)))
                (values G8115 G9115))
              (LET* ((G8124 (TABLES.LANG::I8* G8135 G8101))
                     (G9124 (TABLES.LANG::I8* G8141 G8101)))
                (values G8124 G9124))))
         (G8143 (TABLES.LANG::I8+ G8136 G8142)))
    G8143))
