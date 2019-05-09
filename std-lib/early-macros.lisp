(in-package :tables.lang)

(define-tables-macro let* (bindings &body body)
  (reduce (lambda (accum b)
            `(let (,b) ,accum))
          (reverse bindings)
          :initial-value (cons 'progn body)))
