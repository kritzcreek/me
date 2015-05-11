;; The number two in lambda calculus (l = lambda):
;;
;; (((lx(ly(lz((zx)y))))(lx(lyy)))(((lx(ly(lz((zx)y))))(lx(lyy)))(lxx)))
;;
;; The number two in common lisp:

(((lambda (x)
    (lambda (y)
      (lambda (z)
        ((z x) y))))
  (lambda (x)
    (lambda (y) y)))
 (((lambda (x)
     (lambda (y)
       (lambda (z)
         ((z x) y))))
   (lambda (x)
     (lambda (y) y)))
  (lambda (x) x)))

;; This doesn't actually compile...
