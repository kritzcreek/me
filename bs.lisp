;; hmm...

(defpackage :bs  (:use :common-lisp))
(in-package :bs) (use-package :bs)


;; how to make it accept multiple (var-name var-type) expressions?
;; does &rest rest do this?
(defmacro defunt (fn-name ((var-name var-type) &rest rest) &body body)
  (declare (type var-type var-name)))


;;usage:
(defunt add (x: integer y: integer) -> integer
    (+ x y))

;; expands to:
(def add
    (eval-when (:execute :compile-toplevel :load-toplevel)
      ;; this is for type safety on the body of the function
      ;; also serves to declare the function signature
      ;; in Haskell this would be:
      ;;    add :: Integer -> Integer -> Integer
      (proclaim '(ftype (function (integer integer) integer) add)))
    (lambda (x y)
      (ftype )
      (declare
       ;; this is for type safety on the argument values themselves
       (type integer x)
       (type integer y))
      (+ x y)))