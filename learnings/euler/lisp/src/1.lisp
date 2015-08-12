(in-package :cl-user)
(defpackage one
  (:use :cl))
(in-package :one)

;; Multiples of 3 and 5
;;
;; Problem 1
;;
;; If we list all the natural numbers below 10 that are multiples of 3 or 5, we
;; get 3, 5, 6 and 9. The sum of these multiples is 23.
;;
;;Find the sum of all the multiples of 3 or 5 below 1000.

;; don't actually need this function but I wrote it
;; while working so might as well keep it
(defun range (start end)
  "Returns the range of the selected values. end value
designates a bounding index and is not included in calculation."
  (loop for i from start below end collect i))

;; As a loop
(defun collector (n)
  (if (or (= 0 (mod n 5))
          (= 0 (mod n 3)))
      n
      0))

(defun one (n)
  "Returns the sum of all multiples of 3 or 5 < n which satisfy c."
  (reduce #'+ (loop for i from 0 to n collect (collector i))))

(one 999)
