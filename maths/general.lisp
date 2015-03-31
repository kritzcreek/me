(in-package :bs)

(defsection @bs-maths-general (:title "BS manual")
  (mgl-pac asdf:system)
  (@bs-count-stuff))


(defun range (start end)
  "Returns the range of the selected values. end value
  designates a bounding index and is not included in calculation."
  (loop for i from start below end collect i))

(defun square (n)
  "Convenience function for taking a number n to it's second power."
  (expt n 2))

(defun squarep (n)
  "Basic perfect square predicate."
  (= (square (isqrt n)) n))


;;;; This can be moved to something else I guess

(defun fibonacci (n)
  "Naive Fibonacci implementation:

   F_n = F_{n-1} + F_{n-2}, F_1=1, F_2=2"
  (if (< n 2)
      n
      (+ (fibonacci (- n 1))
	 (fibonacci (- n 2)))))

(defun reverse-fibonacci (n)
  "A number n is a Fibonacci number if one or both of 5n^2+4 or 5n^2-4 is a
  perfect square (Gessel, 1972, _The Fibonacci Quarterly_)."
  (let* ((fn  (lambda (x) (* 5 (expt x 2))))
	 (pos (+ (funcall fn n) 4))
	 (neg (- (funcall fn n) 4)))
    (or (squarep pos)
	(squarep neg))))
