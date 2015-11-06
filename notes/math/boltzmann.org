;;; The Bolzmann formula for entropy as a measure of molecular disorder.
;;;
;;; Consider a container partitioned into two equal volumes. The number of ways,
;;; P, in which N molecules can be divided into two groups, N1 and N2, is given by
;;; the simple combinatorial formula
;;;
;;;                     P = N! / N1! N2!
;;;
;;; in which N! = N(N-1)(N-2)...3*2*1. The quantity P is called the "number of
;;; complections" (see Landau and Lifschitz, 1968) [p. 10]
;;;
;;; Here, the difference between N1 and N2 is described by the "ratio" parameter,
;;; which is expected to be a decimal.

(defun factorial (n &optional (acc 1))
  "Naive factorial implementation"
  (if (<= n 1)
      acc
      (factorial (- n 1) (* acc n))))

(defun boltzmann (n ratio)
  "Prigogine, 10"
  (/ (factorial n)
     (* (factorial (* n ratio))
	(factorial (* n (- 1 ratio))))))
