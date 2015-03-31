(in-package :bs)

(defsection @bs-maths-combinatorics (:title "BS manual")
  (mgl-pac asdf:system)
  (@bs-natural-numbers))

(defun catalan (n)
  "Catalan numbers form a sequence of natural numbers that conform to or describe
  a large selection of counting problems, such as:

   * the number of Dyck words of length 2n
   * the number of pairs of parantheses
   * the number of full binary trees with n+1 leaves
   * etc.
  
  Formally, it is defined as (in LaTeX):

     C_n=\Pi^{n}_{k=2}\frac{n+k}{k}\text{for}n>=0

  This function takes a single parameter, n, and returns the Catalan number of index
  n within the sequence of all Catalan numbers. To get to the sequence of all Catalan
  numbers *up to* n, just do `(mapcar (lambda (x) (catalan x)) '(1 2 3 ... n))`."
  (if (zerop n)
      1
      (/ (* 2
	    (+ n n -1)
	    (catalan (1- n)))
	 (1+ n))))
