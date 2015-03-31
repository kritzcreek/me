(in-package :bs-test)

(defun test-range ()
  (assert (equal (range 1 5)
		 '(1 2 3 4)))
  (assert (equal (range 0 30)
		 '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28
 29))))

(defun test-catalan ()
  (assert (equal (catalan 1)
		 1))
  (assert (equal (catalan 25)
		 4861946401452)))

(defun test-fibonaccis ()
  (assert (equal (mapcar (lambda (x) (reverse-fibonacci x))
			 (mapcar (lambda (x) (fibonacci x))
				 (range 0 30)))
		 '(T T T T T T T T T T T T T T T T T T T T T T T T T T T T T T))))
