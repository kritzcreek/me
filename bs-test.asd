;;; -*- mode: Lisp -*-

(asdf:defsystem #:bs.test
  :name "Some BS tests."
  :author "Ben Sima"
  :license "MIT"
  :components ((:module "test"
		:serial t
		:components ((:file "maths"))))
  :depends-on (#:bs))
