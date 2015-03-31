;;;; -*- mode: Lisp -*-

(in-package :asdf-user)

(asdf:defsystem #:bs
  :license "MIT"
  :version "0.0.1"
  :name "BS."
  :author "Ben Sima"
  :mailto "bensima@gmail.com"
  :homepage "http://www.bsima.me"
  :description "I maked these."
  :components ((:module "maths"
		:serial t
		:components ((:file "general")
			     (:file "combinatorics"))))
  :depends-on (#:alexandria))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (asdf:find-system '#:bs))))
  (asdf:oos 'asdf:load-op '#:bs-test)
  (funcall (intern (symbol-name '#:test) (find-package '#:bs-test))))
