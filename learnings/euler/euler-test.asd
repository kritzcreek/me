#|
  This file is a part of euler project.
  Copyright (c) 2014 Ben Sima (bensima@gmail.com)
|#

(in-package :cl-user)
(defpackage euler-test-asd
  (:use :cl :asdf))
(in-package :euler-test-asd)

(defsystem euler-test
  :author "Ben Sima"
  :license ""
  :depends-on (:euler
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:test-file "euler"))))

  :defsystem-depends-on (:cl-test-more)
  :perform (test-op :after (op c)
                    (funcall (intern #. (string :run-test-system) :cl-test-more)
                             c)
                    (asdf:clear-system c)))
