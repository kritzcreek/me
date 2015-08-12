#|
  This file is a part of euler project.
  Copyright (c) 2014 Ben Sima (bensima@gmail.com)
|#

#|
  Author: Ben Sima (bensima@gmail.com)
|#

(in-package :cl-user)
(defpackage euler-asd
  (:use :cl :asdf))
(in-package :euler-asd)

(defsystem euler
  :version "0.1"
  :author "Ben Sima"
  :license ""
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "euler"))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op euler-test))))
