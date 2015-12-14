;;;;-*- mode: Lisp -*-


(in-package :cl-user)

(defpackage :bs
  (:use
   :cl
   :cl-ppcre)
  (:export
   :flatten
   :partial
   :str))
(in-package :bs)

;;; Syntax for simpler anonymous functions.

(defun numbered-arg-as-string (arg)
  (cl-ppcre:scan-to-strings "^&\\d+$" (string arg)))

(defun single-arg-as-string (arg)
  (let ((sarg (string arg)))
    (when (string-equal "%" sarg)
      sarg)))

(defun arc-arg-as-string (arg)
  (let ((sarg (string arg)))
    (when (string-equal "_" sarg)
      sarg)))

(defun rest-arg-as-string (arg)
  (let ((sarg (string arg)))
    (when (string-equal "%&" sarg)
      sarg)))

(defun flatten (l)
  "Flattens a list"
  (cond ((null l) l)
        ((atom l) (list l))
        (t (append (flatten (car l))
                   (flatten (cdr l))))))

(defun make-arg-list (predicate delimited-list)
  (labels ((string-list (delimited-list)
             (mapcar (lambda (x)
                       (cond ((symbolp x) (funcall predicate x))
                             ((listp x) (string-list x))))
                     delimited-list)))
    (remove-duplicates (mapcar #'intern
                               (sort (flatten (string-list delimited-list))
                                     #'string-lessp) ;; BUG: if more than 9 numbered arguments are used
                               ))))

(set-macro-character
 #\[
 (lambda (stream char)
   (let* ((sexp      (read-delimited-list #\] stream t))
          (args      (make-arg-list #'numbered-arg-as-string sexp))
          (rest-args (make-arg-list #'rest-arg-as-string sexp))
          (rest-arg  (or (car rest-args) (gensym))))
     (unless args
       (setf args (make-arg-list #'single-arg-as-string sexp)))
     (unless args
       (setf args (make-arg-list #'arc-arg-as-string sexp)))
     `(lambda (,@args &rest ,rest-arg) (identity ,rest-arg) (,@sexp)))))

(set-macro-character #\] (get-macro-character #\)))


;;; Currying support

(defun partial (f &rest args)
  "Currying function"
  (lambda (&rest more-args)
    (apply f (append args more-args))))

;;; Clojure-like `str'

(defmethod to-string (arg)           (string arg))
(defmethod to-string ((arg integer)) (write-to-string arg))

(defun str (&rest args)
  (apply (partial 'concatenate 'string) (mapcar #'to-string args)))
