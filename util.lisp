;;;; util.lisp

(defpackage #:pochopedia.util
  (:use #:cl)
  (:export #:serve-file))

(in-package #:pochopedia.util)

(defun serve-file (file &optional (encoding "utf-8"))
  (funcall (lack.app.file:make-app :root (make-pathname :directory (pathname-directory file))
                                   :file (make-pathname :name (pathname-name file)
                                                  :type (pathname-type file))
                                   :encoding encoding)
           nil))
