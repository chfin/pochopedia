;;;; util.lisp

(defpackage #:pochopedia.util
  (:use #:cl)
  (:export #:serve-file))

(in-package #:pochopedia.util)

(defun serve-file (file &optional (encoding "utf-8"))
  (clack:call (make-instance 'clack.app.file:<clack-app-file>
                             :root (make-pathname :directory (pathname-directory file))
                             :file (make-pathname :name (pathname-name file)
                                                  :type (pathname-type file))
                             :encoding encoding)
              nil))
