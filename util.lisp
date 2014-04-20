;;;; util.lisp

(defpackage #:pochopedia.util
  (:use #:cl #:pochopedia.config)
  (:export #:serve-file
           #:id-to-url
           #:id-to-yaml-fn))

(in-package #:pochopedia.util)

(defun serve-file (file &optional (encoding "utf-8"))
  (clack:call (make-instance 'clack.app.file:<clack-app-file>
                             :root (make-pathname :directory (pathname-directory file))
                             :file (make-pathname :name (pathname-name file)
                                                  :type (pathname-type file))
                             :encoding encoding)
              nil))

(defun id-to-url (id)
  (format nil "~a~a" (config :base-url) (make-pathname :type "html" :defaults id)))

(defun id-to-yaml-fn (id)
  (merge-pathnames id (rel-path (config :data-path))))
