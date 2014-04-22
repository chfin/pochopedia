;;;; error-pages.lisp

(defpackage #:clack.middleware.error-pages
  (:use #:cl #:clack.response)
  (:export #:<clack-middleware-error-pages>
           #:<page-not-found-error>
           #:error-http-code
           #:print-error-page))

(in-package #:clack.middleware.error-pages)

(defclass <clack-middleware-error-pages> (clack:<middleware>)
  ()
  (:documentation "An error handler middleware for clack.
Handles errors occuring during page loads by providing an error page.
Allows to create custom error pages for certain errors."))

(define-condition <page-not-found-error> (error) ())

(defgeneric print-error-page (error middleware env)
  (:documentation "Handles a given error and returns an error page to be displayed."))

(defgeneric error-http-code (error)
  (:documentation "=> an http error code (integer) according to `error`."))

(defmethod clack:call ((this <clack-middleware-error-pages>) env)
  (handler-case (clack:call-next this env)
    (t (err)
      (finalize (make-response (error-http-code err) '(:content-type "text/html")
                               (print-error-page err this env))))))

(defmethod error-http-code (error)
  500)

(defmethod error-http-code ((err <page-not-found-error>))
  404)

(defun make-html (title body)
  (format nil "<!DOCTYPE html>
<html lang=\"en\">
  <head>
    <meta charset=\"utf-8\" />
    <title>~a</title>
  </head>
  <body>
    ~a
  </body>
</html>" title body))

(defmethod print-error-page (error middleware env)
  (typecase error
    (<page-not-found-error>
     (make-html "404 - Page not found"
                "<h1>Page not found</h1>
    <p>The page you are looking for could not be found.</p>"))
    (t (make-html "500 - Internal Server Error"
                  (format nil "<h1>An error occured</h1>
    <p>
      The following error occured while trying to load this page:
    </p>
    <code>
      ~a
    </code>" error)))))
