;;;; opensearch.lisp

(defpackage #:lack.middleware.opensearch
  (:use #:cl #:lack.response)
  (:export #:*lack-middleware-opensearch*))

(in-package #:lack.middleware.opensearch)

(defun make-body (title description tags url html-query image)
  (with-output-to-string (xml)
    (format xml "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<OpenSearchDescription xmlns=\"http://a9.com/-/spec/opensearch/1.1/\"
  xmlns:moz=\"http://www.mozilla.org/2006/browser/search/\">
  <InputEncoding>UTF-8</InputEncoding>
  <ShortName>~a</ShortName>
  <Description>~a</Description>"
            title description tags)
    (when tags
      (format xml "~&  <Tags>~{~a~^ ~}</Tags>" tags))
    (when html-query
      (format xml "~&  <Url type=\"text/html\" method=\"GET\" template=\"~a\"/>" html-query))
    (when image
      (format xml "~&  <Image height=\"16\" width=\"16\" type=\"image/x-icon\">~a</Image>"
              image))
    (format xml "~&</OpenSearchDescription>")))

(defparameter *lack-middleware-opensearch*
  (lambda (app &key title description tags (url "/opensearch.xml") html-query image)
    (let ((response (make-response 200
                                   '(:content-type "application/opensearchdescription+xml")
                                   (make-body title description tags url html-query image))))
      (lambda (env)
        (if (equal (getf env :path-info) url)
            response
            (funcall app env))))))
