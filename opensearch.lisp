;;;; opensearch.lisp

(defpackage #:clack.middleware.opensearch
  (:use #:cl #:clack.response)
  (:export #:<clack-middleware-opensearch>))

(in-package #:clack.middleware.opensearch)

(defclass <clack-middleware-opensearch> (clack:<middleware>)
  ((title :type string
          :initarg :title
          :accessor opensearch-title)
   (description :type string
                :initarg :description
                :accessor opensearch-description)
   (tags :type list
         :initarg :tags
         :initform nil
         :accessor opensearch-tags)
   (url :type string
        :initarg :url
        :initform "/opensearch.xml"
        :accessor opensearch-url)
   (html-query :type (or string nil)
               :initarg :html-query
               :initform nil
               :accessor opensearch-html-query)
   (response :type <response>
             :accessor opensearch-response))
  (:documentation "A middleware to add an /opensearch.xml"))

(defmethod clack:call ((this <clack-middleware-opensearch>) env)
  (if (equal (getf env :path-info) (opensearch-url this))
      (finalize (opensearch-response this))
      (clack:call-next this env)))

(defun make-opensearch-body (opensearch)
  (with-slots (title description tags url html-query) opensearch
    (with-output-to-string (xml)
      (format xml "<?xml version=\"1.0\" encoding=\"UTF-8\"?>
<OpenSearchDescription xmlns=\"http://a9.com/-/spec/opensearch/1.1/\">
  <InputEncoding>UTF-8</InputEncoding>
  <ShortName>~a</ShortName>
  <Description>~a</Description>"
              title description tags)
      (when tags
        (format xml "~&  <Tags>~{~a~^ ~}</Tags>" tags))
      (when html-query
        (format xml "~&  <Url type=\"text/html\" method=\"GET\" template=\"~a\"/>" html-query))
      (format xml "~&</OpenSearchDescription>"))))

(defmethod initialize-instance :after ((this <clack-middleware-opensearch>) &rest initargs)
  (let ((body (make-opensearch-body this)))
    (setf (opensearch-response this)
          (make-response 200 '(:content-type "application/opensearchdescription+xml") body))))
