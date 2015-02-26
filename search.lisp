;;;; search.lisp

(defpackage #:pochopedia.search
  (:use #:cl)
  (:import-from #:site-compiler.document
                #:document-pathnames
                #:load-document
                #:document-contents)
  (:import-from #:montezuma
                #:add-field
                #:make-field
                #:add-document-to-index
                #:each)
  (:export #:search-index #:fill-search-index))

(in-package #:pochopedia.search)

(defvar *search-index* nil)

(defun fill-search-index ()
  (setf *search-index* (make-instance 'montezuma:index))
  (dolist (docpath (document-pathnames))
    (let* ((mdoc (make-instance 'montezuma:document))
           (doc (load-document docpath))
           (content (document-contents doc)))
      (add-field mdoc (make-field "link" (gethash "_link" content) :index nil))
      (add-field mdoc (make-field "title" (gethash "_link_text" content)))
      (add-field mdoc (make-field "" (gethash "_link_text" content)))
      (add-document-to-index *search-index* mdoc))))

(defun query-index (query)
  (mapcar
   (lambda (score-doc)
     (montezuma:doc score-doc))
   (each (montezuma:search *search-index* query) #'identity)))

(defun resolve-link (doc)
  (montezuma:document-value
   (montezuma:get-document *search-index* doc)
   "link"))

(defun search-index (query)
  (let ((title-results
         (query-index (format nil "title:(~a)" query)))
        full-results)
    (mapcar #'resolve-link title-results)))
