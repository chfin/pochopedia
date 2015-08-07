;;;; pochopedia.lisp

(defpackage #:pochopedia
  (:use #:cl #:pochopedia.config #:pochopedia.search)
  (:import-from #:site-compiler
                #:compile-all)
  (:import-from #:ningle
                #:*response* #:route)
  (:export #:compile-db
           #:compile-styles
           #:start-server
           #:stop-server
           #:restart-server))

(in-package #:pochopedia)

;; helper, until ningle is fixed
(defun get-param (params key)
  (cdr (assoc key params :test #'string=)))

;;reset cache
(setf site-compiler.document::*schema-cache* (make-hash-table :test 'equal))
(setf site-compiler.document::*doc-cache* (make-hash-table :test 'equal))

(defparameter *app*
  (make-instance 'ningle:<app>))
(defparameter *wrapped-app*
  (lack:builder
   (:opensearch
    :title "Pochopedia"
    :description "Die Pochopedia Notendatenbank durchsuchen"
    :html-query (format nil "~a/search?q={searchTerms}" (config :domain-name))
    :image (format nil "~a/static/favicon.ico" (config :domain-name))
    :tags '("pochopedia" "posaunenchor" "noten" "notendatenbank"))
   *app*))
(defvar *handler* nil)

(defvar *stylesheet-cron-job*
  (cron:make-cron-job 'compile-styles :step-min 15))

(setf (route *app* "/")
      (lambda (params)
        (pochopedia.util:serve-file (rel-path "site/index.html"))))

(setf (route *app* "/testtest")
      (lambda (params)
        "blablabla"))

(setf (route *app* "/search")
      (lambda (params)
        (let ((results (search-index (get-param params "q")))
              (djula:*auto-escape* nil)
              (djula:*current-store* (make-instance 'djula:file-store)))
          (djula:add-template-directory (rel-path "view/"))
          (djula:add-template-directory (rel-path (config :template-path)))
          (with-output-to-string (stream)
            (djula:render-template* "search.tmpl" stream :results results)))))

(setf (route *app* "*")
      (lambda (params)
        (setf (lack.response:response-status *response*) 404)
        "not found"))

(defun start-server ()
  (unless *handler*
    (setf *handler*
          (clack:clackup
           (if (config :serve-static)
               (lack:builder
                (:static :path "/site/" :root (rel-path "site/"))
                (:static :path "/static/" :root (rel-path "static/"))
                ;;(<lack-middleware-static> :path "/" :root (rel-path "site/index.html"))
                *wrapped-app*)
               *wrapped-app*)
           :port (config :port)
           :server (config :server)))))

(defun stop-server ()
  (clack:stop *handler*)
  (setf *handler* nil))

(defun restart-server ()
  "Restarts the server, iff it is already running."
  (when *handler*
    (print "restarting the server.")
    (stop-server)
    (start-server)))

(defun compile-db ()
  (let ((site-compiler:*data-dir* (rel-path (config :data-path)))
        (site-compiler:*site-dir* (rel-path (config :site-path)))
        (site-compiler:*template-dir* (rel-path (config :template-path)))
        (site-compiler:*schema-dir* (rel-path (config :schema-path)))
        (site-compiler:*base-url* (config :base-url)))
    (compile-all :clear-caches t)
    (fill-search-index)
    t))

(defun compile-styles (&optional (time (local-time:now)))
  (let* ((lit-region (lit-colors:get-region time))
         (color (lit-colors:region-color lit-region)))
    (libsass:sass-file (rel-path "scss/app.scss") (rel-path "static/css/foundation.css")
                       :include-paths (list (rel-path "foundation/scss/")
                                            (rel-path (format nil "scss/~(~a~)/"
                                                              color)))))
  t)
