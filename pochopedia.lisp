;;;; pochopedia.lisp

(defpackage #:pochopedia
  (:use #:cl #:pochopedia.config #:pochopedia.search)
  (:import-from #:site-compiler
                #:compile-all)
  (:import-from #:clack.builder
                #:builder)
  (:import-from #:clack.middleware.static
                #:<clack-middleware-static>)
  (:import-from #:clack.middleware.opensearch
                #:<clack-middleware-opensearch>)
  (:import-from #:ningle
                #:*response* #:route)
  (:export #:compile-db
           #:start-server
           #:stop-server
           #:restart-server))

(in-package #:pochopedia)

(defparameter *css-lit-colors*
  (list
   :white (list
           :fg "#777"
           :bg "#f8f8f8"
           :a-fg "#444"
           :a-bg "#e7e7e7"
           :da "#ddd")
   :black (list
           :fg "#999"
           :bg "#222"
           :a-fg "#fff"
           :a-bg "#080808"
           :da "#444"
           )
   :red (list
         :fg "#fbb"
         :bg "#e00500"
         :a-fg "#fff"
         :a-bg "#ad0400"
         :da "#ad0400")
   :green (list
           :fg "#fff"
           :bg "#29720c" ; "#00ad04"
           :a-fg "#51fe1c" ;"#44be15"
           :a-bg "#21580c"
           :da "#21580c")
   :purple (list
           :fg "#fbf"
           :bg "#661a72"
           :a-fg "#fff"
           :a-bg "#aa2bbe"
           :da "#aa2bbe")))

;;reset cache
(setf site-compiler.document::*schema-cache* (make-hash-table :test 'equal))
(setf site-compiler.document::*doc-cache* (make-hash-table :test 'equal))

(defparameter *app*
  (make-instance 'ningle:<app>))
(defparameter *wrapped-app*
  (builder
   (<clack-middleware-opensearch>
    :title "Pochopedia"
    :description "Die Pochopedia Notendatenbank durchsuchen"
    :html-query "/search?q={searchTerms}"
    :tags '("pochopedia" "posaunenchor" "noten" "notendatenbank"))
   *app*))
(defvar *handler* nil)

(setf (route *app* "/")
      (lambda (params)
        (pochopedia.util:serve-file (rel-path "site/index.html"))))

(setf (route *app* "/testtest")
      (lambda (params)
        "blablabla"))

(setf (route *app* "/search")
      (lambda (params)
        (let ((results (mapcar
                        (lambda (x) (list :this x))
                        (search-index (getf params :|q|)))))
          (cl-emb:execute-emb (rel-path "search.tmpl") :env (list :results results)))))

(setf (route *app* "*")
      (lambda (params)
        (setf (clack.response:status *response*) 404)
        "not found"))

(defun start-server ()
  (unless *handler*
    (setf *handler*
          (clack:clackup
           (if (config :serve-static)
               (builder
                (<clack-middleware-static> :path "/site/" :root (rel-path "site/"))
                (<clack-middleware-static> :path "/static/" :root (rel-path "static/"))
                ;;(<clack-middleware-static> :path "/" :root (rel-path "site/index.html"))
                *wrapped-app*)
               *wrapped-app*)
           :port (config :port)))))

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

(defun update-stylesheet (&optional (time (local-time:now)))
  (let* ((lit-region (lit-colors:get-region time))
         (color (lit-colors:region-color lit-region)))
    (alexandria:write-string-into-file
     (cl-emb:execute-emb (rel-path "static/css/time-dependent.tmpl")
                         :env (cons :color
                                    (cons color
                                          (getf *css-lit-colors* color))))
     (rel-path "static/css/time-dependent.css")
     :if-exists :supersede)))
