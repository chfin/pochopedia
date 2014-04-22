;;;; pochopedia.lisp

(defpackage #:pochopedia
  (:use #:cl #:pochopedia.config #:pochopedia.search #:pochopedia.util
        #:clack.middleware.error-pages)
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
           #:compile-styles
           #:start-server
           #:stop-server
           #:restart-server))

(in-package #:pochopedia)

;;reset cache
(setf site-compiler.document::*schema-cache* (make-hash-table :test 'equal))
(setf site-compiler.document::*doc-cache* (make-hash-table :test 'equal))

(defparameter *app*
  (make-instance 'ningle:<app>))
(defparameter *wrapped-app*
  (builder
   (<clack-middleware-error-pages>)
   (<clack-middleware-opensearch>
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
        (let ((results (mapcar
                        (lambda (x) (list :this x))
                        (search-index (getf params :|q|)))))
          (cl-emb:execute-emb (rel-path "view/search.tmpl") :env (list :results results)))))

(define-condition document-not-found (error)
  ((id :type string
       :initarg :id
       :reader document-not-found-id)))

(defmethod error-http-code ((err document-not-found))
  (declare (ignore err))
  404)

(defmethod print-error-page ((err document-not-found) middleware env)
  (cl-emb:execute-emb (rel-path "view/doc-not-found.tmpl")
                      :env (list :id (document-not-found-id err))))

(setf (route *app* "/edit/*")
      (lambda (params)
        (let* ((id (car (getf params :splat)))
               (fn (id-to-yaml-fn id)))
          (unless (probe-file fn)
            (error 'document-not-found :id id))
          (cl-emb:execute-emb (rel-path "view/edit.tmpl")
                              :env (list :id id :filename fn)))))

(setf (route *app* "/create/*")
      (lambda (params)
        (let* ((id (car (getf params :splat)))
               (fn (id-to-yaml-fn id)))
          (alexandria:write-string-into-file "schema: " fn)
          (clack.response:redirect *response* (format nil "/edit/~a" id)))))

(setf (route *app* "/preview/*")
      (lambda (params)
        (let ((text (getf params :|text|))
              (id (car (getf params :splat))))
          (preview text id))))

(setf (route *app* "/save/*" :method :post)
      (lambda (params)
        (let ((text (getf params :|text|))
              (id (car (getf params :splat))))
          (alexandria:write-string-into-file text (id-to-yaml-fn id)
                                             :if-exists :overwrite)
          (compile-db)
          (clack.response:redirect *response* (id-to-url id)))))

(defmethod print-error-page (<page-not-found-error> middleware env)
  (cl-emb:execute-emb (rel-path "view/404.tmpl")))

(setf (route *app* "*")
      (lambda (params)
        (error '<page-not-found-error>)))

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

(defun preview (string name)
  (let ((site-compiler:*data-dir* (rel-path (config :data-path)))
        (site-compiler:*site-dir* (rel-path (config :site-path)))
        (site-compiler:*template-dir* (rel-path (config :template-path)))
        (site-compiler:*schema-dir* (rel-path (config :schema-path)))
        (site-compiler:*base-url* (config :base-url)))
    (site-compiler:preview-yaml string name)))

(defun compile-styles (&optional (time (local-time:now)))
  (let* ((lit-region (lit-colors:get-region time))
         (color (lit-colors:region-color lit-region)))
    (libsass:sass-file (rel-path "scss/app.scss") (rel-path "static/css/foundation.css")
                       :include-paths (list (rel-path "foundation/scss/")
                                            (rel-path (format nil "scss/~(~a~)/"
                                                              color)))))
  nil)
