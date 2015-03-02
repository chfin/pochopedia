;;;; pochopedia.asd

(asdf:defsystem #:pochopedia
  :serial t
  :description "Describe pochopedia here"
  :author "Christoph Finkensiep <chfin@freenet.de>"
  :license "MIT/X11"
  :depends-on (#:alexandria
               #:libsass
               #:ningle
               #:clack-handler-hunchentoot
               #:ev-liturgical-colors
               #:envy
               #:osicat
               #:site-compiler
               #:montezuma
               #:cl-cron)
  :components ((:file "opensearch")
               (:file "util")
               (:file "config")
               (:file "search")
               (:file "pochopedia")))

