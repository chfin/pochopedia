;;;; pochopedia.asd

(asdf:defsystem #:pochopedia
  :serial t
  :description "Describe pochopedia here"
  :author "Christoph Finkensiep <chfin@freenet.de>"
  :license "MIT/X11"
  :depends-on (#:alexandria
               #:ev-liturgical-colors
               #:ningle
               #:clack-handler-hunchentoot
               #:site-compiler
               #:envy
               #:osicat
               #:montezuma)
  :components ((:file "opensearch")
               (:file "util")
               (:file "config")
               (:file "search")
               (:file "pochopedia")))

