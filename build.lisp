
#+quicklisp (ql:quickload "osicat")
#-asdf (require 'asdf)
#-quicklisp (asdf:load-system "osicat")
(setf (osicat:environment-variable "POCHO_ENV") "production")
#+quicklisp
(progn
  (ql:quickload "pochopedia")
  (ql:quickload "trivial-dump-core")
  (ql:quickload "swank")
  (ql:quickload "bordeaux-threads"))
#-quicklisp
(progn
  (asdf:load-system "pochopedia")
  (asdf:load-system "trivial-dump-core")
  (asdf:load-system "swank")
  (asdf:load-system "bordeaux-threads"))

(defpackage #:pochopedia.executable
  (:use #:cl))

(in-package #:pochopedia.executable)

(defun run ()
  (setf (osicat:environment-variable "POCHO_ENV") "production")
  (pochopedia:compile-db)
  (pochopedia:compile-styles)
  (pochopedia:start-server)
  (cron:start-cron)
  (let ((env-port (osicat:environment-variable "POCHO_SWANK_PORT")))
    (swank:create-server :port (or (and env-port
                                        (parse-integer env-port))
                                   (pochopedia.config:config :swank-port)
                                   swank::default-server-port)
                         :dont-close t))
  (mapcar #'bt:join-thread (bt:all-threads)))

(trivial-dump-core:save-executable "pochopedia" #'run)
