(ql:quickload "pochopedia")
(ql:quickload "trivial-dump-core")
(ql:quickload "swank")
(ql:quickload "bordeaux-threads")

(defpackage #:pochopedia.executable
  (:use #:cl))

(in-package #:pochopedia.executable)

(defun run ()
  (setf (osicat:environment-variable "POCHO_ENV") "production")
  (pochopedia:compile-db)
  (pochopedia:start-server)
  (let ((env-port (osicat:environment-variable "POCHO_SWANK_PORT")))
    (swank:create-server :port (or (and env-port
                                        (parse-integer env-port))
                                   (pochopedia.config:config :swank-port)
                                   swank::default-server-port)
                         :dont-close t))
  (mapcar #'bt:join-thread (bt:all-threads)))

(trivial-dump-core:save-executable "pochopedia" #'run)
