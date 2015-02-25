;;;; build.lisp
#-asdf (require 'asdf)
#+quicklisp (ql:quickload "osicat")
#-quicklisp (asdf:load-system "osicat")
(setf (osicat:environment-variable "POCHO_ENV") "production")
(let ((load-list '("pochopedia"
                   "trivial-dump-core"
                   "bordeaux-threads"
                   "swank")))
  #+quicklisp (mapcar #'ql:quickload load-list)
  #-quicklisp (mapcar #'asdf:load-system load-list))

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
  (write-line "Server is running. To quit, hit <C-c>.")
  (handler-case (mapcar #'bt:join-thread (bt:all-threads))
    #+sbcl(sb-sys:interactive-interrupt ()
            (write-line "Bye!")
            (sb-ext:exit))))

(trivial-dump-core:save-executable "pochopedia" #'run)
