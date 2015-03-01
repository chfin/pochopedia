;;;; config.lisp

(defpackage #:pochopedia.config
  (:use #:cl)
  (:import-from #:envy
                #:config-env-var
                #:defconfig)
  (:export #:config #:rel-path))

(in-package #:pochopedia.config)

(setf (config-env-var) "POCHO_ENV")
(unless (osicat:environment-variable "POCHO_ENV")
  (setf (osicat:environment-variable "POCHO_ENV") "local"))

(defconfig :common
    `(:application-root ,(asdf:system-source-directory "pochopedia")
      :site-path "site/default.html"
      :data-path "pochopedia-content/data/default.yaml"
      :schema-path "pochopedia-content/schema/default.yaml"
      :template-path "pochopedia-content/template/default.tmpl"
      :base-url "/site/"
      :server :hunchentoot))

(defconfig |local|
    `(:port 5001
      :swank-port 5002
      :serve-static t
      :domain-name "http://localhost:5001"))

(defconfig |production|
    `(:port 61819
      :swank-port 61820
      :serve-static t
      :domain-name "http://pochopedia.chfin.de"))

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun rel-path (path)
  (merge-pathnames path (config :application-root)))
