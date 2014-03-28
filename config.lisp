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
    `(:application-root ,(asdf:system-source-directory "pochopedia")))

(defconfig |local|
    `(:site-path "site/default.html"
      :data-path "data/default.yaml"
      :schema-path "schema/default.yaml"
      :template-path "template/default.tmpl"
      :base-url "/site/"
      :port 5001
      :serve-static t))

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun rel-path (path)
  (merge-pathnames path (config :application-root)))
