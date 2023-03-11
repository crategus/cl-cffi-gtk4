;;; gtk4-application.lisp

(defpackage :gtk4-application
  (:use :common-lisp)
  (:import-from :gtk #:cl-cffi-gtk-build-info)
  (:export #:application-cmdline
           #:application-inhibit
           #:application-menu
           #:application-notification
           #:application-simple
  ))

(in-package :gtk4-application)

(defun sys-path (filename)
  (let ((system-path (asdf:system-source-directory :gtk4-application)))
    (princ-to-string (merge-pathnames filename system-path))))

;;; 2022-11-25
