;;; gtk4-application.lisp

(defpackage :gtk4-application
  (:use :common-lisp)
  (:export #:application-cmdline
           #:application-inhibit
           #:application-menubar
           #:application-notification
           #:application-simple
           #:application-resources
           #:application-register
           #:sunny
  ))

(in-package :gtk4-application)

;;; 2024-5-24
