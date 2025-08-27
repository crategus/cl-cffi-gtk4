;;; gtk4-application.lisp

(defpackage :gtk4-application
  (:use :iterate :common-lisp)
  (:export #:application-cmdline
           #:application-inhibit
           #:application-menubar
           #:application-notification
           #:application-simple
           #:application-resources
           #:application-register
           #:sunny
           #:bloatpad
  ))

(in-package :gtk4-application)

;;; 2024-5-27
