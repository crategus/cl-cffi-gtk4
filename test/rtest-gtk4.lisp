(defpackage :gtk-test
  (:use :fiveam :cffi :common-lisp)
  (:export #:run!)
  (:import-from :gobject #:+g-type-invalid+
                         #:+g-type-none+
                         #:+g-type-interface+
                         #:+g-type-char+
                         #:+g-type-uchar+
                         #:+g-type-boolean+
                         #:+g-type-int+
                         #:+g-type-uint+
                         #:+g-type-long+
                         #:+g-type-ulong+
                         #:+g-type-int64+
                         #:+g-type-uint64+
                         #:+g-type-enum+
                         #:+g-type-flags+
                         #:+g-type-float+
                         #:+g-type-double+
                         #:+g-type-string+
                         #:+g-type-pointer+
                         #:+g-type-boxed+
                         #:+g-type-param+
                         #:+g-type-object+
                         #:+g-type-gtype+
                         #:+g-type-variant+
                         #:+g-type-checksum+

                         #:define-g-enum
                         #:define-g-flags
                         #:define-g-object-class
                         #:define-g-interface

                         #:get-enum-items
                         #:get-flags-items
                         #:enum-item-name
                         #:enum-item-nick
                         #:enum-item-value
                         #:flags-item-name
                         #:flags-item-nick
                         #:flags-item-value
                         #:get-g-type-definition
                         )
  (:import-from :gio     #:with-g-resource)
  (:import-from :gtk
                         ;; Symbols from gtk.style-provider.lisp
                         #:+gtk-priority-fallback+
                         #:+gtk-priority-theme+
                         #:+gtk-priority-settings+
                         #:+gtk-priority-application+
                         #:+gtk-priority-user+)
  (:import-from :gdk     #:+gdk-event-propagate+
                         #:+gdk-event-stop+))

(in-package :gtk-test)

(defun sys-path (filename &optional (package :cl-cffi-gtk4))
  (let ((system-path (asdf:system-source-directory package)))
    (princ-to-string (merge-pathnames filename system-path))))

(def-suite gtk-tests)
(in-suite gtk-tests)

(def-suite gtk-suite :in gtk-tests)
(in-suite gtk-suite)

;;; 2022-10-21
