;;;; About Dialog
;;;;
;;;; The <b><tt>gtk:about-dialog</tt></b> widget offers a simple way to display
;;;; information about a program like its logo, name, copyright, website and
;;;; license. It is also possible to give credits to the authors, documenters,
;;;; translators and artists who have worked on the program. The about dialog is
;;;; typically opened when the user selects the About option from the Help menu.
;;;; All parts of the about dialog are optional.
;;;;
;;;; To make constructing an about dialog as convenient as possible, you can use
;;;; the <b><tt>gtk:show-about-dialog</tt></b> function which constructs and
;;;; shows an about dialog and keeps it around so that it can be shown again.
;;;;
;;;; 2026-03-01

(in-package :gtk4-example)

(defun create-about-dialog (parent)
  (let* ((version (format nil "Runing against GTK ~a" (gtk:version-string)))
         (path (glib-sys:sys-path "resource/gtk-logo.png"))
         (logo (gtk:icon-paintable-new-for-file path 64 1))
         (system (with-output-to-string (stream)
                   (gtk:cl-cffi-gtk-build-info stream))))
    (gtk:show-about-dialog parent
                           :modal t
                           :program-name "GTK Lisp Code Demos"
                           :comments
                           "Program that demonstrates the Lisp API for GTK"
                           :version version
                           :copyright "(C) 2011 - 2026 Dieter Kaiser"
                           :website "http://github.com/crategus/cl-cffi-gtk4"
                           :website-label "Project web site"
                           :license-type :mit-x11
                           :authors '("Dieter Kaiser")
                           :logo logo
                           :system-information system)))
