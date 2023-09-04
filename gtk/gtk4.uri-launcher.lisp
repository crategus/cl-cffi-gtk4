;;; ----------------------------------------------------------------------------
;;; gtk4.uri-launcher.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 Dieter Kaiser
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------
;;;
;;; Types and Values
;;;
;;;     GtkUriLauncher
;;;
;;; Accessors
;;;
;;;     gtk_uri_launcher_get_uri
;;;     gtk_uri_launcher_set_uri
;;;
;;; Functions
;;;
;;;     gtk_uri_launcher_uri
;;;     gtk_uri_launcher_launch
;;;     gtk_uri_launcher_launch_finish
;;;
;;; Properties
;;;
;;;     uri
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkUriLauncher
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkUriLauncher

;;;final class Gtk.UriLauncher : GObject.Object
;;;{
;;;  /* No available fields */
;;;}
;;;A GtkUriLauncher object collects the arguments that are needed to open a uri with an application.

;;;Depending on system configuration, user preferences and available APIs, this may or may not show an app chooser dialog or launch the default application right away.

;;;The operation is started with the gtk_uri_launcher_launch() function. This API follows the GIO async pattern, and the result can be obtained by calling gtk_uri_launcher_launch_finish().

;;;To launch a file, use GtkFileLauncher.

;;;Available since: 4.10
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkUriLauncher" uri-launcher
  (:superclass g:object
   :export t
   :interfaces ()
   :type-initializer "gtk_uri_launcher_get_type")
  ((uri
    uri-launcher-uri
    "uri" "gchararray" t t)))

;;; ----------------------------------------------------------------------------
;;; gtk_uri_launcher_set_uri
;;;
;;; Sets the uri that will be opened.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ---------------------------------------------------------------------------- 

;;; ----------------------------------------------------------------------------
;;; Gtk.UriLauncher:uri
;;;
;;; The uri to launch.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_uri_launcher_get_uri
;;;
;;; Gets the uri that will be opened.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_uri_launcher_set_uri
;;;
;;; Sets the uri that will be opened.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; gtk_uri_launcher_new
;;;
;;; Creates a new GtkUriLauncher object.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

(declaim (inline uri-launcher-new))

(defun uri-launcher-new (uri)
  (make-instance 'uri-launcher
                 :uri uri))

(export 'uri-launcher-new)

;;; ----------------------------------------------------------------------------
;;; gtk_uri_launcher_launch
;;;
;;; Launch an application to open the uri.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_uri_launcher_launch" %uri-launcher-launch) :void
  (launcher (g:object uri-launcher))
  (parent (g:object window))
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun uri-launcher-launch (launcher parent cancellable func)
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%uri-launcher-launch launcher
                          parent
                          cancellable
                          (cffi:callback g:async-ready-callback)
                          ptr)))

(export 'uri-launcher-launch)

;;; ----------------------------------------------------------------------------
;;; gtk_uri_launcher_launch_finish
;;;
;;; Finishes the gtk_uri_launcher_launch() call and returns the result.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_uri_launcher_launch-finish" %uri-launcher-launch-finish)
    :boolean
  (launcher (g:object file-launcher))
  (result (g:object g:async-result))
  (err :pointer))

(defun uri-launcher-launch-finish (launcher result)
  (glib:with-ignore-g-error (err)
    (%uri-launcher-launch-finish launcher result err)))

(export 'uri-launcher-launch-finish)

;;; --- End of file gtk4.uri-launcher.lisp -------------------------------------
