;;; ----------------------------------------------------------------------------
;;; gtk4.uri-launcher.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2026 Dieter Kaiser
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
;;;     gtk_uri_launcher_new
;;;     gtk_uri_launcher_can_launch                         Since 4.20
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
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkUriLauncher" uri-launcher
  (:superclass g:object
   :export t
   :interfaces ()
   :type-initializer "gtk_uri_launcher_get_type")
  ((uri
    uri-launcher-uri
    "uri" "gchararray" t t)))

#+liber-documentation
(setf (documentation 'uri-launcher 'type)
 "@version{2026-01-03}
  @begin{short}
    The @class{gtk:uri-launcher} object collects the arguments that are needed
    to open a URI with an application.
  @end{short}
  Depending on system configuration, user preferences and available APIs, this
  may or may not show an app chooser dialog or launch the default application
  right away.

  The operation is started with the @fun{gtk:uri-launcher-launch} function. This
  API follows the GIO async pattern, and the result can be obtained by calling
  the @fun{gtk:uri-launcher-launch-finish} function.

  To launch a file, use the @class{gtk:file-launcher} object.

  Since 4.10
  @begin[Examples]{dictionary}
  The following example shows a signal handler for a button. The signal handler
  retrieves the URI from the label of the button and launches it.
    @begin{pre}
(g:signal-connect picker \"clicked\"
    (lambda (button)
      (let* ((parent (gtk:widget-root button))
             (uri (gtk:button-label button))
             (launcher (gtk:uri-launcher-new uri)))
        (gtk:uri-launcher-launch launcher parent nil
            (lambda (source result)
              (let ((success (gtk:uri-launcher-launch-finish source result)))
                (if success
                    (format t \"URI ~a successfully launched.~%\" uri)
                    (format t \"URI ~a not launched.~%\" uri))))))))
    @end{pre}
  @end{dictionary}
  @see-constructor{gtk:uri-launcher-new}
  @see-slot{gtk:uri-launcher-uri}
  @see-class{gtk:file-launcher}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:uri-launcher-uri ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "uri" 'uri-launcher) t)
 "The @code{uri} property of type @code{:string} (Read / Write) @br{}
  The URI to launch.")

#+liber-documentation
(setf (liber:alias-for-function 'uri-launcher-uri)
      "Accessor"
      (documentation 'uri-launcher-uri 'function)
 "@version{2026-01-03}
  @syntax{(gtk:uri-launcher-uri object) => uri}
  @syntax{(setf (gtk:uri-launcher-uri object) uri)}
  @argument[object]{a @class{gtk:uri-launcher} object}
  @argument[uri]{a string for the URI}
  @begin{short}
    The accessor for the @slot[gtk:uri-launcher]{uri} slot of the
    @class{gtk:uri-launcher} class gets or sets the URI that will be opened.
  @end{short}

  Since 4.10
  @see-class{gtk:uri-launcher}")

;;; ----------------------------------------------------------------------------
;;; gtk_uri_launcher_new
;;; ----------------------------------------------------------------------------

(declaim (inline uri-launcher-new))

(defun uri-launcher-new (uri)
 #+liber-documentation
 "@version{2026-01-03}
  @argument[uri]{a string for the URI to open, can be @code{nil}}
  @return{The new @class{gtk:uri-launcher} object.}
  @short{Creates a new @class{gtk:uri-launcher} object.}

  Since 4.10
  @see-class{gtk:uri-launcher}"
  (make-instance 'uri-launcher
                 :uri (if uri uri (cffi:null-pointer))))

(export 'uri-launcher-new)

;;; ----------------------------------------------------------------------------
;;; gtk_uri_launcher_can_launch                             Since 4.20
;;; ----------------------------------------------------------------------------

#+gtk-4-20
(cffi:defcfun ("gtk_uri_launcher_can_launch" uri-launcher-can-launch) :boolean
 #+liber-documentation
 "@version{2026-01-03}
  @argument[launcher]{a @class{gtk:uri-launcher} object}
  @argument[parent]{a @class{gtk:window} parent window, can be @code{nil}}
  @begin{return}
    @em{False} if the launcher is known not to support the URI, @em{true}
    otherwise.
  @end{return}
  @begin{short}
    Returns whether the launcher is likely to succeed in launching an
    application for its URI.
  @end{short}
  This can be used to disable controls that trigger the launcher when they are
  known not to work.

  Since 4.20
  @see-class{gtk:uri-launcher}
  @see-class{gtk:window}"
  (launcher (g:object uri-launcher))
  (parent (g:object window)))

#+gtk-4-20
(export 'uri-launcher-can-launch)

;;; ----------------------------------------------------------------------------
;;; gtk_uri_launcher_launch
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_uri_launcher_launch" %uri-launcher-launch) :void
  (launcher (g:object uri-launcher))
  (parent (g:object window))
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun uri-launcher-launch (launcher parent cancellable func)
 #+liber-documentation
 "@version{2026-01-03}
  @argument[launcher]{a @class{gtk:uri-launcher} object}
  @argument[parent]{a parent @class{gtk:window} widget}
  @argument[cancellable]{a @class{g:cancellable} object to cancel the operation}
  @argument[func]{a @sym{g:async-ready-callback} callback function to call when
    the operation is complete}
  @begin{short}
    Launch an application to open the URI.
  @end{short}
  This may present an app chooser dialog to the user.

  The callback will be called when the operation is completed. It should call
  the @fun{gtk:uri-launcher-launch-finish} function to obtain the result.

  Since 4.10
  @see-class{gtk:uri-launcher}
  @see-class{gtk:window}
  @see-class{g:cancellable}
  @see-symbol{g:async-ready-callback}
  @see-function{gtk:uri-launcher-launch-finish}"
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%uri-launcher-launch launcher
                          parent
                          cancellable
                          (cffi:callback g:async-ready-callback)
                          ptr)))

(export 'uri-launcher-launch)

;;; ----------------------------------------------------------------------------
;;; gtk_uri_launcher_launch_finish
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_uri_launcher_launch_finish" %uri-launcher-launch-finish)
    :boolean
  (launcher (g:object uri-launcher))
  (result (g:object g:async-result))
  (err :pointer))

(defun uri-launcher-launch-finish (launcher result)
 #+liber-documentation
 "@version{2026-01-03}
  @argument[launcher]{a @class{gtk:uri-launcher} object}
  @argument[result]{a @class{g:async-result} object for the result}
  @return{@em{True} if an application was launched, or @em{false} on error.}
  @begin{short}
    Finishes the the @fun{gtk:uri-launcher-launch} function call and returns
    the result.
  @end{short}

  Since 4.10
  @see-class{gtk:uri-launcher}
  @see-class{g:async-result}
  @see-function{gtk:uri-launcher-launch}"
  (glib:with-ignore-error (err)
    (%uri-launcher-launch-finish launcher result err)))

(export 'uri-launcher-launch-finish)

;;; --- End of file gtk4.uri-launcher.lisp -------------------------------------
