;;; ----------------------------------------------------------------------------
;;; gtk4.uri-launcher.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2024 Dieter Kaiser
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
 "@version{#2023-10-19}
  @begin{short}
    A @class{gtk:uri-launcher} object collects the arguments that are needed to
    open a URI with an application.
  @end{short}
  Depending on system configuration, user preferences and available APIs, this
  may or may not show an app chooser dialog or launch the default application
  right away.

  The operation is started with the @fun{gtk:uri-launcher-launch} function. This
  API follows the GIO async pattern, and the result can be obtained by calling
  the @fun{gtk:uri-launcher-launch-finish} function.

  To launch a file, use the @class{gtk:file-launcher} object.

  Since 4.10
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
 "@version{#2023-10-12}
  @syntax{(gtk:uri-launcher-uri object) => uri}
  @syntax{(setf (gtk:uri-launcher-uri object) uri)}
  @argument[object]{a @class{gtk:uri-launcher} object}
  @argument[uri]{a string with the URI}
  @begin{short}
    Accessor of the @slot[gtk:uri-launcher]{uri} slot of the
    @class{gtk:uri-launcher} class.
  @end{short}
  The @fun{gtk:uri-launcher-uri} function gets the URI that will be opened.
  The @setf{gtk:uri-launcher-uri} function sets the URI.

  Since 4.10
  @see-class{gtk:uri-launcher}")

;;; ----------------------------------------------------------------------------
;;; gtk_uri_launcher_new
;;; ----------------------------------------------------------------------------

(declaim (inline uri-launcher-new))

(defun uri-launcher-new (uri)
 #+liber-documentation
 "@version{#2023-10-19}
  @argument[uri]{a string with the URI to open}
  @return{The new @class{gtk:uri-launcher} object.}
  @short{Creates a new @class{gtk:uri-launcher} object.}

  Since 4.10
  @see-class{gtk:uri-launcher}"
  (make-instance 'uri-launcher
                 :uri uri))

(export 'uri-launcher-new)

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
 "@version{#2023-10-19}
  @argument[launcher]{a @class{gtk:uri-launcher} object}
  @argument[parent]{a parent @class{gtk:window} widget}
  @argument[cancellable]{a @class{g:cancellable} object to cancel the operation}
  @argument[func]{a @symbol{g:async-ready-callback} callback function to call
    when the operation is complete}
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

(cffi:defcfun ("gtk_uri_launcher_launch-finish" %uri-launcher-launch-finish)
    :boolean
  (launcher (g:object file-launcher))
  (result (g:object g:async-result))
  (err :pointer))

(defun uri-launcher-launch-finish (launcher result)
 #+liber-documentation
 "@version{#2023-10-19}
  @argument[launcher]{a @class{gtk:uri-launcher} object}
  @argument[result]{a @class{g:async-result} object with the result}
  @return{@em{True} if an application was launched, or @em{false} on error}
  @begin{short}
    Finishes the the @fun{gtk:uri-launcher-launch} function call and returns
    the result.
  @end{short}

  Since 4.10
  @see-class{gtk:uri-launcher}
  @see-class{g:async-result}
  @see-function{gtk:uri-launcher-launch}"
  (glib:with-ignore-g-error (err)
    (%uri-launcher-launch-finish launcher result err)))

(export 'uri-launcher-launch-finish)

;;; --- End of file gtk4.uri-launcher.lisp -------------------------------------
