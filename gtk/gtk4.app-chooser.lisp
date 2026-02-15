;;; ----------------------------------------------------------------------------
;;; gtk4.app-chooser.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.12 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2013 - 2025 Dieter Kaiser
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
;;; GtkAppChooser
;;;
;;;     Interface implemented by widgets for choosing an application
;;;
;;; Types and Values
;;;
;;;     GtkAppChooser
;;;
;;; Accessors
;;;
;;;     gtk_app_chooser_get_content_type
;;;
;;; Functions
;;;
;;;     gtk_app_chooser_get_app_info
;;;     gtk_app_chooser_refresh
;;;
;;; Properties
;;;
;;;     content-type
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkAppChooser
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkAppChooser
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GtkAppChooser" app-chooser
  (:export t
   :type-initializer "gtk_app_chooser_get_type")
  ((content-type
    app-chooser-content-type
    "content-type" "gchararray" t t)))

#+liber-documentation
(setf (liber:alias-for-class 'app-chooser)
      "Interface"
      (documentation 'app-chooser 'type)
 "@version{2024-04-26}
  @begin{short}
    The @class{gtk:app-chooser} interface is an interface that can be
    implemented by widgets which allow the user to choose an application,
    typically for the purpose of opening a file.
  @end{short}
  The main objects that implement this interface are the
  @class{gtk:app-chooser-widget}, @class{gtk:app-chooser-dialog} and
  @class{gtk:app-chooser-button} widgets.

  Applications are represented by GIO @class{g:app-info} objects here. GIO has
  a concept of recommended and fallback applications for a given content type.
  Recommended applications are those that claim to handle the content type
  itself, while fallback also includes applications that handle a more generic
  content type. GIO also knows the default and last-used application for a
  given content type. The @class{gtk:app-chooser-widget} widget provides
  detailed control over whether the shown list of applications should include
  default, recommended or fallback applications.

  To obtain the application that has been selected in a @class{gtk:app-chooser}
  widget, use the @fun{gtk:app-chooser-app-info} function.
  @begin[Warning]{dictionary}
    The @class{gtk:app-chooser} implementation is deprecated since 4.10. The
    application selection widgets should be implemented according to the design
    of each platform and/or application requiring them.
  @end{dictionary}
  @see-slot{gtk:app-chooser-content-type}
  @see-class{g:app-info}
  @see-class{gtk:app-chooser-widget}
  @see-class{gtk:app-chooser-dialog}
  @see-class{gtk:app-chooser-button}")

#+(and gtk-4-10 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj app-chooser) &key)
  (when gtk-init:*warn-deprecated*
    (warn "GTK:APP-CHOOSER is deprecated since 4.10")))

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "content-type" 'app-chooser) t)
 "The @code{content-type} property of type @code{:string}
  (Read / Write / Construct) @br{}
  The content type of the application chooser. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'app-chooser-content-type)
      "Accessor"
      (documentation 'app-chooser-content-type 'function)
 "@version{2025-08-12}
  @syntax{(gtk:app-chooser-content-type object) => content-type}
  @argument[object]{a @class{gtk:app-chooser} object}
  @argument[content-type]{a string for the content type}
  @begin{short}
    The accessor for the @slot[gtk:app-chooser]{content-type} slot of the
    @class{gtk:app-chooser} interface returns the current value of the content
    type.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:app-chooser} implementation is deprecated since 4.10.
  @end{dictionary}
  @see-class{gtk:app-chooser}")

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_get_app_info
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_app_chooser_get_app_info" app-chooser-app-info)
    (g:object g-app-info)
 #+liber-documentation
 "@version{2024-02-22}
  @argument[object]{a @class{gtk:app-chooser} object}
  @begin{return}
    The @class{g:app-info} object for the currently selected application, or
    @code{nil} if none is selected.
  @end{return}
  @begin{short}
    Returns the currently selected application.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:app-chooser} implementation is deprecated since 4.10.
  @end{dictionary}
  @see-class{gtk:app-chooser}
  @see-class{g:app-info}"
  (object (g:object app-chooser)))

(export 'app-chooser-app-info)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_refresh
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_app_chooser_refresh" app-chooser-refresh) :void
 #+liber-documentation
 "@version{2024-02-22}
  @argument[object]{a @class{gtk:app-chooser} object}
  @short{Reloads the list of applications.}
  @begin[Warning]{dictionary}
    The @class{gtk:app-chooser} implementation is deprecated since 4.10.
  @end{dictionary}
  @see-class{gtk:app-chooser}"
  (object (g:object app-chooser)))

(export 'app-chooser-refresh)

;;; --- End of file gtk4.app-chooser.lisp --------------------------------------
