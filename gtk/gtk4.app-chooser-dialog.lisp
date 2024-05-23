;;; ----------------------------------------------------------------------------
;;; gtk4.app-chooser-dialog.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2013 - 2024 Dieter Kaiser
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
;;; GtkAppChooserDialog
;;;
;;;     An application chooser dialog
;;;
;;; Types and Values
;;;
;;;     GtkAppChooserDialog
;;;
;;; Accessors
;;;
;;;     gtk_app_chooser_dialog_set_heading
;;;     gtk_app_chooser_dialog_get_heading
;;;
;;; Functions
;;;
;;;     gtk_app_chooser_dialog_new
;;;     gtk_app_chooser_dialog_new_for_content_type
;;;     gtk_app_chooser_dialog_get_widget
;;;
;;; Properties
;;;
;;;     gfile
;;;     heading
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkWindow
;;;                 ╰── GtkDialog
;;;                     ╰── GtkAppChooserDialog
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkNative
;;;     GtkShortcutManager
;;;     GtkRoot
;;;     GtkAppChooser
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkAppChooserDialog
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkAppChooserDialog" app-chooser-dialog
  (:superclass dialog
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkNative"
                "GtkRoot"
                "GtkShortcutManager"
                "GtkAppChooser")
   :type-initializer "gtk_app_chooser_dialog_get_type")
  ((gfile
    app-chooser-dialog-gfile
    "gfile" "GFile" t t)
   (heading
    app-chooser-dialog-heading
    "heading" "gchararray" t t)))

#+(and gtk-4-10 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj app-chooser-dialog) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:APP-CHOOSER-DIALOG is deprecated since 4.10")))

#+liber-documentation
(setf (documentation 'app-chooser-dialog 'type)
 "@version{2023-8-29}
  @begin{short}
    The @class{gtk:app-chooser-dialog} widget shows a
    @class{gtk:app-chooser-widget} widget inside a @class{gtk:dialog} widget.
  @end{short}

  Note that the @class{gtk:app-chooser-dialog} widget does not have any
  interesting methods of its own. Instead, you should get the embedded
  @class{gtk:app-chooser-widget} class using the
  @fun{gtk:app-chooser-dialog-widget} function and call its methods if the
  @class{gtk:app-chooser} interface is not sufficient for your needs.

  To set the heading that is shown above the @class{gtk:app-chooser-widget}
  widget, use the @fun{gtk:app-chooser-dialog-heading} function.
  @begin[Warning]{dictionary}
    The @class{gtk:app-chooser-dialog} implementation is deprecated since 4.10.
    The application selection widgets should be implemented according to the
    design of each platform and/or application requiring them.
  @end{dictionary}
  @see-constructor{gtk:app-chooser-dialog-new}
  @see-constructor{gtk:app-chooser-dialog-new-for-content-type}
  @see-slot{gtk:app-chooser-dialog-gfile}
  @see-slot{gtk:app-chooser-dialog-heading}
  @see-class{gtk:dialog}
  @see-class{gtk:app-chooser}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:app-chooser-dialog-gfile -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "gfile" 'app-chooser-dialog) t)
 "The @code{gfile} property of type @class{g:file}
  (Read / Write / Construct Only) @br{}
  The @class{g:file} object used by the @class{gtk:app-chooser-dialog} widget.
  The dialog's content type will be guessed from the file, if present.")

#+liber-documentation
(setf (liber:alias-for-function 'app-chooser-dialog-gfile)
      "Accessor"
      (documentation 'app-chooser-dialog-gfile 'function)
 "@version{2024-4-26}
  @syntax{(gtk:app-chooser-dialog-gfile object) => file}
  @syntax{(setf (gtk:app-chooser-dialog-gfile object) file)}
  @argument[object]{a @class{gtk:app-chooser-dialog} widget}
  @argument[file]{a @class{g:file} object}
  @begin{short}
    Accessor of the @slot[gtk:app-chooser-dialog]{gfile} slot of the
    @class{gtk:app-chooser-dialog} class.
  @end{short}
  The @class{g:file} object used by the @class{gtk:app-chooser-dialog} widget.
  The dialog's content type will be guessed from the file, if present.
  @begin[Warning]{dictionary}
    The @class{gtk:app-chooser-dialog} implementation is deprecated since 4.10.
  @end{dictionary}
  @see-class{gtk:app-chooser-dialog}
  @see-class{gtk:app-chooser-widget}
  @see-class{g:file}")

;;; --- gtk:app-chooser-dialog-heading -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "heading" 'app-chooser-dialog) t)
 "The @code{heading} property of type @code{:string} (Read / Write) @br{}
  The text to show at the top of the dialog. The string may contain Pango
  markup. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'app-chooser-dialog-heading)
      "Accessor"
      (documentation 'app-chooser-dialog-heading 'function)
 "@version{2024-4-26}
  @syntax{(gtk:app-chooser-dialog-heading object) => heading}
  @syntax{(setf (gtk:app-chooser-dialog-heading object) heading)}
  @argument[object]{a @class{gtk:app-chooser-dialog} widget}
  @argument[heading]{a string containing Pango markup}
  @begin{short}
    Accessor of the @slot[gtk:app-chooser-dialog]{heading} slot of the
    @class{gtk:app-chooser-dialog} class.
  @end{short}
  The @fun{gtk:app-chooser-dialog-heading} function returns the text to display
  at the top of the dialog. The @setf{gtk:app-chooser-dialog-heading} function
  sets the text to display at the top of the dialog. If the heading is not set,
  the dialog displays a default text.
  @begin[Warning]{dictionary}
    The @class{gtk:app-chooser-dialog} implementation is deprecated since 4.10.
  @end{dictionary}
  @see-class{gtk:app-chooser-dialog}")

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_dialog_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_app_chooser_dialog_new" app-chooser-dialog-new)
    (g:object widget)
 #+liber-documentation
 "@version{#2024-2-22}
  @argument[parent]{a @class{gtk:window}, or @code{nil}}
  @argument[flags]{a @symbol{gtk:dialog-flags} value with the flags for this
    dialog}
  @argument[file]{a @class{g:file} object}
  @return{The newly created @class{gtk:app-chooser-dialog} widget.}
  @begin{short}
    Creates a new application chooser dialog for the provided @class{g:file}
    object, to allow the user to select an application for it.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:app-chooser-dialog} implementation is deprecated since 4.10.
  @end{dictionary}
  @see-class{gtk:app-chooser-dialog}
  @see-class{gtk:window}
  @see-class{g:file}
  @see-symbol{gtk:dialog-flags}"
  (parent (g:object window))
  (flags dialog-flags)
  (file g:object))

(export 'app-chooser-dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_dialog_new_for_content_type
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_app_chooser_dialog_new_for_content_type"
               app-chooser-dialog-new-for-content-type) (g:object widget)
 #+liber-documentation
 "@version{#2024-2-22}
  @argument[parent]{a @class{gtk:window}, or @code{nil}}
  @argument[flags]{a @symbol{gtk:dialog-flags} value with the flags for this
    dialog}
  @argument[content-type]{a content type string}
  @return{The newly created @class{gtk:app-chooser-dialog} widget.}
  @begin{short}
    Creates a new application chooser dialog for the provided content type, to
    allow the user to select an application for it.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:app-chooser-dialog} implementation is deprecated since 4.10.
  @end{dictionary}
  @see-class{gtk:app-chooser-dialog}
  @see-class{gtk:window}
  @see-symbol{gtk:dialog-flags}"
  (parent (g:object gtk:window))
  (flags dialog-flags)
  (content-type :string))

(export 'app-chooser-dialog-new-for-content-type)

;;; ----------------------------------------------------------------------------
;;; gtk_app_chooser_dialog_get_widget
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_app_chooser_dialog_get_widget" app-chooser-dialog-widget)
    (g:object widget)
 #+liber-documentation
 "@version{#2024-4-26}
  @argument[dialog]{a @class{gtk:app-chooser-dialog} widget}
  @return{The @class{gtk:app-chooser-widget} widget of @arg{dialog}.}
  @begin{short}
    Returns the application chooser widget of the dialog.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:app-chooser-dialog} implementation is deprecated since 4.10.
  @end{dictionary}
  @see-class{gtk:app-chooser-dialog}
  @see-class{gtk:app-chooser-widget}"
  (widget (g:object app-chooser-dialog)))

(export 'app-chooser-dialog-widget)

;;; --- End of file gtk4.app-chooser-dialog.lisp -------------------------------
