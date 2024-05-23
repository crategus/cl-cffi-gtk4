;;; ----------------------------------------------------------------------------
;;; gtk4.font-chooser-dialog.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2024 Dieter Kaiser
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
;;; GtkFontChooserDialog
;;;
;;;     A dialog for selecting fonts
;;;
;;; Types and Values
;;;
;;;     GtkFontChooserDialog
;;;
;;; Functions
;;;
;;;     gtk_font_chooser_dialog_new ()
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkWindow
;;;                 ╰── GtkDialog
;;;                     ╰── GtkFontChooserDialog
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkNative
;;;     GtkShortcutManager
;;;     GtkRoot
;;;     GtkFontChooser
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFontChooserDialog
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkFontChooserDialog" font-chooser-dialog
  (:superclass dialog
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkNative"
                "GtkRoot"
                "GtkShortcutManager"
                "GtkFontChooser")
   :type-initializer "gtk_font_chooser_dialog_get_type")
  nil)

#+(and gtk-4-10 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj font-chooser-dialog) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:FONT-CHOOSER-DIALOG is deprecated since 4.10")))

#+liber-documentation
(setf (documentation 'font-chooser-dialog 'type)
 "@version{2023-8-28}
  @begin{short}
    The @class{gtk:font-chooser-dialog} widget is a dialog for selecting a font.
  @end{short}
  It implements the @class{gtk:font-chooser} interface.

  @image[font-chooser-dialog]{Figure: GtkFontChooserDialog}
  @begin[GtkFontChooserDialog as GtkBuildable]{dictionary}
    The @class{gtk:font-chooser-dialog} implementation of the
    @class{gtk:buildable} interface exposes the buttons with the names
    @code{select_button} and @code{cancel_button}.
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    The @class{gtk:font-chooser-dialog} implementation has a single CSS node
    with the name @arg{fontchooser} and @code{.fontchooser} style class.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:font-chooser-dialog} implementation is deprecated since 4.10.
    Use the @class{gtk:font-dialog} widget instead.
  @end{dictionary}
  @see-class{gtk:font-chooser}
  @see-class{gtk:dialog}")

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_dialog_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_chooser_dialog_new" font-chooser-dialog-new)
    (g:object widget)
 #+liber-documentation
 "@version{2023-8-28}
  @argument[title]{a string with the title of the dialog, or @code{nil}}
  @argument[parent]{a @class{gtk:window} transient parent of the dialog, or
    @code{nil}}
  @return{The new @class{gtk:font-chooser-dialog} widget.}
  @short{Creates a new font chooser dialog.}
  @begin[Warning]{dictionary}
    The @class{gtk:font-chooser-dialog} implementation is deprecated since 4.10.
    Use the @class{gtk:font-dialog} widget instead.
  @end{dictionary}
  @see-class{gtk:font-chooser-dialog}"
  (title :string)
  (parent (g:object window)))

(export 'font-chooser-dialog-new)

;;; --- End of file gtk4.font-chooser-dialog.lisp ------------------------------
