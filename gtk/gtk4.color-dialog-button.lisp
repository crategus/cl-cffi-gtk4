;;; ----------------------------------------------------------------------------
;;; gtk4.color-dialog-button.lisp
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
;;;     GtkColorDialogButton
;;;
;;; Accessors
;;;
;;;     gtk_color_dialog_button_get_dialog
;;;     gtk_color_dialog_button_set_dialog
;;;     gtk_color_dialog_button_get_rgba
;;;     gtk_color_dialog_button_set_rgba
;;;
;;; Functions
;;;
;;;     gtk_color_dialog_button_new
;;;
;;; Properties
;;;
;;;     dialog
;;;     rgba
;;;
;;; Hierachy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkColorDialogButton
;;;
;;; Implements
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkColorDialogButton
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkColorDialogButton" color-dialog-button
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_color_dialog_button_get_type")
  ((dialog
    color-dialog-button-dialog
    "dialog" "GtkColorDialog" t t)
   (rgba
    color-dialog-button-rgba
    "rgba" "GdkRGBA" t t)))

#+liber-documentation
(setf (documentation 'color-dialog-button 'type)
 "@version{2023-7-28}
  @begin{short}
    The @class{gtk:color-dialog-button} widget is a wrapped around a
    @class{gtk:color-dialog} object and allows to open a color chooser dialog
    to change the color.
  @end{short}

  @image[color-button]{Figure: GtkColorDialogButton}

  It is a suitable widget for selecting a color in a preference dialog.
  @begin[CSS nodes]{dictionary}
    @begin{pre}
colorbutton
╰── button.color
    ╰── [content]
    @end{pre}
    The @class{gtk:color-dialog-button} implementation has a single CSS node
    with name @code{colorbutton} which contains a button node. To differentiate
    it from a plain @class{gtk:button} widget, it gets the @code{.color} style
    class.
  @end{dictionary}
  Since 4.10
  @see-class{gtk:color-dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- color-dialog-button-dialog ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "dialog" 'color-dialog-button) t)
 "The @code{dialog} property of type @class{gtk:color-dialog} (Read / Write)
  @br{}
  The @class{gtk:color-dialog} object that contains parameters for the color
  chooser dialog.")

#+liber-documentation
(setf (liber:alias-for-function 'color-dialog-button-dialog)
      "Accessor"
      (documentation 'color-dialog-button-dialog 'function)
 "@version{2023-7-28}
  @syntax[]{(gtk:color-dialog-button-dialog object) => dialog}
  @syntax[]{(setf (gtk:color-dialog-button-dialog object) dialog)}
  @argument[object]{a @class{gtk:color-dialog-button} object}
  @argument[dialog]{a @class{gtk:color-dialog} object}
  @begin{short}
    Accessor of the @slot[color-dialog-button]{dialog} slot of the
    @class{gtk:color-dialog-button} class.
  @end{short}
  The @fun{gtk:color-dialog-button-dialog} function returns the
  @class{gtk:color-dialog} object. The @setf{gtk:color-dialog-button-dialog}
  function sets a @class{gtk:color-dialog} object to use for creating the color
  chooser dialog that is presented when the user clicks the button.

  Since 4.10
  @see-class{gtk:color-dialog-button}
  @see-class{gtk:color-dialog}")

;;; --- color-dialog-button-rgba -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "rgba" 'color-dialog-button) t)
 "The @code{rgba} property of type @class{gdk:rgba} (Read / Write) @br{}
  The selected color. This property can be set to give the button its initial
  color, and it will be updated to reflect the users choice in the color chooser
  dialog. Listen to the \"notify::rgba\" signal to get informed about changes to
  the buttons color. @br{}
  Default value: @code{#s(gdk:rgba :red 0.0 :green 0.0 :blue 0.0 :alpha 0.0)}")

#+liber-documentation
(setf (liber:alias-for-function 'color-dialog-button-rgba)
      "Accessor"
      (documentation 'color-dialog-button-rgba 'function)
 "@version{2023-7-28}
  @syntax[]{(gtk:color-dialog-button-rgba object) => rgba}
  @syntax[]{(setf (gtk:color-dialog-button-rgba object) rgba)}
  @argument[object]{a @class{gtk:color-dialog-button} object}
  @argument[rgba]{a @class{gdk:rgba} instance}
  @begin{short}
    Accessor of the @slot[color-dialog-button]{rgba} slot of the
    @class{gtk:color-dialog-button} class.
  @end{short}
  The @fun{gtk:color-dialog-button-rgba} function returns the color of the
  button. The @setf{gtk:color-dialog-button-rgba} function sets the color.

  This function is what should be used to obtain the color that was chosen by
  the user. To get informed about changes, listen to \"notify::color\" signal.

  Since 4.10
  @see-class{gtk:color-dialog-button}
  @see-class{gdk:rgba}")

;;; ----------------------------------------------------------------------------
;;; gtk_color_dialog_button_new
;;; ----------------------------------------------------------------------------

(declaim (inline color-dialog-button-new))

(defun color-dialog-button-new (&optional (dialog nil))
 #+liber-documentation
 "@version{2023-7-28}
  @argument[dialog]{an optional @class{gtk:color-dialog} object to use}
  @return{A new @class{gtk:color-dialog-button} widget.}
  @begin{short}
    Creates a new @class{gtk:color-dialog-button} widget with the given
    @arg{dialog}.
  @end{short}
  You can pass @code{nil} to this function and set a @class{gtk:color-dialog}
  object later. The button will be insensitive until that happens.
  @see-class{gtk:color-dialog-button}
  @see-class{gtk:color-dialog}"
  (if dialog
      (make-instance 'color-dialog-button
                     :dialog dialog)
      (make-instance 'color-dialog-button)))

(export 'color-dialog-button-new)

;;; --- End of file gtk4.color-dialog-button.lisp ------------------------------
