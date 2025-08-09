;;; ----------------------------------------------------------------------------
;;; gtk4.color-dialog-button.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2025 Dieter Kaiser
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

(gobject:define-gobject "GtkColorDialogButton" color-dialog-button
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
 "@version{2025-07-24}
  @begin{short}
    The @class{gtk:color-dialog-button} widget is a wrapped around a
    @class{gtk:color-dialog} object and allows to open a color chooser dialog
    to change the color.
  @end{short}

  @image[color-button]{Figure: GtkColorDialogButton}

  It is a suitable widget for selecting a color in a preference dialog.

  Since 4.10
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
  @begin[Signals]{dictionary}
    @begin[color-dialog-button::activate]{signal}
      @begin{pre}
lambda (button)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[button]{The @class{gtk:color-dialog-button} widget that received
          the signal.}
      @end{simple-table}
      Emitted when the color dialog button is activated. The signal is an action
      signal and emitting it causes the button to pop up its dialog. The signal
      can be directly emitted on objects from user code. Since 4.14
    @end{signal}
  @end{dictionary}
  @see-class{gtk:color-dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:color-dialog-button-dialog -----------------------------------------

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
 "@version{2025-08-06}
  @syntax{(gtk:color-dialog-button-dialog object) => dialog}
  @syntax{(setf (gtk:color-dialog-button-dialog object) dialog)}
  @argument[object]{a @class{gtk:color-dialog-button} object}
  @argument[dialog]{a @class{gtk:color-dialog} object}
  @begin{short}
    The accessor for the @slot[color-dialog-button]{dialog} slot of the
    @class{gtk:color-dialog-button} class gets or sets a
    @class{gtk:color-dialog} object to use for creating the color chooser dialog
    that is presented when the user clicks the button.
  @end{short}

  Since 4.10
  @see-class{gtk:color-dialog-button}
  @see-class{gtk:color-dialog}")

;;; --- gtk:color-dialog-button-rgba -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "rgba" 'color-dialog-button) t)
 "The @code{rgba} property of type @class{gdk:rgba} (Read / Write) @br{}
  The selected color. This property can be set to give the button its initial
  color, and it will be updated to reflect the users choice in the color chooser
  dialog. Listen to the @sig[g:object]{notify::rgba} signal to get informed
  about changes to the buttons color. @br{}
  Default value: @code{#s(gdk:rgba :red 0.0 :green 0.0 :blue 0.0 :alpha 0.0)}")

#+liber-documentation
(setf (liber:alias-for-function 'color-dialog-button-rgba)
      "Accessor"
      (documentation 'color-dialog-button-rgba 'function)
 "@version{2025-08-06}
  @syntax{(gtk:color-dialog-button-rgba object) => rgba}
  @syntax{(setf (gtk:color-dialog-button-rgba object) rgba)}
  @argument[object]{a @class{gtk:color-dialog-button} object}
  @argument[rgba]{a @class{gdk:rgba} instance}
  @begin{short}
    The accessor for the @slot[color-dialog-button]{rgba} slot of the
    @class{gtk:color-dialog-button} class gets or sets the color of the button.
  @end{short}

  This function is what should be used to obtain the color that was chosen by
  the user. To get informed about changes, listen to the
  @sig[g:object]{notify::color} signal.

  Since 4.10
  @see-class{gtk:color-dialog-button}
  @see-class{gdk:rgba}")

;;; ----------------------------------------------------------------------------
;;; gtk_color_dialog_button_new
;;; ----------------------------------------------------------------------------

(declaim (inline color-dialog-button-new))

(defun color-dialog-button-new (&optional (dialog nil))
 #+liber-documentation
 "@version{2025-08-06}
  @argument[dialog]{an optional @class{gtk:color-dialog} object to use}
  @return{The new @class{gtk:color-dialog-button} widget.}
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
