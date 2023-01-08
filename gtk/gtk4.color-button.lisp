;;; ----------------------------------------------------------------------------
;;; gtk.color-button.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2022 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; GtkColorButton
;;;
;;;     A button to launch a color chooser dialog
;;;
;;; Types and Values
;;;
;;;     GtkColorButton
;;;
;;; Accessors
;;;
;;;     gtk_color_button_set_modal
;;;     gtk_color_button_get_modal
;;;     gtk_color_button_set_title
;;;     gtk_color_button_get_title
;;;
;;; Functions
;;;
;;;     gtk_color_button_new
;;;     gtk_color_button_new_with_rgba
;;;
;;; Properties
;;;
;;;     modal
;;;     show-editor
;;;     title
;;;
;;; Signals
;;;
;;;     activate                                           Since 4.4
;;;     color-set
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkColorButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkColorChooser
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkColorButton
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkColorButton" color-button
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkColorChooser")
   :type-initializer "gtk_color_button_get_type")
  ((modal
    color-button-modal
    "modal" "gboolean" t t)
   (show-editor
    color-button-show-editor
    "show-editor" "gboolean" t t)
   (title
    color-button-title
    "title" "gchararray" t t)))

#+liber-documentation
(setf (documentation 'color-button 'type)
 "@version{#2022-8-27}
  @begin{short}
    The @sym{gtk:color-button} widget is a button which displays the currently
    selected color and allows to open a color chooser dialog to change the
    color.
  @end{short}
  It is a suitable widget for selecting a color in a preference dialog.

  @image[color-button]{}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
colorbutton
╰── button.color
    ╰── [content]
    @end{pre}
    The @sym{gtk:color-button} implementation has a single CSS node with name
    @code{colorbutton} which contains a button node. To differentiate it from
    a plain @class{gtk:button} widget, it gets the @code{.color} style class.
  @end{dictionary}
  @begin[Example]{dictionary}
    The example shows a color button. The button is initialized with the color
    \"Blue\". The handler for the \"color-set\" signal prints the selected
    color on the console.
    @begin{pre}
(defun do-color-button (&optional application)
  (let* ((button (make-instance 'gtk:color-button
                                 :rgba (gdk:rgba-parse \"Blue\")
                                 :title \"Choose a color from the palette\"
                                 :margin-top 48
                                 :margin-bottom 48
                                 :margin-start 48
                                 :margin-end 48))
         (window (make-instance 'gtk:window
                                 :title \"Color Button\"
                                 :application application
                                 :child button
                                 :default-width 270
                                 :default-height 210)))
    (g-signal-connect button \"color-set\"
        (lambda (widget)
          (let ((rgba (gtk:color-chooser-rgba widget)))
            (format t \"Selected color is ~a~%\" (gdk:rgba-to-string rgba)))))
    (gtk:widget-show window)))
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
lambda (button)    :run-first
      @end{pre}
      The signal on the @sym{gtk:color-button} widget is an action signal and
      emitting it causes the button to pop up its color chooser dialog.
      Since 4.4
      @begin[code]{table}
        @entry[button]{The @sym{gtk:color-button} widget which received the
          signal.}
      @end{table}
    @subheading{The \"color-set\" signal}
      @begin{pre}
lambda (button)    :run-first
      @end{pre}
      The signal is emitted when the user selects a color. When handling this
      signal, use the @fun{gtk:color-chooser-rgba} function to find out which
      color was just selected. Note that this signal is only emitted when the
      user changes the color. If you need to react to programmatic color
      changes as well, use the \"notify::color\" signal.
      @begin[code]{table}
        @entry[button]{The @sym{gtk:color-button} widget which received the
          signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:color-button-modal}
  @see-slot{gtk:color-button-show-editor}
  @see-slot{gtk:color-button-title}
  @see-constructor{gtk:color-button-new}
  @see-constructor{gtk:color-button-new-with-rgba}
  @see-class{gtk:color-chooser}
  @see-class{gtk:color-chooser-dialog}
  @see-class{gtk:font-button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- color-button-modal -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "modal" 'color-button) t)
 "The @code{modal} property of type @code{:boolean} (Read / Write) @br{}
  Whether the color chooser dialog is modal. @br{}
  Default value: @em{true}")

#+ liber-documentation
(setf (liber:alias-for-function 'color-button-modal)
      "Accessor"
      (documentation 'color-button-modal 'function)
 "@version{#2022-8-27}
  @syntax[]{(gtk:color-button-modal object) => modal)}
  @syntax[]{(setf (gtk:color-button-modal object) modal)}
  @argument[object]{a @class{gtk:color-button} widget}
  @argument[modal]{a boolean whether the color chooser dialog is modal}
  @begin{short}
    Accessor of the @slot[gtk:color-button]{modal} slot of the
    @class{gtk:color-button} class.
  @end{short}
  The @sym{gtk:color-button-modal} function sets whether the color chooser
  dialog is modal. The @sym{(setf gtk:color-button)} function sets the property.
  @see-class{gtk:color-button}")

;;; --- color-button-show-editor -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-editor"
                                               'color-button) t)
 "The @code{show-editor} property of type @code{:boolean} (Read / Write) @br{}
  Set the property to @em{true} to skip the palette in the color chooser dialog
  and go directly to the color editor. This property should be used in cases
  where the palette in the editor would be redundant, such as when the color
  button is already part of a palette. @br{}
  Default value: @em{false}")

#+ liber-documentation
(setf (liber:alias-for-function 'color-button-show-editor)
      "Accessor"
      (documentation 'color-button-show-editor 'function)
 "@version{#2022-8-27}
  @syntax[]{(gtk:color-button-show-editor object) => show-editor)}
  @syntax[]{(setf (gtk:color-button-show-editor object) show-editor)}
  @argument[object]{a @class{gtk:color-button} widget}
  @argument[show-editor]{a boolean whether to skip the palette in the color
    chooser dialog}
  @begin{short}
    Accessor of the @slot[gtk:color-button]{show-editor} slot of the
    @class{gtk:color-button} class.
  @end{short}
  Set this property to @em{true} to skip the palette in the color chooser
  dialog and go directly to the color editor. This property should be used in
  cases where the palette in the editor would be redundant, such as when the
  color button is already part of a palette.
  @see-class{gtk:color-button}")

;;; --- color-button-title -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title" 'color-button) t)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title of the color chooser dialog. @br{}
  Default value: \"Pick a Color\"")

#+liber-documentation
(setf (liber:alias-for-function 'color-button-title)
      "Accessor"
      (documentation 'color-button-title 'function)
 "@version{#2022-8-27}
  @syntax[]{(gtk:color-button-title object) => title)}
  @syntax[]{(setf (gtk:color-button-title object) title)}
  @argument[object]{a @class{gtk:color-button} widget}
  @argument[title]{a string containing the title of the color chooser dialog}
  @begin{short}
    Accessor of the @slot[gtk:color-button]{title} slot of the
    @class{gtk:color-button} class.
  @end{short}
  The @sym{gtk:color-button-title} function gets the title of the color
  chooser dialog. The @sym{(setf gtk:color-button-title)} function sets the
  title.
  @see-class{gtk:color-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline color-button-new))

(defun color-button-new ()
 #+liber-documentation
 "@version{#2022-8-27}
  @return{A new @class{gtk:color-button} widget.}
  @begin{short}
    Creates a new color button.
  @end{short}
  This returns a widget in the form of a small button containing a swatch
  representing the currently selected color. When the button is clicked, a
  color chooser dialog will open, allowing the user to select a color. The
  swatch will be updated to reflect the new color when the user finishes.
  @see-class{gtk:color-button}
  @see-function{gtk:color-button-new-with-rgba}"
  (make-instance 'color-button))

(export 'color-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_color_button_new_with_rgba ()
;;; ----------------------------------------------------------------------------

(declaim (inline color-button-new-with-rgba))

(defun color-button-new-with-rgba (rgba)
 #+liber-documentation
 "@version{#2022-8-27}
  @argument[rgba]{a @struct{gdk:rgba} color to set the current color with}
  @return{A new @class{gtk:color-button} widget.}
  @begin{short}
    Creates a new color button with the given RGBA color.
  @end{short}
  This returns a widget in the form of a small button containing a swatch
  representing the currently selected color. When the button is clicked, a
  color chooser dialog will open, allowing the user to select a color. The
  swatch will be updated to reflect the new color when the user finishes.
  @see-class{gtk:color-button}
  @see-struct{gdk:rgba}
  @see-function{gtk:color-button-new}"
  (make-instance 'color-button
                 :rgba rgba))

(export 'color-button-new-with-rgba)

;;; --- End of file gtk.color-button.lisp --------------------------------------
