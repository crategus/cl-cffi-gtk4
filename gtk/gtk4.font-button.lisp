;;; ----------------------------------------------------------------------------
;;; gtk.font-button.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
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
;;; GtkFontButton
;;;
;;;     A button to launch a font chooser dialog
;;;
;;; Types and Values
;;;
;;;     GtkFontButton
;;;
;;; Accessors
;;;
;;;     gtk_font_button_set_modal
;;;     gtk_font_button_get_modal
;;;     gtk_font_button_set_title
;;;     gtk_font_button_get_title
;;;     gtk_font_button_set_use_font
;;;     gtk_font_button_get_use_font
;;;     gtk_font_button_set_use_size
;;;     gtk_font_button_get_use_size
;;;
;;; Functions
;;;
;;;     gtk_font_button_new
;;;     gtk_font_button_new_with_font
;;;
;;; Properties
;;;
;;;     modal
;;;     title
;;;     use-font
;;;     use-size
;;;
;;; Signals
;;;
;;;     font-set
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkFontButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkFontChooser
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFontButton
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFontButton" font-button
  (:superclass widget
    :export t
    :interfaces ("GtkAccessible"
                 "GtkBuildable"
                 "GtkConstraintTarget"
                 "GtkFontChooser")
    :type-initializer "gtk_font_button_get_type")
  ((modal
    font-button-modal
    "modal" "gboolean" t t)
   (title
    font-button-title
    "title" "gchararray" t t)
   (use-font
    font-button-use-font
    "use-font" "gboolean" t t)
   (use-size
    font-button-use-size
    "use-size" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'font-button 'type)
 "@version{#2021-2-11}
  @begin{short}
    The @sym{gtk:font-button} widget is a button which displays the currently
    selected font and allows to open a font chooser dialog to change the font.
  @end{short}
  It is a suitable widget for selecting a font in a preference dialog.

  @image[font-button]{}
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:font-button} implementation has a single CSS node with name
    @code{button} and @code{.font} style class.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"font-set\" signal}
      @begin{pre}
lambda (widget)    :run-first
      @end{pre}
      The signal is emitted when the user selects a font. When handling this
      signal, use the @fun{gtk:font-chooser-font} function to find out which
      font was just selected. Note that this signal is only emitted when the
      user changes the font. If you need to react to programmatic font changes
      as well, use the \"notify::font-name\" signal.
      @begin[code]{table}
        @entry[widget]{The @sym{gtk:font-button} widget which received the
          signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:font-button-modal}
  @see-slot{gtk:font-button-title}
  @see-slot{gtk:font-button-use-font}
  @see-slot{gtk:font-button-use-size}
  @see-class{gtk:font-chooser-dialog}
  @see-class{gtk:color-button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- font-button-modal --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "modal" 'font-button) t)
 "The @code{font-name} property of type @code{:boolean} (Read / Write) @br{}
  Whether the dialog is modal. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'font-button-modal)
      "Accessor"
      (documentation 'font-button-modal 'function)
 "@version{#2022-6-28}
  @syntax[]{(gtk:font-button-modal object) => modal}
  @syntax[]{(setf (gtk:font-button-modal object) modal)}
  @argument[object]{a @class{gtk:font-button} widget}
  @argument[modal]{a boolean whether the dialog is modal}
  @begin{short}
    Accessor of the @slot[gtk:font-button]{modal} slot of the
    @class{gtk:font-button} class.
  @end{short}

  The @sym{gtk:font-button-modal} function gets whether the dialog is modal. The
  @sym{(setf gtk:font-button-modal)} funtion sets whether the dialog should be
  modal.
  @see-class{gtk:font-button}")

;;; --- font-button-title --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title" 'font-button) t)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title of the font chooser dialog. @br{}
  Default value: \"Pick a Font\"")

#+liber-documentation
(setf (liber:alias-for-function 'font-button-title)
      "Accessor"
      (documentation 'font-button-title 'function)
 "@version{#2020-6-6}
  @syntax[]{(gtk:font-button-title object) => title}
  @syntax[]{(setf (gtk:font-button-title object) title)}
  @argument[object]{a @class{gtk:font-button} widget}
  @argument[title]{a string containing the font chooser dialog title}
  @begin{short}
    Accessor of the @slot[gtk:font-button]{title} slot of the
    @class{gtk:font-button} class.
  @end{short}

  The @sym{gtk:font-button-title} function retrieves the title of the font
  chooser dialog. The @sym{(setf gtk:font-button-title)} function sets the title
  for the font chooser dialog.
  @see-class{gtk:font-button}")

;;; --- font-button-use-font -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-font" 'font-button) t)
 "The @code{use-font} property of type  @code{:boolean} (Read / Write) @br{}
  If this property is set to @em{true}, the label will be drawn in the selected
  font. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'font-button-use-font)
      "Accessor"
      (documentation 'font-button-use-font 'function)
 "@version{#2020-6-6}
  @syntax[]{(gtk:font-button-title object) => use-font}
  @syntax[]{(setf (gtk:font-button-title object) use-font)}
  @argument[object]{a @class{gtk:font-button} widget}
  @argument[use-font]{if @em{true}, font name will be written using font chosen}
  @begin{short}
    Accessor of the @slot[gtk:font-button]{use-font} slot of the
    @class{gtk:font-button} class.
  @end{short}

  If @arg{use-font} is @em{true}, the font name will be written using the
  selected font.
  @see-class{gtk:font-button}")

;;; --- font-button-use-size -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-size" 'font-button) t)
 "The @code{use-size} property of type @code{:boolean} (Read / Write) @br{}
  If this property is set to @em{true}, the label will be drawn with the
  selected font size. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'font-button-use-size)
      "Accessor"
      (documentation 'font-button-use-size 'function)
 "@version{#2020-6-6}
  @syntax[]{(gtk:font-button-use-size object) => use-size}
  @syntax[]{(setf (gtk:font-button-use-size object) use-size)}
  @argument[object]{a @class{gtk:font-button} widget}
  @argument[use-size]{if @em{true}, the font name will be written using the
    selected size}
  @begin{short}
    Accessor of the @slot[gtk:font-button]{use-size} slot of the
    @class{gtk:font-button} class.
  @end{short}

  If @arg{use-size} is @em{true}, the font name will be written using the
  selected size.
  @see-class{gtk:font-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline font-button-new))

(defun font-button-new ()
 #+liber-documentation
 "@version{#2020-6-6}
  @return{A new @class{gtk:font-button} widget.}
  @short{Creates a new font picker widget.}
  @see-class{gtk:font-button}"
  (make-instance 'font-button))

(export 'font-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_new_with_font ()
;;; ----------------------------------------------------------------------------

(declaim (inline font-button-new-with-font))

(defun font-button-new-with-font (fontname)
 #+liber-documentation
 "@version{#2020-6-6}
  @argument[fontname]{a string with the name of the font to display in the font
    chooser dialog}
  @return{A new @class{gtk:font-button} widget.}
  @short{Creates a new font picker widget.}
  @see-class{gtk:font-button}"
  (make-instance 'font-button
                 :font-name fontname))

(export 'font-button-new-with-font)

;;; --- End of file gtk.font-button.lisp ---------------------------------------
