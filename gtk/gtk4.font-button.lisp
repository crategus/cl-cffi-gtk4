;;; ----------------------------------------------------------------------------
;;; gtk4.font-button.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.12 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
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
;;;     activate                                           Since 4.4
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
;;; GtkFontButton
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkFontButton" font-button
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
 "@version{2025-07-31}
  @begin{short}
    The @class{gtk:font-button} widget is a button which displays the currently
    selected font and allows to open a font chooser dialog to change the font.
  @end{short}
  It is a suitable widget for selecting a font in a preference dialog.

  @image[font-button]{Figure: GtkFontButton}
  @begin[CSS nodes]{dictionary}
    The @class{gtk:font-button} implementation has a single CSS node with name
    @code{button} and @code{.font} style class.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:font-button} implementation is deprecated since 4.10. Use
    the @class{gtk:font-dialog-button} widget instead.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[font-button::activate]{signal}
      @begin{pre}
lambda (fontbutton)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[fontbutton]{The @class{gtk:font-button} widget that received
          the signal.}
      @end{simple-table}
      Emitted to when the font button is activated. The signal on the
      @class{gtk:font-button} widget is an action signal and emitting it causes
      the button to present its dialog. Since 4.4
    @end{signal}
    @begin[font-button::font-set]{signal}
      @begin{pre}
lambda (fontbutton)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[fontbutton]{The @class{gtk:font-button} widget that received
          the signal.}
      @end{simple-table}
      The signal is emitted when the user selects a font. When handling this
      signal, use the @fun{gtk:font-chooser-font} function to find out which
      font was just selected. Note that this signal is only emitted when the
      user changes the font. If you need to react to programmatic font changes
      as well, use the @sig[g:object]{notify::font-name} signal.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:font-button-new}
  @see-constructor{gtk:font-button-new-with-font}
  @see-slot{gtk:font-button-modal}
  @see-slot{gtk:font-button-title}
  @see-slot{gtk:font-button-use-font}
  @see-slot{gtk:font-button-use-size}
  @see-class{gtk:font-chooser-dialog}
  @see-class{gtk:color-button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:font-button-modal --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "modal" 'font-button) t)
 "The @code{font-name} property of type @code{:boolean} (Read / Write) @br{}
  Whether the dialog is modal. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'font-button-modal)
      "Accessor"
      (documentation 'font-button-modal 'function)
 "@version{2025-09-29}
  @syntax{(gtk:font-button-modal object) => modal}
  @syntax{(setf (gtk:font-button-modal object) modal)}
  @argument[object]{a @class{gtk:font-button} widget}
  @argument[modal]{a boolean whether the dialog is modal}
  @begin{short}
    The accessor for the @slot[gtk:font-button]{modal} slot of the
    @class{gtk:font-button} class gets or sets whether the dialog is modal.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:font-button} implementation is deprecated since 4.10. Use
    the @class{gtk:font-dialog-button} widget instead.
  @end{dictionary}
  @see-class{gtk:font-button}")

;;; --- gtk:font-button-title --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title" 'font-button) t)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title of the font chooser dialog. @br{}
  Default value: \"Pick a Font\"")

#+liber-documentation
(setf (liber:alias-for-function 'font-button-title)
      "Accessor"
      (documentation 'font-button-title 'function)
 "@version{2025-09-29}
  @syntax{(gtk:font-button-title object) => title}
  @syntax{(setf (gtk:font-button-title object) title)}
  @argument[object]{a @class{gtk:font-button} widget}
  @argument[title]{a string containing the font chooser dialog title}
  @begin{short}
    The accessor for the @slot[gtk:font-button]{title} slot of the
    @class{gtk:font-button} class gets or sets the title of the font chooser
    dialog.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:font-button} implementation is deprecated since 4.10. Use
    the @class{gtk:font-dialog-button} widget instead.
  @end{dictionary}
  @see-class{gtk:font-button}")

;;; --- gtk:font-button-use-font -----------------------------------------------

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
 "@version{2025-09-29}
  @syntax{(gtk:font-button-title object) => use-font}
  @syntax{(setf (gtk:font-button-title object) use-font)}
  @argument[object]{a @class{gtk:font-button} widget}
  @argument[use-font]{if @em{true}, font name will be written using font chosen}
  @begin{short}
    The accessor for the @slot[gtk:font-button]{use-font} slot of the
    @class{gtk:font-button} class gets or sets whether the label will be drawn
    in the selected font.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:font-button} implementation is deprecated since 4.10. Use
    the @class{gtk:font-dialog-button} widget instead.
  @end{dictionary}
  @see-class{gtk:font-button}")

;;; --- gtk:font-button-use-size -----------------------------------------------

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
 "@version{2025-09-29}
  @syntax{(gtk:font-button-use-size object) => use-size}
  @syntax{(setf (gtk:font-button-use-size object) use-size)}
  @argument[object]{a @class{gtk:font-button} widget}
  @argument[use-size]{if @em{true}, the font name will be written using the
    selected size}
  @begin{short}
    The accessor for the @slot[gtk:font-button]{use-size} slot of the
    @class{gtk:font-button} class gets or sets the font name will be written
    using the selected size.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:font-button} implementation is deprecated since 4.10. Use
    the @class{gtk:font-dialog-button} widget instead.
  @end{dictionary}
  @see-class{gtk:font-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_new
;;; ----------------------------------------------------------------------------

(declaim (inline font-button-new))

(defun font-button-new ()
 #+liber-documentation
 "@version{2023-08-28}
  @return{The new @class{gtk:font-button} widget.}
  @short{Creates a new font picker widget.}
  @begin[Warning]{dictionary}
    The @class{gtk:font-button} implementation is deprecated since 4.10. Use
    the @class{gtk:font-dialog-button} widget instead.
  @end{dictionary}
  @see-class{gtk:font-button}"
  (make-instance 'font-button))

(export 'font-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_font_button_new_with_font
;;; ----------------------------------------------------------------------------

(declaim (inline font-button-new-with-font))

(defun font-button-new-with-font (fontname)
 #+liber-documentation
 "@version{2025-07-25}
  @argument[fontname]{a string for the name of the font to display in the font
    chooser dialog}
  @return{The new @class{gtk:font-button} widget.}
  @short{Creates a new font picker widget.}
  @begin[Warning]{dictionary}
    The @class{gtk:font-button} implementation is deprecated since 4.10. Use
    the @class{gtk:font-dialog-button} widget instead.
  @end{dictionary}
  @see-class{gtk:font-button}"
  (make-instance 'font-button
                 :font-name fontname))

(export 'font-button-new-with-font)

;;; --- End of file gtk4.font-button.lisp --------------------------------------
