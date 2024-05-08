;;; ----------------------------------------------------------------------------
;;; gtk4.font-dialog-button.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
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
;;;     GtkFontDialogButton
;;;     GtkFontLevel
;;;
;;; Accessors
;;;
;;;     gtk_font_dialog_button_get_dialog
;;;     gtk_font_dialog_button_set_dialog
;;;     gtk_font_dialog_button_get_font_desc
;;;     gtk_font_dialog_button_set_font_desc
;;;     gtk_font_dialog_button_get_font_features
;;;     gtk_font_dialog_button_set_font_features
;;;     gtk_font_dialog_button_get_language
;;;     gtk_font_dialog_button_set_language
;;;     gtk_font_dialog_button_get_level
;;;     gtk_font_dialog_button_set_level
;;;     gtk_font_dialog_button_get_use_font
;;;     gtk_font_dialog_button_set_use_font
;;;     gtk_font_dialog_button_get_use_size
;;;     gtk_font_dialog_button_set_use_size
;;;
;;; Functions
;;;
;;;     gtk_font_dialog_button_new
;;;
;;; Properties
;;;
;;;     dialog
;;;     font-desc
;;;     font-features
;;;     language
;;;     level
;;;     use-font
;;;     use-size
;;;
;;; Signals
;;;
;;;     activate                                           Since 4.14 unstable
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
;;; GtkFontLevel
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GtkFontLevel" font-level
  (:export t
   :type-initializer "gtk_font_level_get_type")
  (:family 0)
  (:face 1)
  (:font 2)
  (:features 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'font-level)
      "GEnum"
      (liber:symbol-documentation 'font-level)
 "@version{2023-8-27}
  @begin{declaration}
    @begin{pre}
(gobject:define-g-enum \"GtkFontLevel\" font-level
  (:export t
   :type-initializer \"gtk_font_level_get_type\")
  (:family 0)
  (:face 1)
  (:font 2)
  (:features 3))
    @end{pre}
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:family]{Select a font family.}
      @entry[:face]{Select a font face (i.e. a family and a style).}
      @entry[:font]{Select a font (i.e. a face with a size, and possibly font
        variations).}
      @entry[:features]{Select a font and font features.}
    @end{table}
  @end{values}
  @begin{short}
    The level of granularity for the font selection.
  @end{short}
  Depending on this value, the @class{pango:font-description} instance that is
  returned by the @fun{gtk:font-dialog-button-font-desc} function will have
  more or less fields set.

  Since 4.10
  @see-class{gtk:font-dialog-button}
  @see-class{pango:font-description}
  @see-function{gtk:font-dialog-button-font-desc}")

;;; ----------------------------------------------------------------------------
;;; GtkFontDialogButton
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkFontDialogButton" font-dialog-button
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_font_dialog_button_get_type")
  ((dialog
    font-dialog-button-dialog
    "dialog" "GtkFontDialog" t t)
   (font-desc
    font-dialog-button-font-desc
    "font-desc" "PangoFontDesription" t t)
   (font-features
    font-dialog-button-font-features
    "font-features" "gchararray" t t)
   (language
    font-dialog-button-language
    "language" "PangoLanguage" t t)
   (level
    font-dialog-button-level
    "level" "GtkFontLevel" t t)
   (use-font
    font-dialog-button-use-font
    "use-font" "gboolean" t t)
   (use-size
    font-dialog-button-use-size
    "use-size" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'font-dialog-button 'type)
 "@version{#2023-10-19}
  @begin{short}
    The @class{gtk:font-dialog-button} class is wrapped around a
    @class{gtk:font-dialog} object and allows to open a font chooser dialog to
    change the font.
  @end{short}

  @image[font-button]{Figure: GtkFontButton}

  It is the suitable widget for selecting a font in a preference dialog.
  @begin[CSS nodes]{dictionary}
    @begin{pre}
fontbutton
╰── button.font
    ╰── [content]
    @end{pre}
    The @class{gtk:font-dialog-button} widget has a single CSS node with name
    @code{fontbutton} which contains a button node with the @code{.font} style
    class.
  @end{dictionary}

  Since 4.10
  @see-constructor{gtk:font-dialog-button-new}
  @see-slot{gtk:font-dialog-button-dialog}
  @see-slot{gtk:font-dialog-button-font-desc}
  @see-slot{gtk:font-dialog-button-font-features}
  @see-slot{gtk:font-dialog-button-language}
  @see-slot{gtk:font-dialog-button-level}
  @see-slot{gtk:font-dialog-button-use-font}
  @see-slot{gtk:font-dialog-button-use-size}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:font-dialog-button-dialog ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "dialog" 'font-dialog-button) t)
 "The @code{dialog} property of type @class{gtk:font-dialog} (Read / Write)
  @br{}
  The font dialog that contains parameters for the font chooser dialog.")

#+liber-documentation
(setf (liber:alias-for-function 'font-dialog-button-dialog)
      "Accessor"
      (documentation 'font-dialog-button-dialog 'function)
 "@version{#2023-10-19}
  @syntax{(gtk:font-dialog-button-dialog object) => dialog}
  @syntax{(setf (gtk:font-dialog-button-dialog object) dialog)}
  @argument[object]{a @class{gtk:font-dialog-button} object}
  @argument[dialog]{a @class{gtk:font-dialog} object}
  @begin{short}
    Accessor of the @slot[gtk:font-dialog-button]{dialog} slot of the
    @class{gtk:font-dialog-button} class.
  @end{short}
  The @fun{gtk:font-dialog-button-dialog} function returns the font dialog. The
  @setf{gtk:font-dialog-button-dialog} function sets a font dialog to use for
  creating the font chooser dialog that is presented when the user clicks the
  button.

  Since 4.10
  @see-class{gtk:font-dialog-button}")

;;; --- gtk:font-dialog-button-font-desc ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "font-desc"
                                               'font-dialog-button) t)
 "The @code{font-desc} property of type @class{pango:font-description}
  (Read / Write) @br{}
  The selected font. This property can be set to give the button its initial
  font, and it will be updated to reflect the users choice in the font chooser
  dialog. Listen to @code{\"notify::font-desc\"} signal to get informed about
  changes to the buttons font.")

#+liber-documentation
(setf (liber:alias-for-function 'font-dialog-button-font-desc)
      "Accessor"
      (documentation 'font-dialog-button-font-desc 'function)
 "@version{#2023-10-19}
  @syntax{(gtk:font-dialog-button-font-desc object) => desc}
  @syntax{(setf (gtk:font-dialog-button-font-desc object) desc)}
  @argument[object]{a @class{gtk:font-dialog-button} object}
  @argument[desc]{a @class{pango:font-description} instance}
  @begin{short}
    Accessor of the @slot[gtk:font-dialog-button]{font-desc} slot of the
    @class{gtk:font-dialog-button} class.
  @end{short}
  The @fun{gtk:font-dialog-button-font-desc} function returns the font dialog.
  The @setf{gtk:font-dialog-button-font-desc} function sets a font dialog to use
  for creating the font chooser dialog that is presented when the user clicks
  the button.

  Since 4.10
  @see-class{gtk:font-dialog-button}
  @see-class{pango:font-description}")

;;; --- gtk:font-dialog-button-font-features -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "font-features"
                                               'font-dialog-button) t)
 "The @code{font-features} property of type @code{:string} (Read / Write) @br{}
  The selected font features. This property will be updated to reflect the users
  choice in the font chooser dialog. Listen to the
  @code{\"notify::font-features\"} signal to get informed about changes to the
  buttons font features.")

#+liber-documentation
(setf (liber:alias-for-function 'font-dialog-button-font-features)
      "Accessor"
      (documentation 'font-dialog-button-font-features 'function)
 "@version{#2023-10-19}
  @syntax{(gtk:font-dialog-button-font-features object) => features}
  @syntax{(setf (gtk:font-dialog-button-font-features object) features)}
  @argument[object]{a @class{gtk:font-dialog-button} object}
  @argument[features]{a string with the selected font features}
  @begin{short}
    Accessor of the @slot[gtk:font-dialog-button]{font-features} slot of the
    @class{gtk:font-dialog-button} class.
  @end{short}
  The @fun{gtk:font-dialog-button-font-features} function returns the font
  features of the button. The @setf{gtk:font-dialog-button-font-features}
  function sets the font features.

  This function is what should be used to obtain the font features that were
  chosen by the user. To get informed about changes, listen to the
  @code{\"notify::font-features\"} signal.

  Note that the button will only let users choose font features if
  the @slot[gtk:font-dialog-button]{level} property is set to @code{:features}.

  Since 4.10
  @see-class{gtk:font-dialog-button}")

;;; --- gtk:font-dialog-button-language ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "language"
                                               'font-dialog-button) t)
 "The @code{language} property of type @class{pango:language} (Read / Write)
  @br{}
  The selected language for font features. This property will be updated to
  reflect the users choice in the font chooser dialog. Listen to
  @code{\"notify::language\"} to get informed about changes to the buttons
  language.")

#+liber-documentation
(setf (liber:alias-for-function 'font-dialog-button-language)
      "Accessor"
      (documentation 'font-dialog-button-language 'function)
 "@version{#2023-10-19}
  @syntax{(gtk:font-dialog-button-language object) => language}
  @syntax{(setf (gtk:font-dialog-button-language object) language)}
  @argument[object]{a @class{gtk:font-dialog-button} object}
  @argument[language]{a @class{pango:language} instance}
  @begin{short}
    Accessor of the @slot[gtk:font-dialog-button]{language} slot of the
    @class{gtk:font-dialog-button} class.
  @end{short}
  The @fun{gtk:font-dialog-button-language} function returns the language that
  is used for font features. The @setf{gtk:font-dialog-button-language} function
  sets the language.

  Since 4.10
  @see-class{gtk:font-dialog-button}
  @see-class{pango:language}")

;;; --- gtk:font-dialog-button-level -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "level" 'font-dialog-button) t)
 "The @code{level} property of type @symbol{gtk:font-level} (Read / Write) @br{}
  The level of detail for the font chooser dialog. @br{}
  Default value: @code{:font}")

#+liber-documentation
(setf (liber:alias-for-function 'font-dialog-button-level)
      "Accessor"
      (documentation 'font-dialog-button-level 'function)
 "@version{#2023-10-19}
  @syntax{(gtk:font-dialog-button-level object) => level}
  @syntax{(setf (gtk:font-dialog-button-level object) level)}
  @argument[object]{a @class{gtk:font-dialog-button} object}
  @argument[level]{a @symbol{gtk:font-level} value}
  @begin{short}
    Accessor of the @slot[gtk:font-dialog-button]{level} slot of the
    @class{gtk:font-dialog-button} class.
  @end{short}
  The @fun{gtk:font-dialog-button-level} function returns the level of detail
  at which this dialog lets the user select fonts. The
  @setf{gtk:font-dialog-button-level} function sets the level.

  Since 4.10
  @see-class{gtk:font-dialog-button}
  @see-symbol{gtk:font-level}")

;;; --- gtk:font-dialog-button-use-font ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-font"
                                               'font-dialog-button) t)
 "The @code{use-font} property of type @code{:boolean} (Read / Write) @br{}
  Whether the buttons label will be drawn in the selected font. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'font-dialog-button-use-font)
      "Accessor"
      (documentation 'font-dialog-button-use-font 'function)
 "@version{#2023-10-19}
  @syntax{(gtk:font-dialog-button-use-font object) => setting}
  @syntax{(setf (gtk:font-dialog-button-use-font object) setting)}
  @argument[object]{a @class{gtk:font-dialog-button} object}
  @argument[setting]{a boolean whether the buttons label will be drawn in the
    selected font}
  @begin{short}
    Accessor of the @slot[gtk:font-dialog-button]{use-font} slot of the
    @class{gtk:font-dialog-button} class.
  @end{short}
  The @fun{gtk:font-dialog-button-use-font} function returns whether the
  selected font is used in the label. The @setf{gtk:font-dialog-button-use-font}
  function sets the property.

  Since 4.10
  @see-class{gtk:font-dialog-button}")

;;; --- gtk:font-dialog-button-use-size ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-size"
                                               'font-dialog-button) t)
 "The @code{use-size} property of type @code{:boolean} (Read / Write) @br{}
  Whether the buttons label will use the selected font size. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'font-dialog-button-use-size)
      "Accessor"
      (documentation 'font-dialog-button-use-size 'function)
 "@version{#2023-10-19}
  @syntax{(gtk:font-dialog-button-use-size object) => setting}
  @syntax{(setf (gtk:font-dialog-button-use-size object) setting)}
  @argument[object]{a @class{gtk:font-dialog-button} object}
  @argument[setting]{a boolean whether the buttons label will use the selected
    font size}
  @begin{short}
    Accessor of the @slot[gtk:font-dialog-button]{use-size} slot of the
    @class{gtk:font-dialog-button} class.
  @end{short}
  The @fun{gtk:font-dialog-button-use-size} function returns whether the
  selected font size is used in the label. The
  @setf{gtk:font-dialog-button-use-size} function sets the property.

  Since 4.10
  @see-class{gtk:font-dialog-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_font_dialog_button_new
;;; ----------------------------------------------------------------------------

(declaim (inline font-dialog-button-new))

(defun font-dialog-button-new (&optional dialog)
 #+liber-documentation
 "@version{#2023-10-19}
  @argument[dialog]{an optional @class{gtk:font-dialog} object to use}
  @begin{short}
    Creates a new @class{gtk:font-dialog-button} widget with the given
    @class{gtk:font-dialog} object.
  @end{short}
  You can pass @code{nil} to this function and set a @class{gtk:font-dialog}
  object later. The button will be insensitive until that happens.

  Since 4.10
  @see-class{gtk:font-dialog-button}
  @see-class{gtk:font-dialog}"
  (make-instance 'font-dialog-button
                 :dialog dialog))

(export 'font-dialog-button-new)

;;; --- End of file gtk4.font-dialog-button.lisp -------------------------------
