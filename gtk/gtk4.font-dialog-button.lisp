;;; ----------------------------------------------------------------------------
;;; gtk4.font-dialog-button.lisp
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
  @begin{short}
    The level of granularity for the font selection.
  @end{short}
  Depending on this value, the @class{pango:font-description} instance that is
  returned by the @fun{gtk:font-dialog-button-font-desc} function will have
  more or less fields set.
  @begin{pre}
(gobject:define-g-enum \"GtkFontLevel\" font-level
  (:export t
   :type-initializer \"gtk_font_level_get_type\")
  (:family 0)
  (:face 1)
  (:font 2)
  (:features 3))
  @end{pre}
  @begin[code]{table}
    @entry[:family]{Select a font family.}
    @entry[:face]{Select a font face (i.e. a family and a style).}
    @entry[:font]{Select a font (i.e. a face with a size, and possibly font
      variations).}
    @entry[:features]{Select a font and font features.}
  @end{table}
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

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;;Gtk.FontDialogButton:dialog
;;;The GtkFontDialog that contains parameters for the font chooser dialog.

;;;since: 4.10

;;;Gtk.FontDialogButton:font-desc
;;;The selected font.

;;;since: 4.10

;;;Gtk.FontDialogButton:font-features
;;;The selected font features.

;;;since: 4.10

;;;Gtk.FontDialogButton:language
;;;The selected language for font features.

;;;since: 4.10

;;;Gtk.FontDialogButton:level
;;;The level of detail for the font chooser dialog.

;;;Gtk.FontDialogButton:use-font
;;;Whether the buttons label will be drawn in the selected font.

;;;Gtk.FontDialogButton:use-size
;;;Whether the buttons label will use the selected font size.


;;;Signals
;;;Gtk.FontDialogButton::activate
;;;Emitted when the font dialog button is activated.

;;;unstable since: 4.14


;;; ----------------------------------------------------------------------------
;;; gtk_font_dialog_button_new
;;;
;;; Creates a new GtkFontDialogButton with the given GtkFontDialog.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

(declaim (inline font-dialog-button-new))

(defun font-dialog-button-new (&optional dialog)
  (make-instance 'font-dialog-button
                 :dialog dialog))

(export 'font-dialog-button-new)

;;; ----------------------------------------------------------------------------
;;;gtk_font_dialog_button_get_dialog
;;;Returns the GtkFontDialog of self.

;;;since: 4.10

;;;gtk_font_dialog_button_get_font_desc
;;;Returns the font of the button.

;;;since: 4.10

;;;gtk_font_dialog_button_get_font_features
;;;Returns the font features of the button.

;;;since: 4.10

;;;gtk_font_dialog_button_get_language
;;;Returns the language that is used for font features.

;;;since: 4.10

;;;gtk_font_dialog_button_get_level
;;;Returns the level of detail at which this dialog lets the user select fonts.

;;;since: 4.10

;;;gtk_font_dialog_button_get_use_font
;;;Returns whether the selected font is used in the label.

;;;since: 4.10

;;;gtk_font_dialog_button_get_use_size
;;;Returns whether the selected font size is used in the label.

;;;since: 4.10

;;;gtk_font_dialog_button_set_dialog
;;;Sets a GtkFontDialog object to use for creating the font chooser dialog that is presented when the user clicks the button.

;;;since: 4.10

;;;gtk_font_dialog_button_set_font_desc
;;;Sets the font of the button.

;;;since: 4.10

;;;gtk_font_dialog_button_set_font_features
;;;Sets the font features of the button.

;;;since: 4.10

;;;gtk_font_dialog_button_set_language
;;;Sets the language to use for font features.

;;;since: 4.10

;;;gtk_font_dialog_button_set_level
;;;Sets the level of detail at which this dialog lets the user select fonts.

;;;since: 4.10

;;;gtk_font_dialog_button_set_use_font
;;;If use_font is TRUE, the font name will be written using the selected font.

;;;since: 4.10

;;;gtk_font_dialog_button_set_use_size
;;;If use_size is TRUE, the font name will be written using the selected font size.

;;;since: 4.10

;;; --- End of file gtk4.font-dialog-button.lisp -------------------------------
