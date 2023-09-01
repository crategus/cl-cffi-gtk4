;;; ----------------------------------------------------------------------------
;;; gtk4.font-dialog.lisp
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
;;;     GtkFontDialog
;;;
;;; Accessors
;;;
;;;     gtk_font_dialog_get_filter
;;;     gtk_font_dialog_set_filter
;;;     gtk_font_dialog_get_font_map
;;;     gtk_font_dialog_set_font_map
;;;     gtk_font_dialog_get_language
;;;     gtk_font_dialog_set_language
;;;     gtk_font_dialog_get_modal
;;;     gtk_font_dialog_set_modal
;;;     gtk_font_dialog_get_title
;;;     gtk_font_dialog_set_title
;;;
;;; Functions
;;;
;;;     gtk_font_dialog_new
;;;
;;;     gtk_font_dialog_choose_face
;;;     gtk_font_dialog_choose_face_finish
;;;     gtk_font_dialog_choose_family
;;;     gtk_font_dialog_choose_family_finish
;;;     gtk_font_dialog_choose_font
;;;     gtk_font_dialog_choose_font_finish
;;;     gtk_font_dialog_choose_font_and_features
;;;     gtk_font_dialog_choose_font_and_features_finish
;;;
;;; Properties
;;;
;;;     filter
;;;     font-map
;;;     language
;;;     modal
;;;     title
;;;
;;; Hierachy
;;;
;;;     GObject
;;;     ╰── GtkFontDialog
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFontDialog
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkFontDialog" font-dialog
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_font_dialog_get_type")
  ((filter
    font-dialog-filter
    "filter" "GtkFilter" t t)
   (font-map
    font-dialog-font-map
    "font-map" "PangoFontMap" t t)
   (language
    font-dialog-language
    "language" "PangoLanguage" t t)
   (modal
    font-dialog-modal
    "modal" "gboolean" t t)
   (title
    font-dialog-title
    "title" "gchararray" t t)))

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;;Gtk.FontDialog:filter
;;;Sets a filter to restrict what fonts are shown in the font chooser dialog.

;;;since: 4.10

;;;Gtk.FontDialog:font-map
;;;Sets a custom font map to select fonts from.

;;;since: 4.10

;;;Gtk.FontDialog:language
;;;The language for which the font features are selected.

;;;since: 4.10

;;;Gtk.FontDialog:modal
;;;Whether the font chooser dialog is modal.

;;;since: 4.10

;;;Gtk.FontDialog:title
;;;A title that may be shown on the font chooser dialog that is presented by gtk_font_dialog_choose_font().

;;;since: 4.10

;;; ----------------------------------------------------------------------------
;;; gtk_font_dialog_new
;;;
;;; Creates a new GtkFontDialog object.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

(declaim (inline font-dialog-new))

(defun font-dialog-new ()
  (make-instance 'font-dialog))

(export 'font-dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_font_dialog_choose_face
;;;
;;; This function initiates a font selection operation by presenting a dialog to
;;; the user for selecting a font face (i.e. a font family and style, but not a
;;; specific font size).
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_dialog_choose_face" %font-dialog-choose-face) :void
  (dialog (g:object font-dialog))
  (parent (g:object window))
  (face (g:object pango:font-face))
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun font-dialog-choose-face (dialog parent face cancellable func)
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%font-dialog-choose-face dialog
                              parent
                              face
                              cancellable
                              (cffi:callback g:async-ready-callback)
                              ptr)))

(export 'font-dialog-choose-face)

;;; ----------------------------------------------------------------------------
;;; gtk_font_dialog_choose_face_finish
;;;
;;; Finishes the gtk_font_dialog_choose_face() call and returns the resulting
;;; font face.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_dialog_choose_face_finish"
               %font-dialog-choose-face-finish) (g:object pango:font-face)
  (dialog (g:object font-dialog))
  (result (g:object g:async-result))
  (err :pointer))

(defun font-dialog-choose-face-finish (dialog result)
  (glib:with-ignore-g-error (err)
    (%font-dialog-choose-face-finish dialog result err)))

(export 'font-dialog-choose-face-finish)

;;; ----------------------------------------------------------------------------
;;; gtk_font_dialog_choose_family
;;;
;;; This function initiates a font selection operation by presenting a dialog to
;;; the user for selecting a font family.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_dialog_choose_family" %font-dialog-choose-family) :void
  (dialog (g:object font-dialog))
  (parent (g:object window))
  (family (g:object pango:font-family))
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun font-dialog-choose-family (dialog parent family cancellable func)
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%font-dialog-choose-family dialog
                                parent
                                family
                                cancellable
                                (cffi:callback g:async-ready-callback)
                                ptr)))

(export 'font-dialog-choose-family)

;;; ----------------------------------------------------------------------------
;;; gtk_font_dialog_choose_family_finish
;;;
;;; Finishes the gtk_font_dialog_choose_family() call and returns the resulting
;;; family.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_dialog_choose_family_finish"
               %font-dialog-choose-family-finish) (g:object pango:font-family)
  (dialog (g:object font-dialog))
  (result (g:object g:async-result))
  (err :pointer))

(defun font-dialog-choose-family-finish (dialog result)
  (glib:with-ignore-g-error (err)
    (%font-dialog-choose-family-finish dialog result err)))

(export 'font-dialog-choose-family-finish)

;;; ----------------------------------------------------------------------------
;;; gtk_font_dialog_choose_font
;;;
;;; This function initiates a font selection operation by presenting a dialog to
;;; the user for selecting a font.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_dialog_choose_font" %font-dialog-choose-font) :void
  (dialog (g:object font-dialog))
  (parent (g:object window))
  (desc (g:object pango:font-description))
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun font-dialog-choose-font (dialog parent desc cancellable func)
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%font-dialog-choose-font dialog
                              parent
                              desc
                              cancellable
                              (cffi:callback g:async-ready-callback)
                              ptr)))

(export 'font-dialog-choose-font)

;;; ----------------------------------------------------------------------------
;;; gtk_font_dialog_choose_font_finish
;;;
;;; Finishes the gtk_font_dialog_choose_font() call and returns the resulting
;;; font description.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_dialog_choose_font_finish"
               %font-dialog-choose-font-finish)
    (g:object pango:font-description)
  (dialog (g:object font-dialog))
  (result (g:object g:async-result))
  (err :pointer))

(defun font-dialog-choose-font-finish (dialog result)
  (glib:with-ignore-g-error (err)
    (%font-dialog-choose-font-finish dialog result err)))

(export 'font-dialog-choose-font-finish)

;;; ----------------------------------------------------------------------------
;;; gtk_font_dialog_choose_font_and_features
;;;
;;; This function initiates a font selection operation by presenting a dialog to
;;; the user for selecting a font and font features.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_dialog_choose_font_and_features"
               %font-dialog-choose-font-and-features) :void
  (dialog (g:object font-dialog))
  (parent (g:object window))
  (desc (g:object pango:font-description))
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun font-dialog-choose-font-and-features (dialog
                                             parent desc cancellable func)
  (let ((ptr (glib:allocate-stable-pointer func)))
    (%font-dialog-choose-font-and-features
            dialog
            parent
            desc
            cancellable
            (cffi:callback g:async-ready-callback)
            ptr)))

(export 'font-dialog-choose-font-and-features)

;;; ----------------------------------------------------------------------------
;;; gtk_font_dialog_choose_font_and_features_finish
;;;
;;; Finishes the gtk_font_dialog_choose_font_and_features() call and returns the
;;; resulting font description and font features.
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;; TODO: Check this implementation carefully.

(cffi:defcfun ("gtk_font_dialog_choose_font_and_features_finish"
               %font-dialog-choose-font-and-features-finish) :boolean
  (dialog (g:object font-dialog))
  (result (g:object g:async-result))
  (desc :pointer)
  (features :string)
  (language :pointer)
  (err :pointer))

(defun font-dialog-choose-font-and-features-finish (dialog result)
  (let ((desc (pango:font-description-new))
        (features nil)
        (language (pango:language-default)))
    (glib:with-ignore-g-error (err)
      (when (%font-dialog-choose-font-and-features-finish dialog
                                                          result
                                                          desc
                                                          features
                                                          language
                                                          err))
        (values desc features language))))

(export 'font-dialog-choose-font-and-features-finish)


;;;gtk_font_dialog_get_filter
;;;Returns the filter that decides which fonts to display in the font chooser dialog.

;;;since: 4.10

;;;gtk_font_dialog_get_font_map
;;;Returns the fontmap from which fonts are selected, or NULL for the default fontmap.

;;;since: 4.10

;;;gtk_font_dialog_get_language
;;;Returns the language for which font features are applied.

;;;since: 4.10

;;;gtk_font_dialog_get_modal
;;;Returns whether the font chooser dialog blocks interaction with the parent window while it is presented.

;;;since: 4.10

;;;gtk_font_dialog_get_title
;;;Returns the title that will be shown on the font chooser dialog.

;;;since: 4.10

;;;gtk_font_dialog_set_filter
;;;Adds a filter that decides which fonts to display in the font chooser dialog.

;;;since: 4.10

;;;gtk_font_dialog_set_font_map
;;;Sets the fontmap from which fonts are selected.

;;;since: 4.10

;;;gtk_font_dialog_set_language
;;;Sets the language for which font features are applied.

;;;since: 4.10

;;;gtk_font_dialog_set_modal
;;;Sets whether the font chooser dialog blocks interaction with the parent window while it is presented.

;;;since: 4.10

;;;gtk_font_dialog_set_title
;;;Sets the title that will be shown on the font chooser dialog.

;;;since: 4.10

;;; --- End of file gtk4.font-dialog.lisp --------------------------------------
