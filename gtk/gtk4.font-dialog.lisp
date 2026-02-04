;;; ----------------------------------------------------------------------------
;;; gtk4.font-dialog.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2026 Dieter Kaiser
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

(gobject:define-gobject "GtkFontDialog" font-dialog
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

#+liber-documentation
(setf (documentation 'font-dialog 'type)
 "@version{2026-01-03}
  @begin{short}
    The @class{gtk:font-dialog} object collects the arguments that are needed to
    present a font chooser dialog to the user, such as a title for the dialog
    and whether it should be modal.
  @end{short}

  The dialog is shown with the @fun{gtk:font-dialog-choose-font} function or its
  variants. This API follows the GIO async pattern, and the result can be
  obtained by calling the corresponding finish function, such as the
  @fun{gtk:font-dialog-choose-font-finish} function.

  See the @class{gtk:font-dialog-button} widget for a convenient control that
  uses the @class{gtk:font-dialog} object and presents the results.

  Since 4.10
  @see-constructor{gtk:font-dialog-new}
  @see-slot{gtk:font-dialog-filter}
  @see-slot{gtk:font-dialog-font-map}
  @see-slot{gtk:font-dialog-language}
  @see-slot{gtk:font-dialog-modal}
  @see-slot{gtk:font-dialog-title}
  @see-class{gtk:font-dialog-button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:font-dialog-filter -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "filter" 'font-dialog) t)
 "The @code{filter} property of type @class{gtk:filter} (Read / Write) @br{}
  Sets a filter to restrict what fonts are shown in the font chooser dialog.")

#+liber-documentation
(setf (liber:alias-for-function 'font-dialog-filter)
      "Accessor"
      (documentation 'font-dialog-filter 'function)
 "@version{2026-01-03}
  @syntax{(gtk:font-dialog-filter object) => filter}
  @syntax{(setf (gtk:font-dialog-filter object) filter)}
  @argument[object]{a @class{gtk:font-dialog} object}
  @argument[filter]{a @class{gtk:filter} object}
  @begin{short}
    The accessor for the @slot[gtk:font-dialog]{filter} slot of the
    @class{gtk:font-dialog} class gets or sets the filter that decides which
    fonts to display in the font chooser dialog.
  @end{short}

  The @class{gtk:filter} object must be able to handle both
  @class{pango:font-family} and @class{pango:font-face} objects.

  Since 4.10
  @see-class{gtk:font-dialog}
  @see-class{gtk:filter}
  @see-class{pango:font-family}
  @see-clas{pango-font-face}")

;;; --- gtk:font-dialog-font-map -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "font-map" 'font-dialog) t)
 "The @code{font-map} property of type @class{pango:font-map} (Read / Write)
  @br{}
  Sets a custom font map to select fonts from. A custom font map can be used to
  present application specific fonts instead of or in addition to the normal
  system fonts.")

#+liber-documentation
(setf (liber:alias-for-function 'font-dialog-font-map)
      "Accessor"
      (documentation 'font-dialog-font-map 'function)
 "@version{2026-01-03}
  @syntax{(gtk:font-dialog-font-map object) => fontmap}
  @syntax{(setf (gtk:font-dialog-font-map object) fontmap)}
  @argument[object]{a @class{gtk:font-dialog} object}
  @argument[fontmap]{a @class{pango:font-map} object}
  @begin{short}
    The accessor for the @slot[gtk:font-dialog]{font-map} slot of the
    @class{gtk:font-dialog} class gets or sets the fontmap from which fonts are
    selected.
  @end{short}
  Returns @code{nil} for the default fontmap.

  Since 4.10
  @see-class{gtk:font-dialog}
  @see-class{pango:font-map}")

;;; --- gtk:font-dialog-language -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "language" 'font-dialog) t)
 "The @code{language} property of type @class{pango:language} (Read / Write)
  @br{}
  The language for which the font features are selected.")

#+liber-documentation
(setf (liber:alias-for-function 'font-dialog-language)
      "Accessor"
      (documentation 'font-dialog-language 'function)
 "@version{2026-01-03}
  @syntax{(gtk:font-dialog-language object) => language}
  @syntax{(setf (gtk:font-dialog-language object) language)}
  @argument[object]{a @class{gtk:font-dialog} object}
  @argument[fontmap]{a @class{pango:language} object}
  @begin{short}
    The accessor for the @slot[gtk:font-dialog]{language} slot of the
    @class{gtk:font-dialog} class gets or sets the language for which font
    features are applied.
  @end{short}

  Since 4.10
  @see-class{gtk:font-dialog}
  @see-class{pango:language}")

;;; --- gtk:font-dialog-modal --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "modal" 'font-dialog) t)
 "The @code{modal} property of type @code{:boolean} (Read / Write) @br{}
  Whether the font chooser dialog is modal. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'font-dialog-modal)
      "Accessor"
      (documentation 'font-dialog-modal 'function)
 "@version{2026-01-03}
  @syntax{(gtk:font-dialog-modal object) => modal}
  @syntax{(setf (gtk:font-dialog-modal object) modal)}
  @argument[object]{a @class{gtk:font-dialog} object}
  @argument[modal]{a boolean whether the font chooser dialog is modal}
  @begin{short}
    The accessor for the @slot[gtk:font-dialog]{modal} slot of the
    @class{gtk:font-dialog} class gets or sets whether the font chooser dialog
    blocks interaction with the parent window while it is presented.
  @end{short}

  Since 4.10
  @see-class{gtk:font-dialog}")

;;; --- gtk:font-dialog-title --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title" 'font-dialog) t)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title that may be shown on the font chooser dialog that is presented by
  the @fun{gtk:font-dialog-choose-font} function. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'font-dialog-title)
      "Accessor"
      (documentation 'font-dialog-title 'function)
 "@version{2026-01-03}
  @syntax{(gtk:font-dialog-title object) => title}
  @syntax{(setf (gtk:font-dialog-title object) title)}
  @argument[object]{a @class{gtk:font-dialog} object}
  @argument[title]{a string for the title}
  @begin{short}
    The accessor for the @slot[gtk:font-dialog]{title} slot of the
    @class{gtk:font-dialog} class gets or sets the title that will be shown on
    the font chooser dialog.
  @end{short}

  Since 4.10
  @see-class{gtk:font-dialog}")

;;; ----------------------------------------------------------------------------
;;; gtk_font_dialog_new
;;; ----------------------------------------------------------------------------

(declaim (inline font-dialog-new))

(defun font-dialog-new ()
 #+liber-documentation
 "@version{2026-01-03}
  @return{The new @class{gtk:font-dialog} object.}
  @short{Creates a new @class{gtk:font-dialog} object.}

  Since 4.10
  @see-class{gtk:font-dialog}"
  (make-instance 'font-dialog))

(export 'font-dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_font_dialog_choose_face
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_dialog_choose_face" %font-dialog-choose-face) :void
  (dialog (g:object font-dialog))
  (parent (g:object window))
  (face (g:object pango:font-face))
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun font-dialog-choose-face (dialog parent face cancellable func)
 #+liber-documentation
 "@version{2026-01-04}
  @argument[dialog]{a @class{gtk:font-dialog} object}
  @argument[parent]{a parent @class{gtk:window} widget}
  @argument[face]{a @class{pango:font-face} object}
  @argument[cancellable]{a @class{g:cancellable} object to cancel the operation}
  @argument[func]{a @sym{g:async-ready-callback} callback function to call when
    the operation is complete}
  @begin{short}
    This function initiates a font selection operation by presenting a dialog to
    the user for selecting a font face, that is a font family and style, but not
    a specific font size.
  @end{short}
  The callback will be called when the dialog is dismissed. It should call the
  @fun{gtk:font-dialog-choose-face-finish} function to obtain the result.

  Since 4.10
  @see-class{gtk:font-dialog}
  @see-class{gtk:window}
  @see-class{pango:font-face}
  @see-class{g:cancellable}
  @see-symbol{g:async-ready-callback}
  @see-function{gtk:font-dialog-choose-face-finish}"
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
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_dialog_choose_face_finish"
               %font-dialog-choose-face-finish)
    (g:object pango:font-face :return)
  (dialog (g:object font-dialog))
  (result (g:object g:async-result))
  (err :pointer))

(defun font-dialog-choose-face-finish (dialog result)
 #+liber-documentation
 "@version{2026-01-04}
  @argument[dialog]{a @class{gtk:font-dialog} object}
  @argument[result]{a @class{g:async-result} object}
  @begin{short}
    Finishes the the @fun{gtk:font-dialog-choose-face} function call and returns
    the resulting font face.
  @end{short}

  Since 4.10
  @see-class{gtk:font-dialog}
  @see-class{g:async-result}
  @see-function{gtk:font-dialog-choose-face}"
  (glib:with-ignore-error (err)
    (%font-dialog-choose-face-finish dialog result err)))

(export 'font-dialog-choose-face-finish)

;;; ----------------------------------------------------------------------------
;;; gtk_font_dialog_choose_family
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_dialog_choose_family" %font-dialog-choose-family) :void
  (dialog (g:object font-dialog))
  (parent (g:object window))
  (family (g:object pango:font-family))
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun font-dialog-choose-family (dialog parent family cancellable func)
 #+liber-documentation
 "@version{2026-01-04}
  @argument[dialog]{a @class{gtk:font-dialog} object}
  @argument[parent]{a parent @class{gtk:window} widget}
  @argument[family]{a @class{pango:font-family} object}
  @argument[cancellable]{a @class{g:cancellable} object to cancel the operation}
  @argument[func]{a @sym{g:async-ready-callback} callback function to call when
    the operation is complete}
  @begin{short}
    This function initiates a font selection operation by presenting a dialog to
    the user for selecting a font family.
  @end{short}
  The callback will be called when the dialog is dismissed. It should call the
  @fun{gtk:font-dialog-choose-family-finish} function to obtain the result.

  Since 4.10
  @see-class{gtk:font-dialog}
  @see-class{gtk:window}
  @see-class{pango:font-family}
  @see-class{g:cancellable}
  @see-symbol{g:asyn-ready-callback}
  @see-function{gtk:font-dialog-choose-family-finish}"
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
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_dialog_choose_family_finish"
               %font-dialog-choose-family-finish)
    (g:object pango:font-family :return)
  (dialog (g:object font-dialog))
  (result (g:object g:async-result))
  (err :pointer))

(defun font-dialog-choose-family-finish (dialog result)
 #+liber-documentation
 "@version{2026-01-04}
  @argument[dialog]{a @class{gtk:font-dialog} object}
  @argument[result]{a @class{g:async-result} object}
  @begin{short}
    Finishes the the @fun{gtk:font-dialog-choose-family} function call and
    returns the resulting font family.
  @end{short}
  This function never returns an error. If the operation is not finished
  successfully, the value passed as @arg{family} to the
  @fun{gtk:font-dialog-choose-family} function is returned.

  Since 4.10
  @see-class{gtk:font-dialog}
  @see-class{g:async-result}
  @see-function{gtk:font-dialog-choose-family}"
  (glib:with-ignore-error (err)
    (%font-dialog-choose-family-finish dialog result err)))

(export 'font-dialog-choose-family-finish)

;;; ----------------------------------------------------------------------------
;;; gtk_font_dialog_choose_font
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_dialog_choose_font" %font-dialog-choose-font) :void
  (dialog (g:object font-dialog))
  (parent (g:object window))
  (desc (g:boxed pango:font-description))
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun font-dialog-choose-font (dialog parent desc cancellable func)
 #+liber-documentation
 "@version{2026-01-04}
  @argument[dialog]{a @class{gtk:font-dialog} object}
  @argument[parent]{a parent @class{gtk:window} widget}
  @argument[desc]{a @class{pango:font-description} object}
  @argument[cancellable]{a @class{g:cancellable} object to cancel the operation}
  @argument[func]{a @sym{g:async-ready-callback} callback function to call when
    the operation is complete}
  @begin{short}
    This function initiates a font selection operation by presenting a dialog to
    the user for selecting a font.
  @end{short}
  The callback will be called when the dialog is dismissed. It should call the
  @fun{gtk:font-dialog-choose-font-finish} function to obtain the result.

  If you want to let the user select font features as well, use the
  @fun{gtk:font-dialog-choose-font-and-features} function instead.

  Since 4.10
  @see-class{gtk:font-dialog}
  @see-class{gtk:window}
  @see-class{pango:font-description}
  @see-class{g:cancellable}
  @see-symbol{g:async-ready-callback}
  @see-function{gtk:font-dialog-choose-font-finish}
  @see-function{gtk:font-dialog-choose-font-and-features}"
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
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_dialog_choose_font_finish"
               %font-dialog-choose-font-finish)
    (g:boxed pango:font-description :return)
  (dialog (g:object font-dialog))
  (result (g:object g:async-result))
  (err :pointer))

(defun font-dialog-choose-font-finish (dialog result)
 #+liber-documentation
 "@version{2026-01-04}
  @argument[dialog]{a @class{gtk:font-dialog} object}
  @argument[result]{a @class{g:async-result} object}
  @return{The @class{pango:font-description} object for the selected font.}
  @begin{short}
    Finishes the the @fun{gtk:font-dialog-choose-font} function call and
    returns the resulting font description.
  @end{short}

  Since 4.10
  @see-class{gtk:font-dialog}
  @see-class{g:async-result}
  @see-class{pango:font-description}
  @see-function{gtk:font-dialog-choose-font}"
  (glib:with-ignore-error (err)
    (%font-dialog-choose-font-finish dialog result err)))

(export 'font-dialog-choose-font-finish)

;;; ----------------------------------------------------------------------------
;;; gtk_font_dialog_choose_font_and_features
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_dialog_choose_font_and_features"
               %font-dialog-choose-font-and-features) :void
  (dialog (g:object font-dialog))
  (parent (g:object window))
  (desc (g:boxed pango:font-description))
  (cancellable (g:object g:cancellable))
  (func :pointer)
  (data :pointer))

(defun font-dialog-choose-font-and-features (dialog
                                             parent desc cancellable func)
 #+liber-documentation
 "@version{2026-01-04}
  @argument[dialog]{a @class{gtk:font-dialog} object}
  @argument[parent]{a parent @class{gtk:window} widget}
  @argument[desc]{a @class{pango:font-description} object}
  @argument[cancellable]{a @class{g:cancellable} object to cancel the operation}
  @argument[func]{a @sym{g:async-ready-callback} callback function to call when
    the operation is complete}
  @begin{short}
    This function initiates a font selection operation by presenting a dialog to
    the user for selecting a font and font features.
  @end{short}
  Font features affect how the font is rendered, for example enabling glyph
  variants or ligatures.

  The callback will be called when the dialog is dismissed. It should call the
  @fun{gtk:font-dialog-choose-font-and-features-finish} function to obtain the
  result.

  Since 4.10
  @see-class{gtk:font-dialog}
  @see-class{gtk:window}
  @see-class{pango:font-description}
  @see-class{g:cancellable}
  @see-symbol{g:async-ready-callback}
  @see-function{gtk:font-dialog-choose-font-and-features-finish}"
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
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_dialog_choose_font_and_features_finish"
               %font-dialog-choose-font-and-features-finish) :boolean
  (dialog (g:object font-dialog))
  (result (g:object g:async-result))
  (desc :pointer)
  (features :pointer)
  (language :pointer)
  (err :pointer))

(defun font-dialog-choose-font-and-features-finish (dialog result)
 #+liber-documentation
 "@version{2026-01-04}
  @syntax{(gtk:font-dialog-choose-font-and-features-finish dialog result)
    => desc, features, language}
  @argument[dialog]{a @class{gtk:font-dialog} object}
  @argument[result]{a @class{g:async-result} object}
  @argument[desc]{a @class{pango:font-description} object}
  @argument[features]{a string for the font features}
  @argument[language]{a @class{pango:language} object}
  @begin{short}
    Finishes the the @fun{gtk:font-dialog-choose-font-and-features} function
    call and returns the resulting font description.
  @end{short}

  Since 4.10
  @see-class{gtk:font-dialog}
  @see-class{g:async-result}
  @see-class{pango:font-description}
  @see-class{pango:language}
  @see-function{gtk:font-dialog-choose-font-and-features}"
  (glib:with-ignore-error (err)
    (cffi:with-foreign-objects ((desc :pointer)
                                (feat :pointer)
                                (lang :pointer))
      (when (%font-dialog-choose-font-and-features-finish dialog
                                                          result
                                                          desc
                                                          feat
                                                          lang
                                                          err))
        (values (cffi:convert-from-foreign
                    desc '(g:boxed pango:font-description :return))
                (cffi:convert-from-foreign
                    (cffi:mem-ref feat :pointer) '(:string :free-from-foreign t))
                (cffi:convert-from-foreign
                    lang '(g:boxed pango:language :return))))))

(export 'font-dialog-choose-font-and-features-finish)

;;; --- End of file gtk4.font-dialog.lisp --------------------------------------
