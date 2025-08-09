;;; ----------------------------------------------------------------------------
;;; gtk4.font-chooser.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.12 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2025 Dieter Kaiser
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
;;; GtkFontChooser
;;;
;;;     Interface implemented by widgets displaying fonts
;;;
;;; Types and Values
;;;
;;;     GtkFontChooser
;;;     GtkFontChooserLevel
;;;
;;; Accessors
;;;
;;;     gtk_font_chooser_get_font
;;;     gtk_font_chooser_set_font
;;;     gtk_font_chooser_get_font_desc
;;;     gtk_font_chooser_set_font_desc
;;;     gtk_font_chooser_get_font_features
;;;     gtk_font_chooser_get_language
;;;     gtk_font_chooser_set_language
;;;     gtk_font_chooser_get_level
;;;     gtk_font_chooser_set_level
;;;     gtk_font_chooser_get_preview_text
;;;     gtk_font_chooser_set_preview_text
;;;     gtk_font_chooser_get_show_preview_entry
;;;     gtk_font_chooser_set_show_preview_entry
;;;
;;; Functions
;;;
;;;     gtk_font_chooser_get_font_family
;;;     gtk_font_chooser_get_font_face
;;;     gtk_font_chooser_get_font_size
;;;     GtkFontFilterFunc
;;;     gtk_font_chooser_set_filter_func
;;;     gtk_font_chooser_set_font_map
;;;     gtk_font_chooser_get_font_map
;;;
;;; Properties
;;;
;;;     font
;;;     font-desc
;;;     font-features
;;;     language
;;;     level
;;;     preview-text
;;;     show-preview-entry
;;;
;;; Signals
;;;
;;;     font-activated
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkFontChooser
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkFontChooserLevel
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "GtkFontChooserLevel" font-chooser-level
  (:export t
   :type-initializer "gtk_font_chooser_level_get_type")
  (:family 0)
  (:style      #.(ash 1 0))
  (:size       #.(ash 1 1))
  (:variations #.(ash 1 2))
  (:features   #.(ash 1 3)))

#+liber-documentation
(setf (liber:alias-for-symbol 'font-chooser-level)
      "GFlags"
      (liber:symbol-documentation 'font-chooser-level)
 "@version{2025-07-25}
  @begin{declaration}
(gobject:define-gflags \"GtkFontChooserLevel\" font-chooser-level
  (:export t
   :type-initializer \"gtk_font_chooser_level_get_type\")
  (:family 0)
  (:style      #.(ash 1 0))
  (:size       #.(ash 1 1))
  (:variations #.(ash 1 2))
  (:features   #.(ash 1 3)))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:family]{Allow selecting a font family.}
      @entry[:style]{Allow selecting a specific font face.}
      @entry[:size]{Allow selecting a specific font size.}
      @entry[:variations]{Allow changing OpenType font variation axes.}
      @entry[:features]{Allow selecting specific OpenType font features.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The @sym{gtk:font-chooser-level} flags specifies the granularity of font
    selection that is desired in a font chooser.
  @end{short}
  Applications should ignore unknown values.
  @see-class{gtk:font-chooser}")

;;; ----------------------------------------------------------------------------
;;; GtkFontChooser
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GtkFontChooser" font-chooser
  (:export t
   :type-initializer "gtk_font_chooser_get_type")
  ((font
    font-chooser-font
    "font" "gchararray" t t)
   (font-desc
    font-chooser-font-desc
    "font-desc" "PangoFontDescription" t t)
   (font-features
    font-chooser-font-features
    "font-features" "gchararray" t nil)
   (language
    font-chooser-language
    "language" "gchararray" t t)
   (level
    font-chooser-level
    "level" "GtkFontChooserLevel" t t)
   (preview-text
    font-chooser-preview-text
    "preview-text" "gchararray" t t)
   (show-preview-entry
    font-chooser-show-preview-entry
    "show-preview-entry" "gboolean" t t)))

#+(and gtk-4-10 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj font-chooser) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:FONT-CHOOSER is deprecated since 4.10")))

#+liber-documentation
(setf (liber:alias-for-class 'font-chooser)
      "Interface"
      (documentation 'font-chooser 'type)
 "@version{2025-07-19}
  @begin{short}
    The @class{gtk:font-chooser} interface is an interface that can be
    implemented by widgets displaying the list of fonts.
  @end{short}
  In GTK, the main widgets that implement this interface are the
  @class{gtk:font-chooser-widget}, @class{gtk:font-chooser-dialog} and
  @class{gtk:font-button} widgets.
  @begin[Warning]{dictionary}
    The @class{gtk:font-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:font-dialog} and @class{gtk:font-dialog-button} widgets
    instead.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[font-chooser::font-activated]{signal}
      @begin{pre}
lambda (fontchooser fontname)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[fontchooser]{The @class{gtk:font-chooser} widget that received
          the signal.}
        @entry[fontname]{The string for the font name.}
      @end{simple-table}
      Emitted when a font is activated. This usually happens when the user
      double clicks an item, or an item is selected and the user presses one
      of the @kbd{Space}, @kbd{Shift+Space}, @kbd{Return} or @kbd{Enter} keys.
    @end{signal}
  @end{dictionary}
  @see-slot{gtk:font-chooser-font}
  @see-slot{gtk:font-chooser-font-desc}
  @see-slot{gtk:font-chooser-font-features}
  @see-slot{gtk:font-chooser-language}
  @see-slot{gtk:font-chooser-level}
  @see-slot{gtk:font-chooser-preview-text}
  @see-slot{gtk:font-chooser-show-preview-entry}
  @see-class{gtk:font-button}
  @see-class{gtk:font-chooser-dialog}
  @see-class{gtk:font-chooser-widget}
  @see-class{gtk:font-dialog}
  @see-class{gtk:font-dialog-button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:font-chooser-font --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "font" 'font-chooser) t)
 "The @code{font} property of type @code{:string} (Read / Write) @br{}
  The font description as a string, for example, \"Sans Italic 12\". @br{}
  Default value: \"Sans 12\"")

#+liber-documentation
(setf (liber:alias-for-function 'font-chooser-font)
      "Accessor"
      (documentation 'font-chooser-font 'function)
 "@version{2025.07-27}
  @syntax{(gtk:font-chooser-font object) => fontname}
  @syntax{(setf (gtk:font-chooser-font object) fontname)}
  @argument[object]{a @class{gtk:font-chooser} object}
  @argument[fontname]{a string for the font name like \"Helvetica 12\" or
    \"Times Bold 18\"}
  @begin{short}
    Accessor of the @slot[gtk:font-chooser]{font} slot of the
    @class{gtk:font-chooser} class.
  @end{short}
  The @fun{gtk:font-chooser-font} function gets the currently selected font
  name. The @setf{gtk:font-chooser-font} function sets the font name.

  Note that this can be a different string than what you set with the
  @setf{gtk:font-chooser-font} function, as the font chooser widget may
  normalize the font names and thus return a string with a different structure.
  For example, \"Helvetica Italic Bold 12\" could be normalized to
  \"Helvetica Bold Italic 12\".

  Use the @fun{pango:font-description-equal} function if you want to compare
  two font descriptions.
  @begin[Warning]{dictionary}
    The @class{gtk:font-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:font-dialog} and @class{gtk:font-dialog-button} widgets
    instead.
  @end{dictionary}
  @see-class{gtk:font-chooser}
  @see-function{pango:font-description-equal}")

;;; --- gtk:font-chooser-font-desc ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "font-desc" 'font-chooser) t)
 "The @code{font-desc} property of type @class{pango:font-description}
  (Read / Write) @br{}
  The Pango font description.")

#+liber-documentation
(setf (liber:alias-for-function 'font-chooser-font-desc)
      "Accessor"
      (documentation 'font-chooser-font-desc 'function)
 "@version{2023-08-28}
  @syntax{(gtk:font-chooser-font-desc object) => font-desc}
  @syntax{(setf (gtk:font-chooser-font-desc object) font-desc)}
  @argument[object]{a @class{gtk:font-chooser} object}
  @argument[font-desc]{a @class{pango:font-description} instance}
  @begin{short}
    Accessor of the @slot[gtk:font-chooser]{font-desc} slot of the
    @class{gtk:font-chooser} class.
  @end{short}
  The @fun{gtk:font-chooser-font-desc} function gets the Pango font description
  for the currently selected font. The @setf{gtk:font-chooser-font-desc}
  function sets the currently selected font.

  Use the @fun{pango:font-description-equal} function if you want to compare
  two Pango font descriptions.
  @begin[Warning]{dictionary}
    The @class{gtk:font-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:font-dialog} and @class{gtk:font-dialog-button} widgets
    instead.
  @end{dictionary}
  @see-class{gtk:font-chooser}
  @see-class{pango:font-description}
  @see-function{pango:font-description-equal}")

;;; --- gtk:font-chooser-font-features -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "font-features" 'font-chooser) t)
 "The @code{font-features} property of type @code{:string} (Read) @br{}
  The selected font features, in a format that is compatible with CSS and with
  Pango attributes. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'font-chooser-font-features)
      "Accessor"
      (documentation 'font-chooser-font-features 'function)
 "@version{2025-07-27}
  @syntax{(gtk:font-chooser-font-features object) => features}
  @argument[object]{a @class{gtk:font-chooser} object}
  @argument[features]{a string for the currently selected font features}
  @begin{short}
    Accessor of the @slot[gtk:font-chooser]{font-features} slot of the
    @class{gtk:font-chooser} class.
  @end{short}
  The @fun{gtk:font-chooser-font-features} function gets the currently selected
  font features, in a format that is compatible with CSS and with Pango
  attributes. The @setf{gtk:font-chooser-font-features} function sets the font
  features.
  @begin[Warning]{dictionary}
    The @class{gtk:font-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:font-dialog} and @class{gtk:font-dialog-button} widgets
    instead.
  @end{dictionary}
  @see-class{gtk:font-chooser}")

;;; --- gtk:font-chooser-language ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "language" 'font-chooser) t)
 "The @code{language} property of type @code{:string} (Read / Write) @br{}
  The language for which the @slot[gtk:font-chooser]{font-features} property
  were selected, in a format that is compatible with CSS and with Pango
  attributes.")

#+liber-documentation
(setf (liber:alias-for-function 'font-chooser-language)
      "Accessor"
      (documentation 'font-chooser-language 'function)
 "@version{2024-05-22}
  @syntax{(gtk:font-chooser-language object) => language}
  @syntax{(setf (gtk:font-chooser-language object) language)}
  @argument[object]{a @class{gtk:font-chooser} object}
  @argument[language]{a RFC-3066 format string representing the language}
  @begin{short}
    Accessor of the @slot[gtk:font-chooser]{language} slot of the
    @class{gtk:font-chooser} class.
  @end{short}
  The @fun{gtk:font-chooser-language} function gets the language that is used
  for font features. The @setf{gtk:font-chooser-language} function sets the
  language. See the @fun{pango:language-to-string} function.
  @begin[Examples]{dictionary}
    @begin{pre}
(gtk:font-chooser-language (make-instance 'gtk:font-button)) => \"de-de\"
    @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:font-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:font-dialog} and @class{gtk:font-dialog-button} widgets
    instead.
  @end{dictionary}
  @see-class{gtk:font-chooser}
  @see-function{pango:language-to-string}")

;;; --- gtk:font-chooser-level -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "level" 'font-chooser) t)
 "The @code{level} property of type @sym{gtk:font-chooser-level} (Read / Write)
  @br{}
  The level of granularity to offer for selecting fonts. @br{}
  Default value: @code{'(:STYLE :SIZE)}")

#+liber-documentation
(setf (liber:alias-for-function 'font-chooser-level)
      "Accessor"
      (documentation 'font-chooser-level 'function)
 "@version{2025-07-25}
  @syntax{(gtk:font-chooser-level object) => level}
  @syntax{(setf (gtk:font-chooser-level object) level)}
  @argument[object]{a @class{gtk:font-chooser} object}
  @argument[level]{a @sym{gtk:font-chooser-level} value for the desired level
    of granularity of type}
  @begin{short}
    Accessor of the @slot[gtk:font-chooser]{level} slot of the
    @class{gtk:font-chooser} class.
  @end{short}
  The @fun{gtk:font-chooser-level} function returns the current level of
  granularity for selecting fonts. The @setf{gtk:font-chooser-level} function
  sets the desired level of granularity for selecting fonts.
  @begin[Warning]{dictionary}
    The @class{gtk:font-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:font-dialog} and @class{gtk:font-dialog-button} widgets
    instead.
  @end{dictionary}
  @see-class{gtk:font-chooser}
  @see-symbol{gtk:font-chooser-level}")

;;; --- gtk:font-chooser-preview-text ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "preview-text" 'font-chooser) t)
 "The @code{preview-text} property of type @code{:string} (Read / Write) @br{}
  The string with which to preview the font. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'font-chooser-preview-text)
      "Accessor"
      (documentation 'font-chooser-preview-text 'function)
 "@version{2025-07-27}
  @syntax{(gtk:font-chooser-preview-text object) => text}
  @syntax{(setf (gtk:font-chooser-preview-text object) text)}
  @argument[object]{a @class{gtk:font-chooser} object}
  @argument[text]{a string for the text to display in the preview area}
  @begin{short}
    Accessor of the @slot[gtk:font-chooser]{preview-text} slot of the
    @class{gtk:font-chooser} class.
  @end{short}
  The @fun{gtk:font-chooser-preview-text} function gets the text displayed in
  the preview area. The @setf{gtk:font-chooser-preview-text} function sets the
  text displayed in the preview area. The text is used to show how the selected
  font looks. See the @fun{pango:language-sample-string} function.
  @begin[Warning]{dictionary}
    The @class{gtk:font-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:font-dialog} and @class{gtk:font-dialog-button} widgets
    instead.
  @end{dictionary}
  @see-class{gtk:font-chooser}
  @see-function{pango:language-sample-string}")

;;; --- gtk:font-chooser-show-preview-entry ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-preview-entry"
                                               'font-chooser) t)
 "The @code{show-preview-entry} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to show an entry to change the preview text. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'font-chooser-show-preview-entry)
      "Accessor"
      (documentation 'font-chooser-show-preview-entry 'function)
 "@version{2023-08-28}
  @syntax{(gtk:font-chooser-show-preview-entry object) => show-entry}
  @syntax{(setf (gtk:font-chooser-show-preview-entry object) show-entry)}
  @argument[object]{a @class{gtk:font-chooser} object}
  @argument[show-entry]{a boolean whether to show the editable preview entry or
    not}
  @begin{short}
    Accessor of the @slot[gtk:font-chooser]{show-preview-entry} slot of the
    @class{gtk:font-chooser} class.
  @end{short}
  Shows or hides the editable preview entry.
  @begin[Warning]{dictionary}
    The @class{gtk:font-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:font-dialog} and @class{gtk:font-dialog-button} widgets
    instead.
  @end{dictionary}
  @see-class{gtk:font-chooser}")

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_get_font_family
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_chooser_get_font_family" font-chooser-font-family)
    (g:object pango:font-family)
 #+liber-documentation
 "@version{#2024-05-22}
  @argument[fontchooser]{a @class{gtk:font-chooser} object}
  @begin{return}
    The @class{pango:font-family} object representing the selected font family,
    or @code{nil}.
  @end{return}
  @begin{short}
    Gets the Pango font family representing the selected font family.
  @end{short}
  Font families are a collection of font faces. If the selected font is not
  installed, returns @code{nil}.
  @begin[Examples]{dictionary}
    @begin{pre}
(defvar fontbutton (make-instance 'gtk:font-button :font \"Serif Bold 10\"))
=> FONTBUTTON
(gtk:font-chooser-font-family fontbutton)
=> #<PANGO-FONT-FAMILY {1002728D63@}>
(pango:font-family-name *)
=> \"Sans\"
    @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:font-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:font-dialog} and @class{gtk:font-dialog-button} widgets
    instead.
  @end{dictionary}
  @see-class{gtk:font-chooser}
  @see-class{pango:font-family}"
  (fontchooser (g:object font-chooser)))

(export 'font-chooser-font-family)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_get_font_face
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_chooser_get_font_face" font-chooser-font-face)
    (g:object pango:font-face)
 #+liber-documentation
 "@version{#2025-07-17}
  @argument[fontchooser]{a @class{gtk:font-chooser} object}
  @begin{return}
    The @class{pango:font-face} object representing the selected font group
    details, or @code{nil}.
  @end{return}
  @begin{short}
    Gets the Pango font face representing the selected font group details,
    for example, family, slant, weight, width, and so on.
  @end{short}
  If the selected font is not installed, returns @code{nil}.
  @begin[Warning]{dictionary}
    The @class{gtk:font-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:font-dialog} and @class{gtk:font-dialog-button} widgets
    instead.
  @end{dictionary}
  @see-class{gtk:font-chooser}
  @see-class{pango:font-face}"
  (fontchooser (g:object font-chooser)))

(export 'font-chooser-font-face)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_get_font_size
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_chooser_get_font_size" font-chooser-font-size) :int
 #+liber-documentation
 "@version{#2024-05-22}
  @argument[fontchooser]{a @class{gtk:font-chooser} object}
  @begin{return}
    The integer representing the selected font size in Pango units,
    or -1 if no font size is selected.
  @end{return}
  @begin{short}
    The selected font size.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:font-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:font-dialog} and @class{gtk:font-dialog-button} widgets
    instead.
  @end{dictionary}
  @see-class{gtk:font-chooser}"
  (fontchooser (g:object font-chooser)))

(export 'font-chooser-font-size)

;;; ----------------------------------------------------------------------------
;;; GtkFontFilterFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback font-filter-func :boolean
    ((family (g:object pango:font-family))
     (face (g:object pango:font-face))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (funcall func family face)))

#+liber-documentation
(setf (liber:alias-for-symbol 'font-filter-func)
      "Callback"
      (liber:symbol-documentation 'font-filter-func)
 "@version{#2024-05-22}
  @syntax{lambda (family face) => result}
  @argument[family]{a @class{pango:font-family} object}
  @argument[face]{a @class{pango:font-face} object belonging to @arg{family}}
  @argument[result]{@em{true} if the font should be displayed}
  @begin{short}
    The callback function that is used for deciding what fonts get shown in a
    @class{gtk:font-chooser} widget.
  @end{short}
  See the @fun{gtk:font-chooser-set-filter-func} function.
  @see-class{gtk:font-chooser}
  @see-class{pango:font-family}
  @see-class{pango:font-face}
  @see-function{gtk:font-chooser-set-filter-func}")

(export 'font-filter-func)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_set_filter_func
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_font_chooser_set_filter_func" %font-chooser-set-filter-func)
    :void
  (fontchooser (g:object font-chooser))
  (filter :pointer)
  (user-data :pointer)
  (destroy :pointer))

(defun font-chooser-set-filter-func (fontchooser func)
 #+liber-documentation
 "@version{#2025-07-25}
  @argument[fontchooser]{a @class{gtk:font-chooser} object}
  @argument[filter]{a @sym{gtk:font-filter-func} callback, or @code{nil}}
  @begin{short}
    Adds a filter function that decides which fonts to display in the font
    chooser.
  @end{short}
  @begin[Examples]{dictionary}
    A callback filter function to select fonts from the font families \"Sans\"
    and \"Serif\":
    @begin{pre}
;; Define the callback function
(defun font-filter (family face)
  (declare (ignore face))
  (member (pango:font-family-name family)
          '(\"Sans\" \"Serif\")
          :test #'equal))
;; Set the function FONT-FILTER as the callback function
(gtk:font-chooser-set-filter-func button #'font-filter)
;; Remove the filter function from the font button
(gtk:font-chooser-set-filter-func button nil)
    @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:font-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:font-dialog} and @class{gtk:font-dialog-button} widgets
    instead.
  @end{dictionary}
  @see-class{gtk:font-chooser}
  @see-symbol{gtk:font-filter-func}"
  (if func
      (%font-chooser-set-filter-func
              fontchooser
              (cffi:callback font-filter-func)
              (glib:allocate-stable-pointer func)
              (cffi:callback glib:stable-pointer-destroy-notify))
      (%font-chooser-set-filter-func
              fontchooser
              (cffi:null-pointer)
              (cffi:null-pointer)
              (cffi:null-pointer))))

(export 'font-chooser-set-filter-func)

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_set_font_map
;;; gtk_font_chooser_get_font_map
;;; ----------------------------------------------------------------------------

(defun (setf font-chooser-font-map) (fontmap fontchooser)
  (cffi:foreign-funcall "gtk_font_chooser_set_font_map"
                        (g:object font-chooser) fontchooser
                        (g:object pango:font-map) fontmap
                        :void)
  fontmap)

(cffi:defcfun ("gtk_font_chooser_get_font_map" font-chooser-font-map)
    (g:object pango:font-map)
 #+liber-documentation
 "@version{#2024-05-22}
  @syntax{(gtk:font-chooser-font-map fontchooser) => fontmap}
  @syntax{(setf (gtk:font-chooser-font-map fontchooser) fontmap)}
  @argument[fontchooser]{a @class{gtk:font-chooser} widget}
  @argument[fontmap]{a @class{pango:font-map} object, or @code{nil}}
  @begin{short}
    Accessor of the Pango font map of the font chooser widget.
  @end{short}
  The @fun{gtk:font-chooser-font-map} function gets the custom font map of the
  font chooser widget, or @code{nil} if it does not have one. The
  @setf{gtk:font-chooser-font-map} function sets a custom font map to use for
  the font chooser widget. A custom font map can be used to present application
  specific fonts instead of or in addition to the normal system fonts.
  @begin[Examples]{dictionary}
    Note that other GTK widgets will only be able to use the application
    specific font if it is present in the font map they use. The following
    code updates the font map for a @class{gtk:label} widget with @arg{fontmap}.
    @begin{pre}
(setf (pango:context-font-map (gtk:widget-pango-context label)) fontmap)
    @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:font-chooser} implementation is deprecated since 4.10. Use
    the @class{gtk:font-dialog} and @class{gtk:font-dialog-button} widgets
    instead.
  @end{dictionary}
  @see-class{gtk:font-chooser}
  @see-class{pango:font-map}"
  (fontchooser (g:object font-chooser)))

(export 'font-chooser-font-map)

;;; --- End of file gtk4.font-chooser.lisp -------------------------------------
