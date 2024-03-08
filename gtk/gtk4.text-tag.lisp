;;; ----------------------------------------------------------------------------
;;; gtk4.text-tag.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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
;;; GtkTextTag
;;;
;;;     A tag that can be applied to text in a GtkTextBuffer
;;;
;;; Types and Values
;;;
;;;     GtkTextTag
;;;
;;; Functions
;;;
;;;     gtk_text_tag_new
;;;     gtk_text_tag_get_priority
;;;     gtk_text_tag_set_priority
;;;     gtk_text_tag_changed
;;;
;;; Properties
;;;
;;;     accumulative-margin
;;;     allow-breaks
;;;     allow-breaks-set
;;;     background
;;;     background-full-height
;;;     background-full-height-set
;;;     background-rgba
;;;     background-set
;;;     direction
;;;     editable
;;;     editable-set
;;;     fallback
;;;     fallback-set
;;;     family
;;;     family-set
;;;     font
;;;     font-desc
;;;     font-features
;;;     font-features-set
;;;     foreground
;;;     foreground-rgba
;;;     foreground-set
;;;     indent
;;;     indent-set
;;;     insert-hyphens
;;;     insert-hyphens-set
;;;     invisible
;;;     invisible-set
;;;     justification
;;;     justification-set
;;;     language
;;;     language-set
;;;     left-margin
;;;     left-margin-set
;;;     letter-spacing
;;;     letter-spacing-set
;;;     line-height                                        Since 4.6
;;;     line-height-set                                    Since 4.6
;;;     name
;;;     overline
;;;     overline-rgba
;;;     overline-set
;;;     paragraph-background
;;;     paragraph-background-rgba
;;;     paragraph-background-set
;;;     pixels-above-lines
;;;     pixels-above-lines-set
;;;     pixels-below-lines
;;;     pixels-below-lines-set
;;;     pixels-inside-wrap
;;;     pixels-inside-wrap-set
;;;     right-margin
;;;     right-margin-set
;;;     rise
;;;     rise-set
;;;     scale
;;;     scale-set
;;;     sentence                                           Since 4.6
;;;     sentence-set                                       Since 4.6
;;;     show-spaces
;;;     show-spaces-set
;;;     size
;;;     size-points
;;;     size-set
;;;     stretch
;;;     stretch-set
;;;     strikethrough
;;;     strikethrough-rgba
;;;     strikethrough-rgba-set
;;;     strikethrough-set
;;;     style
;;;     style-set
;;;     tabs
;;;     tabs-set
;;;     text-transform                                     Since 4,6
;;;     text-transform-set                                 Since 4.6
;;;     underline
;;;     underline-rgba
;;;     underline-rgba-set
;;;     underline-set
;;;     variant
;;;     variant-set
;;;     weight
;;;     weight-set
;;;     word                                               Since 4.6
;;;     word-set                                           Since 4.6
;;;     wrap-mode
;;;     wrap-mode-set
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkTextTag
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;; TODO: Implement the PangoTextTransform enumeration. It is available since
;; Pango Version 1.50

;;; ----------------------------------------------------------------------------
;;; GtkTextTag
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkTextTag" text-tag
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_text_tag_get_type")
  ((accumulative-margin
    text-tag-accumulative-margin
    "accumulative-margin" "gboolean" t t)
   (allow-breaks
    text-tag-allow-breaks
    "allow-breaks" "gboolean" t t)
   (allow-breaks-set
    text-tag-allow-breaks-set
    "allow-breaks-set" "boolean" t t)
   (background
    text-tag-background
    "background" "gchararray" nil t)
   (background-full-height
    text-tag-background-full-height
    "background-full-height" "gboolean" t t)
   (background-full-height-set
    text-tag-background-full-height-set
    "background-full-height-set" "gboolean" t t)
   (background-rgba
    text-tag-background-rgba
    "background-rgba" "GdkRGBA" t t)
   (background-set
    text-tag-background-set
    "background-set" "gboolean" t t)
   (direction
    text-tag-direction
    "direction" "GtkTextDirection" t t)
   (editable
    text-tag-editable
    "editable" "gboolean" t t)
   (editable-set
    text-tag-editable-set
    "editable-set" "gboolean" t t)
   (fallback
    text-tag-fallback
    "fallback" "gboolean" t t)
   (fallback-set
    text-tag-fallback-set
    "fallback-set" "gboolean" t t)
   (family
    text-tag-family
    "family" "gchararray" t t)
   (family-set
    text-tag-family-set
    "family-set" "gboolean" t t)
   (font
    text-tag-font
    "font" "gchararray" t t)
   (font-desc
    text-tag-font-desc
    "font-desc" "PangoFontDescription" t t)
   (font-features
    text-tag-font-features
    "font-features" "gchararray" t t)
   (font-features-set
    text-tag-font-features-set
    "font-features-set" "gboolean" t t)
   (foreground
    text-tag-foreground
    "foreground" "gchararray" nil t)
   (foreground-rgba
    text-tag-foreground-rgba
    "foreground-rgba" "GdkRGBA" t t)
   (foreground-set
    text-tag-foreground-set
    "foreground-set" "gboolean" t t)
   (indent
    text-tag-indent
    "indent" "gint" t t)
   (indent-set
    text-tag-indent-set
    "indent-set" "gboolean" t t)
   (insert-hyphens
    text-tag-insert-hyphens
    "insert-hyphens" "gboolean" t t)
   (insert-hyphens-set
    text-tag-insert-hyphens-set
    "insert-hyphens-set" "gboolean" t t)
   (invisible
    text-tag-invisible
    "invisible" "gboolean" t t)
   (invisible-set
    text-tag-invisible-set
    "invisible-set" "gboolean" t t)
   (justification
    text-tag-justification
    "justification" "GtkJustification" t t)
   (justification-set
    text-tag-justification-set
    "justification-set" "gboolean" t t)
   (language
    text-tag-language
    "language" "gchararray" t t)
   (language-set
    text-tag-language-set
    "language-set" "gboolean" t t)
   (left-margin
    text-tag-left-margin
    "left-margin" "gint" t t)
   (left-margin-set
    text-tag-left-margin-set
    "left-margin-set" "gboolean" t t)
   (letter-spacing
    text-tag-letter-spacing
    "letter-spacing" "gint" t t)
   (letter-spacing-set
    text-tag-letter-spacing-set
    "letter-spacing-set" "gboolean" t t)
   #+gtk-4-6
   (line-height
    text-tag-line-height
    "line-height" "gfloat" t t)
   #+gtk-4-6
   (line-height-set
    text-tag-line-height-set
    "line-height-set" "gboolean" t t)
   (name
    text-tag-name
    "name" "gchararray" t nil)
   (overline
    text-tag-overline
    "overline" "PangoOverline" t t)
   (overline-rgba
    text-tag-overline-rgba
    "overline-rgba" "GdkRGBA" t t)
   (overline-rgba-set
    text-tag-overline-rgba-set
    "overline-rgba-set" "gboolean" t t)
   (overline-set
    text-tag-overline-set
    "overline-set" "gboolean" t t)
   (paragraph-background
    text-tag-paragraph-background
    "paragraph-background" "gchararray" nil t)
   (paragraph-background-rgba
    text-tag-paragraph-background-rgba
    "paragraph-background-rgba" "GdkRGBA" t t)
   (paragraph-background-set
    text-tag-paragraph-background-set
    "paragraph-background-set" "gboolean" t t)
   (pixels-above-lines
    text-tag-pixels-above-lines
    "pixels-above-lines" "gint" t t)
   (pixels-above-lines-set
    text-tag-pixels-above-lines-set
    "pixels-above-lines-set" "gboolean" t t)
   (pixels-below-lines
    text-tag-pixels-below-lines
    "pixels-below-lines" "gint" t t)
   (pixels-below-lines-set
    text-tag-pixels-below-lines-set
    "pixels-below-lines-set" "gboolean" t t)
   (pixels-inside-wrap
    text-tag-pixels-inside-wrap
    "pixels-inside-wrap" "gint" t t)
   (pixels-inside-wrap-set
    text-tag-pixels-inside-wrap-set
    "pixels-inside-wrap-set" "gboolean" t t)
   (right-margin
    text-tag-right-margin
    "right-margin" "gint" t t)
   (right-margin-set
    text-tag-right-margin-set
    "right-margin-set" "gboolean" t t)
   (rise
    text-tag-rise
    "rise" "gint" t t)
   (rise-set
    text-tag-rise-set
    "rise-set" "gboolean" t t)
   (scale
    text-tag-scale
    "scale" "gdouble" t t)
   (scale-set
    text-tag-scale-set
    "scale-set" "gboolean" t t)
   #+gtk-4-6
   (sentence
    text-tag-sentence
    "sentence" "gboolean" t t)
   #+gtk-4-6
   (sentence-set
    text-tag-sentence-set
    "sentence-set" "gboolean" t t)
   (show-spaces
    text-tag-show-spaces
    "show-spaces" "PangoShowFlags" t t)
   (show-spaces-set
    text-tag-show-spaces-set
    "show-spaces-set" "gboolean" t t)
   (size
    text-tag-size
    "size" "gint" t t)
   (size-points
    text-tag-size-points
    "size-points" "gdouble" t t)
   (size-set
    text-tag-size-set
    "size-set" "gboolean" t t)
   (stretch
    text-tag-stretch
    "stretch" "PangoStretch" t t)
   (stretch-set
    text-tag-stretch-set
    "stretch-set" "gboolean" t t)
   (strikethrough
    text-tag-strikethrough
    "strikethrough" "gboolean" t t)
   (strikethrough-rgba
    text-tag-strikethrough-rgba
    "strikethrough-rgba" "GdkRGBA" t t)
   (strikethrough-rgba-set
    text-tag-strikethrough-rgba-set
    "strikethrough-rgba-set" "gboolean" t t)
   (strikethrough-set
    text-tag-strikethrough-set
    "strikethrough-set" "gboolean" t t)
   (style
    text-tag-style
    "style" "PangoStyle" t t)
   (style-set
    text-tag-style-set
    "style-set" "gboolean" t t)
   (tabs
    text-tag-tabs
    "tabs" "PangoTabArray" t t)
   (tabs-set
    text-tag-tabs-set
    "tabs-set" "gboolean" t t)
   #+gtk-4-6
   (text-transform
    text-tag-text-transform
    "text-transform" "PangoTextTransform" t t)
   #+gtk-4-6
   (text-transform-set
    text-tag-text-transform-set
    "text-transform-set" "gboolean" t t)
   (underline
    text-tag-underline
    "underline" "PangoUnderline" t t)
   (underline-rgba
    text-tag-underline-rgba
    "underline-rgba" "GdkRGBA" t t)
   (underline-rgba-set
    text-tag-underline-rgba-set
    "underline-rgba-set" "gboolean" t t)
   (underline-set
    text-tag-underline-set
    "underline-set" "gboolean" t t)
   (variant
    text-tag-variant
    "variant" "PangoVariant" t t)
   (variant-set
    text-tag-variant-set
    "variant-set" "gboolean" t t)
   (weight
    text-tag-weight
    ;; TODO: It is not possible to use the PangoWeight enumeration for the type
    ;; at this place. Is there a way to improve this?!
    "weight" "gint" t t)
   (weight-set
    text-tag-weight-set
    "weight-set" "gboolean" t t)
   #+gtk-4-6
   (word
    text-tag-word
    "word" "gboolean" t t)
   #+gtk-4-6
   (word-set
    text-tag-word-set
    "word-set" "gboolean" t t)
   (wrap-mode
    text-tag-wrap-mode
    "wrap-mode" "GtkWrapMode" t t)
   (wrap-mode-set
    text-tag-wrap-mode-set
    "wrap-mode-set" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'text-tag 'type)
 "@version{2024-3-8}
  @begin{short}
    A tag that can be applied to text contained in a @class{gtk:text-buffer}
    object.
  @end{short}
  You may wish to begin by reading the text widget conceptual overview which
  gives an overview of all the objects and data types related to the text
  widget and how they work together.

  Tags should be in the @class{gtk:text-tag-table} object for a given
  @class{gtk:text-buffer} object before using them with that text buffer. The
  @fun{gtk:text-buffer-create-tag} function is the best way to create tags.

  For each property of the @class{gtk:text-tag} class, there is a @code{set}
  property, e.g. @code{font-set} corresponds to @code{font}. These @code{set}
  properties reflect whether a property has been set or not. They are maintained
  by GTK and you should not set them independently.
  @see-constructor{gtk:text-tag-new}
  @see-slot{gtk:text-tag-accumulative-margin}
  @see-slot{gtk:text-tag-allow-breaks}
  @see-slot{gtk:text-tag-allow-breaks-set}
  @see-slot{gtk:text-tag-background}
  @see-slot{gtk:text-tag-background-full-height}
  @see-slot{gtk:text-tag-background-full-height-set}
  @see-slot{gtk:text-tag-background-rgba}
  @see-slot{gtk:text-tag-background-set}
  @see-slot{gtk:text-tag-direction}
  @see-slot{gtk:text-tag-editable}
  @see-slot{gtk:text-tag-editable-set}
  @see-slot{gtk:text-tag-fallback}
  @see-slot{gtk:text-tag-fallback-set}
  @see-slot{gtk:text-tag-family}
  @see-slot{gtk:text-tag-family-set}
  @see-slot{gtk:text-tag-font}
  @see-slot{gtk:text-tag-font-desc}
  @see-slot{gtk:text-tag-font-features}
  @see-slot{gtk:text-tag-font-features-set}
  @see-slot{gtk:text-tag-foreground}
  @see-slot{gtk:text-tag-foreground-rgba}
  @see-slot{gtk:text-tag-foreground-set}
  @see-slot{gtk:text-tag-indent}
  @see-slot{gtk:text-tag-indent-set}
  @see-slot{gtk:text-tag-insert-hyphens}
  @see-slot{gtk:text-tag-insert-hyphens-set}
  @see-slot{gtk:text-tag-invisible}
  @see-slot{gtk:text-tag-invisible-set}
  @see-slot{gtk:text-tag-justification}
  @see-slot{gtk:text-tag-justification-set}
  @see-slot{gtk:text-tag-language}
  @see-slot{gtk:text-tag-language-set}
  @see-slot{gtk:text-tag-left-margin}
  @see-slot{gtk:text-tag-left-margin-set}
  @see-slot{gtk:text-tag-letter-spacing}
  @see-slot{gtk:text-tag-letter-spacing-set}
  @see-slot{gtk:text-tag-line-height}
  @see-slot{gtk:text-tag-line-height-set}
  @see-slot{gtk:text-tag-name}
  @see-slot{gtk:text-tag-overline}
  @see-slot{gtk:text-tag-overline-rgba}
  @see-slot{gtk:text-tag-overline-rgba-set}
  @see-slot{gtk:text-tag-overline-set}
  @see-slot{gtk:text-tag-paragraph-background}
  @see-slot{gtk:text-tag-paragraph-background-rgba}
  @see-slot{gtk:text-tag-paragraph-background-set}
  @see-slot{gtk:text-tag-pixels-above-lines}
  @see-slot{gtk:text-tag-pixels-above-lines-set}
  @see-slot{gtk:text-tag-pixels-below-lines}
  @see-slot{gtk:text-tag-pixels-below-lines-set}
  @see-slot{gtk:text-tag-pixels-inside-wrap}
  @see-slot{gtk:text-tag-pixels-inside-wrap-set}
  @see-slot{gtk:text-tag-right-margin}
  @see-slot{gtk:text-tag-right-margin-set}
  @see-slot{gtk:text-tag-rise}
  @see-slot{gtk:text-tag-rise-set}
  @see-slot{gtk:text-tag-scale}
  @see-slot{gtk:text-tag-scale-set}
  @see-slot{gtk:text-tag-sentence}
  @see-slot{gtk:text-tag-sentence-set}
  @see-slot{gtk:text-tag-show-spaces}
  @see-slot{gtk:text-tag-show-spaces-set}
  @see-slot{gtk:text-tag-size}
  @see-slot{gtk:text-tag-size-points}
  @see-slot{gtk:text-tag-size-set}
  @see-slot{gtk:text-tag-stretch}
  @see-slot{gtk:text-tag-stretch-set}
  @see-slot{gtk:text-tag-strikethrough}
  @see-slot{gtk:text-tag-strikethrough-rgba}
  @see-slot{gtk:text-tag-strikethrough-rgba-set}
  @see-slot{gtk:text-tag-strikethrough-set}
  @see-slot{gtk:text-tag-style}
  @see-slot{gtk:text-tag-style-set}
  @see-slot{gtk:text-tag-tabs}
  @see-slot{gtk:text-tag-tabs-set}
  @see-slot{gtk:text-tag-text-transform}
  @see-slot{gtk:text-tag-text-transform-set}
  @see-slot{gtk:text-tag-underline}
  @see-slot{gtk:text-tag-underline-rgba}
  @see-slot{gtk:text-tag-underline-rgba-set}
  @see-slot{gtk:text-tag-underline-set}
  @see-slot{gtk:text-tag-variant}
  @see-slot{gtk:text-tag-variant-set}
  @see-slot{gtk:text-tag-weight}
  @see-slot{gtk:text-tag-weight-set}
  @see-slot{gtk:text-tag-word}
  @see-slot{gtk:text-tag-word-set}
  @see-slot{gtk:text-tag-wrap-mode}
  @see-slot{gtk:text-tag-wrap-mode-set}
  @see-class{gtk:text-tag-table}
  @see-class{gtk:text-buffer}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:text-tag-accumulative-margin ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accumulative-margin"
                                               'text-tag) t)
 "The @code{accumulative-margin} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the margins accumulate or override each other. When set to @em{true}
  the margins of this tag are added to the margins of any other non-accumulative
  margins present. When set to @em{false} the margins override one another
  (the default). @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-accumulative-margin)
      "Accessor"
      (documentation 'text-tag-accumulative-margin 'function)
 "@version{2023-8-26}
  @syntax[]{(gtk:text-tag-accumulative-margin object) => setting}
  @syntax[]{(setf (gtk:text-tag-accumulative-margin object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether the margins accumulate}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{accumulative-margin} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether the margins accumulate or override each other. When set to @em{true}
  the margins of this tag are added to the margins of any other non-accumulative
  margins present. When set to @em{false} the margins override one another,
  the default.
  @see-class{gtk:text-tag}")

;;; --- gtk:text-tag-allow-breaks ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "allow-breaks" 'text-tag) t)
 "The @code{allow-breaks} property of type @code{:boolean} (Read / Write) @br{}
  Whether breaks are allowed. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-allow-breaks)
      "Accessor"
      (documentation 'text-tag-allow-breaks 'function)
 "@version{2023-8-26}
  @syntax[]{(gtk:text-tag-allow-breaks object) => setting}
  @syntax[]{(setf (gtk:text-tag-allow-breaks object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether breaks are allowed}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{allow-breaks} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether breaks are allowed.
  @see-class{gtk:text-tag}")

;;; --- gtk:text-tag-allow-breaks-set ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "allow-breaks-set" 'text-tag) t)
 "The @code{allow-breaks-set} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether this tag affects line breaks. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-allow-breaks-set)
      "Accessor"
      (documentation 'text-tag-allow-breaks-set 'function)
 "@version{2023-8-26}
  @syntax[]{(gtk:text-tag-allow-breaks-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-allow-breaks-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects line breaks}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{allow-breaks-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects line breaks.
  @see-class{gtk:text-tag}")

;;; --- gtk:text-tag-background ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "background" 'text-tag) t)
 "The @code{background} property of type @code{:string} (Write) @br{}
  Background color as a string. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-background)
      "Accessor"
      (documentation 'text-tag-background 'function)
 "@version{2023-10-24}
  @syntax[]{(setf (gtk:text-tag-background object) background)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[background]{a string with the background color}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{background} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  @begin[Note]{dictionary}
    The @slot[gtk:text-tag]{background} property is not readable, but writable.
    @begin{pre}
(defvar tag (gtk:text-tag-new nil)) => TAG
(setf (gtk:text-tag-background tag) \"red\") => \"red\"
(gtk:text-tag-background-rgba tag)
=> #S(GDK:RGBA :RED 1.0 :GREEN 0.0 :BLUE 0.0 :ALPHA 1.0)
    @end{pre}
  @end{dictionary}
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-background-rgba}
  @see-function{gtk:text-tag-background-set}")

;;; --- gtk:text-tag-background-full-height ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "background-full-height"
                                               'text-tag) t)
 "The @code{background-full-height} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the background color fills the entire line height or only the height
  of the tagged characters. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-background-full-height)
      "Accessor"
      (documentation 'text-tag-background-full-height 'function)
 "@version{2023-8-26}
  @syntax[]{(gtk:text-tag-background-full-height object) => setting}
  @syntax[]{(setf (gtk:text-tag-background-full-height object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether the background fills the entire line
    height}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{background-full-height} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether the background color fills the entire line height or only the height
  of the tagged characters.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-background-full-height-set}")

;;; --- gtk:text-tag-background-full-height-set --------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "background-full-height-set"
                                               'text-tag) t)
 "The @code{background-full-height-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this tag affects background height. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-background-full-height-set)
      "Accessor"
      (documentation 'text-tag-background-full-height-set 'function)
 "@version{2023-8-26}
  @syntax[]{(gtk:text-tag-background-full-height-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-background-full-height-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects background height}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{background-full-height-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects background height.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-background-full-height}")

;;; --- gtk:text-tag-background-rgba -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "background-rgba" 'text-tag) t)
 "The @code{background-rgba} property of type @class{gdk:rgba} (Read / Write)
  @br{}
  The background color.")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-background-rgba)
      "Accessor"
      (documentation 'text-tag-background-rgba 'function)
 "@version{2023-8-26}
  @syntax[]{(gtk:text-tag-background-rgba object) => color}
  @syntax[]{(setf (gtk:text-tag-background-rgba object) color)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[color]{a @class{gdk:rgba} color}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{background-rgba} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  The background color.
  @see-class{gtk:text-tag}
  @see-class{gdk:rgba}
  @see-function{gtk:text-tag-background}
  @see-function{gtk:text-tag-background-set}")

;;; --- gtk:text-tag-background-set --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "background-set" 'text-tag) t)
 "The @code{background-set} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether this tag affects the background color. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-background-set)
      "Accessor"
      (documentation 'text-tag-background-set 'function)
 "@version{2023-8-26}
  @syntax[]{(gtk:text-tag-background-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-background-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects the background color}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{background-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects the background color.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-background}
  @see-function{gtk:text-tag-background-rgba}")

;;; --- gtk:text-tag-direction -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "direction" 'text-tag) t)
 "The @code{direction} property of type @symbol{gtk:text-direction}
  (Read / Write) @br{}
  Text direction, e.g. the @code{:ltr} value for left-to-right. @br{}
  Default value: @code{:none}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-direction)
      "Accessor"
      (documentation 'text-tag-direction 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-direction object) => direction}
  @syntax[]{(setf (gtk:text-tag-direction object) direction)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[direction]{a @symbol{gtk:text-direction} value}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{direction} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  The text direction.
  @see-class{gtk:text-tag}
  @see-symbol{gtk:text-direction}")

;;; --- gtk:text-tag-editable --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "editable" 'text-tag) t)
 "The @code{editable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the text can be modified by the user. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-editable)
      "Accessor"
      (documentation 'text-tag-editable 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-editable object) => editable}
  @syntax[]{(setf (gtk:text-tag-editable object) editable)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[editable]{a boolean whether the text can be modified}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{editable} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether the text can be modified by the user.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-editable-set}")

;;; --- gtk:text-tag-editable-set ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "editable-set" 'text-tag) t)
 "The @code{editable-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects text editability. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-editable-set)
      "Accessor"
      (documentation 'text-tag-editable-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-editable-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-editable-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects text editability}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{editable-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects text editability.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-editable}")

;;; --- gtk:text-tag-fallback --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "fallback" 'text-tag) t)
 "The @code{fallback} property of type @code{:boolean} (Read / Write) @br{}
  Whether font fallback is enabled. When set to @em{true}, other fonts will be
  substituted where the current font is missing glyphs. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-fallback)
      "Accessor"
      (documentation 'text-tag-fallback 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-fallback object) => fallback}
  @syntax[]{(setf (gtk:text-tag-fallback object) fallback)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[fallback]{a boolean whether font fallback is enabled}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{fallback} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether font fallback is enabled. When set to @em{true}, other fonts will be
  substituted where the current font is missing glyphs.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-fallback-set}")

;;; --- gtk:text-tag-fallback-set ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "fallback-set" 'text-tag) t)
 "The @code{fallback-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects font fallback. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-fallback-set)
      "Accessor"
      (documentation 'text-tag-fallback-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-fallback-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-fallback-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this affects font fallback}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{fallback-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-fallback}")

;;; --- gtk:text-tag-family ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "family" 'text-tag) t)
 "The @code{family} property of type @code{:string} (Read / Write) @br{}
  Name of the font family, e.g. Sans, Helvetica, Times, Monospace. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-family)
      "Accessor"
      (documentation 'text-tag-family 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-family object) => family}
  @syntax[]{(setf (gtk:text-tag-family object) family)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[family]{a string with the name of the font family}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{family} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Name of the font family, e.g. Sans, Helvetica, Times, Monospace.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-family-set}")

;;; --- gtk:text-tag-family-set ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "family-set" 'text-tag) t)
 "The @code{family-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the font family. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-family-set)
      "Accessor"
      (documentation 'text-tag-family-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-family-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-family-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects the font family}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{family-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects the font family.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-family}")

;;; --- gtk:text-tag-font ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "font" 'text-tag) t)
 "The @code{font} property of type @code{:string} (Read / Write) @br{}
  Font description as string, e.g. \"Sans Italic 12\". Note that the initial
  value of this property depends on the internals of the Pango font description.
  @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-font)
      "Accessor"
      (documentation 'text-tag-font 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-font object) => font}
  @syntax[]{(setf (gtk:text-tag-font object) font)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[font]{a string with a font description}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{font} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Font description as string, e.g. \"Sans Italic 12\". Note that the initial
  value of this property depends on the internals of the
  @class{pango:font-description} structure.
  @see-class{gtk:text-tag}
  @see-class{pango:font-description}")

;;; --- gtk:text-tag-font-desc -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "font-desc" 'text-tag) t)
 "The @code{font-desc} property of type @class{pango:font-description}
  (Read / Write) @br{}
  Font description as a Pango font description.")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-font-desc)
      "Accessor"
      (documentation 'text-tag-font-desc 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-font-desc object) => font-desc}
  @syntax[]{(setf (gtk:text-tag-font object) font-desc)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[font-desc]{a @class{pango:font-description} instance with a font
    description}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{font-desc} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Font description as a Pango font description.
  @see-class{gtk:text-tag}
  @see-class{pango:font-description}")

;;; --- gtk:text-tag-font-features ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "font-features" 'text-tag) t)
 "The @code{font-features} property of type @code{:string} (Read / Write) @br{}
  OpenType font features, as a string. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-font-features)
      "Accessor"
      (documentation 'text-tag-font-features 'function)
 "@version{2024-3-9}
  @syntax[]{(gtk:text-tag-font-features object) => features}
  @syntax[]{(setf (gtk:text-tag-font-features object) features)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[features]{a string with the OpenType font features}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{font-features} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  OpenType font features, as a string.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-font-features-set}")

;;; --- gtk:text-tag-font-features-set -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "font-features-set" 'text-tag) t)
 "The @code{font-features-set} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether this tag affects font features. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-font-features-set)
      "Accessor"
      (documentation 'text-tag-font-features-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-font-features-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-font-features-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects font features}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{font-features-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects font features.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-font-features}")

;;; --- gtk:text-tag-foreground ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "foreground" 'text-tag) t)
 "The @code{foreground} property of type @code{:string} (Write) @br{}
  Foreground color as a string. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-foreground)
      "Accessor"
      (documentation 'text-tag-foreground 'function)
 "@version{2023-10-24}
  @syntax[]{(setf (gtk:text-tag-foreground object) color)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[color]{a string with the foreground color}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{foreground} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  @begin[Note]{dictionary}
    The @slot[gtk:text-tag]{foreground} property is not readable, but writable.
    @begin{pre}
(defvar tag (gtk:text-tag-new nil)) => TAG
(setf (gtk:text-tag-foreground tag) \"red\") => \"red\"
(gtk:text-tag-foreground-rgba tag)
=> #S(GDK:RGBA :RED 1.0 :GREEN 0.0 :BLUE 0.0 :ALPHA 1.0)
    @end{pre}
  @end{dictionary}
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-foreground-rgba}
  @see-function{gtk:text-tag-foreground-set}")

;;; --- gtk:text-tag-foreground-rgba -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "foreground-rgba" 'text-tag) t)
 "The @code{foreground-rgba} property of type @class{gdk:rgba} (Read / Write)
  @br{}
  The foreground color.")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-foreground-rgba)
      "Accessor"
      (documentation 'text-tag-foreground-rgba 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-foreground-rgba object) => color}
  @syntax[]{(setf (gtk:text-tag-foreground-rgba object) color)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[color]{a @class{gdk:rgba} color}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{foreground-rgba} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  The foreground color.
  @see-class{gtk:text-tag}
  @see-class{gdk:rgba}
  @see-function{gtk:text-tag-foreground}
  @see-function{gtk:text-tag-foreground-set}")

;;; --- gtk:text-tag-foreground-set --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "foreground-set" 'text-tag) t)
 "The @code{foreground-set} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether this tag affects the foreground color. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-foreground-set)
      "Accessor"
      (documentation 'text-tag-foreground-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-foreground-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-foreground-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tags affect the foreground color}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{foreground-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects the foreground color.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-foreground}
  @see-function{gtk:text-tag-foreground-rgba}")

;;; --- gtk:text-tag-indent ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "indent" 'text-tag) t)
 "The @code{indent} property of type @code{:int} (Read / Write) @br{}
  Amount to indent the paragraph, in pixels. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-indent)
      "Accessor"
      (documentation 'text-tag-indent 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-indent object) => indent}
  @syntax[]{(setf (gtk:text-tag-indent object) indent)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[indent]{an integer with the amount to indent}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{indent} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Amount to indent the paragraph, in pixels.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-indent-set}")

;;; --- gtk:text-tag-indent-set ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "indent-set" 'text-tag) t)
 "The @code{indent-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects indentation. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-indent-set)
      "Accessor"
      (documentation 'text-tag-indent-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-indent-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-indent-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects indentation}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{indent-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects indentation.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-indent}")

;;; --- gtk:text-tag-insert-hyphens --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "insert-hyphens" 'text-tag) t)
 "The @code{insert-hyphens} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to insert hyphens at breaks. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-insert-hyphens)
      "Accessor"
      (documentation 'text-tag-insert-hyphens 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-insert-hyphens object) => setting}
  @syntax[]{(setf (gtk:text-tag-insert-hyphens object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether to insert hyphens at breaks}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{insert-hyphens} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether to insert hyphens at breaks.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-insert-hyphens-set}")

;;; --- gtk:text-tag-insert-hyphens-set ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "insert-hyphens-set"
                                               'text-tag) t)
 "The @code{insert-hyphens-set} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether this tag affects insertion of hyphens. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-insert-hyphens-set)
      "Accessor"
      (documentation 'text-tag-insert-hyphens-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-insert-hyphens-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-insert-hyphens-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects insertion of hyphens}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{insert-hyphens-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects insertion of hyphens.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-insert-hyphens}")

;;; --- gtk:text-tag-invisible -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "invisible" 'text-tag) t)
 "The @code{invisible} property of type @code{:boolean} (Read / Write) @br{}
  Whether this text is hidden. Note that there may still be problems with the
  support for invisible text, in particular when navigating programmatically
  inside a text buffer containing invisible segments. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-invisible)
      "Accessor"
      (documentation 'text-tag-invisible 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-invisible object) => invisible}
  @syntax[]{(setf (gtk:text-tag-invisible object) invisible)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[invisible]{a boolean whether this text is hidden}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{invisible} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this text is hidden. Note that there may still be problems with the
  support for invisible text, in particular when navigating programmatically
  inside a text buffer containing invisible segments.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-invisible-set}")

;;; --- gtk:text-tag-invisible-set ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "invisible-set" 'text-tag) t)
 "The @code{invisible-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects text visibility. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-invisible-set)
      "Accessor"
      (documentation 'text-tag-invisible-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-invisible-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-invisible-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects visibility}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{invisible-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects text visibility.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-invisible}")

;;; --- gtk:text-tag-justification ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "justification" 'text-tag) t)
 "The @code{justification} property of type @symbol{gtk:justification}
  (Read / Write) @br{}
  Left, right, or center justification. @br{}
  Default value: @code{:left}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-justification)
      "Accessor"
      (documentation 'text-tag-justification 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-justification object) => justification}
  @syntax[]{(setf (gtk:text-tag-justification object) justification)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[justification]{a @symbol{gtk:justification} value}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{justification} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Left, right, or center justification.
  @see-class{gtk:text-tag}
  @see-symbol{gtk:justification}
  @see-function{gtk:text-tag-justification-set}")

;;; --- gtk:text-tag-justification-set -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "justification-set" 'text-tag) t)
 "The @code{justification-set} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether this tag affects paragraph justification. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-justification-set)
      "Accessor"
      (documentation 'text-tag-justification-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-justification-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-justification-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects paragraph justification}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{justification-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects paragraph justification.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-justification}")

;;; --- gtk:text-tag-language --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "language" 'text-tag) t)
 "The @code{language} property of type @code{:string} (Read / Write) @br{}
  The language this text is in, as an ISO code. Pango can use this as a hint
  when rendering the text. If not set, an appropriate default will be used.
  Note that the initial value of this property depends on the current locale,
  see also the @fun{gtk:default-language} function. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-language)
      "Accessor"
      (documentation 'text-tag-language 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-language object) => language}
  @syntax[]{(setf (gtk:text-tag-language object) language)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[language]{a string with language this text is in}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{language} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  The language this text is in, as an ISO code. Pango can use this as a hint
  when rendering the text. If not set, an appropriate default will be used.
  Note that the initial value of this property depends on the current locale,
  see also the @fun{gtk:default-language} function.
  @see-class{gtk:text-tag}
  @see-function{gtk:default-language}
  @see-function{gtk:text-tag-language-set}")

;;; --- gtk:text-tag-language-set ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "language-set" 'text-tag) t)
 "The @code{language-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the language the text is rendered as. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-language-set)
      "Accessor"
      (documentation 'text-tag-language-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-language-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-language-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects the language}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{language-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects the language the text is rendered as.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-language}")

;;; --- gtk:text-tag-left-margin -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "left-margin" 'text-tag) t)
 "The @code{left-margin} property of type @code{:int} (Read / Write) @br{}
  Width of the left margin in pixels. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-left-margin)
      "Accessor"
      (documentation 'text-tag-left-margin 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-left-margin object) => margin}
  @syntax[]{(setf (gtk:text-tag-left-margin object) margin)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[margin]{an integer with the width of the left margin in pixels}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{left-margin} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Width of the left margin in pixels.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-left-margin-set}")

;;; --- gtk:text-tag-left-margin-set -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "left-margin-set" 'text-tag) t)
 "The @code{left-margin-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this tag affects the left margin. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-left-margin-set)
      "Accessor"
      (documentation 'text-tag-left-margin-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-left-margin-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-left-margin-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects the left margin}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{left-margin-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects the left margin.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-left-margin}")

;;; --- gtk:text-tag-letter-spacing --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "letter-spacing" 'text-tag) t)
 "The @code{letter-spacing} property of type @code{:int} (Read / Write) @br{}
  Extra spacing between graphemes, in Pango units. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-letter-spacing)
      "Accessor"
      (documentation 'text-tag-letter-spacing 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-letter-spacing object) => spacing}
  @syntax[]{(setf (gtk:text-tag-letter-spacing object) spacing)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[spacing]{an integer with extra spacing between graphems in Pango
    units}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{letter-spacing} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Extra spacing between graphemes, in Pango units.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-letter-spacing-set}")

;;; --- gtk:text-tag-letter-spacing-set ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "letter-spacing-set"
                                               'text-tag) t)
 "The @code{letter-spacing-set} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether this tag affects letter spacing. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-letter-spacing-set)
      "Accessor"
      (documentation 'text-tag-letter-spacing-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-letter-spacing-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-letter-spacing-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects letter spacing}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{letter-spacing-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects letter spacing.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-letter-spacing}")

;;; --- gtk:text-tag-line-height -----------------------------------------------

#+(and gtk-4-6 liber-documentation)
(setf (documentation (liber:slot-documentation "line-height" 'text-tag) t)
 "The @code{line-height} property of type @code{:float} (Read / Write) @br{}
  Factor to scale line height by. Since 4.6")

#+(and gtk-4-6 liber-documentation)
(setf (liber:alias-for-function 'text-tag-line-height)
      "Accessor"
      (documentation 'text-tag-line-height 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-line-height object) => height}
  @syntax[]{(setf (gtk:text-tag-line-height object) height)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[height]{a float with the factor to scale line height by}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{line-height} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Factor to scale line height by.

  Since 4.6
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-line-height-set}")

;;; --- gtk:text-tag-line-height-set -------------------------------------------

#+(and gtk-4-6 liber-documentation)
(setf (documentation (liber:slot-documentation "line-height-set" 'text-tag) t)
 "The @code{line-height-set} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether this tag affects line height. Since 4.6 @br{}
  Default value: @em{false}")

#+(and gtk-4-6 liber-documentation)
(setf (liber:alias-for-function 'text-tag-line-height-set)
      "Accessor"
      (documentation 'text-tag-line-height-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-line-height-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-line-height-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects line height}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{line-height-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects line height.

  Since 4.6
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-line-height}")

;;; --- gtk:text-tag-name ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "name" 'text-tag) t)
 "The @code{name} property of type @code{:string} (Read / Write / Construct)
  @br{}
  Name used to refer to the text tag, @code{nil} for anonymous tags. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-name)
      "Accessor"
      (documentation 'text-tag-name 'function)
 "@version{2023-10-5}
  @syntax[]{(gtk:text-tag-name object) => name}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[name]{a string with the name of the text tag}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{name} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Name used to refer to the text tag, @code{nil} for anonymous tags.
  @see-class{gtk:text-tag}")

;;; --- gtk:text-tag-overline --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "overline" 'text-tag) t)
 "The @code{overline} property of type @symbol{pango:overline} (Read / Write)
  @br{}
  Style of overline for this text. @br{}
  Default value: @code{:none}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-overline)
      "Accessor"
      (documentation 'text-tag-overline 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-overline object) => setting}
  @syntax[]{(setf (gtk:text-tag-overline object) setting}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a @symbol{pango:overline} value}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{overline} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Style of overline for this text.
  @see-class{gtk:text-tag}")

;;; --- gtk:text-tag-overline-rgba ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "overline-rgba" 'text-tag) t)
 "The @code{overline-rgba} property of type @class{gdk:rgba} (Read / Write)
  @br{}
  Color of overline for this text.")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-overline-rgba)
      "Accessor"
      (documentation 'text-tag-overline-rgba 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-overline-rgba object) => rgba}
  @syntax[]{(setf (gtk:text-tag-overline-rgba object) rgba}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[rgba]{a @class{gdk:rgba} instance with the color}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{overline-rgba} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Color of overline for this text.
  @see-class{gtk:text-tag}
  @see-class{gdk:rgba}
  @see-function{gtk:text-tag-overline-rgba-set}")

;;; --- gtk:text-tag-overline-rgba-set -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "overline-rgba-set" 'text-tag) t)
 "The @code{overline-rgba-set} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether this tag affects overlining color. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-overline-rgba-set)
      "Accessor"
      (documentation 'text-tag-overline-rgba-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-overline-rgba-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-overline-rgba-set object) setting}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects overlining color}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{overline-rgba-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects overlining color.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-overline-rgba}")

;;; --- gtk:text-tag-overline-set ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "overline-set" 'text-tag) t)
 "The @code{overline-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects overlining. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-overline-set)
      "Accessor"
      (documentation 'text-tag-overline-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-overline-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-overline-set object) setting}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects overlining}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{overline-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects overlining.
  @see-class{gtk:text-tag}")

;;; --- gtk:text-tag-paragraph-background --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "paragraph-background"
                                               'text-tag) t)
 "The @code{paragraph-background} property of type @code{:string}
  (Write) @br{}
  The paragraph background color as a string. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-paragraph-background)
      "Accessor"
      (documentation 'text-tag-paragraph-background 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-paragraph-background object) => color}
  @syntax[]{(setf (gtk:text-tag-paragraph-background object) color)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[color]{a string with the paragraph background color}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{paragraph-background} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  The paragraph background color as a string.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-paragraph-background-rgba}
  @see-function{gtk:text-tag-paragraph-background-set}")

;;; --- gtk:text-tag-paragraph-background-rgba ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "paragraph-background-rgba"
                                               'text-tag) t)
 "The @code{paragraph-background-rgba} property of type @class{gdk:rgba}
  (Read / Write) @br{}
  The paragraph background color.")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-paragraph-background-rgba)
      "Accessor"
      (documentation 'text-tag-paragraph-background-rgba 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-paragraph-background-rgba object) => color}
  @syntax[]{(setf (gtk:text-tag-paragraph-background-rgba object) color)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[color]{a @class{gdk:rgba} color}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{paragraph-background-rgba} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  The paragraph background color.
  @see-class{gtk:text-tag}
  @see-class{gdk:rgba}
  @see-function{gtk:text-tag-paragraph-background}
  @see-function{gtk:text-tag-paragraph-background-set}")

;;; --- gtk:text-tag-paragraph-background-set ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "paragraph-background-set"
                                               'text-tag) t)
 "The @code{paragraph-background-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this tag affects the paragraph background color. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-paragraph-background-set)
      "Accessor"
      (documentation 'text-tag-paragraph-background-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-paragraph-background-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-paragraph-background-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this affects the paragraph background
    color}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{paragraph-background-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects the paragraph background color.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-paragraph-background}
  @see-function{gtk:text-tag-paragraph-background-rgba}")

;;; --- gtk:text-tag-pixels-above-lines ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pixels-above-lines"
                                               'text-tag) t)
 "The @code{pixels-above-lines} property of type @code{:int} (Read / Write)
  @br{}
  Pixels of blank space above paragraphs. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-pixels-above-lines)
      "Accessor"
      (documentation 'text-tag-pixels-above-lines 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-pixels-above-lines object) => pixels}
  @syntax[]{(setf (gtk:text-tag-pixels-above-lines object) pixels)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[pixels]{an integer with the pixels of blank space above paragraphs}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{pixels-above-lines} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Pixels of blank space above paragraphs.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-pixels-above-lines-set}")

;;; --- gtk:text-tag-pixels-above-lines-set ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pixels-above-lines-set"
                                               'text-tag) t)
 "The @code{pixels-above-lines-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this tag affects the number of pixels above lines. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-pixels-above-lines-set)
      "Accessor"
      (documentation 'text-tag-pixels-above-lines-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-pixels-above-lines-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-pixels-above-lines-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects the number of pixels
    above lines}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{pixels-above-lines-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects the number of pixels above lines.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-pixels-above-lines}")

;;; --- gtk:text-tag-pixels-below-lines ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pixels-below-lines"
                                               'text-tag) t)
 "The @code{pixels-below-lines} property of type @code{:int}
  (Read / Write) @br{}
  Pixels of blank space below paragraphs. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-pixels-below-lines)
      "Accessor"
      (documentation 'text-tag-pixels-below-lines 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-pixels-below-lines object) => pixels}
  @syntax[]{(setf (gtk:text-tag-pixels-below-lines object) pixels)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[pixels]{an integer with the pixels of blank space below paragraphs}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{pixels-below-lines} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Pixels of blank space below paragraphs.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-pixels-below-lines-set}")

;;; --- gtk:text-tag-pixels-below-lines-set ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pixels-below-lines-set"
                                               'text-tag) t)
 "The @code{pixels-below-lines-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this tag affects the number of pixels below lines. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-pixels-below-lines-set)
      "Accessor"
      (documentation 'text-tag-pixels-below-lines-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-pixels-below-lines-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-pixels-below-lines-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects the number of pixels
    below lines}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{pixels-below-lines-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects the number of pixels below lines.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-pixels-below-lines}")

;;; --- gtk:text-tag-pixels-inside-wrap ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pixels-inside-wrap"
                                               'text-tag) t)
 "The @code{pixels-inside-wrap} property of type @code{:int} (Read / Write)
  @br{}
  Pixels of blank space between wrapped lines in a paragraph. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-pixels-inside-wrap)
      "Accessor"
      (documentation 'text-tag-pixels-inside-wrap 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-pixels-inside-wrap object) => pixels}
  @syntax[]{(setf (gtk:text-tag-pixels-inside-wrap object) pixels)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[pixels]{an integer with the pixels of blank space between wrapped
    lines in a paragraph}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{pixels-inside-wrap} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Pixels of blank space between wrapped lines in a paragraph.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-pixels-inside-wrap-set}")

;;; --- gtk:text-tag-pixels-inside-wrap-set ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pixels-inside-wrap-set"
                                               'text-tag) t)
 "The @code{pixels-inside-wrap-set} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether this tag affects the number of pixels between wrapped lines. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-pixels-inside-wrap-set)
      "Accessor"
      (documentation 'text-tag-pixels-inside-wrap-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-pixels-inside-wrap-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-pixels-inside-wrap-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects the number of pixels
    between wrapped lines}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{pixels-inside-wrap-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects the number of pixels between wrapped lines.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-pixels-inside-wrap}")

;;; --- gtk:text-tag-right-margin ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "right-margin" 'text-tag) t)
 "The @code{right-margin} property of type @code{:int} (Read / Write) @br{}
  Width of the right margin in pixels. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-right-margin)
      "Accessor"
      (documentation 'text-tag-right-margin 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-right-margin object) => margin}
  @syntax[]{(setf (gtk:text-tag-right-margin object) margin)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[margin]{an integer with the right margin in pixels}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{right-margin} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Width of the right margin in pixels.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-right-margin-set}")

;;; --- gtk:text-tag-right-margin-set ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "right-margin-set" 'text-tag) t)
 "The @code{right-margin-set} property of type @code{:boolean} Read / Write)
  @br{}
  Whether this tag affects the right margin. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-right-margin-set)
      "Accessor"
      (documentation 'text-tag-right-margin-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-right-margin-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-right-margin-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects the right margin}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{right-margin-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects the right margin.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-right-margin}")

;;; --- gtk:text-tag-rise ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "rise" 'text-tag) t)
 "The @code{rise} property of type @code{:int} (Read / Write) @br{}
  Offset of text above the baseline (below the baseline if rise is negative)
  in Pango units. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-rise)
      "Accessor"
      (documentation 'text-tag-rise 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-rise object) => rise}
  @syntax[]{(setf (gtk:text-tag-rise object) rise)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[rise]{an integer with the offset of text above the baseline in
    Pango units}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{rise} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Offset of text above the baseline (below the baseline if rise is negative)
  in Pango units.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-rise-set}")

;;; --- gtk:text-tag-rise-set --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "rise-set" 'text-tag) t)
 "The @code{rise-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the rise. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-rise-set)
      "Accessor"
      (documentation 'text-tag-rise-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-rise-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-rise-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects the rise}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{rise-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects the rise.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-rise}")

;;; --- gtk:text-tag-scale -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "scale" 'text-tag) t)
 "The @code{scale} property of type @code{:double} (Read / Write) @br{}
  Font size as a scale factor relative to the default font size. This properly
  adapts to theme changes etc. so is recommended. Pango predefines some scales
  such as the @var{pango:+scale-x-large+} value. @br{}
  Allowed values: >= 0 @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-scale)
      "Accessor"
      (documentation 'text-tag-scale 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-scale object) => scale}
  @syntax[]{(setf (gtk:text-tag-scale object) scale)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[scale]{a double float with the font size as a scale factor}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{scale} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Font size as a scale factor relative to the default font size. This property
  adapts to theme changes etc. so is recommended. Pango predefines some scales
  such as the @var{pango:+scale-x-large+} value.
  @see-class{gtk:text-tag}
  @see-variable{pango:+scale-x-large+}
  @see-function{gtk:text-tag-scale-set}")

;;; --- gtk:text-tag-scale-set -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "scale-set" 'text-tag) t)
 "The @code{scale-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag scales the font size by a factor. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-scale-set)
      "Accessor"
      (documentation 'text-tag-scale-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-scale-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-scale-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag scales the font size}
  @begin{short}
    Accessor of the @slot[gtk:text-view]{scale-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag scales the font size by a factor.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-scale}")

;;; --- gtk:text-tag-sentence --------------------------------------------------

#+(and gtk-4-6 liber-documentation)
(setf (documentation (liber:slot-documentation "sentence" 'text-tag) t)
 "The @code{sentence} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag represents a single sentence. This affects cursor movement.
  Since 4.6")

#+(and gtk-4-6 liber-documentation)
(setf (liber:alias-for-function 'text-tag-sentence)
      "Accessor"
      (documentation 'text-tag-sentence 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-sentence object) => setting}
  @syntax[]{(setf (gtk:text-tag-sentence object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag represents a single sentence}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{sentence} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag represents a single sentence. This affects cursor movement.

  Since 4.6
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-sentence-set}")

;;; --- gtk:text-tag-sentence-set ----------------------------------------------

#+(and gtk-4-6 liber-documentation)
(setf (documentation (liber:slot-documentation "sentence-set" 'text-tag) t)
 "The @code{sentence-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the @code{sentence} property. Since 4.6 @br{}
  Default value: @em{false}")

#+(and gtk-4-6 liber-documentation)
(setf (liber:alias-for-function 'text-tag-sentence-set)
      "Accessor"
      (documentation 'text-tag-sentence-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-sentence-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-sentence-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects the
    @slot[gtk:text-tag]{sentence} property.}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{sentence-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects the @slot[gtk:text-tag]{sentence} property.

  Since 4.6
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-sentence}")

;;; --- gtk:text-tag-show-spaces -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-spaces" 'text-tag) t)
 "The @code{show-spaces} property of type @symbol{pango:show-flags}
  (Read / Write) @br{}
  How to render invisible characters.")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-show-spaces)
      "Accessor"
      (documentation 'text-tag-show-spaces 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-show-spaces object) => setting}
  @syntax[]{(setf (gtk:text-tag-show-spaces object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a @symbol{pango:show-flags} value}
  @begin{short}
    Accessor of the @slot[gtk:text-view]{show-spaces} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  How to render invisible characters.
  @see-class{gtk:text-tag}
  @see-symbol{gtk:show-flags}
  @see-function{gtk:text-tag-show-spaces-set}")

;;; --- gtk:text-tag-show-spaces-set -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-spaces-set" 'text-tag) t)
 "The @code{show-spaces-set} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether this tag affects rendering of invisible characters. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-show-spaces-set)
      "Accessor"
      (documentation 'text-tag-show-spaces-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-show-spaces-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-show-spaces-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects rendering of invisible
    characters}
  @begin{short}
    Accessor of the @slot[gtk:text-view]{show-spaces-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects rendering of invisible characters.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-show-spaces}")

;;; --- gtk:text-tag-size ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "size" 'text-tag) t)
 "The @code{size} property of type @code{:int} (Read / Write) @br{}
  Font size in Pango units. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-size)
      "Accessor"
      (documentation 'text-tag-size 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-size object) => size}
  @syntax[]{(setf (gtk:text-tag-size object) size)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[size]{an integer with the the font size in Pango units}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{size} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Font size in Pango units.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-size-points}
  @see-function{gtkt-text-tag-size-set}")

;;; --- gtk:text-tag-size-points -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "size-points" 'text-tag) t)
 "The @code{size-points} property of type @code{:double} (Read / Write) @br{}
  Font size in points. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-size-points)
      "Accessor"
      (documentation 'text-tag-size-points 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-size-points object) => size}
  @syntax[]{(setf (gtk:text-tag-size-points object) size)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[size]{a double float with the the font size in points}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{size-points} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Font size in points.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-size}
  @see-function{gtk:text-tag-size-set}")

;;; --- gtk:text-tag-size-set --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "size-set" 'text-tag) t)
 "The @code{size-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the font size. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-size-set)
      "Accessor"
      (documentation 'text-tag-size-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-size-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-size-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects the font size}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{size-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects the font size.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-size}
  @see-function{gtk:text-tag-size-points}")

;;; --- gtk:text-tag-stretch ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "stretch" 'text-tag) t)
 "The @code{stretch} property of type @symbol{pango:stretch} (Read / Write)
  @br{}
  Font stretch, e.g. the @code{:condensed} value. @br{}
  Default value: @code{:normal}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-stretch)
      "Accessor"
      (documentation 'text-tag-stretch 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-stretch object) => stretch}
  @syntax[]{(setf (gtk:text-tag-stretch object) stretch)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[stretch]{a @symbol{pango:stretch} value}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{stretch} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Font stretch as a value of the @symbol{pango:stretch} enumeration, e.g. the
  @code{:condensed} value.
  @see-class{gtk:text-tag}
  @see-symbol{pango:stretch}
  @see-function{gtk:text-tag-stretch-set}")

;;; --- gtk:text-tag-stretch-set -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "stretch-set" 'text-tag) t)
 "The @code{stretch-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the font stretch. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-stretch-set)
      "Accessor"
      (documentation 'text-tag-stretch-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-stretch-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-stretch-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag afects the font stretch}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{stretch-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects the font stretch.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-stretch}")

;;; --- gtk:text-tag-strikethrough ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "strikethrough" 'text-tag) t)
 "The @code{strikethrough} property of type @code{:boolean} (Read / Write) @br{}
  Whether to strike through the text. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-strikethrough)
      "Accessor"
      (documentation 'text-tag-strikethrough 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-strikethrough object) => strikethrough}
  @syntax[]{(setf (gtk:text-tag-strikethrough object) strikethrough)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[strikethrough]{a boolean whether to strike through the text}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{strikethrough} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether to strike through the text.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-strikethrough-rgba}
  @see-function{gtk:text-tag-strikethrough-set}")

;;; --- gtk:text-tag-strikethrough-rgba ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "strikethrough-rgba"
                                               'text-tag) t)
 "The @code{strikethrough-rgba} property of type @class{gdk:rgba} (Read / Write)
  @br{}
  This property modifies the color of strikeouts. If not set, strikeouts will
  use the forground color.")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-strikethrough-rgba)
      "Accessor"
      (documentation 'text-tag-strikethrough-rgba 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-strikethrough-rgba object) => color}
  @syntax[]{(setf (gtk:text-tag-strikethrough-rgba object) color)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[color]{a @class{gdk:rgba} instance with the color}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{strikethrough-rgba} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  This property modifies the color of strikeouts. If not set, strikeouts will
  use the forground color.
  @see-class{gtk:text-tag}
  @see-class{gdk:rgba}
  @see-function{gtk:text-tag-strikethrough}
  @see-function{gtk:text-tag-strikethrough-rgba-set}")

;;; --- gtk:text-tag-strikethrough-rgba-set ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "strikethrough-rgba-set"
                                               'text-tag) t)
 "The @code{strikethrough-rgba-set} property of type @code{:boolean}
  (Read / Write) @br{}
  If the @code{strikethrough-rgba} property has been set. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-strikethrough-rgba-set)
      "Accessor"
      (documentation 'text-tag-strikethrough-rgba-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-strikethrough-rgba-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-strikethrough-rgba-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether a @slot[gtk:text-tag]{strikethrough-rgba}
    property has been set}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{strikethrough-rgba-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  If the @slot[gtk:text-tag]{strikethrough-rgba} property has been set.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-strikethrough-rgba}")

;;; --- gtk:text-tag-strikethrough-set -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "strikethrough-set" 'text-tag) t)
 "The @code{strikethrough-set} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether this tag affects strikethrough. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-strikethrough-set)
      "Accessor"
      (documentation 'text-tag-strikethrough-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-strikethrough-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-strikethrough-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects strikethrough}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{strikethrough-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects strikethrough.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-strikethrough}")

;;; --- gtk:text-tag-style -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "style" 'text-tag) t)
 "The @code{style} property of type @symbol{pango:style} (Read / Write) @br{}
  Font style, e.g. the @code{:italic} value. @br{}
  Default value: @code{:normal}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-style)
      "Accessor"
      (documentation 'text-tag-style 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-style object) => style}
  @syntax[]{(setf (gtk:text-tag-style object) style)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[style]{a @symbol{pango:style} value}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{style} slot of the @class{gtk:text-tag}
    class.
  @end{short}
  Font style as a value of the @symbol{pango:style} enumeration, e.g. the
  @code{:italic} value.
  @see-class{gtk:text-tag}
  @see-symbol{pango:style}
  @see-function{gtk:text-tag-style-set}")

;;; --- gtk:text-tag-style-set -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "style-set" 'text-tag) t)
 "The @code{style-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the font style. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-style-set)
      "Accessor"
      (documentation 'text-tag-style-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-style-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-style-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects the font style}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{style-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects the font style.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-style}")

;;; --- gtk:text-tag-tabs ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tabs" 'text-tag) t)
 "The @code{tabs} property of type @class{pango:tab-array} (Read / Write) @br{}
  Custom tabs for this text.")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-tabs)
      "Accessor"
      (documentation 'text-tag-tabs 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-tabs object) => tabs}
  @syntax[]{(setf (gtk:text-tag-tabs object) tabs)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[tabs]{a @class{pango:tab-array} instance with the custom tabs}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{tabs} slot of the @class{gtk:text-tag}
    class.
  @end{short}
  Custom tabs for this text.
  @see-class{gtk:text-tag}
  @see-class{pango:tab-array}
  @see-function{gtk:text-tag-tabs-set}")

;;; --- gtk:text-tag-tabs-set --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tabs-set" 'text-tag) t)
 "The @code{tabs-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects tabs. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-tabs-set)
      "Accessor"
      (documentation 'text-tag-tabs-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-tabs-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-tabs-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this affects tabs}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{tabs-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects tabs.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-tabs}")

;;; --- gtk:text-tag-text-transform --------------------------------------------

#+(and gtk-4-6 liber-documentation)
(setf (documentation (liber:slot-documentation "text-transform" 'text-tag) t)
 "The @code{text-transform} property of type @symbol{pango:text-transform}
  (Read / Write) @br{}
  How to transform the text for display. Since 4.6")

#+(and gtk-4-6 liber-documentation)
(setf (liber:alias-for-function 'text-tag-text-transform)
      "Accessor"
      (documentation 'text-tag-text-transform 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-text-transform object) => transform}
  @syntax[]{(setf (gtk:text-tag-text-transform object) transform)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[transform]{a @symbol{pango:text-transform} value}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{text-transform} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  How to transform the text for display.

  Since 4.6
  @see-class{gtk:text-tag}
  @see-symbol{pango:text-transform}
  @see-function{gtk:text-tag-text-transform-set}")

;;; --- gtk:text-tag-text-transform-set ----------------------------------------

#+(and gtk-4-6 liber-documentation)
(setf (documentation (liber:slot-documentation "text-transform-set"
                                               'text-tag) t)
 "The @code{text-transform-set} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether this tag affects the @code{text-transform} property. Since 4.6 @br{}
  Default value: @em{false}")

#+(and gtk-4-6 liber-documentation)
(setf (liber:alias-for-function 'text-tag-text-transform-set)
      "Accessor"
      (documentation 'text-tag-text-transform-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-text-transform-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-text-transform-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects the
    @slot[gtk:text-tag]{text-transform} property.}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{text-transform-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects the @slot[gtk:text-tag]{text-transform} property.

  Since 4.6
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-text-transform}")

;;; --- gtk:text-tag-underline -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "underline" 'text-tag) t)
 "The @code{underline} property of type @symbol{pango:underline} (Read / Write)
  @br{}
  Style of underline for this text. @br{}
  Default value: @code{:none}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-underline)
      "Accessor"
      (documentation 'text-tag-underline 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-underline object) => underline}
  @syntax[]{(setf (gtk:text-tag-underline object) underline)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[underline]{a @symbol{pango:underline} value}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{underline} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Style of underline for this text.
  @see-class{gtk:text-tag}
  @see-symbol{pango:underline}
  @see-function{gtk:text-tag-underline-rgba}
  @see-function{gtk:text-tag-underline-set}")

;;; --- gtk:text-tag-underline-rgba --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "underline-rgba" 'text-tag) t)
 "The @code{underline-rgba} property of type @class{gdk:rgba} (Read / Write)
  @br{}
  This property modifies the color of underlines. If not set, underlines will
  use the forground color. If the @code{underline} property is set to the
  @code{:error} value of the @symbol{pango:underline} enumeration, an alternate
  color may be applied instead of the foreground. Setting this property will
  always override those defaults.")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-underline-rgba)
      "Accessor"
      (documentation 'text-tag-underline-rgba 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-underline-rgba object) => color}
  @syntax[]{(setf (gtk:text-tag-underline-rgba object) color)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[color]{a @class{gdk:rgba} instance with the color}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{underline-rgba} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  This property modifies the color of underlines. If not set, underlines will
  use the forground color. If the @slot[gtk:text-tag]{underline} property is
  set to the @code{:error} value of the @symbol{pango:underline} enumeration,
  an alternate color may be applied instead of the foreground. Setting this
  property will always override those defaults.
  @see-class{gtk:text-tag}
  @see-class{gdk:rgba}
  @see-symbol{pango:underline}
  @see-function{gtk:text-tag-underline}
  @see-function{gtk:text-tag-underline-rgba-set}")

;;; --- gtk:text-tag-underline-rgba-set ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "underline-rgba-set"
                                               'text-tag) t)
 "The @code{underline-rgba-set} property of type @code{:boolean} (Read / Write)
  @br{}
  If the @code{underline-rgba} property has been set. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-underline-rgba-set)
      "Accessor"
      (documentation 'text-tag-underline-rgba-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-underline-rgba-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-underline-rgba-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether the @slot[gtk:text-tag]{underline-rgba}
    property has been set}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{underline-rgba-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  If the @slot[gtk:text-tag]{underline-rgba} property has been set.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-underline-rgba}")

;;; --- gtk:text-tag-underline-set ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "underline-set" 'text-tag) t)
 "The @code{underline-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects underlining. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-underline-set)
      "Accessor"
      (documentation 'text-tag-underline-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-underline-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-underline-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects underlining}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{underline-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects underlining.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-underline}
  @see-function{gtk:text-tag-underline-rgba}")

;;; --- gtk:text-tag-variant ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "variant" 'text-tag) t)
 "The @code{variant} property of type @symbol{pango:variant}
  (Read / Write) @br{}
  Font variant, e.g. the @code{:small-caps} value. @br{}
  Default value: @code{:normal}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-variant)
      "Accessor"
      (documentation 'text-tag-variant 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-variant object) => variant}
  @syntax[]{(setf (gtk:text-tag-variant object) variant)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[variant]{a @symbol{pango:variant} value}
  @begin{short}
    Accessor of the @slot[gtk:text-view]{variant} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Font variant as a value of the @symbol{pango:variant} enumeration, e.g. the
  @code{:small-caps} value.
  @see-class{gtk:text-tag}
  @see-symbol{pango:variant}
  @see-function{gtk:text-tag-variant-set}")

;;; --- gtk:text-tag-variant-set -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "variant-set" 'text-tag) t)
 "The @code{variant-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the font variant. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-variant-set)
      "Accessor"
      (documentation 'text-tag-variant-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-variant-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-variant-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this affects the font variant}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{variant-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects the font variant.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-variant}")

;;; --- gtk:text-tag-weight ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "weight" 'text-tag) t)
 "The @code{weight} property of type @code{:int} (Read / Write) @br{}
  Font weight as an integer, see predefined values in the @symbol{pango:weight}
  enumeration, for example, the @code{:bold} value. @br{}
  Allowed values: >= 0 @br{}
  Default value: 400")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-weight)
      "Accessor"
      (documentation 'text-tag-weight 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-weight object) => weight}
  @syntax[]{(setf (gtk:text-tag-weight object) weight)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[weight]{an integer for the font weight}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{weight} slot of the @class{gtk:text-tag}
    class.
  @end{short}
  Font weight as an integer, see predefined values in the @symbol{pango:weight}
  enumeration, for example, the @code{:bold} value.
  @see-class{gtk:text-tag}
  @see-symbol{pango:weight}
  @see-function{gtk:text-tag-weight-set}")

;;; --- gtk:text-tag-weight-set ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "weight-set" 'text-tag) t)
 "The @code{weight-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the font weight. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-weight-set)
      "Accessor"
      (documentation 'text-tag-weight-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-weight-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-weight-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects the font weight}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{weight-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects the font weight.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-weight}")

;;; --- gtk:text-tag-word ------------------------------------------------------

#+(and gtk-4-6 liber-documentation)
(setf (documentation (liber:slot-documentation "word" 'text-tag) t)
 "The @code{word} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag represents a single word. This affects line breaks and
  cursor movement. Since 4.6")

#+(and gtk-4-6 liber-documentation)
(setf (liber:alias-for-function 'text-tag-word)
      "Accessor"
      (documentation 'text-tag-word 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-word object) => setting}
  @syntax[]{(setf (gtk:text-tag-word object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag represents a single word}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{word} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag represents a single word. This affects line breaks and
  cursor movement.

  Since 4.6
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-word-set}")

;;; --- gtk:text-tag-word-set --------------------------------------------------

#+(and gtk-4-6 liber-documentation)
(setf (documentation (liber:slot-documentation "word-set" 'text-tag) t)
 "The @code{word-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the @code{word} property. Since 4.6 @br{}
  Default value: @em{false}")

#+(and gtk-4-6 liber-documentation)
(setf (liber:alias-for-function 'text-tag-word-set)
      "Accessor"
      (documentation 'text-tag-word-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-word-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-word-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects the
    @slot[gtk:text-tag]{word} property.}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{word-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects the @slot[gtk:text-tag]{word} property.

  Since 4.6
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-word}")

;;; --- gtk:text-tag-wrap-mode -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "wrap-mode" 'text-tag) t)
 "The @code{wrap-mode} property of type @symbol{gtk:wrap-mode} (Read / Write)
  @br{}
  Whether to wrap lines never, at word boundaries, or at character boundaries.
  @br{}
  Default value: @code{:none}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-wrap-mode)
      "Accessor"
      (documentation 'text-tag-wrap-mode 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-wrap-mode object) => wrap-mode}
  @syntax[]{(setf (gtk:text-tag-wrap-mode object) wrap-mode)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[wrap-mode]{a @symbol{gtk:wrap-mode} value}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{wrap-mode} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether to wrap lines never, at word boundaries, or at character boundaries.
  @see-class{gtk:text-tag}
  @see-symbol{gtk:wrap-mode}
  @see-function{gtk:text-tag-wrap-mode-set}")

;;; --- gtk:text-tag-wrap-mode-set ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "wrap-mode-set" 'text-tag) t)
 "The @code{wrap-mode-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects line wrap mode. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-tag-wrap-mode-set)
      "Accessor"
      (documentation 'text-tag-wrap-mode-set 'function)
 "@version{2024-3-8}
  @syntax[]{(gtk:text-tag-wrap-mode-set object) => setting}
  @syntax[]{(setf (gtk:text-tag-wrap-mode-set object) setting)}
  @argument[object]{a @class{gtk:text-tag} object}
  @argument[setting]{a boolean whether this tag affects line wrap mode}
  @begin{short}
    Accessor of the @slot[gtk:text-tag]{wrap-mode-set} slot of the
    @class{gtk:text-tag} class.
  @end{short}
  Whether this tag affects line wrap mode.
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-wrap-mode}")

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_new
;;; ----------------------------------------------------------------------------

(defun text-tag-new (name &rest args)
 #+liber-documentation
 "@version{2023-10-24}
  @argument[name]{a string with the tag name, or @code{nil}}
  @argument[args]{list of property keywords and values}
  @return{A new @class{gtk:text-tag} object.}
  @begin{short}
    Creates a new tag.
  @end{short}
  @begin[Example]{dictionary}
    Create a tag with name \"font-italic\":
    @begin{pre}
(gtk:text-tag-new \"font-italic\" :font \"fixed\" :style :italic)
=> #<gtk:text-tag {1006C86E63@}>
    @end{pre}
  @end{dictionary}
  @see-class{gtk:text-tag}"
  (if name
      (apply #'make-instance 'text-tag :name name args)
      (apply #'make-instance 'text-tag args)))

(export 'text-tag-new)

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_get_priority
;;; gtk_text_tag_set_priority -> text-tag-priority
;;; ----------------------------------------------------------------------------

(defun (setf text-tag-priority) (priority tag)
  (cffi:foreign-funcall "gtk_text_tag_set_priority"
                        (g:object text-tag) tag
                        :int priority
                        :void)
  priority)

(cffi:defcfun ("gtk_text_tag_get_priority" text-tag-priority) :int
 #+liber-documentation
 "@version{2023-10-24}
  @syntax[]{(gtk:text-tag-priority tag) => priority}
  @syntax[]{(setf (gtk:text-tag-priority tag) priority)}
  @argument[tag]{a @class{gtk:text-tag} object}
  @argument[priority]{an integer with the priority of the tag}
  @begin{short}
    Accessor for the priority of a @class{gtk:text-tag} object.
  @end{short}
  The @fun{gtk:text-tag-priority} function gets the tag priority. The
  @setf{gtk:text-tag-priority} function sets the priority.

  Valid priorities start at 0 and go to one less than the value of the result
  of the @fun{gtk:text-tag-table-size} function. Each tag in a
  @class{gt:tag-table} object has a unique priority. Setting the priority of one
  tag shifts the priorities of all the other tags in the tag table to maintain a
  unique priority for each tag. Higher priority tags \"win\" if two tags both
  set the same text attribute. When adding a tag to a tag table, it will be
  assigned the highest priority in the tag table by default. So normally the
  precedence of a set of tags is the order in which they were added to the tag
  table, or created with the @fun{gtk:text-buffer-create-tag} function, which
  adds the tag to the tag table of the text buffer automatically.
  @see-class{gtk:text-tag}
  @see-class{gtk:text-tag-table}
  @see-function{gtk:text-tag-table-size}
  @see-function{gtk:text-buffer-create-tag}"
  (tag (g:object text-tag)))

(export 'text-tag-priority)

;;; ----------------------------------------------------------------------------
;;; gtk_text_tag_changed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_tag_changed" text-tag-changed) :void
 #+liber-documentation
 "@version{2023-10-24}
  @argument[tag]{a @class{gtk:text-tag} object}
  @argument[changed]{a boolean whether the change affects the
    @class{gtk:text-view} layout}
  @begin{short}
    Emits the @code{\"tag-changed\"} signal on the @class{gtk:text-tag-table}
    object where the tag is included.
  @end{short}
  The signal is already emitted when setting a @class{gtk:text-tag} property.
  @see-class{gtk:text-tag}
  @see-class{gtk:text-tag-table}
  @see-class{gtk:text-view}"
  (tag (g:object text-tag))
  (changed :boolean))

(export 'text-tag-changed)

;;; --- End of file gtk4.text-tag.lisp -----------------------------------------
