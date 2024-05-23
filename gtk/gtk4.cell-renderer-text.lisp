;;; ----------------------------------------------------------------------------
;;; gtk4.cell-renderer-text.lisp
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
;;; --------------------------------------------------------------------------
;;;
;;; GtkCellRendererText
;;;
;;;     Renders text in a cell
;;;
;;; Types and Values
;;;
;;;     GtkCellRendererText
;;;
;;; Functions
;;;
;;;     gtk_cell_renderer_text_new
;;;     gtk_cell_renderer_text_set_fixed_height_from_font
;;;
;;; Properties
;;;
;;;     align-set
;;;     alignment
;;;     attributes
;;;     background
;;;     background-rgba
;;;     background-set
;;;     editable
;;;     editable-set
;;;     ellipsize
;;;     ellipsize-set
;;;     family
;;;     family-set
;;;     font
;;;     font-desc
;;;     foreground
;;;     foreground-rgba
;;;     foreground-set
;;;     language
;;;     language-set
;;;     markup
;;;     max-width-chars
;;;     placeholder-text
;;;     rise
;;;     rise-set
;;;     scale
;;;     scale-set
;;;     single-paragraph-mode
;;;     size
;;;     size-points
;;;     size-set
;;;     stretch
;;;     stretch-set
;;;     strikethrough
;;;     strikethrough-set
;;;     style
;;;     style-set
;;;     text
;;;     underline
;;;     underline-set
;;;     variant
;;;     variant-set
;;;     weight
;;;     weight-set
;;;     width-chars
;;;     wrap-mode
;;;     wrap-width
;;;
;;; Signals
;;;
;;;     edited
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkCellRenderer
;;;             ╰── GtkCellRendererText
;;;                 ├── GtkCellRendererAccel
;;;                 ├── GtkCellRendererCombo
;;;                 ╰── GtkCellRendererSpin
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkCellRendererText
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkCellRendererText" cell-renderer-text
  (:superclass cell-renderer
   :export t
   :interfaces nil
   :type-initializer "gtk_cell_renderer_text_get_type")
  ((align-set
    cell-renderer-text-align-set
    "align-set" "gboolean" t t)
   (alignment
    cell-renderer-text-alignment
    "alignment" "PangoAlignment" t t)
   (attributes
    cell-renderer-text-attributes
    "attributes" "PangoAttrList" t t)
   (background
    cell-renderer-text-background
    "background" "gchararray" nil t)
   (background-rgba
    cell-renderer-text-background-rgba
    "background-rgba" "GdkRGBA" t t)
   (background-set
    cell-renderer-text-background-set
    "background-set" "gboolean" t t)
   (editable
    cell-renderer-text-editable
    "editable" "gboolean" t t)
   (editable-set
    cell-renderer-text-editable-set
    "editable-set" "gboolean" t t)
   (ellipsize
    cell-renderer-text-ellipsize
    "ellipsize" "PangoEllipsizeMode" t t)
   (ellipsize-set
    cell-renderer-text-ellipsize-set
    "ellipsize-set" "gboolean" t t)
   (family
    cell-renderer-text-family
    "family" "gchararray" t t)
   (family-set
    cell-renderer-text-family-set
    "family-set" "gboolean" t t)
   (font
    cell-renderer-text-font
    "font" "gchararray" t t)
   (font-desc
    cell-renderer-text-font-desc
    "font-desc" "PangoFontDescription" t t)
   (foreground
    cell-renderer-text-foreground
    "foreground" "gchararray" nil t)
   (foreground-rgba
    cell-renderer-text-foreground-rgba
    "foreground-rgba" "GdkRGBA" t t)
   (foreground-set
    cell-renderer-text-foreground-set
    "foreground-set" "gboolean" t t)
   (language
    cell-renderer-text-language
    "language" "gchararray" t t)
   (language-set
    cell-renderer-text-language-set
    "language-set" "gboolean" t t)
   (markup
    cell-renderer-text-markup
    "markup" "gchararray" nil t)
   (max-width-chars
    cell-renderer-text-max-width-chars
    "max-width-chars" "gint" t t)
   (placeholder-text
    cell-renderer-text-placeholder-text
    "placeholder-text" "gchararray" t t)
   (rise
    cell-renderer-text-rise
    "rise" "gint" t t)
   (rise-set
    cell-renderer-text-rise-set
    "rise-set" "gboolean" t t)
   (scale
    cell-renderer-text-scale
    "scale" "gdouble" t t)
   (scale-set
    cell-renderer-text-scale-set
    "scale-set" "gboolean" t t)
   (single-paragraph-mode
    cell-renderer-text-single-paragraph-mode
    "single-paragraph-mode" "gboolean" t t)
   (size
    cell-renderer-text-size
    "size" "gint" t t)
   (size-points
    cell-renderer-text-size-points
    "size-points" "gdouble" t t)
   (size-set
    cell-renderer-text-size-set
    "size-set" "gboolean" t t)
   (stretch
    cell-renderer-text-stretch
    "stretch" "PangoStretch" t t)
   (stretch-set
    cell-renderer-text-stretch-set
    "stretch-set" "gboolean" t t)
   (strikethrough
    cell-renderer-text-strikethrough
    "strikethrough" "gboolean" t t)
   (strikethrough-set
    cell-renderer-text-strikethrough-set
    "strikethrough-set" "gboolean" t t)
   (style
    cell-renderer-text-style
    "style" "PangoStyle" t t)
   (style-set
    cell-renderer-text-style-set
    "style-set" "gboolean" t t)
   (text
    cell-renderer-text-text
    "text" "gchararray" t t)
   (underline
    cell-renderer-text-underline
    "underline" "PangoUnderline" t t)
   (underline-set
    cell-renderer-text-underline-set
    "underline-set" "gboolean" t t)
   (variant
    cell-renderer-text-variant
    "variant" "PangoVariant" t t)
   (variant-set
    cell-renderer-text-variant-set
    "variant-set" "gboolean" t t)
   (weight
    cell-renderer-text-weight
    "weight" "gint" t t)
   (weight-set
    cell-renderer-text-weight-set
    "weight-set" "gboolean" t t)
   (width-chars
    cell-renderer-text-width-chars
    "width-chars" "gint" t t)
   (wrap-mode
    cell-renderer-text-wrap-mode
    "wrap-mode" "PangoWrapMode" t t)
   (wrap-width
    cell-renderer-text-wrap-width
    "wrap-width" "gint" t t)))

#+liber-documentation
(setf (documentation 'cell-renderer-text 'type)
 "@version{2024-2-21}
  @begin{short}
    The @class{gtk:cell-renderer-text} object renders a given text in its cell,
    using the font, color and style information provided by its properties.
  @end{short}
  The text will be ellipsized if it is too long and the
  @slot[gtk:cell-renderer-text]{ellipsize} property allows it. If the
  @slot[gtk:cell-renderer]{mode} property is @code{:editable}, the
  @class{gtk:cell-renderer-text} object allows to edit its text using a
  @class{gtk:entry} widget.
  @begin[Signal Details]{dictionary}
    @subheading{The \"edited\" signal}
      @begin{pre}
lambda (renderer path text)    :run-last
      @end{pre}
      The signal is emitted after @arg{renderer} has been edited. It is the
      responsibility of the application to update the model and store @arg{text}
      at the position indicated by @arg{path}.
      @begin[code]{table}
        @entry[renderer]{The @class{gtk:cell-renderer-text} object which
          received the signal.}
        @entry[path]{A string with the path identifying the edited cell.}
        @entry[text]{A string with the new text.}
      @end{table}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    List views use widgets to display their contents. You should use the
    @class{gtk:inscription} or @class{gtk:label} widgets instead.
  @end{dictionary}
  @see-constructor{gtk:cell-renderer-text-new}
  @see-slot{gtk:cell-renderer-text-align-set}
  @see-slot{gtk:cell-renderer-text-alignment}
  @see-slot{gtk:cell-renderer-text-attributes}
  @see-slot{gtk:cell-renderer-text-background}
  @see-slot{gtk:cell-renderer-text-background-rgba}
  @see-slot{gtk:cell-renderer-text-background-set}
  @see-slot{gtk:cell-renderer-text-editable}
  @see-slot{gtk:cell-renderer-text-editable-set}
  @see-slot{gtk:cell-renderer-text-ellipsize}
  @see-slot{gtk:cell-renderer-text-ellipsize-set}
  @see-slot{gtk:cell-renderer-text-family}
  @see-slot{gtk:cell-renderer-text-family-set}
  @see-slot{gtk:cell-renderer-text-font}
  @see-slot{gtk:cell-renderer-text-font-desc}
  @see-slot{gtk:cell-renderer-text-foreground}
  @see-slot{gtk:cell-renderer-text-foreground-rgba}
  @see-slot{gtk:cell-renderer-text-foreground-set}
  @see-slot{gtk:cell-renderer-text-language}
  @see-slot{gtk:cell-renderer-text-language-set}
  @see-slot{gtk:cell-renderer-text-markup}
  @see-slot{gtk:cell-renderer-text-max-width-chars}
  @see-slot{gtk:cell-renderer-text-placeholder-text}
  @see-slot{gtk:cell-renderer-text-rise}
  @see-slot{gtk:cell-renderer-text-rise-set}
  @see-slot{gtk:cell-renderer-text-scale}
  @see-slot{gtk:cell-renderer-text-scale-set}
  @see-slot{gtk:cell-renderer-text-single-paragraph-mode}
  @see-slot{gtk:cell-renderer-text-size}
  @see-slot{gtk:cell-renderer-text-size-points}
  @see-slot{gtk:cell-renderer-text-size-set}
  @see-slot{gtk:cell-renderer-text-stretch}
  @see-slot{gtk:cell-renderer-text-stretch-set}
  @see-slot{gtk:cell-renderer-text-strikethrough}
  @see-slot{gtk:cell-renderer-text-strikethrough-set}
  @see-slot{gtk:cell-renderer-text-style}
  @see-slot{gtk:cell-renderer-text-style-set}
  @see-slot{gtk:cell-renderer-text-text}
  @see-slot{gtk:cell-renderer-text-underline}
  @see-slot{gtk:cell-renderer-text-underline-set}
  @see-slot{gtk:cell-renderer-text-variant}
  @see-slot{gtk:cell-renderer-text-variant-set}
  @see-slot{gtk:cell-renderer-text-weight}
  @see-slot{gtk:cell-renderer-text-weight-set}
  @see-slot{gtk:cell-renderer-text-width-chars}
  @see-slot{gtk:cell-renderer-text-wrap-mode}
  @see-slot{gtk:cell-renderer-text-wrap-width}
  @see-class{gtk:cell-renderer}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:cell-renderer-text-align-set ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "align-set"
                                               'cell-renderer-text) t)
 "The @code{align-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the alignment mode. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-align-set)
      "Accessor"
      (documentation 'cell-renderer-text-align-set 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-align-set object) => align-set}
  @syntax{(setf (gtk:cell-renderer-text-align-set object) align-set)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[align-set]{a boolean whether this tag affects the alignment mode}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{align-set} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-alignment ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "alignment"
                                               'cell-renderer-text) t)
 "The @code{alignment} property of type @symbol{pango:alignment} (Read / Write)
  @br{}
  Specifies how to align the lines of text with respect to each other. Note that
  this property describes how to align the lines of text in case there are
  several of them. The @slot[gtk:cell-renderer]{xalign} property, on the other
  hand, sets the horizontal alignment of the whole text. @br{}
  Default value: @code{:left}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-alignment)
      "Accessor"
      (documentation 'cell-renderer-text-alignment 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-alignment object) => alignment}
  @syntax{(setf (gtk:cell-renderer-text-alignment object) alignment)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[alignment]{a @symbol{pango:alignment} value}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{alignment} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  Specifies how to align the lines of text with respect to each other. Note that
  this property describes how to align the lines of text in case there are
  several of them. The @slot[gtk:cell-renderer]{xalign} property, on the other
  hand, sets the horizontal alignment of the whole text.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}
  @see-symbol{pango:alignment}")

;;; --- gtk:cell-renderer-text-attributes --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "attributes"
                                               'cell-renderer-text) t)
 "The @code{attributes} property of type @class{pango:attr-list} (Read / Write)
  @br{}
  A list of style attributes to apply to the text of the renderer.")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-attributes)
      "Accessor"
      (documentation 'cell-renderer-text-attributes 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-attributes object) => attributes}
  @syntax{(setf (gtk:cell-renderer-text-attributes object) attributes)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[attributes]{a @class{pango:attr-list} instance}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer]{attributes} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  A list of style attributes to apply to the text of the renderer.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}
  @see-class{pango:attr-list}")

;;; --- gtk:cell-renderer-text-background --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "background"
                                               'cell-renderer-text) t)
 "The @code{background} property of type @code{:string} (Write) @br{}
  Background color as a string. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-background)
      "Accessor"
      (documentation 'cell-renderer-text-background 'function)
 "@version{2024-2-21}
  @syntax{(setf (gtk:cell-renderer-text-background object) background)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[background]{a string with the background color}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{background} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-background-rgba ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "background-rgba"
                                               'cell-renderer-text) t)
 "The @code{background-rgba} property of type @class{gdk:rgba} (Read / Write)
  @br{}
  Background color.")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-background-rgba)
      "Accessor"
      (documentation 'cell-renderer-text-background-rgba 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-background-rgba object) => background}
  @syntax{(setf (gtk:cell-renderer-text-background-rgba object) background)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[background]{a @class{gdk:rgba} instance with the background color}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{background-rgba} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}
  @see-class{gdk:rgba}")

;;; --- gtk:cell-renderer-text-background-set ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "background-set"
                                               'cell-renderer-text) t)
 "The @code{background-set} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether this tag affects the background color. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-background-set)
      "Accessor"
      (documentation 'cell-renderer-text-background-set 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-background-set object) => setting}
  @syntax{(setf (gtk:cell-renderer-text-background-set object) setting)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[setting]{a boolean whether this tag affects the background color}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{background-set} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-editable ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "editable"
                                               'cell-renderer-text) t)
 "The @code{editable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the text can be modified by the user. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-editable)
      "Accessor"
      (documentation 'cell-renderer-text-editable 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-editable object) => setting}
  @syntax{(setf (gtk:cell-renderer-text-editable object) setting)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[setting]{a boolean whether the text can be modified by the user}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{editable} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-editable-set ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "editable-set"
                                               'cell-renderer-text) t)
 "The @code{editable-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects text editability. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-editable-set)
      "Accessor"
      (documentation 'cell-renderer-text-editable-set 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-editable-set object) => setting}
  @syntax{(setf (gtk:cell-renderer-text-editable-set object) setting)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[setting]{a boolean whether this tag affects text editability}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{editable-set} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-ellipsize ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "ellipsize"
                                               'cell-renderer-text) t)
 "The @code{ellipsize} property of type @symbol{pango:ellipsize-mode}
  (Read / Write) @br{}
  Specifies the preferred place to ellipsize the string, if the cell renderer
  does not have enough room to display the entire string. Setting it to
  the @code{:none} value turns off ellipsizing. See the
  @slot[gtk:cell-renderer-text]{wrap-width} property for another way of making
  the text fit into a given width. @br{}
  Default value: @code{:none}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-ellipsize)
      "Accessor"
      (documentation 'cell-renderer-text-ellipsize 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-ellipsize object) => mode}
  @syntax{(setf (gtk:cell-renderer-text-ellipsize object) mode)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[mode]{a @symbol{pango:ellipsize-mode} value}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{ellipsize} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  Specifies the preferred place to ellipsize the string, if the cell renderer
  does not have enough room to display the entire string. Setting it to the
  @code{:none} value turns off ellipsizing. See the
  @slot[gtk:cell-renderer-text]{wrap-width} property for another way of making
  the text fit into a given width.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}
  @see-symbol{pango:ellipsize-mode}
  @see-function{gtk:cell-renderer-text-wrap-width}")

;;; --- gtk:cell-renderer-text-ellipsize-set -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "ellipsize-set"
                                               'cell-renderer-text) t)
 "The @code{ellipsize-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the ellipsize mode. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-ellipsize-set)
      "Accessor"
      (documentation 'cell-renderer-text-ellipsize-set 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-ellipsize-set object) => setting}
  @syntax{(setf (gtk:cell-renderer-text-ellipsize-set object) setting)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[setting]{a boolean whether this tag affects the ellipsize mode}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{ellipsize-set} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-family ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "family"
                                               'cell-renderer-text) t)
 "The @code{family} property of type @code{:string} (Read / Write) @br{}
  Name of the font family, e.g. Sans, Helvetica, Times, Monospace. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-family)
      "Accessor"
      (documentation 'cell-renderer-text-family 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-family object) => font}
  @syntax{(setf (gtk:cell-renderer-text-family object) font)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[font]{a string with the name of the font family}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{family} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  Name of the font family, e.g. Sans, Helvetica, Times, Monospace.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-family-set --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "family-set"
                                               'cell-renderer-text) t)
 "The @code{family-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the font family. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-family-set)
      "Accessor"
      (documentation 'cell-renderer-text-family-set 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-family-set object) => setting}
  @syntax{(setf (gtk:cell-renderer-text-family-set object) setting)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[setting]{a boolean whether this tag affects the font family}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{family-set} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}
  @see-function{gtk:cell-renderer-text-family}")

;;; --- gtk:cell-renderer-text-font --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "font"
                                               'cell-renderer-text) t)
 "The @code{font} property of type @code{:string} (Read / Write) @br{}
  Font description as a string, e.g. \"Sans Italic 12\". @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-font)
      "Accessor"
      (documentation 'cell-renderer-text-font 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-font object) => font}
  @syntax{(setf (gtk:cell-renderer-text-font object) font)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[font]{a string with the font description}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{font} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  Font description as a string, e.g. \"Sans Italic 12\".
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-font-desc ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "font-desc"
                                               'cell-renderer-text) t)
 "The @code{font-desc} property of type @class{pango:font-description}
  (Read / Write) @br{}
  A Pango font description.")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-font-desc)
      "Accessor"
      (documentation 'cell-renderer-text-font-desc 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-font-desc object) => font-desc}
  @syntax{(setf (gtk:cell-renderer-text-font-desc object) font-desc)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[font-desc]{a @class{pango:font-description} instance}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{font-desc} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  A Pango font description.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}
  @see-class{pango:font-description}")

;;; --- gtk:cell-renderer-text-foreground --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "foreground"
                                               'cell-renderer-text) t)
 "The @code{foreground} property of type @code{:string} (Write) @br{}
  Foreground color as a string. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-foreground)
      "Accessor"
      (documentation 'cell-renderer-text-foreground 'function)
 "@version{2024-5-22}
  @syntax{(setf (gtk:cell-renderer-text-foreground object) foreground)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[foreground]{a string with the foreground color}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{foreground} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-foreground-rgba ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "foreground-rgba"
                                               'cell-renderer-text) t)
 "The @code{foreground-rgba} property of type @class{gdk:rgba} (Read / Write)
  @br{}
  Foreground color.")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-foreground-rgba)
      "Accessor"
      (documentation 'cell-renderer-text-foreground-rgba 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-foreground-rgba object) => foreground}
  @syntax{(setf (gtk:cell-renderer-text-foreground-rgba object) foreground)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[foreground]{a @class{gdk:rgba} instance}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{foreground-rgba} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}
  @see-class{gdk:rgba}")

;;; --- gtk:cell-renderer-text-foreground-set ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "foreground-set"
                                               'cell-renderer-text) t)
 "The @code{foreground-set} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether this tag affects the foreground color. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-foreground-set)
      "Accessor"
      (documentation 'cell-renderer-text-foreground-set 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-foreground-set object) => setting}
  @syntax{(setf (gtk:cell-renderer-text-foreground-set object) setting)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[setting]{a boolean whether this tag affects the foreground color}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{foreground-set} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-language ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "language"
                                               'cell-renderer-text) t)
 "The @code{language} property of type @code{:string} (Read / Write) @br{}
  The language this text is in, as an ISO code. Pango can use this as a hint
  when rendering the text. If you do not understand this parameter, you
  probably do not need it. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-language)
      "Accessor"
      (documentation 'cell-renderer-text-language 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-language object) => language}
  @syntax{(setf (gtk:cell-renderer-text-language object) language)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[language]{a string with the language this text is in}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{language} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  The language this text is in, as an ISO code. Pango can use this as a hint
  when rendering the text. If you do not understand this parameter, you
  probably do not need it.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-language-set ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "language-set"
                                               'cell-renderer-text) t)
 "The @code{language-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the language the text is rendered as. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-language-set)
      "Accessor"
      (documentation 'cell-renderer-text-language-set 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-language-set object) => setting}
  @syntax{(setf (gtk:cell-renderer-text-language-set object) setting)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[setting]{a boolean whether this tag affects the language}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{language-set} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-markup ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "markup"
                                               'cell-renderer-text) t)
 "The @code{markup} property of type @code{:string} (Write) @br{}
  Marked up text to render. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-markup)
      "Accessor"
      (documentation 'cell-renderer-text-markup 'function)
 "@version{2024-2-21}
  @syntax{(setf (gtk:cell-renderer-text-markup object) markup)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[markup]{a string with the marked up text to render}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{markup} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-max-width-chars ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "max-width-chars"
                                               'cell-renderer-text) t)
 "The @code{max-width-chars} property of type @code{:int} (Read / Write) @br{}
  The desired maximum width of the cell, in characters. If this property is
  set to -1, the width will be calculated automatically. For cell renderers that
  ellipsize or wrap text. This property controls the maximum reported width of
  the cell. The cell should not receive any greater allocation unless it is set
  to expand in its @class{gtk:cell-layout} object and all of the cell's siblings
  have received their natural width. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-max-width-chars)
      "Accessor"
      (documentation 'cell-renderer-text-max-width-chars 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-max-width-chars object) => width}
  @syntax{(setf (gtk:cell-renderer-text-max-width-chars object) width)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[width]{an integer with the maximum width of the cell}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{max-width-chars} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  The desired maximum width of the cell, in characters. If this property is
  set to -1, the width will be calculated automatically. For cell renderers that
  ellipsize or wrap text. This property controls the maximum reported width of
  the cell. The cell should not receive any greater allocation unless it is set
  to expand in its @class{gtk:cell-layout} object and all of the cell's siblings
  have received their natural width.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}
  @see-class{gtk:cell-layout}")

;;; --- gtk:cell-renderer-text-placeholder-text --------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "placeholder-text"
                                               'cell-renderer-text) t)
 "The @code{placeholder-text} property of type @code{:string} (Read / Write)
  @br{}
  The text that will be displayed in the @class{gtk:cell-renderer} object if the
  @slot[gtk:cell-renderer-text]{editable} property is @em{true} and the cell is
  empty. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-placeholder-text)
      "Accessor"
      (documentation 'cell-renderer-text-placeholder-text 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-placeholder-text object) => text}
  @syntax{(setf (gtk:cell-renderer-text-placeholder-text object) text)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[text]{a string with the placeholder text}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{placeholder-text} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  The text that will be displayed in the @class{gtk:cell-renderer} object if
  the @slot[gtk:cell-renderer-text]{editable} property is @em{true} and the cell
  is empty.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}
  @see-class{gtk:cell-renderer}")

;;; --- gtk:cell-renderer-text-rise --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "rise" 'cell-renderer-text) t)
 "The @code{rise} property of type @code{:int} (Read / Write) @br{}
  Offset of text above the baseline, below the baseline if this property is
  negative. @br{}
  Allowed values: >= -2147483647 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-rise)
      "Accessor"
      (documentation 'cell-renderer-text-rise 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-rise object) => rise}
  @syntax{(setf (gtk:cell-renderer-text-rise object) rise)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[rise]{an integer with the offset of text above the baseline}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{rise} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  Offset of text above the baseline, below the baseline if the
  @slot[gtk:cell-renderer-text]{rise} property is negative.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-rise-set ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "rise-set"
                                               'cell-renderer-text) t)
 "The @code{rise-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the rise. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-rise-set)
      "Accessor"
      (documentation 'cell-renderer-text-rise-set 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-rise-set object) => setting}
  @syntax{(setf (gtk:cell-renderer-text-rise-set object) setting)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[setting]{a boolean whether this tag affects the rise}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{rise-set} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-scale -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "scale"
                                               'cell-renderer-text) t)
 "The @code{scale} property of type @code{:double} (Read / Write) @br{}
  Font scaling factor. @br{}
  Allowed values: >= 0 @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-scale)
      "Accessor"
      (documentation 'cell-renderer-text-scale 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-scale object) => scale}
  @syntax{(setf (gtk:cell-renderer-text-scale object) scale)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[scale]{a double float with the font scaling factor}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{scale} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-scale-set ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "scale-set"
                                               'cell-renderer-text) t)
 "The @code{scale-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag scales the font size by a factor. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-scale-set)
      "Accessor"
      (documentation 'cell-renderer-text-scale-set 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-scale-set object) => setting}
  @syntax{(setf (gtk:cell-renderer-text-scale-set object) setting)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[setting]{a boolean whether this tag scales the font size by a
   factor}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{scale-set} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-single-paragraph-mode ---------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "single-paragraph-mode"
                                               'cell-renderer-text) t)
 "The @code{single-paragraph-mode} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to keep all text in a single paragraph. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-single-paragraph-mode)
      "Accessor"
      (documentation 'cell-renderer-text-single-paragraph-mode 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-single-paragraph-mode object) => mode}
  @syntax{(setf (gtk:cell-renderer-text-single-paragraph-mode object) mode)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[mode]{a boolean whether to keep all text in a single paragraph}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{single-paragraph-mode} slot
    of the @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-size --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "size" 'cell-renderer-text) t)
 "The @code{size} property of type @code{:int} (Read / Write) @br{}
  Font size. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-size)
      "Accessor"
      (documentation 'cell-renderer-text-size 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-size object) => size}
  @syntax{(setf (gtk:cell-renderer-text-size object) size)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[size]{an integer with the font size}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{size} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-size-points -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "size-points"
                                               'cell-renderer-text) t)
 "The @code{size-points} property of type @code{:double} (Read / Write) @br{}
  Font size in points. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-size-points)
      "Accessor"
      (documentation 'cell-renderer-text-size-points 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-size-points object) => size}
  @syntax{(setf (gtk:cell-renderer-text-size-points object) size)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[size]{a double float with the font size in points}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{size-points} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-size-set ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "size-set"
                                               'cell-renderer-text) t)
 "The @code{size-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the font size. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-size-set)
      "Accessor"
      (documentation 'cell-renderer-text-size-set 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-size-set object) => setting}
  @syntax{(setf (gtk:cell-renderer-text-size-set object) setting)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[setting]{a boolean whether this tag affects the font size}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{size-set} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-stretch -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "stretch"
                                               'cell-renderer-text) t)
 "The @code{stretch} property of type @symbol{pango:stretch} (Read / Write)
  @br{}
  Font stretch. @br{}
  Default value: @code{:normal}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-stretch)
      "Accessor"
      (documentation 'cell-renderer-text-stretch 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-stretch object) => stretch}
  @syntax{(setf (gtk:cell-renderer-text-stretch object) stretch)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[stretch]{a @symbol{pango:stretch} value with the font stretch}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{stretch} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}
  @see-symbol{pango:stretch}")

;;; --- gtk:cell-renderer-text-stretch-set -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "stretch-set"
                                               'cell-renderer-text) t)
 "The @code{stretch-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the font stretch. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-stretch-set)
      "Accessor"
      (documentation 'cell-renderer-text-stretch-set 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-stretch-set object) => setting}
  @syntax{(setf (gtk:cell-renderer-text-stretch-set object) setting)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[setting]{a boolean whether this tag affects the font stretch}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{stretch-set} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-strikethrough -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "strikethrough"
                                               'cell-renderer-text) t)
 "The @code{strikethrough} property @code{:boolean} (Read / Write) @br{}
  Whether to strike through the text. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-strikethrough)
      "Accessor"
      (documentation 'cell-renderer-text-strikethrough 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-strikethrough object) => setting}
  @syntax{(setf (gtk:cell-renderer-text-strikethrough object) setting)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[setting]{a boolean whether to strike through the text}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{strikethrough} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-strikethrough-set -------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "strikethrough-set"
                                               'cell-renderer-text) t)
 "The @code{strikethrough-set} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether this tag affects strikethrough. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-strikethrough-set)
      "Accessor"
      (documentation 'cell-renderer-text-strikethrough-set 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-strikethrough-set object) => setting}
  @syntax{(setf (gtk:cell-renderer-text-strikethrough-set object) setting)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[setting]{a boolean whether this tag affects strikethrough}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{strikethrough-set} slot of
    the @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-style -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "style"
                                               'cell-renderer-text) t)
 "The @code{style} property of type @symbol{pango:style} (Read / Write) @br{}
  Font style. @br{}
  Default value: @code{:normal}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-style)
      "Accessor"
      (documentation 'cell-renderer-text-style 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-style object) => style}
  @syntax{(setf (gtk:cell-renderer-text-style object) style)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[style]{a @symbol{pango:style} value with the font style}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{style} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}
  @see-symbol{pango:style}")

;;; --- gtk:cell-renderer-text-style-set ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "style-set"
                                               'cell-renderer-text) t)
 "The @code{style-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the font style. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-style-set)
      "Accessor"
      (documentation 'cell-renderer-text-style-set 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-style-set object) => setting}
  @syntax{(setf (gtk:cell-renderer-text-style-set object) setting)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[setting]{a boolean whether this tag affects the font style}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{style-set} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-text --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "text"
                                               'cell-renderer-text) t)
 "The @code{text} property of type @code{:string} (Read / Write) @br{}
  Text to render. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-text)
      "Accessor"
      (documentation 'cell-renderer-text-text 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-text object) => text}
  @syntax{(setf (gtk:cell-renderer-text-text object) text)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[text]{a string with the text to render}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{text} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-underline ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "underline"
                                               'cell-renderer-text) t)
 "The @code{underline} property of type @symbol{pango:underline} (Read / Write)
  @br{}
  Style of underline for this text. @br{}
  Default value: @code{:none}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-underline)
      "Accessor"
      (documentation 'cell-renderer-text-underline 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-underline object) => underline}
  @syntax{(setf (gtk:cell-renderer-text-underline object) underline)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[underline]{a @symbol{pango:underline} value with the style of
    underline for this text}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{underline} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}
  @see-symbol{pango:underline}")

;;; --- gtk:cell-renderer-text-underline-set -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "underline-set"
                                               'cell-renderer-text) t)
 "The @code{underline-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects underlining. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-underline-set)
      "Accessor"
      (documentation 'cell-renderer-text-underline-set 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-underline-set object) => setting}
  @syntax{(setf (gtk:cell-renderer-text-underline-set object) setting)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[setting]{a boolean whether this tag affects underlining}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{underline-set} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-variant -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "variant"
                                               'cell-renderer-text) t)
 "The @code{variant} property of type @symbol{pango:variant} (Read / Write)
  @br{}
  Font variant. @br{}
  Default value: @code{:normal}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-variant)
      "Accessor"
      (documentation 'cell-renderer-text-variant 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-variant object) => variant}
  @syntax{(setf (gtk:cell-renderer-text-variant object) variant)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[variant]{a @symbol{pango:variant} value with the font variant}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{variant} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}
  @see-symbol{pango:variant}")

;;; --- gtk:cell-renderer-text-variant-set -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "variant-set"
                                               'cell-renderer-text) t)
 "The @code{variant-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the font variant. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-variant-set)
      "Accessor"
      (documentation 'cell-renderer-text-variant-set 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-variant-set object) => setting}
  @syntax{(setf (gtk:cell-renderer-text-variant-set object) setting)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[setting]{a boolean whether this tag affects the font variant}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{variant-set} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-weight ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "weight"
                                               'cell-renderer-text) t)
 "The @code{weight} property of type @code{:int} (Read / Write) @br{}
  Font weight. @br{}
  Allowed values: >= 0 @br{}
  Default value: 400")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-weight)
      "Accessor"
      (documentation 'cell-renderer-text-weight 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-weight object) => weight}
  @syntax{(setf (gtk:cell-renderer-text-weight object) weight)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[weight]{an integer with the font weight}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{weight} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-weight-set --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "weight-set"
                                               'cell-renderer-text) t)
 "The @code{weight-set} property of type @code{:boolean} (Read / Write) @br{}
  Whether this tag affects the font weight. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-weight-set)
      "Accessor"
      (documentation 'cell-renderer-text-weight-set 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-weight-set object) => setting}
  @syntax{(setf (gtk:cell-renderer-text-weight-set object) setting)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[setting]{a boolean whether this tag affects the font weight}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{weight-set} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-width-chars -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "width-chars"
                                               'cell-renderer-text) t)
 "The @code{width-chars} property of type @code{:int} (Read / Write) @br{}
  The desired width of the cell, in characters. If this property is set to -1,
  the width will be calculated automatically, otherwise the cell will request
  either 3 characters or the property value, whichever is greater. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-width-chars)
      "Accessor"
      (documentation 'cell-renderer-text-width-chars 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-width-chars object) => width}
  @syntax{(setf (gtk:cell-renderer-text-width-chars object) width)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[width]{an integer with the width of the cell}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{width-chars} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  The desired width of the cell, in characters. If this property is set to -1,
  the width will be calculated automatically, otherwise the cell will request
  either 3 characters or the property value, whichever is greater.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}")

;;; --- gtk:cell-renderer-text-wrap-mode ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "wrap-mode"
                                               'cell-renderer-text) t)
 "The @code{wrap-mode} property of type @symbol{pango:wrap-mode} (Read / Write)
  @br{}
  Specifies how to break the string into multiple lines, if the cell renderer
  does not have enough room to display the entire string. This property has no
  effect unless the @slot[gtk:cell-renderer-text]{wrap-width} property is set.
  @br{}
  Default value: @code{:char}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-wrap-mode)
      "Accessor"
      (documentation 'cell-renderer-text-wrap-mode 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-wrap-mode object) => mode}
  @syntax{(setf (gtk:cell-renderer-text-wrap-mode object) mode)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[mode]{a @symbol{pango:wrap-mode} value}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{wrap-mode} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  Specifies how to break the string into multiple lines, if the cell renderer
  does not have enough room to display the entire string. This property has no
  effect unless the @slot[gtk:cell-renderer-text]{wrap-width} property is set.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}
  @see-symbol{pango:wrap-mode}
  @see-function{gtk:cell-renderer-text-wrap-width}")

;;; --- gtk:cell-renderer-text-wrap-width --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "wrap-width"
                                               'cell-renderer-text) t)
 "The @code{wrap-width} property of type @code{:int} (Read / Write) @br{}
  Specifies the minimum width at which the text is wrapped. This property can be
  used to influence at what character positions the line breaks can be placed.
  Setting this property to -1 turns wrapping off. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-text-wrap-width)
      "Accessor"
      (documentation 'cell-renderer-text-wrap-width 'function)
 "@version{2024-2-21}
  @syntax{(gtk:cell-renderer-text-wrap-width object) => width}
  @syntax{(setf (gtk:cell-renderer-text-wrap-width object) width)}
  @argument[object]{a @class{gtk:cell-renderer-text} object}
  @argument[width]{an integer with the minimum width at which text is wrapped}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-text]{wrap-width} slot of the
    @class{gtk:cell-renderer-text} class.
  @end{short}
  Specifies the minimum width at which the text is wrapped. The
  @slot[gtk:cell-renderer-text]{wrap-mode} property can be used to influence at
  what character positions the line breaks can be placed. Setting the
  @arg{width} argument to -1 turns wrapping off.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}
  @see-function{gtk:cell-renderer-text-wrap-mode}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_text_new
;;; ----------------------------------------------------------------------------

(declaim (inline cell-renderer-text-new))

(defun cell-renderer-text-new ()
 #+liber-documentation
 "@version{2024-2-21}
  @return{The new @class{gtk:cell-renderer-text} object.}
  @begin{short}
    Creates a new cell renderer text.
  @end{short}
  Adjust how text is drawn using object properties. Object properties can be
  set globally, with the @fun{g:object-property} function. Also, with the
  @class{gtk:tree-view-column} object, you can bind a property to a value in a
  @class{gtk:tree-model} object. For example, you can bind the
  @slot[gtk:cell-renderer-text]{text} property on the cell renderer to a string
  value in the model, thus rendering a different string in each row of the
  @class{gtk:tree-view} widget.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-view-column}
  @see-class{gtk:tree-model}
  @see-function{g:object-property}
  @see-function{gtk:cell-renderer-text-text}"
  (make-instance 'cell-renderer-text))

(export 'cell-renderer-text-new)

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_text_set_fixed_height_from_font
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_cell_renderer_text_set_fixed_height_from_font"
               cell-renderer-text-set-fixed-height-from-font) :void
 #+liber-documentation
 "@version{#2024-2-21}
  @argument[renderer]{a @class{gtk:cell-renderer-text} object}
  @argument[number-of-rows]{an integer with the number of rows of text each cell
    renderer is allocated, or -1}
  @begin{short}
    Sets the height of a renderer to explicitly be determined by the
    @code{font} and @code{y-pad} properties set on it.
  @end{short}
  Further changes in these properties do not affect the height, so they must be
  accompanied by a subsequent call to this function. Using this function is
  unflexible, and should really only be used if calculating the size of a cell
  is too slow, i.e., a massive number of cells displayed. If the
  @arg{number-of-rows} argument is -1, then the fixed height is unset, and the
  height is determined by the properties again.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-text} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-text}"
  (renderer (g:object cell-renderer-text))
  (number-of-rows :int))

(export 'cell-renderer-text-set-fixed-height-from-font)

;;; --- End of file gtk4.cell-renderer-text.lisp -------------------------------
