;;; ----------------------------------------------------------------------------
;;; gtk4.color-chooser.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2023 Dieter Kaiser
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
;;; GtkColorChooser
;;;
;;;     Interface implemented by widgets for choosing colors
;;;
;;; Types and Values
;;;
;;;     GtkColorChooser
;;;
;;; Accessors
;;;
;;;     gtk_color_chooser_get_rgba
;;;     gtk_color_chooser_set_rgba
;;;     gtk_color_chooser_get_use_alpha
;;;     gtk_color_chooser_set_use_alpha
;;;
;;; Functions
;;;
;;;     gtk_color_chooser_add_palette
;;;
;;;     gtk_hsv_to_rgb
;;;     gtk_rgb_to_hsv
;;;
;;; Properties
;;;
;;;     rgba
;;;     use-alpha
;;;
;;; Signals
;;;
;;;     color-activated
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkColorChooser
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkColorChooser
;;; ----------------------------------------------------------------------------

(gobject:define-g-interface "GtkColorChooser" color-chooser
  (:export t
   :type-initializer "gtk_color_chooser_get_type")
  ((rgba
    color-chooser-rgba
    "rgba" "GdkRGBA" t t)
   (use-alpha
    color-chooser-use-alpha
    "use-alpha" "gboolean" t t)))

#+liber-documentation
(setf (liber:alias-for-class 'color-chooser)
      "Interface"
      (documentation 'color-chooser 'type)
 "@version{#2022-8-27}
  @begin{short}
    The @class{gtk:color-chooser} interface is an interface that is implemented
    by widgets for choosing colors.
  @end{short}
  Depending on the situation, colors may be allowed to have alpha
  (translucency).

  The main widgets that implement this interface are the
  @class{gtk:color-button}, @class{gtk:color-chooser-widget}, and
  @class{gtk:color-chooser-dialog} widgets.
  @begin[Warning]{dictionary}
    The @class{gtk:color-chooser} implementation is deprecated since 4.10.
    Use the @class{gtk:color-dialog} and @class{gtk:color-dialog-button} widgets
    instead.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"color-activated\" signal}
      @begin{pre}
lambda (chooser color)    :run-first
      @end{pre}
      Emitted when a color is activated from the color chooser. This usually
      happens when the user clicks a color swatch, or a color is selected and
      the user presses one of the @kbd{Space}, @kbd{Shift+Space}, @kbd{Return}
      or @kbd{Enter} keys.
      @begin[code]{table}
        @entry[chooser]{The @class{gtk:color-chooser} widget which received
          the signal.}
        @entry[color]{The @struct{gdk:rgba} color.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:color-chooser-rgba}
  @see-slot{gtk:color-chooser-use-alpha}
  @see-struct{gdk:rgba}
  @see-class{gtk:color-button}
  @see-class{gtk:color-chooser-dialog}
  @see-class{gtk:color-chooser-widget}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- color-chooser-rgba -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "rgba" 'color-chooser) t)
 "The @code{rgba} property of type @struct{gdk:rgba} (Read / Write) @br{}
  The currently selected color, as a RGBA color. The property can be set to
  change the current selection programmatically.")

#+liber-documentation
(setf (liber:alias-for-function 'color-chooser-rgba)
      "Accessor"
      (documentation 'color-chooser-rgba 'function)
 "@version{#2022-8-27}
  @syntax[]{(gtk:color-chooser-rgba object) => color}
  @syntax[]{(setf (gtk:color-chooser-rgba object) color)}
  @argument[object]{a @class{gtk:color-chooser} widget}
  @argument[color]{a @struct{gdk:rgba} color}
  @begin{short}
    Accessor of the @slot[gtk:color-chooser]{rgba} slot of the
    @class{gtk:color-chooser} class.
  @end{short}
  The @class{gtk:color-chooser-rgba} function gets the currently selected color.
  The @setf{gtk:color-chooser-rgba} function sets the color.
  @begin[Warning]{dictionary}
    The @class{gtk:color-chooser} implementation is deprecated since 4.10.
    Use the @class{gtk:color-dialog} widget instead.
  @end{dictionary}
  @see-struct{gdk:rgba}
  @see-class{gtk:color-chooser}")

;;; --- color-chooser-use-alpha --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-alpha"
                                               'color-chooser) t)
 "The @code{use-alpha} property of type @code{:boolean} (Read / Write) @br{}
  Whether colors may have alpha (translucency). When it is @em{false}, the RGBA
  color obtained via the @code{rgba} property will be forced to have the alpha
  value 1.0. Implementations are expected to show alpha by rendering the color
  over a non-uniform background (like a checkerboard pattern). @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'color-chooser-use-alpha)
      "Accessor"
      (documentation 'color-chooser-use-alpha 'function)
 "@version{#2022-8-27}
  @syntax[]{(gtk:color-chooser-use-alpha object) => use-alpha}
  @syntax[]{(setf (gtk:color-chooser-use-alpha object) use-alpha)}
  @argument[object]{a @class{gtk:color-chooser} widget}
  @argument[use-alpha]{@em{true} if the color chooser should use alpha channel,
    @em{false} if not}
  @begin{short}
    Accessor of the @slot[gtk:color-chooser]{use-alpha} slot of the
    @class{gtk:color-chooser} class.
  @end{short}
  The @fun{gtk:color-chooser-use-alpha} function returns whether the color
  chooser shows the alpha channel. The @setf{gtk:color-chooser-use-alpha}
  function sets the property.
  @begin[Warning]{dictionary}
    The @class{gtk:color-chooser} implementation is deprecated since 4.10.
    Use the @class{gtk:color-dialog} widget instead.
  @end{dictionary}
  @see-class{gtk:color-chooser}")

;;; ----------------------------------------------------------------------------
;;; gtk_color_chooser_add_palette ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_color_chooser_add_palette" %color-chooser-add-palette) :void
  (chooser (g:object color-chooser))
  (orientation orientation)
  (colors-per-line :int)
  (n-colors :int)
  (colors :pointer))

(defun color-chooser-add-palette (chooser
                                  orientation
                                  colors-per-line
                                  colors)
 #+liber-documentation
 "@version{#2022-8-27}
  @argument[chooser]{a @class{gtk:color-chooser} widget}
  @argument[orientation]{a value of the @symbol{gtk:orientation} enumeration}
  @argument[colors-per-line]{an integer with the number of colors to show in
    each row/column}
  @argument[colors]{a list with the @struct{gdk:rgba} colors of the palette,
    or @code{nil}}
  @begin{short}
    Adds a palette to the color chooser.
  @end{short}
  If the orientation is @code{:horizontal}, the colors are grouped in rows,
  with @arg{colors-per-line} colors in each row. If the orientation is
  @code{:vertical}, the colors are grouped in columns instead.

  The default color palette of the color chooser widget has 34 colors,
  organized in columns of 5 colors. This includes some grays. The layout of the
  color chooser widget works best when the palettes have 9-10 columns.

  Calling this function for the first time has the side effect of removing the
  default color and gray palettes from the color chooser. If @arg{colors} is
  @code{nil}, removes all previously added palettes.
  @begin[Warning]{dictionary}
    The @class{gtk:color-chooser} implementation is deprecated since 4.10.
    Use the @class{gtk:color-dialog} widget instead.
  @end{dictionary}
  @see-struct{gdk:rgba}
  @see-class{gtk:color-chooser}
  @see-symbol{gtk:orientation}"
  (if colors
      (glib:with-foreign-boxed-array (n-colors colors-ptr gdk:rgba colors)
        (%color-chooser-add-palette chooser
                                    orientation
                                    colors-per-line
                                    n-colors
                                    colors-ptr))
      (%color-chooser-add-palette chooser
                                  orientation
                                  colors-per-line
                                  0
                                  (cffi:null-pointer))))

(export 'color-chooser-add-palette)

;;; ----------------------------------------------------------------------------
;;; gtk_hsv_to_rgb ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_hsv_to_rgb" %hsv-to-rgb) :void
  (h :float)
  (s :float)
  (v :float)
  (r (:pointer :float))
  (g (:pointer :float))
  (b (:pointer :float)))

(defun hsv-to-rgb (h s v)
 #+liber-documentation
 "@version{2023-12-3}
  @argument[h]{a number coerced to a float with the hue component}
  @argument[s]{a number coerced to a float with the saturation component}
  @argument[v]{a number coerced to a float with the value component}
  @begin{return}
    @arg{r} -- a float with the red component @br{}
    @arg{g} -- a float with the green component @br{}
    @arg{b} -- a float with the blue component
  @end{return}
  @begin{short}
    Converts a color from HSV space to RGB. Input values must be in the
    [0.0, 1.0] range, output values will be in the same range.
  @end{short}
  @see-class{gtk:hsv}
  @see-function{gtk:rgb-to-hsv}"
  (cffi:with-foreign-objects ((r :float) (g :float) (b :float))
    (%hsv-to-rgb (coerce h 'single-float)
                 (coerce s 'single-float)
                 (coerce v 'single-float)
                 r g b)
    (values (cffi:mem-ref r :float)
            (cffi:mem-ref g :float)
            (cffi:mem-ref b :float))))

(export 'hsv-to-rgb)

;;; ----------------------------------------------------------------------------
;;; gtk_rgb_to_hsv ()
;;; ----------------------------------------------------------------------------

;; TODO: Check the implementation for the GTK3 library

(cffi:defcfun ("gtk_rgb_to_hsv" %rgb-to-hsv) :void
  (r :float)
  (g :float)
  (b :float)
  (h (:pointer :float))
  (s (:pointer :float))
  (v (:pointer :float)))

(defun rgb-to-hsv (r g b)
 #+liber-documentation
 "@version{2023-12-3}
  @argument[r]{a number coerced to a float with the red component}
  @argument[g]{a number coerced to a float with the green component}
  @argument[b]{a number coerced to a float with the blue component}
  @begin{return}
    @arg{h} -- a float with the hue component @br{}
    @arg{s} -- a float with the saturation component @br{}
    @arg{v} -- a float with the value component
  @end{return}
  @begin{short}
    Converts a color from RGB space to HSV. Input values must be in the
    [0.0, 1.0] range. Output values will be in the same range.
  @end{short}
  @see-class{gtk:hsv}
  @see-function{gtk:hsv-to-rgb}"
  (cffi:with-foreign-objects ((h :float) (s :float) (v :float))
    (%rgb-to-hsv (coerce r 'single-float)
                 (coerce g 'single-float)
                 (coerce b 'single-float)
                 h s v)
    (values (cffi:mem-ref h :float)
            (cffi:mem-ref s :float)
            (cffi:mem-ref v :float))))

(export 'rgb-to-hsv)

;;; --- End of file gtk4.color-chooser.lisp ------------------------------------
