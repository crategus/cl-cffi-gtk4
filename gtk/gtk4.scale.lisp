;;; ----------------------------------------------------------------------------
;;; gtk.scale.lisp
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
;;; GtkScale
;;;
;;;     A slider widget for selecting a value from a range
;;;
;;; Types and Values
;;;
;;;     GtkScale
;;;
;;; Accessors
;;;
;;;     gtk_scale_get_digits
;;;     gtk_scale_set_digits
;;;     gtk_scale_get_draw_value
;;;     gtk_scale_set_draw_value
;;;     gtk_scale_get_has_origin
;;;     gtk_scale_set_has_origin
;;;     gtk_scale_get_value_pos
;;;     gtk_scale_set_value_pos
;;;
;;; Functions

;;;     GtkScaleFormatValueFunc
;;;
;;;     gtk_scale_new
;;;     gtk_scale_new_with_range
;;;     gtk_scale_set_format_value_func
;;;     gtk_scale_get_layout
;;;     gtk_scale_get_layout_offsets
;;;     gtk_scale_add_mark
;;;     gtk_scale_clear_marks
;;;
;;; Properties
;;;
;;;     digits
;;;     draw-value
;;;     has-origin
;;;     value-pos
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkRange
;;;                 ╰── GtkScale
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkOrientable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkScale
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkScale" scale
  (:superclass range
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkOrientable")
   :type-initializer "gtk_scale_get_type")
  ((digits
    scale-digits
    "digits" "gint" t t)
   (draw-value
    scale-draw-value
    "draw-value" "gboolean" t t)
   (has-origin
    scale-has-origin
    "has-origin" "gboolean" t t)
   (value-pos
    scale-value-pos
    "value-pos" "GtkPositionType" t t)))

#+liber-documentation
(setf (documentation 'scale 'type)
 "@version{#2021-12-27}
  @begin{short}
    A @sym{gtk:scale} widget is a slider control used to select a numeric value.
  @end{short}
  To use it, you will probably want to investigate the methods on its base
  @class{gtk:range} class, in addition to the methods for the @sym{gtk:scale}
  class itself. To set the value of a scale, you would normally use the
  @fun{gtk:range-value} function. To detect changes to the value, you would
  normally use the \"value-changed\" signal.

  @image[scale]{}

  Note that using the same upper and lower bounds for the @sym{gtk:scale}
  widget, through the @class{gtk:range} methods, will hide the slider itself.
  This is useful for applications that want to show an undeterminate value on
  the scale, without changing the layout of the application, such as movie or
  music players.
  @begin[GtkScale as GtkBuildable]{dictionary}
    The @sym{gtk:scale} widget supports a custom @code{<marks>} element, which
    can contain multiple @code{<mark>} elements. The \"value\" and \"position\"
    attributes have the same meaning as the parameters of the
    @fun{gtk:scale-add-mark} function of the same name. If the element is not
    empty, its content is taken as the markup to show at the mark. It can be
    translated with the usual \"translatable\" and \"context\" attributes.
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
 scale[.fine-tune][.marks-before][.marks-after]
 ├── marks.top
 │   ├── mark
 │   ┊    ├── [label]
 │   ┊    ╰── indicator
 ┊   ┊
 │   ╰── mark
 ├── [value]
 ├── contents
 │   ╰── trough
 │       ├── slider
 │       ├── [highlight]
 │       ╰── [fill]
 ╰── marks.bottom
     ├── mark
     ┊    ├── indicator
     ┊    ╰── [label]
     ╰── mark
    @end{pre}
    The @sym{gtk:scale} implementation has a main CSS node with name
    @code{scale} and a subnode for its contents, with subnodes named
    @code{trough} and @code{slider}. The main node gets the @code{.fine-tune}
    style class added when the scale is in 'fine-tuning' mode.

    If the scale has an origin, see the @fun{gtk:scale-has-origin} function,
    there is a subnode with name @code{highlight} below the trough node that
    is used for rendering the highlighted part of the trough.

    If the scale is showing a fill level, see the
    @fun{gtk:range-show-fill-level} function, there is a subnode with name
    @code{fill} below the trough node that is used for rendering the filled in
    part of the trough.

    If marks are present, there is a marks subnode before or after the contents
    node, below which each mark gets a node with name @code{mark}. The marks
    nodes get either the @code{.top} or @code{.bottom} style class.

    The mark node has a subnode named @code{indicator}. If the mark has text,
    it also has a subnode named @code{label}. When the mark is either above or
    left of the scale, the label subnode is the first when present. Otherwise,
    the indicator subnode is the first.

    The main CSS node gets the @code{marks-before} and/or @code{marks-after}
    style classes added depending on what marks are present.

    If the scale is displaying the value, see the @code{draw-value} property,
    there is subnode with name @code{value}.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @sym{gtk:scale} implementation uses the @code{:slider} role of the
    @symbol{gtk:accessible-role} enumeration.
  @end{dictionary}
  @see-slot{gtk:scale-digits}
  @see-slot{gtk:scale-draw-value}
  @see-slot{gtk:scale-has-origin}
  @see-slot{gtk:scale-value-pos}
  @see-class{gtk:range}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- scale-digits -----------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "digits" 'scale) t)
 "The @code{digits} property of type @code{:int} (Read / Write) @br{}
  The number of decimal places that are displayed in the value. @br{}
  Allowed values: [-1, 64] @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'scale-digits)
      "Accessor"
      (documentation 'scale-digits 'function)
 "@version{#2021-12-27}
  @syntax[]{(gtk:scale-digits object) => digits}
  @syntax[]{(setf (gtk:scale-digits object) digits)}
  @argument[object]{a @class{gtk:scale} widget}
  @argument[digits]{an integer with the number of decimal places to display,
    e.g. use 1 to display 1.0, 2 to display 1.00, etc}
  @begin{short}
    Accessor of the @slot[gtk:scale]{digits} slot of the @class{gtk:scale}
    class.
  @end{short}

  The @sym{gtk:scale-digits} function returns the number of decimal places that
  are displayed. The @sym{(setf gtk:scale-digits)} function sets the number of
  decimal places that are displayed in the value. Also causes the value of the
  adjustment to be rounded off to this number of @arg{digits}, so the retrieved
  value matches the value the user saw.
  @see-class{gtk:scale}")

;;; --- scale-draw-value -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "draw-value" 'scale) t)
 "The @code{draw-value} property of type @code{:boolean} (Read / Write) @br{}
  Whether the current value is displayed as a string next to the slider. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'scale-draw-value)
      "Accessor"
      (documentation 'scale-draw-value 'function)
 "@version{#2021-12-27}
  @syntax[]{(gtk:scale-draw-value object) => draw-value}
  @syntax[]{(setf (gtk:scale-digits object) draw-value)}
  @argument[object]{a @class{gtk:scale} widget}
  @argument[draw-value]{@em{true} to draw the value}
  @begin{short}
    Accessor of the @slot[gtk:scale]{draw-value} slot of the @class{gtk:scale}
    class.
  @end{short}

  The @sym{gtk:scale-draw-value} function returns whether the current value is
  displayed as a string next to the slider. The
  @sym{(setf gtk:scale-draw-value)} function specifies whether the current
  value is displayed as a string next to the slider.
  @see-class{gtk:scale}")

;;; --- scale-has-origin -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-origin" 'scale) t)
 "The @code{has-origin} property of type @code{:boolean} (Read / Write) @br{}
  Whether the scale has an origin. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'scale-has-origin)
      "Accessor"
      (documentation 'scale-has-origin 'function)
 "@version{#2021-12-27}
  @syntax[]{(gtk:scale-has-origin object) => has-origin}
  @syntax[]{(setf (gtk:scale-digits object) has-origin)}
  @argument[object]{a @class{gtk:scale} widget}
  @argument[has-origin]{@em{true} if the scale has an origin}
  @begin{short}
    Accessor of the @slot[gtk:scale]{has-origin} slot of the @class{gtk:scale}
    class.
  @end{short}

  The @sym{gtk:scale-has-origin} function returns whether the scale has an
  origin.

  If @arg{has-origin} is set to @em{true}, the default, the scale will
  highlight the part of the scale between the origin, bottom or left
  side, of the scale and the current value.
  @see-class{gtk:scale}")

;;; --- scale-value-pos --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "value-pos" 'scale) t)
 "The @code{value-pos} property of type @symbol{gtk:position-type}
  (Read / Write) @br{}
  The position in which the current value is displayed. @br{}
  Default value: @code{:top}")

#+liber-documentation
(setf (liber:alias-for-function 'scale-value-pos)
      "Accessor"
      (documentation 'scale-value-pos 'function)
 "@version{#2021-12-27}
  @syntax[]{(gtk:scale-value-pos object) => pos}
  @syntax[]{(setf (gtk:scale-digits object) pos)}
  @argument[object]{a @class{gtk:scale} widget}
  @argument[pos]{a @symbol{gtk:position-type} value with the position in which
    the current value is displayed}
  @begin{short}
    Accessor of the @slot[gtk:scale]{value-pos} slot of the @class{gtk:scale}
    class.
  @end{short}

  The @sym{gtk:scale-value-pos} function gets the position in which the current
  value is displayed. The @sym{gtk:scale-value-pos}  function sets the position
  in which the current value is displayed.
  @see-class{gtk:scale}")

;;; ----------------------------------------------------------------------------
;;; gtk_scale_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline scale-new))

(defun scale-new (orientation adjustment)
 #+liber-documentation
 "@version{#2021-12-27}
  @argument[orientation]{a value of the @symbol{gtk:orientation} enumeration}
  @argument[adjustment]{a @class{gtk:adjustment} object which sets the range
    of the scale, or @code{nil} to create a new adjustment}
  @return{A new @class{gtk:scale} widget.}
  @short{Creates a new scale widget.}
  @see-class{gtk:scale}"
  (make-instance 'scale
                 :orientation orientation
                 :adjustment adjustment))

(export 'scale-new)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_new_with_range ()
;;; ----------------------------------------------------------------------------

(declaim (inline scale-new-with-range))

(defun scale-new-with-range (orientation min max step)
 #+liber-documentation
 "@version{#2021-12-27}
  @argument[orientation]{a value of the @symbol{gtk:orientation} enumeration}
  @argument[min]{a double float with the minimum value}
  @argument[max]{a double float with the maximum value}
  @argument[step]{a double float with the step increment, tick size, used
    with keyboard shortcuts}
  @return{A new @class{gtk:scale} widget.}
  @begin{short}
    Creates a new scale widget with the given @arg{orientation} that lets the
    user input a number between @arg{min} and @arg{max}, including @arg{min}
    and @arg{max}, with the increment @arg{step}.
  @end{short}
  The @arg{step} argument must be nonzero. It is the distance the slider moves
  when using the arrow keys to adjust the scale value.

  Note that the way in which the precision is derived works best if the
  @arg{step} argument is a power of ten. If the resulting precision is not
  suitable for your needs, use the @fun{gtk:scale-digits} function to correct
  it.
  @see-class{gtk:scale}
  @see-function{gtk:scale-digits}"
  (make-instance 'scale
                 :orientation orientation
                 :adjustment (make-instance 'adjustment
                                            :lower min
                                            :upper max
                                            :step-increment step)))

(export 'scale-new-with-range)

;;; ----------------------------------------------------------------------------
;;; GtkScaleFormatValueFunc ()
;;;
;;; char *
;;; (*GtkScaleFormatValueFunc) (GtkScale *scale,
;;;                             double value,
;;;                             gpointer user_data);
;;;
;;; scale :
;;;     The GtkScale
;;;
;;; value :
;;;     The numeric value to format
;;;
;;; user_data :
;;;      user data.
;;;
;;; Returns :
;;;     A newly allocated string describing a textual representation of the
;;;     given numerical value.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_scale_set_format_value_func ()
;;;
;;; void
;;; gtk_scale_set_format_value_func (GtkScale *scale,
;;;                                  GtkScaleFormatValueFunc func,
;;;                                  gpointer user_data,
;;;                                  GDestroyNotify destroy_notify);
;;;
;;; func allows you to change how the scale value is displayed. The given
;;; function will return an allocated string representing value . That string
;;; will then be used to display the scale's value.
;;;
;;; If NULL is passed as func , the value will be displayed on its own, rounded
;;; according to the value of the “digits” property.
;;;
;;; scale :
;;;     a GtkScale
;;;
;;; func :
;;;     function that formats the value.
;;;
;;; user_data :
;;;     user data to pass to func .
;;;
;;; destroy_notify :
;;;     destroy function for user_data .
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_scale_get_layout () -> scale-layout
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scale_get_layout" scale-layout)
    (g:object pango:layout :free-from-foreign nil)
 #+liber-documentation
 "@version{#2021-12-27}
  @argument[scale]{a @class{gtk:scale} widget}
  @begin{return}
    The @class{pango:layout} instance for this scale, or @code{nil} if the
    @slot[gtk:scale]{draw-value} property is @code{nil}.
  @end{return}
  @short{Gets the Pango layout used to display the scale.}
  @see-class{gtk:scale}"
  (scale (g:object scale)))

(export 'scale-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_get_layout_offsets () -> scale-layout-offsets
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scale_get_layout_offsets" %scale-get-layout-offsets) :void
  (scale (g:object scale))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun scale-layout-offsets (scale)
 #+liber-documentation
 "@version{#2021-12-27}
  @argument[scale]{a @class{gtk:scale} widget}
  @begin{return}
    @arg{x} -- an integer with the x offset of layout, or @code{nil} @br{}
    @arg{y} -- an integer with the y offset of layout, or @code{nil}
  @end{return}
  @begin{short}
    Obtains the coordinates where the scale will draw the Pango layout
    representing the text in the scale.
  @end{short}
  Remember when using the @class{pango:layout} functions you need to convert
  to and from pixels using the @fun{pango:pixels} function or the
  @var{+pango-scale+} constant. If the @slot[gtk:scale]{draw-value} property is
  @code{nil}, the return values are undefined.
  @see-class{gtk:scale}
  @see-function{pango:pixels}
  @see-variable{+pango-scale+}"
  (with-foreign-objects ((x :int) (y :int))
    (%scale-get-layout-offsets scale x y)
    (values (mem-ref x :int)
            (mem-ref y :int))))

(export 'scale-layout-offsets)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_add_mark ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scale_add_mark" scale-add-mark) :void
 #+liber-documentation
 "@version{#2021-12-27}
  @argument[scale]{a @class{gtk:scale} widget}
  @argument[value]{a double float with the value at which the mark is placed,
    must be between the lower and upper limits of the adjustment of the scale}
  @argument[position]{a value of the @symbol{gtk:position-type} enumeration}
  @argument[markup]{a string with the text to be shown at the mark, using Pango
    markup, or @code{nil}}
  @short{Adds a mark at value.}
  A mark is indicated visually by drawing a tick mark next to the scale, and
  GTK makes it easy for the user to position the scale exactly at the marks
  value. For a horizontal scale, @code{:top} and @code{:left} are drawn above
  the scale, anything else below. For a vertical scale, @code{:left} and
  @code{:top} are drawn to the left of the scale, anything else to the right.

  If markup is not @code{nil}, text is shown next to the tick mark.

  To remove marks from a scale, use the @fun{gtk:scale-clear-marks} function.
  @see-class{gtk:scale}
  @see-function{gtk:scale-clear-marks}"
  (scale (g:object scale))
  (value :double)
  (pos position-type)
  (markup :string))

(export 'scale-add-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_clear_marks ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_scale_clear_marks" scale-clear-marks) :void
 #+liber-documentation
 "@version{#2021-12-27}
  @argument[scale]{a @class{gtk:scale} widget}
  @begin{short}
    Removes any marks that have been added with the @fun{gtk:scale-add-mark}
    function.
  @end{short}
  @see-class{gtk:scale}
  @see-function{gtk:scale-add-mark}"
  (scale (g:object scale)))

(export 'scale-clear-marks)

;;; --- End of file gtk.scale.lisp ---------------------------------------------
