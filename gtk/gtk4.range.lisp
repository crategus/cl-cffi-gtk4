;;; ----------------------------------------------------------------------------
;;; gtk4.range.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library.
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
;;; GtkRange
;;;
;;;     Base class for widgets which visualize an adjustment
;;;
;;; Types and Values
;;;
;;;     GtkRange
;;;
;;; Accessors
;;;
;;;     gtk_range_get_adjustment
;;;     gtk_range_set_adjustment
;;;     gtk_range_get_fill_level
;;;     gtk_range_set_fill_level
;;;     gtk_range_get_inverted
;;;     gtk_range_set_inverted
;;;     gtk_range_get_restrict_to_fill_level
;;;     gtk_range_set_restrict_to_fill_level
;;;     gtk_range_get_round_digits
;;;     gtk_range_set_round_digits
;;;     gtk_range_get_show_fill_level
;;;     gtk_range_set_show_fill_level
;;;
;;; Functions
;;;
;;;     gtk_range_get_value
;;;     gtk_range_set_value
;;;     gtk_range_set_increments
;;;     gtk_range_set_range
;;;     gtk_range_get_flippable
;;;     gtk_range_set_flippable
;;;     gtk_range_get_range_rect
;;;     gtk_range_get_slider_range
;;;     gtk_range_get_slider_size_fixed
;;;     gtk_range_set_slider_size_fixed
;;;
;;; Properties
;;;
;;;     adjustment
;;;     fill-level
;;;     inverted
;;;     restrict-to-fill-level
;;;     round-digits
;;;     show-fill-level
;;;
;;; Signals
;;;
;;;     adjust-bounds
;;;     change-value
;;;     move-slider
;;;     value-changed
;;;
;;; Object Hierarchy
;;;
;;;    GObject
;;;    ╰── GInitiallyUnowned
;;;        ╰── GtkWidget
;;;            ╰── GtkRange
;;;                ╰── GtkScale
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkAccessibleRange                                 Since 4.10
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkOrientable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkRange
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkRange" range
  (:superclass widget
   :export t
   :interfaces (#+gtk-4-10
                "GtkAccessibleRange"
                "GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkOrientable")
   :type-initializer "gtk_range_get_type")
  ((adjustment
    range-adjustment
    "adjustment" "GtkAdjustment" t t)
   (fill-level
    range-fill-level
    "fill-level" "gdouble" t t)
   (inverted
    range-inverted
    "inverted" "gboolean" t t)
   (restrict-to-fill-level
    range-restrict-to-fill-level
    "restrict-to-fill-level" "gboolean" t t)
   (round-digits
    range-round-digits
    "round-digits" "gint" t t)
   (show-fill-level
    range-show-fill-level
    "show-fill-level" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'range 'type)
 "@version{2023-8-24}
  @begin{short}
    The @class{gtk:range} class is the common base class for widgets which
    visualize an adjustment, for example the @class{gtk:scale} widget.
  @end{short}

  Apart from signals for monitoring the parameters of the adjustment, the
  @class{gtk:range} class provides properties and methods for influencing the
  sensitivity of the \"steppers\". It also provides properties and methods for
  setting a \"fill level\" on range widgets. See the @fun{gtk:range-fill-level}
  function.
  @begin[Signal Details]{dictionary}
    @subheading{The \"adjust-bounds\" signal}
      @begin{pre}
lambda (range value)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[range]{The @class{gtk:range} widget that received the signal.}
        @entry[value]{The double float with the value before we clamp.}
      @end{table}
      Emitted before clamping a value, to give the application a chance to
      adjust the bounds.
    @subheading{The \"change-value\" signal}
      @begin{pre}
lambda (range scroll value)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[range]{The @class{gtk:range} widget that received the signal.}
        @entry[scroll]{The @symbol{gtk:scroll-type} value of scroll action that
          was performed.}
        @entry[value]{The double float resulting from the scroll action.}
        @entry[Returns]{@em{True} to prevent other handlers from being invoked
          for the signal, @em{false} to propagate the signal further.}
      @end{table}
      The signal is emitted when a scroll action is performed on a range. It
      allows an application to determine the type of scroll event that occurred
      and the resultant new value. The application can handle the event itself
      and return @em{true} to prevent further processing. Or, by returning
      @em{false}, it can pass the event to other handlers until the default GTK
      handler is reached. The value parameter is unrounded. An application that
      overrides the @code{\"change-value\"} signal is responsible for clamping
      the value to the desired number of decimal digits. The default GTK handler
      clamps the value based on \"round-digits\". It is not possible to use
      delayed update policies in an overridden @code{\"change-value\"} signal
      handler.
    @subheading{The \"move-slider\" signal}
      @begin{pre}
lambda (range step)    :action
      @end{pre}
      @begin[code]{table}
        @entry[range]{The @class{gtk:range} widget that received the signal.}
        @entry[step]{The @symbol{gtk:scroll-type} value how to move the slider.}
      @end{table}
      Virtual function that moves the slider. Used for key bindings.
    @subheading{The \"value-changed\" signal}
      @begin{pre}
lambda (range)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[range]{The @class{gtk:range} widget that received the signal.}
      @end{table}
      Emitted when the range value changes.
  @end{dictionary}
  @see-slot{gtk:range-adjustment}
  @see-slot{gtk:range-fill-level}
  @see-slot{gtk:range-inverted}
  @see-slot{gtk:range-restrict-to-fill-level}
  @see-slot{gtk:range-round-digits}
  @see-slot{gtk:range-show-fill-level}
  @see-class{gtk:scale}
  @see-class{gtk:scrollbar}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:range-adjustment ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "adjustment" 'range) t)
 "The @code{adjustment} property of type @class{gtk:adjustment}
  (Read / Write / Construct) @br{}
  The adjustment that contains the current value of this range object.")

#+liber-documentation
(setf (liber:alias-for-function 'range-adjustment)
      "Accessor"
      (documentation 'range-adjustment 'function)
 "@version{2023-8-24}
  @syntax{(gtk:range-adjustment object) => adjustement}
  @syntax{(setf (gtk:range-adjustment object) adjustment)}
  @argument[object]{a @class{gtk:range} widget}
  @argument[adjustment]{a @class{gtk:adjustment} object}
  @begin{short}
    Accessor of the @slot[gtk:range]{adjustment} slot of the @class{gtk:range}
    class.
  @end{short}
  The @fun{gtk:range-adjustment} function gets the adjustment which is the
  \"model\" object for the range. The @setf{gtk:range-adjustment} function sets
  the adjustment.

  The adjustment indicates the current range value, the minimum and maximum
  range values, the step/page increments used for keybindings and scrolling,
  and the page size. The page size is normally 0 for the @class{gtk:scale}
  widget, and indicates the size of the visible area of the widget being
  scrolled.
  @see-class{gtk:range}
  @see-class{gtk:adjustment}
  @see-class{gtk:scale}")

;;; --- gtk:range-fill-level ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "fill-level" 'range) t)
 "The @code{fill-level} property of type @code{:double} (Read / Write) @br{}
  The fill level, for example, prebuffering of a network stream. @br{}
  Default value: 1.79769e+308")

#+liber-documentation
(setf (liber:alias-for-function 'range-fill-level)
      "Accessor"
      (documentation 'range-fill-level 'function)
 "@version{2023-8-24}
  @syntax{(range-fill-level object) => fill-level}
  @syntax{(setf (gtk:range-fill-level object) fill-level)}
  @argument[object]{a @class{gtk:range} widget}
  @argument[fill-level]{a double float with the new position of the fill level
    indicator}
  @begin{short}
    Accessor of the @slot[gtk:range]{fill-level} slot of the @class{gtk:range}
    class.
  @end{short}
  The @fun{gtk:range-fill-level} function gets the current position of the fill
  level indicator. The @setf{gtk:range-fill-level} function sets the position
  of the fill level indicator.

  The \"fill level\" is probably best described by its most prominent use case,
  which is an indicator for the amount of pre-buffering in a streaming media
  player. In that use case, the value of the range would indicate the current
  play position, and the fill level would be the position up to which the
  file/stream has been downloaded.

  This amount of prebuffering can be displayed on the trough of the range and
  is themeable separately from the trough. To enable fill level display, use
  the @fun{gtk:range-show-fill-level} function. The range defaults to not
  showing the fill level.

  Additionally, it is possible to restrict the slider position of the range to
  values which are smaller than the fill level. This is controller by the
  @fun{gtk:range-restrict-to-fill-level} function and is by default enabled.
  @see-class{gtk:range}
  @see-function{gtk:range-show-fill-level}
  @see-function{gtk:range-restrict-to-fill-level}")

;;; --- gtk:range-inverted -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "inverted" 'range) t)
 "The @code{inverted} property of type @code{:boolean} (Read / Write) @br{}
  Invert direction slider moves to increase range value. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'range-inverted)
      "Accessor"
      (documentation 'range-inverted 'function)
 "@version{2023-8-24}
  @syntax{(gtk:range-inverted object) => setting}
  @syntax{(setf (gtk:range-inverted object) setting)}
  @argument[object]{a @class{gtk:range} widget}
  @argument[setting]{@em{true} to invert the range}
  @begin{short}
    Accessor of the @slot[gtk:range]{inverted} slot of the @class{gtk:range}
    class.
  @end{short}
  The @fun{gtk:range-inverted} function gets whether the range is inverted.

  Ranges normally move from lower to higher values as the slider moves from
  top to bottom or left to right. Inverted ranges have higher values at the
  top or on the right rather than on the bottom or left.
  @see-class{gtk:range}")

;;; --- gtk:range-restrict-to-fill-level ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "restrict-to-fill-level"
                                               'range) t)
 "The @code{restrict-to-fill-level} property of type @code{:boolean}
  (Read / Write) @br{}
  Controls whether slider movement is restricted to an upper boundary set by the
  fill level. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'range-restrict-to-fill-level)
      "Accessor"
      (documentation 'range-restrict-to-fill-level 'function)
 "@version{2023-8-24}
  @syntax{(gtk:range-restrict-to-fill-level object) => setting}
  @syntax{(setf (gtk:range-restrict-to-fill-level object) setting)}
  @argument[object]{a @class{gtk:range} widget}
  @argument[setting]{a boolean whether the fill level restricts slider movement}
  @begin{short}
    Accessor of the @slot[gtk:range]{restrict-to-fill-level} slot of the
    @class{gtk:range} class.
  @end{short}
  The @fun{gtk:range-restrict-to-fill-level} function gets whether the range is
  restricted to the fill level. The @setf{gtk:range-restrict-to-fill-level}
  function sets whether the slider is restricted to the fill level. See the
  @fun{gtk:range-fill-level} function for a general description of the fill
  level concept.
  @see-class{gtk:range}
  @see-function{gtk:range-fill-level}")

;;; --- gtk:range-round-digits -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "round-digits" 'range) t)
 "The @code{round-digits} property of type @code{:int} (Read / Write) @br{}
  The number of digits to round the value to when it changes, or -1. See the
  @code{\"change-value\"} signal. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'range-round-digits)
      "Accessor"
      (documentation 'range-round-digits 'function)
 "@version{2023-8-24}
  @syntax{(gtk:range-round-digits object) => round-digits}
  @syntax{(setf (gtk:range-round-digits object) round-digits)}
  @argument[object]{a @class{gtk:range} widget}
  @argument[round-digits]{an integer with the precision in digits, or -1}
  @begin{short}
    Accessor of the @slot[gtk:range]{round-digits} slot of the @class{gtk:range}
    class.
  @end{short}
  The @fun{gtk:range-round-digits} function gets the number of digits to round
  the value to when it changes. The @setf{gtk:range-round-digits} function sets
  the number of digits to round the value to when it changes. See the
  @code{\"change-value\"} signal.
  @see-class{gtk:range}")

;;; --- gtk:range-show-fill-level ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-fill-level" 'range) t)
 "The @code{show-fill-level} property of type @code{:boolean} (Read / Write)
  @br{}
  Controls whether fill level indicator graphics are displayed on the trough.
  @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'range-show-fill-level)
      "Accessor"
      (documentation 'range-show-fill-level 'function)
 "@version{2023-8-24}
  @syntax{(gtk:range-show-fill-level object) => show-fill-level}
  @syntax{(setf (gtk:range-show-fill-level object) show-fill-level)}
  @argument[object]{a @class{gtk:range} widget}
  @argument[show-fill-level]{a boolean whether a fill level indicator graphics
    is shown}
  @begin{short}
    Accessor of the @slot[gtk:range]{show-fill-level} slot of the
    @class{gtk:range} class.
  @end{short}
  The @fun{gtk:range-show-fill-level} function gets whether the range displays
  the fill level graphically. The @setf{gtk:range-show-fill-level} function
  sets whether a graphical fill level is show on the trough. See the
  @fun{gtk:range-fill-level} function for a general description of the fill
  level concept.
  @see-class{gtk:range}
  @see-function{gtk:range-fill-level}")

;;; ----------------------------------------------------------------------------
;;; gtk_range_get_value
;;; gtk_range_set_value
;;; ----------------------------------------------------------------------------

(defun (setf range-value) (value range)
  (setf (adjustment-value (range-adjustment range)) value))

(defun range-value (range)
 #+liber-documentation
 "@version{2023-8-24}
  @syntax{(gtk:range-value range) => value}
  @syntax{(setf (gtk:range-value range) value)}
  @argument[range]{a @class{gtk:range} widget}
  @argument[value]{a double float with the value of the range}
  @begin{short}
    Accessor of the value of the range.
  @end{short}
  The @fun{gtk:range-value} function gets the current value of the range. The
  @setf{gtk:range-value} function sets the current value of the range.

  If the value is outside the minimum or maximum range values, it will be
  clamped to fit inside them. The range emits the @code{\"value-changed\"}
  signal if the value changes.
  @see-class{gtk:range}"
  (adjustment-value (range-adjustment range)))

(export 'range-value)

;;; ----------------------------------------------------------------------------
;;; gtk_range_set_increments
;;; ----------------------------------------------------------------------------

(declaim (inline range-set-increments))

(defun range-set-increments (range step page)
 #+liber-documentation
 "@version{#2023-8-24}
  @argument[range]{a @class{gtk:range} widget}
  @argument[step]{a double float with the step size}
  @argument[page]{a double float with the page size}
  @begin{short}
    Sets the step and page sizes for the range.
  @end{short}
  The step size is used when the user moves the @class{gtk:scale} widget via
  arrow keys. The page size is used for example when moving via @kbd{Page Up}
  or @kbd{Page Down} keys.
  @see-class{gtk:range}
  @see-class{gtk:scale}"
  (setf (adjustment-page-increment (range-adjustment range)) page
        (adjustment-step-increment (range-adjustment range)) step))

(export 'range-set-increments)

;;; ----------------------------------------------------------------------------
;;; gtk_range_set_range
;;; ----------------------------------------------------------------------------

(declaim (inline range-set-range))

(defun range-set-range (range min max)
 #+liber-documentation
 "@version{#2023-8-24}
  @argument[range]{a @class{gtk:range} widget}
  @argument[min]{a double float with the minimum range value}
  @argument[max]{a double float with the maximum range value}
  @begin{short}
    Sets the allowable values in the range, and clamps the range value to be
    between @arg{min} and @arg{max}.
  @end{short}
  If the range has a non-zero page size, it is clamped between @arg{min} and
  @code{(max - page-size)}.
  @see-class{gtk:range}"
  (setf (adjustment-lower (range-adjustment range)) min
        (adjustment-upper (range-adjustment range)) max))

(export 'range-set-range)

;;; ----------------------------------------------------------------------------
;;; gtk_range_get_flippable
;;; gtk_range_set_flippable
;;; ----------------------------------------------------------------------------

(defun (setf range-flippable) (flippable range)
  (cffi:foreign-funcall "gtk_range_set_flippable"
                        (g:object range) range
                        :boolean flippable
                        :void)
  flippable)

(cffi:defcfun ("gtk_range_get_flippable" range-flippable) :boolean
 #+liber-documentation
 "@version{#2023-8-24}
  @syntax{gtk:range-flippable range) => flippable}
  @syntax{(setf (gtk:range-flippable range) flippable)}
  @argument[range]{a @class{gtk:range} widget}
  @argument[[flippable]{@em{true} to make the range flippable}
  @begin{short}
    Accessor of the flippable property of the range.
  @end{short}
  If a range is flippable, it will switch its direction if it is horizontal and
  its direction is @code{:rtl}. See the @fun{gtk:widget-direction} function.
  @see-class{gtk:range}
  @see-function{gtk:widget-direction}"
  (range (g:object range)))

(export 'range-flippable)

;;; ----------------------------------------------------------------------------
;;; gtk_range_get_range_rect
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_range_get_range_rect" %range-get-range-rect) :void
  (range (g:object range))
  (range-rect (g:boxed gdk:rectangle)))

(defun range-range-rect (range)
 #+liber-documentation
 "@version{#2023-8-24}
  @argument[range]{a @class{gtk:range} widget}
  @return{The @class{gdk:rectangle} instance with the range rectangle.}
  @begin{short}
    This function returns the area that contains the trough of the range and its
    steppers, in widget window coordinates.
  @end{short}
  This function is useful mainly for @class{gtk:range} subclasses.
  @see-class{gtk:range}
  @see-class{gdk:rectangle}"
  (let ((range-rect (gdk:rectangle-new)))
    (%range-get-range-rect range range-rect)
    range-rect))

(export 'range-range-rect)

;;; ----------------------------------------------------------------------------
;;; gtk_range_get_slider_range
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_range_get_slider_range" %range-get-slider-range) :void
  (range (g:object range))
  (start (:pointer :int))
  (end (:pointer :int)))

(defun range-slider-range (range)
 #+liber-documentation
 "@version{#2023-8-24}
  @argument[range]{a @class{gtk:range} widget}
  @begin{return}
    @arg{start} -- an integer with the start of the slider, or @code{nil} @br{}
    @arg{end} -- an integer with the end of the slider, or @code{nil}
  @end{return}
  @begin{short}
    This function returns sliders range along the long dimension, in
    widget window coordinates.
  @end{short}
  This function is useful mainly for @class{gtk:range} subclasses.
  @see-class{gtk:range}"
  (cffi:with-foreign-objects ((start :int) (end :int))
    (%range-get-slider-range range start end)
    (values (if (not (cffi:null-pointer-p start))
                (cffi:mem-ref start :int)
                nil)
            (if (not (cffi:null-pointer-p end))
                (cffi:mem-ref end :int)
                nil))))

(export 'range-slider-range)

;;; ----------------------------------------------------------------------------
;;; gtk_range_set_slider_size_fixed
;;; gtk_range_get_slider_size_fixed
;;; ----------------------------------------------------------------------------

(defun (setf range-slider-size-fixed) (fixed range)
  (cffi:foreign-funcall "gtk_range_set_slider_size_fixed"
                        (g:object range) range
                        :boolean fixed
                        :void)
  fixed)

(cffi:defcfun ("gtk_range_get_slider_size_fixed" range-slider-size-fixed)
    :boolean
 #+liber-documentation
 "@version{#2023-8-24}
  @syntax{(gtk:range-slider-size-fixed range) => fixed}
  @syntax{(setf (gtk:range-slider-size-fixed range) fixed)}
  @argument[range]{a @class{gtk:range} widget}
  @argument[fixed]{@em{true} to make the slider size constant}
  @begin{short}
    Whether the slider of the range has a fixed size.
  @end{short}
  Sets whether the slider of the range has a fixed size, or a size that depends
  on its page size of the adjustment.

  This function is useful mainly for @class{gtk:range} subclasses.
  @see-class{gtk:range}"
  (range (g:object range)))

(export 'range-slider-size-fixed)

;;; --- End of file gtk4.range.lisp --------------------------------------------
