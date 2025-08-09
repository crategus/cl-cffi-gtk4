;;; ----------------------------------------------------------------------------
;;; gtk4.level-bar.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2014 - 2025 Dieter Kaiser
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
;;; GtkLevelBar
;;;
;;;     A bar that can used as a level indicator
;;;
;;; Types and Values
;;;
;;;     GtkLevelBarMode
;;;     GtkLevelBar
;;;
;;; Accessors
;;;
;;;     gtk_level_bar_set_inverted
;;;     gtk_level_bar_get_inverted
;;;     gtk_level_bar_set_max_value
;;;     gtk_level_bar_get_max_value
;;;     gtk_level_bar_set_min_value
;;;     gtk_level_bar_get_min_value
;;;     gtk_level_bar_set_mode
;;;     gtk_level_bar_get_mode
;;;     gtk_level_bar_set_value
;;;     gtk_level_bar_get_value
;;;
;;; Functions
;;;
;;;     gtk_level_bar_new
;;;     gtk_level_bar_new_for_interval
;;;     gtk_level_bar_add_offset_value
;;;     gtk_level_bar_remove_offset_value
;;;     gtk_level_bar_get_offset_value
;;;
;;; Properties
;;;
;;;     inverted
;;;     max-value
;;;     min-value
;;;     mode
;;;     value
;;;
;;; Signals
;;;
;;;     offset-changed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkLevelBar
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
;;; GtkLevelBarMode
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkLevelBarMode" level-bar-mode
  (:export t
   :type-initializer "gtk_level_bar_mode_get_type")
  (:continuous 0)
  (:discrete 1))

#+liber-documentation
(setf (liber:alias-for-symbol 'level-bar-mode)
      "GEnum"
      (liber:symbol-documentation 'level-bar-mode)
 "@version{2025-05-30}
  @begin{declaration}
(gobject:define-genum \"GtkLevelBarMode\" level-bar-mode
  (:export t
   :type-initializer \"gtk_level_bar_mode_get_type\")
  (:continuous 0)
  (:discrete 1))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:continuous]{The level bar has a continuous mode.}
      @entry[:discrete]{The level bar has a discrete mode.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Describes how the @class{gtk:level-bar} widget contents should be rendered.
  @end{short}
  @see-class{gtk:level-bar}")

;;; ----------------------------------------------------------------------------
;;; GtkLevelBar
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkLevelBar" level-bar
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkOrientable")
   :type-initializer "gtk_level_bar_get_type")
  ((inverted
    level-bar-inverted
    "inverted" "gboolean" t t)
   (max-value
    level-bar-max-value
    "max-value" "gdouble" t t)
   (min-value
    level-bar-min-value
    "min-value" "gdouble" t t)
   (mode
    level-bar-mode
    "mode" "GtkLevelBarMode" t t)
   (value
    level-bar-value
    "value" "gdouble" t t)))

#+liber-documentation
(setf (documentation 'level-bar 'type)
 "@version{2025-07-12}
  @begin{short}
    The @class{gtk:level-bar} widget is a level bar widget that can be used as
    a level indicator.
  @end{short}
  Typical use cases are displaying the strength of a password, or showing the
  charge level of a battery.

  @image[level-bar]{Figure: GtkLevelBar}

  Use the @fun{gtk:level-bar-value} function to set the current value, and the
  @fun{gtk:level-bar-add-offset-value} function to set the value offsets at
  which the level bar will be considered in a different state. GTK will add a
  few offsets by default on the level bar: @code{\"low\"}, @code{\"high\"} and
  @code{\"full\"}, with values 0.25, 0.75 and 1.0 respectively.

  Note that it is your responsibility to update preexisting offsets when
  changing the minimum or maximum value. GTK will simply clamp them to the new
  range.

  @subheading{Adding a custom offset on the bar}
  The default interval of values is between zero and one, but it is possible to
  modify the interval using the @fun{gtk:level-bar-min-value} and
  @fun{gtk:level-bar-max-value} functions. The value will be always drawn in
  proportion to the admissible interval, that is, a value of 15 with a specified
  interval between 10 and 20 is equivalent to a value of 0.5 with an interval
  between 0 and 1. When the @val[gtk:level-bar-mode]{:discrete} level bar mode
  is used, the level bar is rendered as a finite number of separated blocks
  instead of a single one. The number of blocks that will be rendered is equal
  to the number of units specified by the admissible interval.

  For instance, to build a lever bar rendered with five blocks, it is sufficient
  to set the minimum value to 0 and the maximum value to 5 after changing the
  indicator mode to discrete.
  @begin[GtkLevelBar as GtkBuildable]{dictionary}
    The @class{gtk:level-bar} implementation of the @class{gtk:buildable}
    interface supports a custom @code{<offsets>} element, which can contain any
    number of @code{<offset>} elements, each of which must have name and value
    attributes.
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
levelbar[.discrete]
╰── trough
    ├── block.filled.level-name
    ┊
    ├── block.empty
    ┊
    @end{pre}
    The @class{gtk:level-bar} implementation has a main CSS node with name
    @code{levelbar} and one of the @code{.discrete} or @code{.continuous} style
    classes and a subnode with name @code{trough}. Below the @code{trough} node
    are a number of nodes with name @code{block} and @code{.filled} or
    @code{.empty} style class. In continuous mode, there is exactly one node of
    each, in discrete mode, the number of filled and unfilled nodes corresponds
    to blocks that are drawn. The @code{block.filled} nodes also get a
    @code{.level-name} style class corresponding to the level for the current
    value.

    In horizontal orientation, the nodes are always arranged from left to right,
    regardless of text direction.
  @end{dictionary}
  @begin[Examples]{dictionary}
    Adding a custom offset on the bar.
    @begin{pre}
(defun create-level-bar (orientation)
  (let* ((levelbar (make-instance 'gtk:level-bar
                                  :orientation orientation)))
    ;; This changes the value of the default low offset
    (gtk:level-bar-add-offset-value levelbar \"low\" 0.10d0)
    ;; This adds a new offset to the bar. The application will
    ;; be able to change its color CSS like this:
    ;;
    ;; levelbar block.my-offset {
    ;;   background-color: magenta;
    ;;   border-style: solid;
    ;;   border-color: black;
    ;;   border-style: 1px;
    ;; @}
    (gtk:level-bar-add-offset-value levelbar \"my-offset\" 0.60d0)
    ;; Return the new level bar
    levelbar))
    @end{pre}
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:level-bar} implementation uses the
    @val[gtk:accessible-role]{:meter} role of the @sym{gtk:accessible-role}
    enumeration.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[level-bar::offset-changed]{signal}
      @begin{pre}
lambda (levelbar name)    :detailed
      @end{pre}
      @begin[code]{simple-table}
        @entry[levelbar]{The @class{gtk:level-bar} widget that received the
          signal.}
        @entry[name]{The string for the name of the offset that changed value.}
      @end{simple-table}
      Emitted when an offset specified on the level bar changes value as an
      effect to the @fun{gtk:level-bar-add-offset-value} function being called.
      The signal supports detailed connections. You can connect to the
      @sig[gtk:level-bar]{changed::x} detailed signal in order to only receive
      callbacks when the value of @code{\"x\"} offset changes.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:level-bar-new}
  @see-constructor{gtk:level-bar-new-for-interval}
  @see-slot{gtk:level-bar-inverted}
  @see-slot{gtk:level-bar-max-value}
  @see-slot{gtk:level-bar-min-value}
  @see-slot{gtk:level-bar-mode}
  @see-slot{gtk:level-bar-value}
  @see-class{gtk:progress-bar}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:level-bar-inverted -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "inverted" 'level-bar) t)
 "The @code{inverted} property of type @code{:boolean} (Read / Write) @br{}
  Level bars normally grow from top to bottom or left to right. Inverted level
  bars grow in the opposite direction. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'level-bar-inverted)
      "Accessor"
      (documentation 'level-bar-inverted 'function)
 "@version{2025-08-04}
  @syntax{(gtk:level-bar-inverted object) => inverted}
  @syntax{(setf (gtk:level-bar-inverted object) inverted)}
  @argument[object]{a @class{gtk:level-bar} widget}
  @argument[inverted]{@em{true} to invert the level bar}
  @begin{short}
    The accessor for the @slot[gtk:level-bar]{inverted} slot of the
    @class{gtk:level-bar} class sets or gets whether the level bar is inverted.
  @end{short}
  Level bars normally grow from top to bottom or left to right. Inverted level
  bars grow in the opposite direction.
  @see-class{gtk:level-bar}")

;;; --- gtk:level-bar-max-value ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "max-value" 'level-bar) t)
 "The @code{max-value} property of type @code{:double} (Read / Write) @br{}
  Determines the maximum value of the interval that can be displayed by the
  level bar. @br{}
  Allowed values: >= 0.0d0 @br{}
  Default value: 1.0d0")

#+liber-documentation
(setf (liber:alias-for-function 'level-bar-max-value)
      "Accessor"
      (documentation 'level-bar-max-value 'function)
 "@version{2025-08-04}
  @syntax{(gtk:level-bar-max-value object) => value}
  @syntax{(setf (gtk:level-bar-max-value object) value)}
  @argument[object]{a @class{gtk:level-bar} widget}
  @argument[value]{a number coerced to a double float for a positive value}
  @begin{short}
    The accessor for the @slot[gtk:level-bar]{max-value} slot of the
    @class{gtk:level-bar} class gets or sets the maximum value of the interval
    that can be displayed by the level bar.
  @end{short}
  @see-class{gtk:level-bar}
  @see-function{gtk:level-bar-min-value}")

;;; --- gtk:level-bar-min-value ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "min-value" 'level-bar) t)
 "The @code{min-value} property of type @code{:double} (Read / Write) @br{}
  Determines the minimum value of the interval that can be displayed by the
  level bar. @br{}
  Allowed values: >= 0.0d0 @br{}
  Default value: 0.0d0")

#+liber-documentation
(setf (liber:alias-for-function 'level-bar-min-value)
      "Accessor"
      (documentation 'level-bar-min-value 'function)
 "@version{2025-08-04}
  @syntax{(gtk:level-bar-min-value object) => value}
  @syntax{(setf (gtk:level-bar-min-value object) value)}
  @argument[object]{a @class{gtk:level-bar} widget}
  @argument[value]{a number coerced to a double float for a positive value}
  @begin{short}
    The accessor for the @slot[gtk:level-bar]{min-value} slot of the
    @class{gtk:level-bar} class gets or sets the miminum value of the interval
    that can be displayed by the level bar.
  @end{short}
  @see-class{gtk:level-bar}
  @see-function{gtk:level-bar-max-value}")

;;; --- gtk:level-bar-mode -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "mode" 'level-bar) t)
 "The @code{mode} property of type @sym{gtk:level-bar-mode} (Read / Write) @br{}
  Determines the way a level bar interprets the value properties to draw the
  level fill area. Specifically, when the value is
  @val[gtk:level-bar-mode]{:continuous}, the level bar will draw a single block
  representing the current value in that area. When the value is
  @val[gtk:level-bar-mode]{:discrete}, the widget will draw a succession of
  separate blocks filling the draw area, with the number of blocks being equal
  to the units separating the integral roundings of the
  @slot[gtk:level-bar]{min-value} and @slot[gtk:level-bar]{max-value} values.
  @br{}
  Default value: @val[gtk:level-bar-mode]{:continuous}")

#+liber-documentation
(setf (liber:alias-for-function 'level-bar-mode)
      "Accessor"
      (documentation 'level-bar-mode 'function)
 "@version{2025-08-04}
  @syntax{(gtk:level-bar-mode object) => mode}
  @syntax{(setf (gtk:level-bar-mode object) mode)}
  @argument[object]{a @class{gtk:level-bar} widget}
  @argument[mode]{a value of the @sym{gtk:level-bar-mode} enumeration}
  @begin{short}
    The accessor for the @slot[gtk:level-bar]{mode} slot of the
    @class{gtk:level-bar} class gets or sets the way a level bar interprets the
    value properties to draw the level fill area.
  @end{short}
  @see-class{gtk:level-bar}
  @see-symbol{gtk:level-bar-mode}")

;;; --- gtk:level-bar-value ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "value" 'level-bar) t)
 "The @code{value} property of type @code{:double} (Read / Write) @br{}
  Determines the currently filled value of the level bar. @br{}
  Allowed values: >= 0.0d0 @br{}
  Default value: 0.0d0")

#+liber-documentation
(setf (liber:alias-for-function 'level-bar-value)
      "Accessor"
      (documentation 'level-bar-value 'function)
 "@version{2025-08-04}
  @syntax{(gtk:level-bar-value object) >= value}
  @syntax{(setf (gtk:level-bar-value object) value)}
  @argument[object]{a @class{gtk:level-bar} widget}
  @argument[value]{a number coerced to a double float for a value in the
    interval between the @slot[gtk:level-bar]{min-value} and
    @slot[gtk:level-bar]{max-value} values}
  @begin{short}
    The accessor for the @slot[gtk:level-bar]{value} slot of the
    @class{gtk:level-bar} class gets or sets the currently filled value of the
    level bar.
  @end{short}
  @see-class{gtk:level-bar}
  @see-function{gtk:level-bar-min-value}
  @see-function{gtk:level-bar-max-value}")

;;; ----------------------------------------------------------------------------
;;; gtk_level_bar_new
;;; ----------------------------------------------------------------------------

(declaim (inline level-bar-new))

(defun level-bar-new ()
 #+liber-documentation
 "@version{2025-05-30}
  @return{The new @class{gtk:level-bar} widget.}
  @short{Creates a new level bar.}
  @see-class{gtk:level-bar}
  @see-function{gtk:level-bar-new-for-interval}"
  (make-instance 'level-bar))

(export 'level-bar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_level_bar_new_for_interval
;;; ----------------------------------------------------------------------------

(declaim (inline level-bar-new-for-interval))

(defun level-bar-new-for-interval (min max)
 #+liber-documentation
 "@version{2025-07-26}
  @argument[min]{a number coerced to a double float for a positive value}
  @argument[max]{a number coerced to a double float for a positive value}
  @return{The new @class{gtk:level-bar} widget.}
  @begin{short}
    Utility constructor that creates a new level bar for the specified interval.
  @end{short}
  @see-class{gtk:level-bar}
  @see-function{gtk:level-bar-new}"
  (make-instance 'level-bar
                 :min-value min
                 :max-value max))

(export 'level-bar-new-for-interval)

;;; ----------------------------------------------------------------------------
;;; gtk_level_bar_add_offset_value
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_level_bar_add_offset_value" %level-bar-add-offset-value)
    :void
  (levelbar (g:object level-bar))
  (name :string)
  (value :double))

(defun level-bar-add-offset-value (levelbar name value)
 #+liber-documentation
 "@version{#2025-05-30}
  @argument[levelbar]{a @class{gtk:level-bar} widget}
  @argument[name]{a string for the name of the new offset}
  @argument[value]{a number coerced to a double float for the new offset}
  @begin{short}
    Adds a new offset marker on the level bar at the position specified by
    @arg{value}.
  @end{short}
  When the level bar value is in the interval topped by @arg{value}, or between
  the @arg{value} and @slot[gtk:level-bar]{max-value} values in case the offset
  is the last one on the bar, a style class named @code{level-name} will be
  applied when rendering the level bar fill. If another offset marker named
  @arg{name} exists, its value will be replaced by @arg{value}.
  @see-class{gtk:level-bar}
  @see-function{gtk:level-bar-remove-offset-value}
  @see-function{gtk:level-bar-max-value}"
  (%level-bar-add-offset-value levelbar
                               name
                               (coerce value 'double-float)))

(export 'level-bar-add-offset-value)

;;; ----------------------------------------------------------------------------
;;; gtk_level_bar_remove_offset_value
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_level_bar_remove_offset_value"
               level-bar-remove-offset-value) :void
 #+liber-documentation
 "@version{#2025-05-30}
  @argument[levelbar]{a @class{gtk:level-bar} widget}
  @argument[name]{a string for the name of an offset in the level bar}
  @begin{short}
    Removes an offset marker previously added with the
    @fun{gtk:level-bar-add-offset-value} function.
  @end{short}
  @see-class{gtk:level-bar}
  @see-function{gtk:level-bar-add-offset-value}"
  (levelbar (g:object level-bar))
  (name :string))

(export 'level-bar-remove-offset-value)

;;; ----------------------------------------------------------------------------
;;; gtk_level_bar_get_offset_value
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_level_bar_get_offset_value" %level-bar-offset-value)
    :boolean
  (levelbar (g:object level-bar))
  (name :string)
  (value (:pointer :double)))

(defun level-bar-offset-value (levelbar name)
 #+liber-documentation
 "@version{#2025-07-12}
  @argument[levelbar]{a @class{gtk:level-bar} widget}
  @argument[name]{a string for the name of an offset in the level bar}
  @return{The double float that specified the offset marker, or @code{nil}.}
  @begin{short}
    Fetches the value specified for the offset marker @arg{name} in the level
    bar, returning @code{nil} in case an offset named @arg{name} was not found.
  @end{short}
  @see-class{gtk:level-bar}"
  (cffi:with-foreign-object (value :double)
    (when (%level-bar-offset-value levelbar name value)
      (cffi:mem-ref value :double))))

(export 'level-bar-offset-value)

;;; --- End of file gtk4.level-bar.lisp ----------------------------------------
