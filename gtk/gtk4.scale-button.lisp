;;; ----------------------------------------------------------------------------
;;; gtk4.scale-button.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
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
;;; GtkScaleButton
;;;
;;;     A button which pops up a scale
;;;
;;; Types and Values
;;;
;;;     GtkScaleButton
;;;
;;; Accessors
;;;
;;;     gtk_scale_button_get_active                         Since 4.10
;;;     gtk_scale_button_get_adjustment
;;;     gtk_scale_button_set_adjustment
;;;     gtk_scale_button_get_has_frame                      Since 4.14
;;;     gtk_scale_button_set_has_frame                      Since 4.14
;;;     gtk_scale_button_set_icons
;;;     gtk_scale_button_get_value
;;;     gtk_scale_button_set_value
;;;
;;; Functions
;;;
;;;     gtk_scale_button_new
;;;     gtk_scale_button_get_popup
;;;     gtk_scale_button_get_plus_button
;;;     gtk_scale_button_get_minus_button
;;;
;;; Properties
;;;
;;;     active                                              Since 4.10
;;;     adjustment
;;;     has-frame                                           Since 4.14
;;;     icons
;;;     value
;;;
;;; Signals
;;;
;;;     popdown
;;;     popup
;;;     value-changed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkScaleButton
;;;                 ╰── GtkVolumeButton
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
;;; GtkScaleButton
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkScaleButton" scale-button
  (:superclass widget
   :export t
   :interfaces ("GtkAccessibleRange"
                "GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkOrientable")
   :type-initializer "gtk_scale_button_get_type")
  (#+gtk-4-10
   (active
    scale-button-active
    "active" "gboolean" t nil)
   (adjustment
    scale-button-adjustment
    "adjustment" "GtkAdjustment" t t)
   #+gtk-4-14
   (has-frame
    scale-button-has-frame
    "has-frame" "gboolean" t t)
   (icons
    scale-button-icons
    "icons" "GStrv" t t)
   (value
    scale-button-value
    "value" "gdouble" t t)))

#+liber-documentation
(setf (documentation 'scale-button 'type)
 "@version{2024-08-06}
  @begin{short}
    The @class{gtk:scale-button} widget provides a button which pops up a scale
    widget.
  @end{short}
  This kind of widget is commonly used for volume controls in multimedia
  applications.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:scale-button} implementation has a single CSS node with name
    @code{scalebutton}. To differentiate it from a plain @class{gtk:button}
    widget, it gets the @code{.scale} style class.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[scal-button::popdown]{signal}
      @begin{pre}
lambda (button)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[button]{The @class{gtk:scale-button} widget that received the
          signal.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted to popdown the scale
      widget. The default binding for this signal is the @kbd{Escape} key.
    @end{signal}
    @begin[scale-button::popup]{signal}
      @begin{pre}
lambda (button)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[button]{The @class{gtk:scale-button} widget that received the
          signal.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted to popup the scale
      widget. The default bindings for this signal are the @kbd{Space},
      @kbd{Enter} and @kbd{Return} keys.
    @end{signal}
    @begin[scale-button::value-changed]{signal}
      @begin{pre}
lambda (button value)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[button]{The @class{gtk:scale-button} widget that received the
          signal.}
        @entry[value]{The double float for the new value.}
      @end{simple-table}
      The signal is emitted when the value field has changed.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:scale-button-new}
  @see-slot{gtk:scale-button-active}
  @see-slot{gtk:scale-button-adjustment}
  @see-slot{gtk:scale-button-has-frame}
  @see-slot{gtk:scale-button-icons}
  @see-slot{gtk:scale-button-value}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:scale-button-active ------------------------------------------------

#+(and gtk-4-10 liber-documentation)
(setf (documentation (liber:slot-documentation "active" 'scale-button) t)
 "The @code{active} property of type @code{:boolean} (Read / Write) @br{}
  Whether the scale button should be pressed in. @br{}
  Default value: @em{false}")

#+(and gtk-4-10 liber-documentation)
(setf (liber:alias-for-function 'scale-button-active)
      "Accessor"
      (documentation 'scale-button-active 'function)
 "@version{2025-08-06}
  @syntax{(gtk:scale-button-active object object) => active}
  @argument[object]{a @class{gtk:scale-button} widget}
  @argument[active]{a boolean whether the scale button should be pressed in}
  @begin{short}
    The accessor for the @slot[gtk:scale-button]{active} slot of the
    @class{gtk:scale-button} class gets or sets whether the scale button should
    be pressed in.
  @end{short}
  Returns @em{true} if the scale button is pressed in and @em{false} if it is
  raised.

  Since: 4.10
  @see-class{gtk:scale-button}")

;;; --- gtk:scale-button-adjustment --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "adjustment" 'scale-button) t)
 "The @code{adjustment} property of type @class{gtk:adjustment} (Read / Write)
  @br{}
  The adjustment that contains the current value of the scale button.")

#+liber-documentation
(setf (liber:alias-for-function 'scale-button-adjustment)
      "Accessor"
      (documentation 'scale-button-adjustment 'function)
 "@version{2025-08-06}
  @syntax{(gtk:scale-button-adjustment object object) => adjustment}
  @syntax{(setf (gtk:scale-button-adjustment object) adjustment)}
  @argument[object]{a @class{gtk:scale-button} widget}
  @argument[adjustment]{a @class{gtk:adjustment} object}
  @begin{short}
    The accessor for the @slot[gtk:scale-button]{adjustment} slot of the
    @class{gtk:scale-button} class gets or sets the adjustment associated with
    the scale button.
  @end{short}
  @see-class{gtk:scale-button}
  @see-class{gtk:adjustment}")

;;; --- gtk:scale-button-has-frame ---------------------------------------------

#+(and gtk-4-14 liber-documentation)
(setf (documentation (liber:slot-documentation "has-frame" 'scale-button) t)
 "The @code{has-frame} property of type @code{gboolean} (Read / Write) @br{}
  Whether the scale button has a frame. Since 4.14 @br{}
  Default value: @em{false}")

#+(and gtk-4-14 liber-documentation)
(setf (liber:alias-for-function 'scale-button-has-frame)
      "Accessor"
      (documentation 'scale-button-has-frame 'function)
 "@version{2025-08-06}
  @syntax{(gtk:scale-button-has-frame object object) => setting}
  @syntax{(setf (gtk:scale-button-has-frame object) setting)}
  @argument[object]{a @class{gtk:scale-button} widget}
  @argument[setting]{a boolean whether the scale button has a frame}
  @begin{short}
    The accessor for the @slot[gtk:scale-button]{has-frame} slot of the
    @class{gtk:scale-button} class gets or sets whether the scale button has a
    frame.
  @end{short}

  Since 4.14
  @see-class{gtk:scale-button}")

;;; --- gtk:scale-button-icons -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icons" 'scale-button) t)
 "The @code{icons} property of type @type{g:strv-t} (Read / Write) @br{}
  The names of the icons to be used by the scale button. The first item in
  the list will be used in the button when the current value is the lowest
  value, the second item for the highest value. All the subsequent icons will
  be used for all the other values, spread evenly over the range of values.
  If there is only one icon name in the icons list, it will be used for all
  the values. If only two icon names are in the icons array, the first one
  will be used for the bottom 50% of the scale, and the second one for the
  top 50%. It is recommended to use at least 3 icons so that the scale button
  reflects the current value of the scale better for the users.")

#+liber-documentation
(setf (liber:alias-for-function 'scale-button-icons)
      "Accessor"
      (documentation 'scale-button-icons 'function)
 "@version{2025-08-06}
  @syntax{(gtk:scale-button-icons object object) => icons}
  @syntax{(setf (gtk:scale-button-icons object) icons)}
  @argument[object]{a @class{gtk:scale-button} widget}
  @argument[icons]{a list of strings for the icon names}
  @begin{short}
    The accessor for the @slot[gtk:scale-button]{icons} slot of the
    @class{gtk:scale-button} class gets or sets sets the icons to be used by the
    scale button.
  @end{short}

  The names of the icons to be used by the scale button. The first item in
  the list will be used in the button when the current value is the lowest
  value, the second item for the highest value. All the subsequent icons will
  be used for all the other values, spread evenly over the range of values.
  If there is only one icon name in the icons list, it will be used for all
  the values. If only two icon names are in the icons array, the first one
  will be used for the bottom 50% of the scale, and the second one for the
  top 50%. It is recommended to use at least 3 icons so that the scale button
  reflects the current value of the scale better for the users.
  @see-class{gtk:scale-button}")

;;; --- gtk:scale-button-value -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "value" 'scale-button) t)
 "The @code{value} property of type @code{:double} (Read / Write) @br{}
  The value of the scale. @br{}
  Default value: 0.0d0")

#+liber-documentation
(setf (liber:alias-for-function 'scale-button-value)
      "Accessor"
      (documentation 'scale-button-value 'function)
 "@version{2025-08-06}
  @syntax{(gtk:scale-button-value object) => value}
  @syntax{(setf (gtk:scale-button-value object) value)}
  @argument[object]{a @class{gtk:scale-button} widget}
  @argument[value]{a number coerced to a double float for the value of the
    scale button}
  @begin{short}
    The accessor for the @slot[gtk:scale-button]{value} slot of the
    @class{gtk:scale-button} class gets or sets the current value of the scale
    button.
  @end{short}

  If the value is outside the minimum or maximum values of the scale button
  adjustment, it will be clamped to fit inside them. The scale button emits the
  @sig[gtk:scale-button]{value-changed} signal if the value changes.
  @see-class{gtk:scale-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_scale_button_new
;;; ----------------------------------------------------------------------------

(declaim (inline scale-button-new))

(defun scale-button-new (min max step icons)
 #+liber-documentation
 "@version{2025-07-13}
  @argument[min]{a number coerced to a double float for the minimum value of
    the scale}
  @argument[max]{a number coerced to a double float for the maximum value of
    the scale}
  @argument[step]{a number coerced to a double float for the stepping of
    the value when a scroll wheel event, or up/down arrow event occurs}
  @argument[icons]{a list of strings for the icon names, or @code{nil} if you
    want to set the list later with the @fun{gtk:scale-button-icons} function}
  @return{The new @class{gtk:scale-button} widget.}
  @begin{short}
    Creates a scale button, with a range between @arg{min} and @arg{max}, with
    a stepping of @arg{step}.
  @end{short}
  @see-class{gtk:scale-button}
  @see-function{gtk:scale-button-icons}"
  (let ((button (make-instance 'scale-button
                               :adjustment
                               (make-instance 'adjustment
                                              :value min
                                              :lower min
                                              :upper max
                                              :step-increment step
                                              :page-increment (* 10 step)
                                              :page-size 0))))
    (when icons
      (setf (scale-button-icons button) icons))
    button))

(export 'scale-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_button_get_popup
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_scale_button_get_popup" scale-button-popup)
    (g:object widget)
 #+liber-documentation
 "@version{2024-11-03}
  @argument[button]{a @class{gtk:scale-button} widget}
  @return{The @class{gtk:widget} widget for the popup of the scale button.}
  @short{Retrieves the popup of the scale button.}
  @see-class{gtk:scale-button}
  @see-class{gtk:widget}"
  (button (g:object scale-button)))

(export 'scale-button-popup)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_button_get_plus_button
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_scale_button_get_plus_button" scale-button-plus-button)
    (g:object widget)
 #+liber-documentation
 "@version{2025-07-13}
  @argument[button]{a @class{gtk:scale-button} widget}
  @begin{return}
    The @class{gtk:widget} widget for the plus button of the scale button.
  @end{return}
  @short{Retrieves the plus button of the scale button.}
  @see-class{gtk:scale-button}
  @see-class{gtk:widget}
  @see-function{gtk:scale-button-minus-button}"
  (button (g:object scale-button)))

(export 'scale-button-plus-button)

;;; ----------------------------------------------------------------------------
;;; gtk_scale_button_get_minus_button
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_scale_button_get_minus_button" scale-button-minus-button)
    (g:object widget)
 #+liber-documentation
 "@version{2025-07-13}
  @argument[button]{a @class{gtk:scale-button} widget}
  @begin{return}
    The @class{gtk:widget} widget for the minus button of the scale button.
  @end{return}
  @short{Retrieves the minus button of the scale button.}
  @see-class{gtk:scale-button}
  @see-class{gtk:widget}
  @see-function{gtk:scale-button-plus-button}"
  (button (g:object scale-button)))

(export 'scale-button-minus-button)

;;; --- End of file gtk4.scale-button.lisp -------------------------------------
