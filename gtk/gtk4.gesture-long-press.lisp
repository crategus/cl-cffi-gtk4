;;; ----------------------------------------------------------------------------
;;; gtk.gesture-long-press.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2022 Dieter Kaiser
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
;;; GtkGestureLongPress
;;;
;;;     "Press and Hold" gesture
;;;
;;; Types and Values
;;;
;;;     GtkGestureLongPress
;;;
;;; Accessors
;;;
;;;     gtk_gesture_long_press_set_delay_factor
;;;     gtk_gesture_long_press_get_delay_factor
;;;
;;; Functions
;;;
;;;     gtk_gesture_long_press_new
;;;
;;; Properties
;;;
;;;     delay-factor
;;;
;;; Signals
;;;
;;;     cancelled
;;;     pressed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkGesture
;;;             ╰── GtkGestureSingle
;;;                 ╰── GtkGestureLongPress
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkGestureLongPress
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkGestureLongPress" gesture-long-press
  (:superclass gesture-single
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_long_press_get_type")
  ((delay-factor
    gesture-long-press-delay-factor
    "delay-factor" "gdouble" t t)))

#+liber-documentation
(setf (documentation 'gesture-long-press 'type)
 "@version{#2020-9-11}
  @begin{short}
    The @sym{gtk:gesture-long-press} object is a gesture for long presses.
  @end{short}
  This gesture is also known as \"Press and Hold\".

  When the timeout is exceeded, the gesture is triggering the \"pressed\"
  signal.

  If the touchpoint is lifted before the timeout passes, or if it drifts too far
  of the initial press point, the \"cancelled\" signal will be emitted.

  How long the timeout is before the \"pressed\" signal gets emitted is
  determined by the @slot[gtk:settings]{gtk-long-press-time} setting. It can be
  modified by the @code{delay-factor} property.
  @begin[Signal Details]{dictionary}
    @subheading{The \"cancelled\" signal}
      @begin{pre}
lambda (gesture)    :run-last
      @end{pre}
      The signal is emitted whenever a press moved too far, or was released
      before the \"pressed\" signal happened.
      @begin[code]{table}
        @entry[gesture]{The @sym{gtk:gesture-long-press} object which received
          the signal.}
      @end{table}
    @subheading{The \"pressed\" signal}
      @begin{pre}
lambda (gesture x y)    :run-last
      @end{pre}
      The signal is emitted whenever a press goes unmoved/unreleased longer
      than what the GTK defaults tell.
      @begin[code]{table}
        @entry[gesture]{The @sym{gtk:gesture-long-press} object which received
          the signal.}
        @entry[x]{A double float with the x coordinate where the press happened,
          relative to the widget allocation.}
        @entry[y]{A double float with the y coordinate where the press happened,
          relative to the widget allocation.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:gesture-long-press-delay-factor}
  @see-constructor{gtk:gesture-long-press-new}
  @see-class{gtk:gesture}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gesture-long-press-delay-factor ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "delay-factor"
                      'gesture-long-press) t)
 "The @code{delay-factor} property of type @code{:double} (Read / Write) @br{}
  Factor by which to modify the default timeout. @br{}
  Allowed values: [0.5d0, 2.0d0] @br{}
  Default value: 1.0d0")

#+liber-documentation
(setf (liber:alias-for-function 'gesture-long-press-delay-factor)
      "Accessor"
      (documentation 'gesture-long-press-delay-factor 'function)
 "@version{#2020-9-11}
  @syntax[]{(gtk:gesture-long-press-delay-factor object) => delay-factor)}
  @syntax[]{(setf (gtk:gesture-long-press-delay-factor object) delay-factor)}
  @argument[object]{a @class{gtk:gesture-long-press} object}
  @argument[delay-factor]{a double float with the factor by which to modify
    the default timeout}
  @begin{short}
    Accessor of the @slot[gtk:gesture-long-press]{delay-factor} slot of the
    @class{gtk:gesture-long-press} class.
  @end{short}
  @see-class{gtk:gesture-long-press}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_long_press_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gesture-long-press-new))

(defun gesture-long-press-new ()
 #+liber-documentation
 "@version{#2020-9-11}
  @argument[widget]{a @class{gtk:widget} object}
  @return{A newly created @class{gtk:gesture-long-press} object.}
  @begin{short}
    Returns a newly created gesture that recognizes long presses.
  @end{short}
  @see-function{gtk:gesture-long-press}"
  (make-instance 'gesture-long-press))

(export 'gesture-long-press-new)

;;; --- End of file gtk.gesture-long-press.lisp --------------------------------
