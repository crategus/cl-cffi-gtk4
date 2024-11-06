;;; ----------------------------------------------------------------------------
;;; gtk4.gesture-long-press.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2024 Dieter Kaiser
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
;;; GtkGestureLongPress
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkGestureLongPress" gesture-long-press
  (:superclass gesture-single
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_long_press_get_type")
  ((delay-factor
    gesture-long-press-delay-factor
    "delay-factor" "gdouble" t t)))

#+liber-documentation
(setf (documentation 'gesture-long-press 'type)
 "@version{2024-2-19}
  @begin{short}
    The @class{gtk:gesture-long-press} object is a gesture for long presses.
  @end{short}
  This gesture is also known as \"Press and Hold\".

  When the timeout is exceeded, the gesture is triggering the @code{\"pressed\"}
  signal. If the touchpoint is lifted before the timeout passes, or if it drifts
  too far of the initial press point, the @code{\"cancelled\"} signal will be
  emitted. How long the timeout is before the @code{\"pressed\"} signal gets
  emitted is determined by the @slot[gtk:settings]{gtk-long-press-time} setting.
  It can be modified by the @slot[gtk:gesture-long-press]{delay-factor}
  property.
  @begin[Signal Details]{dictionary}
    @subheading{The \"cancelled\" signal}
      @begin{pre}
lambda (gesture)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[gesture]{The @class{gtk:gesture-long-press} object which received
          the signal.}
      @end{table}
      The signal is emitted whenever a press moved too far, or was released
      before the @code{\"pressed\"} signal happened.
    @subheading{The \"pressed\" signal}
      @begin{pre}
lambda (gesture x y)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[gesture]{The @class{gtk:gesture-long-press} object which received
          the signal.}
        @entry[x]{The double float with the x coordinate where the press
          happened, relative to the widget allocation.}
        @entry[y]{The double float with the y coordinate where the press
          happened, relative to the widget allocation.}
      @end{table}
      The signal is emitted whenever a press goes unmoved/unreleased longer
      than what the GTK defaults tell.
  @end{dictionary}
  @see-slot{gtk:gesture-long-press-delay-factor}
  @see-constructor{gtk:gesture-long-press-new}
  @see-class{gtk:gesture}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:gesture-long-press-delay-factor ------------------------------------

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
 "@version{2024-2-19}
  @syntax{(gtk:gesture-long-press-delay-factor object) => factor}
  @syntax{(setf (gtk:gesture-long-press-delay-factor object) factor)}
  @argument[object]{a @class{gtk:gesture-long-press} object}
  @argument[factor]{a double float with the factor by which to modify
    the default timeout}
  @begin{short}
    Accessor of the @slot[gtk:gesture-long-press]{delay-factor} slot of the
    @class{gtk:gesture-long-press} class.
  @end{short}
  @see-class{gtk:gesture-long-press}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_long_press_new
;;; ----------------------------------------------------------------------------

(declaim (inline gesture-long-press-new))

(defun gesture-long-press-new ()
 #+liber-documentation
 "@version{2024-2-19}
  @return{The newly created @class{gtk:gesture-long-press} object.}
  @begin{short}
    Returns a newly created gesture that recognizes long presses.
  @end{short}
  @see-function{gtk:gesture-long-press}"
  (make-instance 'gesture-long-press))

(export 'gesture-long-press-new)

;;; --- End of file gtk4.gesture-long-press.lisp -------------------------------
