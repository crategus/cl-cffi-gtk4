;;; ----------------------------------------------------------------------------
;;; gtk4.gesture-rotate.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2023 Dieter Kaiser
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
;;; GtkGestureRotate
;;;
;;;     Rotate gesture
;;;
;;; Types and Values
;;;
;;;     GtkGestureRotate
;;;
;;; Functions
;;;
;;;     gtk_gesture_rotate_new
;;;     gtk_gesture_rotate_get_angle_delta
;;;
;;; Signals
;;;
;;;     angle-changed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkGesture
;;;             ╰── GtkGestureRotate
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkGestureRotate
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkGestureRotate" gesture-rotate
  (:superclass gesture
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_rotate_get_type")
  nil)

#+liber-documentation
(setf (documentation 'gesture-rotate 'type)
 "@version{#2020-9-11}
  @begin{short}
    The @class{gtk:gesture-rotate} class is a @class{gtk:gesture} implementation
    for 2-finger rotations.
  @end{short}
  Whenever the angle between both handled sequences changes, the
  \"angle-changed\" signal is emitted.
  @begin[Signal Details]{dictionary}
    @subheading{The \"angle-changed\" signal}
      @begin{pre}
lambda (gesture angle delta)    :run-first
      @end{pre}
      The signal is emitted when the angle between both tracked points changes.
      @begin[code]{table}
        @entry[gesture]{The @class{gtk:gesture-rotate} object which received the
          signal.}
        @entry[angle]{A double float with the current angle in radians.}
        @entry[delta]{A double float with the difference with the starting angle,
          in radians.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:gesture-rotate-new}
  @see-class{gtk:gesture-zoom}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_rotate_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gesture-rotate-new))

(defun gesture-rotate-new ()
 #+liber-documentation
 "@version{#2020-9-11}
  @argument[widget]{a @class{gtk:widget} object}
  @return{A newly created @class{gtk:gesture-rotate} object.}
  @begin{short}
    Returns a newly created gesture that recognizes 2-touch rotation gestures.
  @end{short}
  @see-class{gtk:gesture-rotate}"
  (make-instance 'gesture-rotate))

(export 'gesture-rotate-new)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_rotate_get_angle_delta ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_rotate_get_angle_delta" gesture-rotate-angle-delta)
    :double
 #+liber-documentation
 "@version{#2020-9-11}
  @argument[gesture]{a @class{gtk:gesture-rotate} object}
  @return{A double float with the angle delta in radians.}
  @begin{short}
    If the gesture is active, this function returns the angle difference in
    radians since the gesture was first recognized.
  @end{short}
  If gesture is not active, 0.0d0 is returned.
  @see-class{gtk:gesture-rotate}"
  (gesture (g:object gesture-rotate)))

(export 'gesture-rotate-angle-delta)

;;; --- End of file gtk4.gesture-rotate.lisp -----------------------------------
