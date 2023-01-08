;;; ----------------------------------------------------------------------------
;;; gtk.gesture-rotate.lisp
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

(define-g-object-class "GtkGestureRotate" gesture-rotate
  (:superclass gesture
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_rotate_get_type")
  nil)

#+liber-documentation
(setf (documentation 'gesture-rotate 'type)
 "@version{#2020-9-11}
  @begin{short}
    The @sym{gtk:gesture-rotate} class is a @class{gtk:gesture} implementation
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
        @entry[gesture]{The @sym{gtk:gesture-rotate} object which received the
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

(defcfun ("gtk_gesture_rotate_get_angle_delta" gesture-rotate-angle-delta)
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

;;; --- End of file gtk.gesture-rotate.lisp ------------------------------------
