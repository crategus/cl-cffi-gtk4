;;; ----------------------------------------------------------------------------
;;; gtk.gesture-swipe.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
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
;;; GtkGestureSwipe
;;;
;;;     Swipe gesture
;;;
;;; Types and Values
;;;
;;;     GtkGestureSwipe
;;;
;;; Functions
;;;
;;;     gtk_gesture_swipe_new
;;;     gtk_gesture_swipe_get_velocity
;;;
;;; Signals
;;;
;;;     swipe
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkGesture
;;;             ╰── GtkGestureSingle
;;;                 ╰── GtkGestureSwipe
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkGestureSwipe
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkGestureSwipe" gesture-swipe
  (:superclass gesture-single
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_swipe_get_type")
  nil)

#+liber-documentation
(setf (documentation 'gesture-swipe 'type)
 "@version{#2020-9-11}
  @begin{short}
    The @sym{gtk:gesture-swipe} class is a @class{gtk:gesture} implementation
    for swipe gestures.
  @end{short}
  After a press/move/.../move/release sequence happens, the \"swipe\" signal
  will be emitted, providing the velocity and directionality of the sequence at
  the time it was lifted.

  If the velocity is desired in intermediate points, the
  @fun{gtk:gesture-swipe-velocity} function can be called in a \"update\"
  handler.

  All velocities are reported in pixels/sec units.
  @begin[Signal Details]{dictionary}
    @subheading{The \"swipe\" signal}
      @begin{pre}
lambda (gesture vel-x vel-y)    :run-last
      @end{pre}
      The signal is emitted when the recognized gesture is finished, velocity
      and direction are a product of previously recorded events.
      @begin[code]{table}
        @entry[gesture]{The @sym{gtk:gesture-multi-press} object which received
          the signal.}
        @entry[vel-x]{A double float with the velocity in the x axis,
          in pixels/sec.}
        @entry[vel-y]{A double float with the velocity in the y axis,
          in pixels/sec.}
    @end{table}
  @end{dictionary}
  @see-constructor{gtk:gesture-swipe-new}
  @see-class{gtk:gesture}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_swipe_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gesture-swipe-new))

(defun gesture-swipe-new ()
 #+liber-documentation
 "@version{#2020-9-11}
  @return{A newly created @class{gtk:gesture-swipe} object.}
  @begin{short}
    Returns a newly created gesture that recognizes swipes.
  @end{short}
  @see-class{gtk:gesture-swipe}"
  (make-instance 'gesture-swipe))

(export 'gesture-swipe-new)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_swipe_get_velocity () ->gesture-swipe-velocity
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_gesture_swipe_get_velocity" %gesture-swipe-velocity) :boolean
  (gesture (g:object gesture-swipe))
  (vel-x (:pointer :double))
  (vel-y (:pointer :double)))

(defun gesture-swipe-velocity (gesture)
 #+liber-documentation
 "@version{#2020-9-11}
  @argument[gesture]{a @class{gtk:gesture-swipe} object}
  @begin{return}
    @arg{vel-x} -- a double float with the velocity in the x axis, in
      pixels/sec. @br{}
    @arg{vel-y} -- a double float with the velocity in the y axis, in
      pixels/sec.
  @end{return}
  @begin{short}
    If the gesture is recognized, this function returns the recorded velocity,
    as per the last event(s) processed.
  @end{short}
  @see-class{gtk:gesture-swipe}"
  (with-foreign-objects ((vel-x :double) (vel-y :double))
    (when (%gesture-swipe-velocity gesture vel-x vel-y)
      (values (mem-ref vel-x :double)
              (mem-ref vel-y :double)))))

(export 'gesture-swipe-velocity)

;;; --- End of file gtk.gesture-swipe.lisp -------------------------------------
