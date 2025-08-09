;;; ----------------------------------------------------------------------------
;;; gtk4.gesture-swipe.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2025 Dieter Kaiser
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
;;; GtkGestureSwipe
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkGestureSwipe" gesture-swipe
  (:superclass gesture-single
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_swipe_get_type")
  nil)

#+liber-documentation
(setf (documentation 'gesture-swipe 'type)
 "@version{2025-07-19}
  @begin{short}
    The @class{gtk:gesture-swipe} class is a @class{gtk:gesture} implementation
    for swipe gestures.
  @end{short}
  After a press/move/.../move/release sequence happens, the @code{\"swipe\"}
  signal will be emitted, providing the velocity and directionality of the
  sequence at the time it was lifted.

  If the velocity is desired in intermediate points, the
  @fun{gtk:gesture-swipe-velocity} function can be called in a @code{\"update\"}
  signal handler for the @class{gdk:frame-clock} object. All velocities are
  reported in pixels/sec units.
  @begin[Signal Details]{dictionary}
    @begin[gesture-swipe::swipe]{signal}
      @begin{pre}
lambda (gesture xvel yvel)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[gesture]{The @class{gtk:gesture-swipe} object that received the
          signal.}
        @entry[xvel]{The double float for the velocity in the x axis,
          in pixels/sec.}
        @entry[yvel]{The double float for the velocity in the y axis,
          in pixels/sec.}
    @end{simple-table}
    The signal is emitted when the recognized gesture is finished, velocity
    and direction are a product of previously recorded events.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:gesture-swipe-new}
  @see-class{gtk:gesture}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_swipe_new
;;; ----------------------------------------------------------------------------

(declaim (inline gesture-swipe-new))

(defun gesture-swipe-new ()
 #+liber-documentation
 "@version{2024-02-19}
  @return{The newly created @class{gtk:gesture-swipe} object.}
  @begin{short}
    Returns a newly created gesture that recognizes swipes.
  @end{short}
  @see-class{gtk:gesture-swipe}"
  (make-instance 'gesture-swipe))

(export 'gesture-swipe-new)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_swipe_get_velocity
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_swipe_get_velocity" %gesture-swipe-velocity)
    :boolean
  (gesture (g:object gesture-swipe))
  (xvel (:pointer :double))
  (yvel (:pointer :double)))

(defun gesture-swipe-velocity (gesture)
 #+liber-documentation
 "@version{2025-07-26}
  @syntax{(gtk:gesture-swipe-velocity gesture) => xvel, yvel}
  @argument[gesture]{a @class{gtk:gesture-swipe} object}
  @argument[xvel]{a double float for the velocity in the x axis, in pixels/sec}
  @argument[yvel]{a double float for the velocity in the y axis, in pixels/sec}
  @begin{short}
    If the gesture is recognized, this function returns the recorded velocity,
    as per the last event(s) processed, otherwise @code{nil} is returned.
  @end{short}
  @see-class{gtk:gesture-swipe}"
  (cffi:with-foreign-objects ((xvel :double) (yvel :double))
    (when (%gesture-swipe-velocity gesture xvel yvel)
      (values (cffi:mem-ref xvel :double)
              (cffi:mem-ref yvel :double)))))

(export 'gesture-swipe-velocity)

;;; --- End of file gtk4.gesture-swipe.lisp ------------------------------------
