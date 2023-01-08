;;; ----------------------------------------------------------------------------
;;; gtk.gesture-zoom.lisp
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
;;; GtkGestureZoom
;;;
;;;     Zoom gesture
;;;
;;; Types and Values
;;;
;;;     GtkGestureZoom
;;;
;;; Functions
;;;
;;;     gtk_gesture_zoom_new
;;;     gtk_gesture_zoom_get_scale_delta
;;;
;;; Signals
;;;
;;;     scale-changed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkGesture
;;;             ╰── GtkGestureZoom
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkGestureZoom
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkGestureZoom" gesture-zoom
  (:superclass gesture
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_zoom_get_type")
  nil)

#+liber-documentation
(setf (documentation 'gesture-zoom 'type)
 "@version{#2020-9-11}
  @begin{short}
    The @sym{gtk:gesture-zoom} class is a @class{gtk:gesture} implementation
    for 2-finger pinch/zoom gestures.
  @end{short}
  Whenever the distance between both tracked sequences changes, the
  \"scale-changed\" signal is emitted to report the scale factor.
  @begin[Signal Details]{dictionary}
    @subheading{The \"scale-changed\" signal}
      @begin{pre}
lambda (gesture scale)    :run-first
      @end{pre}
      The signal is emitted whenever the distance between both tracked sequences
      changes.
      @begin[code]{table}
        @entry[gesture]{The @sym{gtk:gesture-zoom} object on which the signal
          is emitted.}
        @entry[scale]{A double float with the scale delta, taking the initial
          state as 1:1.}
    @end{table}
  @end{dictionary}
  @see-constructor{gtk:gesture-zoom-new}
  @see-class{gtk:gesture-rotate}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_zoom_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gesture-zoom-new))

(defun gesture-zoom-new ()
 #+liber-documentation
 "@version{#2020-9-11}
  @argument[widget]{a @class{gtk:widget} object}
  @return{A newly created @class{gtk:gesture-zoom} object.}
  @begin{short}
    Returns a newly created gesture that recognizes pinch/zoom gestures.
  @end{short}
  @see-class{gtk:gesture-rotate}"
  (make-instance 'gesture-zoom))

(export 'gesture-zoom-new)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_zoom_get_scale_delta () -> gesture-zoom-scale-delta
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_gesture_zoom_get_scale_delta" gesture-zoom-scale-delta)
    :double
 #+liber-documentation
 "@version{#2020-9-11}
  @argument[widget]{a @class{gtk:gesture-zoom} object}
  @return{A double float with the scale delta.}
  @begin{short}
    If the gesture is active, this function returns the zooming difference since
    the gesture was recognized (hence the starting point is considered 1:1).
  @end{short}
  If the gesture is not active, 1 is returned.
  @see-class{gtk:gesture-rotate}"
  (gesture (g:object gesture-zoom)))

(export 'gesture-zoom-scale-delta)

;;; --- End of file gtk.gesture-zoom.lisp --------------------------------------
