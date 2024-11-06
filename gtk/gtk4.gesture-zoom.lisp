;;; ----------------------------------------------------------------------------
;;; gtk4.gesture-zoom.lisp
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

(gobject:define-gobject "GtkGestureZoom" gesture-zoom
  (:superclass gesture
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_zoom_get_type")
  nil)

#+liber-documentation
(setf (documentation 'gesture-zoom 'type)
 "@version{2024-7-27}
  @begin{short}
    The @class{gtk:gesture-zoom} class is a @class{gtk:gesture} implementation
    for 2-finger pinch/zoom gestures.
  @end{short}
  Whenever the distance between both tracked sequences changes, the
  @code{\"scale-changed\"} signal is emitted to report the scale factor.
  @begin[Signal Details]{dictionary}
    @subheading{The \"scale-changed\" signal}
      @begin{pre}
lambda (gesture scale)    :run-first
      @end{pre}
      @begin[code]{table}
        @entry[gesture]{The @class{gtk:gesture-zoom} object on which the signal
          is emitted.}
        @entry[scale]{The double float with the scale delta, taking the initial
          state as 1:1.}
      @end{table}
      The signal is emitted whenever the distance between both tracked sequences
      changes.
  @end{dictionary}
  @see-constructor{gtk:gesture-zoom-new}
  @see-class{gtk:gesture-rotate}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_zoom_new
;;; ----------------------------------------------------------------------------

(declaim (inline gesture-zoom-new))

(defun gesture-zoom-new ()
 #+liber-documentation
 "@version{2024-7-27}
  @return{The newly created @class{gtk:gesture-zoom} object.}
  @begin{short}
    Returns a newly created gesture that recognizes pinch/zoom gestures.
  @end{short}
  @see-class{gtk:gesture-zoom}"
  (make-instance 'gesture-zoom))

(export 'gesture-zoom-new)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_zoom_get_scale_delta
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_zoom_get_scale_delta" gesture-zoom-scale-delta)
    :double
 #+liber-documentation
 "@version{2024-7-27}
  @argument[widget]{a @class{gtk:gesture-zoom} object}
  @return{The double float with the scale delta.}
  @begin{short}
    If the gesture is active, this function returns the zooming difference since
    the gesture was recognized, hence the starting point is considered 1:1.
  @end{short}
  If the gesture is not active, 1.0d0 is returned.
  @see-class{gtk:gesture-zoom}"
  (gesture (g:object gesture-zoom)))

(export 'gesture-zoom-scale-delta)

;;; --- End of file gtk4.gesture-zoom.lisp -------------------------------------
