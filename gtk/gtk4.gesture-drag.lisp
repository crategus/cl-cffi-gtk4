;;; ----------------------------------------------------------------------------
;;; gtk4.gesture-drag.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
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
;;; GtkGestureDrag
;;;
;;;     Drag gesture
;;;
;;; Types and Values
;;;
;;;     GtkGestureDrag
;;;
;;; Functions
;;;
;;;     gtk_gesture_drag_new
;;;     gtk_gesture_drag_get_start_point
;;;     gtk_gesture_drag_get_offset
;;;
;;; Signals
;;;
;;;     drag-begin
;;;     drag-end
;;;     drag-update
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkGesture
;;;             ╰── GtkGestureSingle
;;;                 ╰── GtkGestureDrag
;;;                     ╰── GtkGesturePan
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkGestureDrag
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkGestureDrag" gesture-drag
  (:superclass gesture-single
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_drag_get_type")
  nil)

#+liber-documentation
(setf (documentation 'gesture-drag 'type)
 "@version{2024-2-19}
  @begin{short}
    The @class{gtk:gesture-drag} class is a @class{gtk:gesture} implementation
    that recognizes drag operations.
  @end{short}
  The drag operation itself can be tracked throught the @code{\"drag-begin\"},
  @code{\"drag-update\"} and @code{\"drag-end\"} signals, or the relevant
  coordinates be extracted through the @fun{gtk:gesture-drag-offset} and
  @fun{gtk:gesture-drag-start-point} functions.
  @begin[Signal Details]{dictionary}
    @subheading{The \"drag-begin\" signal}
      @begin{pre}
lambda (gesture xstart ystart)    :run-last
      @end{pre}
      The signal is emitted whenever dragging starts.
      @begin[code]{table}
        @entry[gesture]{The @class{gtk:gesture-drag} object which received the
          signal.}
        @entry[xstart]{A double float with the x coordinate, relative to the
          widget allocation.}
        @entry[ystart]{A double float with the y coordinate, relative to the
          widget allocation.}
      @end{table}
    @subheading{The \"drag-end\" signal}
      @begin{pre}
lambda (gesture xoffset yoffset)    :run-last
      @end{pre}
      The signal is emitted whenever the dragging is finished.
      @begin[code]{table}
        @entry[gesture]{The @class{gtk:gesture-drag} object which received the
          signal.}
        @entry[xoffset]{A double float with the x offset, relative to the
          start point.}
        @entry[yoffset]{A double float with the y offset, relative to the
          start point.}
      @end{table}
    @subheading{The \"drag-update\" signal}
      @begin{pre}
lambda (gesture xoffset yoffset)    :run-last
      @end{pre}
      The signal is emitted whenever the dragging point moves.
      @begin[code]{table}
        @entry[gesture]{The @class{gtk:gesture-drag} object which received the
          signal.}
        @entry[xoffset]{A double float with the x offset, relative to the
          start point.}
        @entry[yoffset]{A double float with the y offset, relative to the
          start point.}
      @end{table}
  @end{dictionary}
  @see-class{gtk:gesture}
  @see-class{gtk:gesture-swipe}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_drag_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gesture-drag-new))

(defun gesture-drag-new ()
 #+liber-documentation
 "@version{2024-2-19}
  @return{A newly created @class{gtk:gesture-drag} object.}
  @short{Returns a newly created gesture that recognizes drags.}
  @see-class{gtk:gesture-drag}"
  (make-instance 'gesture-drag))

(export 'gesture-drag-new)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_drag_get_start_point ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_drag_get_start_point" %gesture-drag-start-point)
    :boolean
  (gesture (g:object gesture-drag))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun gesture-drag-start-point (gesture)
 #+liber-documentation
 "@version{2024-2-19}
  @argument[gesture]{a @class{gtk:gesture-drag} object}
  @begin{return}
    @arg{x} -- a double float with the x coordinate for the drag start point
    @br{}
    @arg{y} -- a double float with the y coordinate for the drag start point
  @end{return}
  @begin{short}
    If the gesture is active, this function returns the drag start coordinates,
    in widget-relative coordinates, otherwise @code{nil} is returned.
  @end{short}
  @see-class{gtk:gesture-drag}"
  (cffi:with-foreign-objects ((x :double) (y :double))
    (when (%gesture-drag-start-point gesture x y)
      (values (cffi:mem-ref x :double)
              (cffi:mem-ref y :double)))))

(export 'gesture-drag-start-point)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_drag_get_offset ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_drag_get_offset" %gesture-drag-offset) :boolean
  (gesture (g:object gesture-drag))
  (x (:pointer :double))
  (y (:pointer :double)))

(defun gesture-drag-offset (gesture)
 #+liber-documentation
 "@version{2024-2-19}
  @argument[gesture]{a @class{gtk:gesture-drag} object}
  @begin{return}
    @arg{x} -- a double float with the x offset for the current point @br{}
    @arg{y} -- a double float with the y offset for the current point
  @end{return}
  @begin{short}
    If the gesture is active, this function returns the coordinates of the
    current point, as an offset to the starting drag point, otherwise @code{nil}
    is returned.
  @end{short}
  @see-class{gtk:gesture-drag}"
  (cffi:with-foreign-objects ((x :double) (y :double))
    (when (%gesture-drag-offset gesture x y)
      (values (cffi:mem-ref x :double)
              (cffi:mem-ref y :double)))))

(export 'gesture-drag-offset)

;;; --- End of file gtk4.gesture-drag.lisp -------------------------------------
