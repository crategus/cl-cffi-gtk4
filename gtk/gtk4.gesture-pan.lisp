;;; ----------------------------------------------------------------------------
;;; gtk4.gesture-pan.lisp
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
;;; GtkGesturePan
;;;
;;;     Pan gesture
;;;
;;; Types and Values
;;;
;;;     GtkGesturePan
;;;     GtkPanDirection
;;;
;;; Accessors
;;;
;;;     gtk_gesture_pan_get_orientation
;;;     gtk_gesture_pan_set_orientation
;;;
;;; Functions
;;;
;;;     gtk_gesture_pan_new
;;;
;;; Properties
;;;
;;;     orientation
;;;
;;; Signals
;;;
;;;     pan
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
;;; GtkPanDirection
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkPanDirection" pan-direction
  (:export t
   :type-initializer "gtk_pan_direction_get_type")
  :left
  :right
  :up
  :down)

#+liber-documentation
(setf (liber:alias-for-symbol 'pan-direction)
      "GEnum"
      (liber:symbol-documentation 'pan-direction)
 "@version{2025-06-29}
  @begin{declaration}
(gobject:define-genum \"GtkPanDirection\" pan-direction
  (:export t
   :type-initializer \"gtk_pan_direction_get_type\")
  :left
  :right
  :up
  :down)
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:left]{Panned towards the left.}
      @entry[:right]{Panned towards the right.}
      @entry[:up]{Panned upwards.}
      @entry[:down]{Panned downwards.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Describes the panning direction of a @class{gtk:gesture-pan} object.
  @end{short}
  @see-class{gtk:gesture-pan}")

;;; ----------------------------------------------------------------------------
;;; GtkGesturePan
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkGesturePan" gesture-pan
  (:superclass gesture-drag
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_pan_get_type")
  ((orientation
    gesture-pan-orientation
    "orientation" "GtkOrientation" t t)))

#+liber-documentation
(setf (documentation 'gesture-pan 'type)
 "@version{2025-07-26}
  @begin{short}
    The @class{gtk:gesture-pan} class is a @class{gtk:gesture} implementation
    for pan gestures.
  @end{short}
  These are drags that are locked to happen along one axis.

  The axis that a @class{gtk:gesture-pan} object handles is defined at construct
  time, and can be changed through the @fun{gtk:gesture-pan-orientation}
  function. When the gesture starts to be recognized, the
  @class{gtk:gesture-pan} object will attempt to determine as early as possible
  whether the sequence is moving in the expected direction, and denying the
  sequence if this does not happen. Once a panning gesture along the expected
  axis is recognized, the @sig[gtk:gesture-pan]{pan} signal will be emitted as
  input events are received, containing the offset in the given axis.
  @begin[Signal Details]{dictionary}
    @begin[gesture-pan::pan]{signal}
      @begin{pre}
lambda (gesture direction offset)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[gesture]{The @class{gtk:gesture-pan} object that received the
          signal.}
        @entry[direction]{The current @sym{gtk:pan-direction} value of the
          pan gesture.}
        @entry[offset]{The double float for the offset along the gesture
          orientation.}
      @end{simple-table}
      The signal is emitted once a panning gesture along the expected axis is
      detected.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:gesture-pan-new}
  @see-slot{gtk:gesture-pan-orientation}
  @see-class{gtk:gesture}
  @see-symbol{gtk:pan-direction}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:gesture-pan-orientation --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "orientation" 'gesture-pan) t)
 "The @code{orientation} property of type @sym{gtk:orientation} (Read / Write)
  @br{}
  The expected orientation of pan gestures. @br{}
  Default value: @val[gtk:orientation]{:horizontal}")

#+liber-documentation
(setf (liber:alias-for-function 'gesture-pan-orientation)
      "Accessor"
      (documentation 'gesture-pan-orientation 'function)
 "@version{2025-08-17}
  @syntax{(gtk:gesture-pan-orientation object) => orientation}
  @syntax{(setf (gtk:gesture-pan-orientation object) orientation)}
  @argument[object]{a @class{gtk:gesture} object}
  @argument[orientation]{a @sym{gtk:orientation} value}
  @begin{short}
    The accessor for the @slot[gtk:gesture-pan]{orientation} slot of the
    @class{gtk:gesture-pan} class gets or sets the orientation of the pan
    gestures that this gesture expects.
  @end{short}
  @see-class{gtk:gesture-pan}
  @see-symbol{gtk:orientation}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_pan_new
;;; ----------------------------------------------------------------------------

(declaim (inline gesture-pan-new))

(defun gesture-pan-new (orientation)
 #+liber-documentation
 "@version{2025-07-26}
  @argument[orientation]{a @sym{gtk:orientation} value}
  @return{The newly created @class{gtk:gesture-pan} object.}
  @begin{short}
    Returns a newly created gesture that recognizes pan gestures.
  @end{short}
  @see-class{gtk:gesture-pan}
  @see-symbol{gtk:orientation}"
  (make-instance 'gesture-pan
                 :orientation orientation))

(export 'gesture-pan-new)

;;; --- End of file gtk4.gesture-pan.lisp --------------------------------------
