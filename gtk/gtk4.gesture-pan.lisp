;;; ----------------------------------------------------------------------------
;;; gtk4.gesture-pan.lisp
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
;;; enum GtkPanDirection
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GtkPanDirection" pan-direction
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
 "@version{#2020-9-11}
  @begin{short}
    Describes the panning direction of a @class{gtk:gesture-pan} object.
  @end{short}
  @begin{pre}
(gobject:define-g-enum \"GtkPanDirection\" pan-direction
  (:export t
   :type-initializer \"gtk_pan_direction_get_type\")
  :left
  :right
  :up
  :down)
  @end{pre}
  @begin[code]{table}
    @entry[:left]{Panned towards the left.}
    @entry[:right]{Panned towards the right.}
    @entry[:up]{Panned upwards.}
    @entry[:down]{Panned downwards.}
  @end{table}
  @see-class{gtk:gesture-pan}")

;;; ----------------------------------------------------------------------------
;;; struct GtkGesturePan
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkGesturePan" gesture-pan
  (:superclass gesture-drag
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_pan_get_type")
  ((orientation
    gesture-pan-orientation
    "orientation" "GtkOrientation" t t)))

#+liber-documentation
(setf (documentation 'gesture-pan 'type)
 "@version{#2020-9-11}
  @begin{short}
    The @class{gtk:gesture-pan} class is a @class{gtk:gesture} implementation
    for pan gestures.
  @end{short}
  These are drags that are locked to happen along one axis.

  The axis that a @class{gtk:gesture-pan} object handles is defined at construct
  time, and can be changed through the @fun{gtk:gesture-pan-orientation}
  function.

  When the gesture starts to be recognized, the @class{gtk:gesture-pan} object
  will attempt to determine as early as possible whether the sequence is moving
  in the expected direction, and denying the sequence if this does not happen.

  Once a panning gesture along the expected axis is recognized, the \"pan\"
  signal will be emitted as input events are received, containing the offset in
  the given axis.
  @begin[Signal Details]{dictionary}
    @subheading{The \"pan\" signal}
    @begin{pre}
lambda (gesture direction offset)    :run-last
    @end{pre}
    The signal is emitted once a panning gesture along the expected axis is
    detected.
    @begin[code]{table}
      @entry[gesture]{The @class{gtk:gesture-pan} object which received the
        signal.}
      @entry[direction]{Current @symbol{gtk:pan-direction} value of the pan
        gesture.}
      @entry[offset]{A double float with the offset along the gesture
        orientation.}
    @end{table}
  @end{dictionary}
  @see-slot{gtk:gesture-pan-orientation}
  @see-constructor{gtk:gesture-pan-new}
  @see-class{gtk:gesture}
  @see-symbol{gtk:pan-direction}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gesture-pan-orientation ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "orientation" 'gesture-pan) t)
 "The @code{orientation} property of type @symbol{gtk:orientation}
  (Read / Write) @br{}
  The expected orientation of pan gestures. @br{}
  Default value: @code{:horizontal}")

#+liber-documentation
(setf (liber:alias-for-function 'gesture-pan-orientation)
      "Accessor"
      (documentation 'gesture-pan-orientation 'function)
 "@version{#2020-9-11}
  @syntax[]{(gtk:gesture-pan-orientation object) => orientation)}
  @syntax[]{(setf (gtk:gesture-pan-orientation object) orientation)}
  @argument[object]{a @class{gtk:gesture} object}
  @argument[orientation]{a @symbol{gtk:orientation} value}
  @begin{short}
    Accessor of the @slot[gtk:gesture-pan]{orientation} slot of the
    @class{gtk:gesture-pan} class.
  @end{short}
  The @fun{gtk:gesture-pan-orientation} function returns the orientation of the
  pan gestures that this gesture expects. The @setf{gtk:gesture-pan-orientation}
  function sets the orientation.
  @see-class{gtk:gesture-pan}
  @see-symbol{gtk:orientation}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_pan_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gesture-pan-new))

(defun gesture-pan-new (orientation)
 #+liber-documentation
 "@version{#2020-9-1}
  @argument[widget]{a @class{gtk:widget}}
  @argument[orientation]{a @symbol{gtk:orientation} value}
  @return{A newly created @class{gtk:gesture-pan}.}
  @begin{short}
    Returns a newly created gesture that recognizes pan gestures.
  @end{short}
  @see-class{gtk:gesture-pan}
  @see-symbol{gtk:orientation}"
  (make-instance 'gesture-pan
                 :orientation orientation))

(export 'gesture-pan-new)

;;; --- End of file gtk4.gesture-pan.lisp --------------------------------------
