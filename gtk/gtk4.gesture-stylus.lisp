;;; ----------------------------------------------------------------------------
;;; gtk4.gesture-stylus.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
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
;;; GtkGestureStylus
;;;
;;;     Gesture for stylus input
;;;
;;; Types and Values
;;;
;;;     GtkGestureStylus
;;;
;;; Functions
;;;
;;;     gtk_gesture_stylus_new
;;;     gtk_gesture_stylus_get_axis
;;;     gtk_gesture_stylus_get_axes
;;;     gtk_gesture_stylus_get_backlog
;;;     gtk_gesture_stylus_get_device_tool
;;;
;;; Signals
;;;
;;;     down
;;;     motion
;;;     proximity
;;;     up
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkGesture
;;;             ╰── GtkGestureSingle
;;;                 ╰── GtkGestureStylus
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkGestureStylus
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkGestureStylus" gesture-stylus
  (:superclass gesture-single
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_stylus_get_type")
  nil)

#+liber-documentation
(setf (documentation 'gesture-stylus 'type)
 "@version{#2020-9-11}
  @begin{short}
    The @sym{gtk:gesture-stylus} class is a @class{gtk:gesture} implementation
    specific to stylus input.
  @end{short}
  The provided signals just provide the basic information of the stylus events.
  @begin[Signal Details]{dictionary}
    @subheading{The \"down\" signal}
      @begin{pre}
lambda (gesture x y)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[gesture]{The @sym{gtk:gesture-stylus} object on which the signal
          is emitted.}
        @entry[x]{A double float with the x coordinate of the stylus event.}
        @entry[y]{A double float with the y coordinate of the stylus event.}
      @end{table}
    @subheading{The \"motion\" signal}
      @begin{pre}
lambda (gesture x y)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[gesture]{The @sym{gtk:gesture-stylus} object on which the signal
          is emitted.}
        @entry[x]{A double float with the x coordinate of the stylus event.}
        @entry[y]{A double float with the y coordinate of the stylus event.}
      @end{table}
    @subheading{The \"proximity\" signal}
      @begin{pre}
lambda (gesture x y)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[gesture]{The @sym{gtk:gesture-stylus} object on which the signal
          is emitted.}
        @entry[x]{A double float with the x coordinate of the stylus event.}
        @entry[y]{A double float with the y coordinate of the stylus event.}
      @end{table}
    @subheading{The \"up\" signal}
      @begin{pre}
lambda (gesture x y)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[gesture]{The @sym{gtk:gesture-stylus} object on which the signal
          is emitted.}
        @entry[x]{A double float with the x coordinate of the stylus event.}
        @entry[y]{A double float with the y coordinate of the stylus event.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:gesture-single-new}
  @see-class{gtk:gesture}
  @see-class{gtk:gesture-single}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_stylus_new ()
;;; ----------------------------------------------------------------------------

(defun gesture-stylus-new ()
 #+liber-documentation
 "@version{#2020-9-11}
  @argument[widget]{a @class{gtk:gesture-stylus} object}
  @return{A newly created @class{gtk:gesture-stylus} object.}
  @begin{short}
    Creates a new stylus gesture.
  @end{short}
  @see-class{gtk:gesture-stylus}"
  (make-instance 'gesture-stylus))

(export 'gesture-stylus-new)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_stylus_get_axis () -> gesture-stylus-axis
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_gesture_stylus_get_axis" %gesture-stylus-axis) :boolean
  (gesture (g:object gesture-stylus))
  (axis gdk:axis-use)
  (value (:pointer :double)))

(defun gesture-stylus-axis (gesture axis)
 #+liber-documentation
 "@version{#2020-9-11}
  @argument[gesture]{a @class{gtk:gesture-stylus} object}
  @argument[axis]{requested device axis of type @symbol{gdk:axis-use}}
  @return{A double float with the current value for the axis.}
  @begin{short}
    Returns the current value for the requested axis.
  @end{short}
  This function must be called from either the \"down\", \"motion\", \"up\" or
  \"proximity\" signals.
  @see-class{gtk:gesture-stylus}"
  (with-foreign-object (value :double)
    (when (%gesture-stylus-axis gesture axis value)
      (cffi:mem-ref value :double))))

(export 'gesture-stylus-axis)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_stylus_get_axes ()
;;;
;;; gboolean
;;; gtk_gesture_stylus_get_axes (GtkGestureStylus *gesture,
;;;                              GdkAxisUse axes[],
;;;                              double **values);
;;;
;;; Returns the current values for the requested axes . This function must be
;;; called from either the “down”, “motion”, “up” or “proximity” signals.
;;;
;;; gesture :
;;;     a GtkGestureStylus
;;;
;;; axes :
;;;     array of requested axes, terminated with GDK_AXIS_IGNORE.
;;;
;;; values :
;;;     return location for the axis values.
;;;
;;; Returns :
;;;     TRUE if there is a current value for the axes
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_stylus_get_backlog ()
;;;
;;; gboolean
;;; gtk_gesture_stylus_get_backlog (GtkGestureStylus *gesture,
;;;                                 GdkTimeCoord **backlog,
;;;                                 guint *n_elems);
;;;
;;; By default, GTK will limit rate of input events. On stylus input where
;;; accuracy of strokes is paramount, this function returns the accumulated
;;; coordinate/timing state before the emission of the current “motion” signal.
;;;
;;; This function may only be called within a “motion” signal handler, the state
;;; given in this signal and obtainable through gtk_gesture_stylus_get_axis()
;;; call express the latest (most up-to-date) state in motion history.
;;;
;;; The backlog is provided in chronological order.
;;;
;;; gesture :
;;;     a GtkGestureStylus
;;;
;;; backlog :
;;;     coordinates and times for the backlog events.
;;;
;;; n_elems :
;;;     return location for the number of elements.
;;;
;;; Returns :
;;;     TRUE if there is a backlog to unfold in the current state.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_stylus_get_device_tool () -> gesture-stylus-device-tool
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_gesture_stylus_get_device_tool" gesture-stylus-device-tool)
    (g:object gdk-device-tool)
 #+liber-documentation
 "@version{#2022-8-3}
  @argument[gesture]{a @class{gtk:gesture-stylus} object}
  @return{The current @class{gdk-device-tool} object.}
  @begin{short}
    Returns the device tool currently driving input through this gesture.
  @end{short}
  This function must be called from either the \"down\", \"motion\", \"up\" or
  \"proximity\" signal handlers.
  @see-class{gtk:gesture-stylus}
  @see-class{gdk-device-tool}"
  (gesture (g:object gesture-stylus)))

(export 'gesture-stylus-device-tool)

;;; --- End of file gtk4.gesture-stylus.lisp -----------------------------------
