;;; ----------------------------------------------------------------------------
;;; gtk4.gesture-stylus.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2026 Dieter Kaiser
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
;;; Accessors
;;;
;;;     gtk_gesture_stylus_get_stylus_only                 Since 4.10
;;;     gtk_gesture_stylus_set_stylus_only                 Since 4.10
;;;
;;; Functions
;;;
;;;     gtk_gesture_stylus_new
;;;     gtk_gesture_stylus_get_axis
;;;     gtk_gesture_stylus_get_axes
;;;     gtk_gesture_stylus_get_backlog
;;;     gtk_gesture_stylus_get_device_tool
;;;
;;; Properties
;;;
;;;     stylus-only                                        Since 4.10
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
;;; GtkGestureStylus
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkGestureStylus" gesture-stylus
  (:superclass gesture-single
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_stylus_get_type")
  (#+gtk-4-10
   (stylus-only
    gesture-stylus-stylus-only
    "stylus-only" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'gesture-stylus 'type)
 "@version{2025-07-19}
  @begin{short}
    The @class{gtk:gesture-stylus} class is a @class{gtk:gesture} implementation
    specific to stylus input.
  @end{short}
  The provided signals just provide the basic information of the stylus events.
  @begin[Signal Details]{dictionary}
    @begin[gesture-stylus::down]{signal}
      @begin{pre}
lambda (gesture x y)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[gesture]{The @class{gtk:gesture-stylus} object on which the
          signal is emitted.}
        @entry[x]{The double float for the x coordinate of the stylus event.}
        @entry[y]{The double float for the y coordinate of the stylus event.}
      @end{simple-table}
    @end{signal}
    @begin[gesture-stylus::motion]{signal}
      @begin{pre}
lambda (gesture x y)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[gesture]{The @class{gtk:gesture-stylus} object on which the
          signal is emitted.}
        @entry[x]{The double float for the x coordinate of the stylus event.}
        @entry[y]{The double float for the y coordinate of the stylus event.}
      @end{simple-table}
    @end{signal}
    @begin[gesture-stylus::proximity]{signal}
      @begin{pre}
lambda (gesture x y)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[gesture]{The @class{gtk:gesture-stylus} object on which the
          signal is emitted.}
        @entry[x]{The double float for the x coordinate of the stylus event.}
        @entry[y]{The double float for the y coordinate of the stylus event.}
      @end{simple-table}
    @end{signal}
    @begin[gesture-stylus::up]{signal}
      @begin{pre}
lambda (gesture x y)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[gesture]{The @class{gtk:gesture-stylus} object on which the
          signal is emitted.}
        @entry[x]{The double float for the x coordinate of the stylus event.}
        @entry[y]{The double float for the y coordinate of the stylus event.}
      @end{simple-table}
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:gesture-stylus-new}
  @see-slot{gtk:gesture-stylus-stylus-only}
  @see-class{gtk:gesture}
  @see-class{gtk:gesture-single}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:gesture-stylus-stylus-only -----------------------------------------

#+(and gtk-4-10 liber-documentation)
(setf (documentation (liber:slot-documentation "stylus-only" 'gesture-stylus) t)
 "The @code{stylus-only} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Whether this gesture should exclusively react to stylus input devices.
  Since 4.10 @br{}
  Default value: @em{true}")

#+(and gtk-4-10 liber-documentation)
(setf (liber:alias-for-function 'gesture-stylus-stylus-only)
      "Accessor"
      (documentation 'gesture-stylus-stylus-only 'function)
 "@version{2025-09-21}
  @syntax{(gtk:gesture-stylus-only object) => setting}
  @syntax{(setf (gtk:gesture-stylus-only object) setting)}
  @argument[object]{a @class{gtk:gesture-stylus} object}
  @argument[setting]{a boolean whether the gesture is used exclusivly for
    stylus events}
  @begin{short}
    The accessor for the @slot[gtk:gesture-stylus]{stylus-only} slot of the
    @class{gtk:gesture-stylus} class gets or sets whether the gesture handle
    events from stylus input devices.
  @end{short}
  Otherwise it will handle events from any pointing device.

  Since 4.10
  @see-class{gtk:gesture-stylus}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_stylus_new
;;; ----------------------------------------------------------------------------

(defun gesture-stylus-new ()
 #+liber-documentation
 "@version{2024-02-21}
  @return{The newly created @class{gtk:gesture-stylus} object.}
  @begin{short}
    Creates a new stylus gesture.
  @end{short}
  @see-class{gtk:gesture-stylus}"
  (make-instance 'gesture-stylus))

(export 'gesture-stylus-new)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_stylus_get_axis
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_stylus_get_axis" %gesture-stylus-axis) :boolean
  (gesture (g:object gesture-stylus))
  (axis gdk:axis-use)
  (value (:pointer :double)))

(defun gesture-stylus-axis (gesture axis)
 #+liber-documentation
 "@version{2025-07-24}
  @argument[gesture]{a @class{gtk:gesture-stylus} object}
  @argument[axis]{a @sym{gdk:axis-use} value for the requested device axis}
  @return{The double float for the current value for the axis.}
  @begin{short}
    Returns the current value for the requested axis.
  @end{short}
  This function must be called from either the @sig[gtk:gesture-stylus]{down},
  @sig[gtk:gesture-stylus]{motion}, @sig[gtk:gesture-stylus]{up} or
  @sig[gtk:gesture-stylus]{proximity} signals.
  @see-class{gtk:gesture-stylus}
  @see-symbol{gdk:axis-use}"
  (cffi:with-foreign-object (value :double)
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
;;; gtk_gesture_stylus_get_device_tool
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_stylus_get_device_tool" gesture-stylus-device-tool)
    (g:object gdk:device-tool)
 #+liber-documentation
 "@version{2025-07-24}
  @argument[gesture]{a @class{gtk:gesture-stylus} object}
  @return{The current @class{gdk:device-tool} object.}
  @begin{short}
    Returns the device tool currently driving input through this gesture.
  @end{short}
  This function must be called from either the @sig[gtk:gesture-stylus]{down},
  @sig[gtk:gesture-stylus]{motion}, @sig[gtk:gesture-stylus]{up} or
  @sig[gtk:gesture-stylus]{proximity} signal handlers.
  @see-class{gtk:gesture-stylus}
  @see-class{gdk:device-tool}"
  (gesture (g:object gesture-stylus)))

(export 'gesture-stylus-device-tool)

;;; --- End of file gtk4.gesture-stylus.lisp -----------------------------------
