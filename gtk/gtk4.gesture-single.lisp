;;; ----------------------------------------------------------------------------
;;; gtk4.gesture-single.lisp
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
;;; GtkGestureSingle
;;;
;;;     Base class for mouse/single-touch gestures
;;;
;;; Types and Values
;;;
;;;     GtkGestureSingle
;;;
;;; Accessors
;;;
;;;     gtk_gesture_single_get_button
;;;     gtk_gesture_single_set_button
;;;     gtk_gesture_single_get_exclusive
;;;     gtk_gesture_single_set_exclusive
;;;     gtk_gesture_single_get_touch_only
;;;     gtk_gesture_single_set_touch_only
;;;
;;; Functions
;;;
;;;     gtk_gesture_single_get_current_button
;;;     gtk_gesture_single_get_current_sequence
;;;
;;; Properties
;;;
;;;     button
;;;     exclusive
;;;     touch-only
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkGesture
;;;             ╰── GtkGestureSingle
;;;                 ├── GtkDragSource
;;;                 ├── GtkGestureClick
;;;                 ├── GtkGestureDrag
;;;                 ├── GtkGestureLongPress
;;;                 ├── GtkGestureStylus
;;;                 ╰── GtkGestureSwipe
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkGestureSingle
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkGestureSingle" gesture-single
  (:superclass gesture
   :export t
   :interfaces nil
   :type-initializer "gtk_gesture_single_get_type")
  ((button
    gesture-single-button
    "button" "guint" t t)
   (exclusive
    gesture-single-exclusive
    "exclusive" "gboolean" t t)
   (touch-only
    gesture-single-touch-only
    "touch-only" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'gesture-single 'type)
 "@version{2024-02-19}
  @begin{short}
    The @class{gtk:gesture-single} class is a @class{gtk:gesture} implementation
    optimized for single-touch and mouse gestures.
  @end{short}
  Under interaction, these gestures stick to the first interacting sequence,
  which is accessible through the @fun{gtk:gesture-single-current-sequence}
  function while the gesture is being interacted with.

  By default gestures react to both the button primary and touch events. The
  @fun{gtk:gesture-single-touch-only} function can be used to change the touch
  behavior. Callers may also specify a different mouse button number to
  interact with through the @fun{gtk:gesture-single-button} function, or react
  to any mouse button by setting 0. While the gesture is active, the button
  being currently pressed can be known through the
  @fun{gtk:gesture-single-current-button} function.
  @see-slot{gtk:gesture-single-button}
  @see-slot{gtk:gesture-single-exclusive}
  @see-slot{gtk:gesture-single-touch-only}
  @see-class{gtk:gesture}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:gesture-single-button ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "button" 'gesture-single) t)
 "The @code{button} property of type @code{:uint} (Read / Write) @br{}
  Mouse button number to listen to, or 0 to listen for any button. @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'gesture-single-button)
      "Accessor"
      (documentation 'gesture-single-button 'function)
 "@version{2025-07-27}
  @syntax{(gtk:gesture-single-button object) => button}
  @syntax{(setf (gtk:gesture-single-button object) button)}
  @argument[object]{a @class{gtk:gesture-single} object}
  @argument[button]{an unsigned integer for the button number to listen to,
    or 0 for any button}
  @begin{short}
    Accessor of the @slot[gtk:gesture-single]{button} slot of the
    @class{gtk:gesture-single} class.
  @end{short}
  The @fun{gtk:gesture-single-button} function returns the button number gesture
  listens for, or 0 to listen for any button. The
  @setf{gtk:gesture-single-button} function sets the button number. If non-0,
  every button press from a different button number will be ignored. Touch
  events implicitly match with button 1.
  @see-class{gtk:gesture-single}")

;;; --- gtk:gesture-single-exclusive -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "exclusive" 'gesture-single) t)
 "The @code{exclusive} property of type @code{:boolean} (Read / Write) @br{}
  Whether the gesture is exclusive. Exclusive gestures only listen to pointer
  and pointer emulated events. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'gesture-single-exclusive)
      "Accessor"
      (documentation 'gesture-single-exclusive 'function)
 "@version{2024-02-19}
  @syntax{(gtk:gesture-single-exclusive object) => exclusive}
  @syntax{(setf (gtk:gesture-single-exclusive object) exclusive)}
  @argument[object]{a @class{gtk:gesture-single} object}
  @argument[exclusive]{@em{true} to make the gesture exclusive}
  @begin{short}
    Accessor of the @slot[gtk:gesture-single]{exclusive} slot of the
    @class{gtk:gesture-single} class.
  @end{short}
  The @fun{gtk:gesture-single-exclusive} function gets whether the gesture is
  exclusive. The @setf{gtk:gesture-single-exclusive} function sets whether the
  gesture is exclusive. An exclusive gesture will only handle pointer and
  \"pointer emulated\" touch events, so at any given time, there is only one
  sequence able to interact with those.
  @see-class{gtk:gesture-single}")

;;; --- gtk:gesture-single-touch-only ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "touch-only" 'gesture-single) t)
 "The @code{touch-only} property of type @code{:boolean} (Read / Write) @br{}
  Whether the gesture handles only touch events. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'gesture-single-touch-only)
      "Accessor"
      (documentation 'gesture-single-touch-only 'function)
 "@version{2024-02-19}
  @syntax{(gtk:gesture-single-touch-only object) => touch-only}
  @syntax{(setf (gtk:gesture-single-touch-only object) touch-only)}
  @argument[object]{a @class{gtk:gesture-single} object}
  @argument[touch-only]{a boolean whether gesture handles only touch events}
  @begin{short}
    Accessor of the @slot[gtk:gesture-single]{touch-only} slot of the
    @class{gtk:gesture-single} class.
  @end{short}
  The @fun{gtk:gesture-single-touch-only} function returns @em{true} if the
  gesture is only triggered by touch events. The
  @setf{gtk:gesture-single-touch-only} function sets whether the gesture is
  only triggered by touch events. If the @arg{touch-only} argument is @em{true},
  the gesture will only handle events of type @code{:touch-begin},
  @code{:touch-update} or @code{:touch-end}. If @em{false}, mouse events will
  be handled too.
  @see-class{gtk:gesture-single}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_single_get_current_button
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_single_get_current_button"
               gesture-single-current-button) :uint
 #+liber-documentation
 "@version{2025-07-27}
  @argument[gesture]{a @class{gtk:gesture-single} object}
  @return{The unsigned integer for the current button number.}
  @begin{short}
    Returns the button number currently interacting with @arg{gesture}, or 0 if
    there is none.
  @end{short}
  @see-class{gtk:gesture-single}"
  (gesture (g:object gesture-single)))

(export 'gesture-single-current-button)

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_single_get_current_sequence
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_gesture_single_get_current_sequence"
               gesture-single-current-sequence) (g:boxed gdk:event-sequence)
 #+liber-documentation
 "@version{2024-02-19}
  @argument[gesture]{a @class{gtk:gesture-single} object}
  @return{The current @class{gdk:event-sequence} instance.}
  @begin{short}
    Returns the event sequence currently interacting with the gesture.
  @end{short}
  This is only meaningful if the @fun{gtk:gesture-is-active} function returns
  @em{true}.
  @see-class{gtk:gesture-single}
  @see-class{gdk:event-sequence}
  @see-function{gtk:gesture-is-active}"
  (gesture (g:object gesture-single)))

(export 'gesture-single-current-sequence)

;;; --- End of file gtk4.gesture-single.lisp -----------------------------------
