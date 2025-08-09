;;; ----------------------------------------------------------------------------
;;; gtk4.event-controller-motion.lisp
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
;;; GtkEventControllerMotion
;;;
;;;     Event controller for motion events
;;;
;;; Types and Values
;;;
;;;     GtkEventControllerMotion
;;;
;;; Accessors
;;;
;;;     gtk_event_controller_motion_contains_pointer
;;;     gtk_event_controller_motion_is_pointer
;;;
;;; Functions
;;;
;;;     gtk_event_controller_motion_new
;;;
;;; Properties
;;;
;;;     contains-pointer
;;;     is-pointer
;;;
;;; Signals
;;;
;;;     enter
;;;     leave
;;;     motion
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkEventControllerMotion
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkEventControllerMotion
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkEventControllerMotion" event-controller-motion
  (:superclass event-controller
   :export t
   :interfaces nil
   :type-initializer "gtk_event_controller_motion_get_type")
  ((contains-pointer
    event-controller-motion-contains-pointer
    "contains-pointer" "gboolean" t nil)
   (is-pointer
    event-controller-motion-is-pointer
    "is-pointer" "gboolean" t nil)))

#+liber-documentation
(setf (documentation 'event-controller-motion 'type)
 "@version{2025-07-24}
  @begin{short}
    The @class{gtk:event-controller-motion} object is an event controller
    tracking the pointer position.
  @end{short}
  The event controller offers @sig[gtk:event-controller-motion]{enter} and
  @sig[gtk:event-controller-motion]{leave} signals, as well as
  @slot[gtk:event-controller-motion]{is-pointer} and
  @slot[gtk:event-controller-motion]{contains-pointer} properties which are
  updated to reflect changes in the pointer position as it moves over the
  widget.
  @begin[Signal Details]{dictionary}
    @begin[event-controller-motion::enter]{signal}
      @begin{pre}
lambda (controller x y)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[controller]{The @class{gtk:event-controller-motion} object which
          received the signal.}
        @entry[x]{The double float for the x coordinate.}
        @entry[y]{The double float for the y coordinate.}
      @end{simple-table}
      Signals that the pointer has entered the widget.
    @end{signal}
    @begin[event-controller-motion::leave]{signal}
      @begin{pre}
lambda (controller)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[controller]{The @class{gtk:event-controller-motion} object that
          received the signal.}
      @end{simple-table}
      Signals that the pointer has left the widget.
    @end{signal}
    @begin[event-controller-motion::motion]{signal}
      @begin{pre}
lambda (controller x y)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[controller]{The @class{gtk:event-controller-motion} object which
          received the signal.}
        @entry[x]{The double float for the x coordinate.}
        @entry[y]{The double float for the y coordinate.}
      @end{simple-table}
      Emitted when the pointer moves inside the widget.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:event-controller-motion-new}
  @see-class{gtk:event-controller}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:event-controller-motion-contains-pointer ---------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "contains-pointer"
                                               'event-controller-motion) t)
 "The @code{contains-pointer} property of type @code{:boolean} (Read) @br{}
  @em{True} if the pointer is in the controllers widget or a descendant. When
  handling crossing events, this property is updated before the
  @sig[gtk:event-controller-motion]{enter} signal but after the
  @sig[gtk:event-controller-motion]{leave} signal is emitted. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'event-controller-motion-contains-pointer)
      "Accessor"
      (documentation 'event-controller-motion-contains-pointer 'function)
 "@version{2024-07-26}
  @syntax{(gtk:event-controller-motion-contains-pointer object) => contains}
  @argument[object]{a @class{gtk:event-controller-motion} object}
  @argument[contains]{a boolean whether the pointer is within @arg{object}}
  @begin{short}
    Accessor of the @slot[gtk:event-controller-motion]{contains-pointer} slot
    of the @class{gtk:event-controller-motion} class.
  @end{short}
  The @fun{gtk:event-controller-motion-contains-pointer} function returns
  @em{true} if the pointer is within the controllers widget or one of its
  children.
  @see-class{gtk:event-controller-motion}")

;;; --- gtk:event-controller-motion-is-pointer ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "is-pointer"
                                               'event-controller-motion) t)
 "The @code{is-pointer} property of type @code{:boolean} (Read) @br{}
  @em{True} if the pointer is contained in the controllers widget, as opposed
  to in a descendent widget. When handling crossing events, this property is
  updated before the @sig[gtk:event-controller-motion]{enter} signal, but after
  the @sig[gtk:event-controller-motion]{leave} signal is emitted. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'event-controller-motion-is-pointer)
      "Accessor"
      (documentation 'event-controller-motion-is-pointer 'function)
 "@version{2024-07-26}
  @syntax{(gtk:event-controller-motion-is-pointer object) => is-pointer}
  @argument[object]{a @class{gtk:event-controller-motion} object}
  @argument[is-pointer]{a boolean whether the pointer is within @arg{object}}
  @begin{short}
    Accessor of the @slot[gtk:event-controller-motion]{is-pointer} slot of the
    @class{gtk:event-controller-motion} class.
  @end{short}
  The @fun{gtk:event-controller-motion-is-pointer} function returns @em{true}
  if the pointer is within the controllers widget but not one of its children.
  @see-class{gtk:event-controller-motion}")

;;; ----------------------------------------------------------------------------
;;; gtk_event_controller_motion_new
;;; ----------------------------------------------------------------------------

(declaim (inline event-controller-motion-new))

(defun event-controller-motion-new ()
 #+liber-documentation
 "@version{2024-07-26}
  @return{The new @class{gtk:event-controller-motion} object.}
  @short{Creates a new event controller that will handle motion events.}
  @see-class{gtk:event-controller-motion}"
  (make-instance 'event-controller-motion))

(export 'event-controller-motion-new)

;;; --- End of file gtk4.event-controller-motion.lisp --------------------------
