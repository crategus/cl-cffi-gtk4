;;; ----------------------------------------------------------------------------
;;; gtk.event-controller-motion.lisp
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
;;; struct GtkEventControllerMotion
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkEventControllerMotion" event-controller-motion
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
 "@version{#2022-8-23}
  @begin{short}
    The @sym{gtk:event-controller-motion} object is an event controller
    tracking the pointer position.
  @end{short}
  The event controller offers \"enter\" and \"leave\" signals, as well as
  @code{is-pointer} and @code{contains-pointer} properties which are updated to
  reflect changes in the pointer position as it moves over the widget.
  @begin[Signal Details]{dictionary}
    @subheading{The \"enter\" signal}
      @begin{pre}
lambda (controller x y)    :run-first
      @end{pre}
      Signals that the pointer has entered the widget.
      @begin[code]{table}
        @entry[controller]{The @sym{gtk:event-controller-motion} object which
          received the signal.}
        @entry[x]{a double float with the x coordinate}
        @entry[y]{a double float with the y coordinate}
      @end{table}
    @subheading{The \"leave\" signal}
      @begin{pre}
lambda (controller)    :run-first
      @end{pre}
      Signals that the pointer has left the widget.
      @begin[code]{table}
        @entry[controller]{The @sym{gtk:event-controller-motion} object which
          received the signal.}
      @end{table}
    @subheading{The \"motion\" signal}
      @begin{pre}
lambda (controller x y)    :run-first
      @end{pre}
      Emitted when the pointer moves inside the widget.
      @begin[code]{table}
        @entry[controller]{The @sym{gtk:event-controller-motion} object which
          received the signal.}
        @entry[x]{a double float with the x coordinate}
        @entry[y]{a double float with the y coordinate}
      @end{table}
  @end{dictionary}
  @see-class{gtk:event-controller}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- event-controller-motion-contains-pointer ---------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "contains-pointer"
                                               'event-controller-motion) t)
 "The @code{contains-pointer} property of type @code{:boolean} (Read) @br{}
  @em{True} if the pointer is in the controllers widget or a descendant. When
  handling crossing events, this property is updated before the \"enter\"
  signal but after the \"leave\" signal is emitted. @br{}
  Default value: @code{false}")

#+liber-documentation
(setf (liber:alias-for-function 'event-controller-motion-contains-pointer)
      "Accessor"
      (documentation 'event-controller-motion-contains-pointer 'function)
 "@version{#2022-8-23}
  @syntax[]{(gtk:event-controller-motion-contains-pointer object) => contains)}
  @argument[object]{a @class{gtk:event-controller-motion} object}
  @argument[contains]{a boolean whether the pointer is within @arg{object}}
  @begin{short}
    Accessor of the @slot[gtk:event-controller-motion]{contains-pointer} slot
    of the @class{gtk:event-controller-motion} class.
  @end{short}
  The @sym{gtk:event-controller-motion-contains-pointer} function returns
  @em{true} if the pointer is within the controllers widget or one of its
  children.
  @see-class{gtk:event-controller-motion}")

;;; --- event-controller-motion-is-pointer ---------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "is-pointer"
                                               'event-controller-motion) t)
 "The @code{is-pointer} property of type @code{:boolean} (Read) @br{}
  @em{True} if the pointer is contained in the controllers widget, as opposed
  to in a descendent widget. When handling crossing events, this property is
  updated before the \"enter\" signal, but after the \"leave\" signal is
  emitted. @br{}
  Default value: @code{false}")

#+liber-documentation
(setf (liber:alias-for-function 'event-controller-motion-is-pointer)
      "Accessor"
      (documentation 'event-controller-motion-is-pointer 'function)
 "@version{#2022-8-23}
  @syntax[]{(gtk:event-controller-motion-is-pointer object) => is-pointer)}
  @argument[object]{a @class{gtk:event-controller-motion} object}
  @argument[is-pointer]{a boolean whether the pointer is within @arg{object}}
  @begin{short}
    Accessor of the @slot[gtk:event-controller-motion]{is-pointer} slot of the
    @class{gtk:event-controller-motion} class.
  @end{short}
  The @sym{gtk:event-controller-motion-is-pointer} function returns @em{true}
  if the pointer is within the controllers widget but not one of its children.
  @see-class{gtk:event-controller-motion}")

;;; ----------------------------------------------------------------------------
;;; gtk_event_controller_motion_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline event-controller-motion-new))

(defun event-controller-motion-new ()
 #+liber-documentation
 "@version{#2022-8-23}
  @return{The new @class{gtk:event-controller-motion} object.}
  @begin{short}
    Creates a new event controller that will handle motion events.
  @end{short}
  @see-class{gtk:event-controller-motion}"
  (make-instance 'event-controller-motion))

(export 'event-controller-motion-new)

;;; --- End of file gtk.event-controller-motion.lisp ---------------------------
