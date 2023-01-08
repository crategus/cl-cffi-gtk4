;;; ----------------------------------------------------------------------------
;;; gtk.event-controller-scroll.lisp
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
;;; GtkEventControllerScroll
;;;
;;;     Event controller for scroll events
;;;
;;; Types and Values
;;;
;;;     GtkEventControllerScroll
;;;     GtkEventControllerScrollFlags
;;;
;;; Accessors
;;;
;;;     gtk_event_controller_scroll_set_flags
;;;     gtk_event_controller_scroll_get_flags
;;;
;;; Functions
;;;
;;;     gtk_event_controller_scroll_new
;;;     gtk_event_controller_get_unit                      Since 4.8
;;;
;;; Properties
;;;
;;;     flags
;;;
;;; Signals
;;;
;;;     decelerate
;;;     scroll
;;;     scroll-begin
;;;     scroll-end
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkEventControllerScroll
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkEventControllerScrollFlags
;;; ----------------------------------------------------------------------------

(define-g-flags "GtkEventControllerScrollFlags" event-controller-scroll-flags
  (:export t
   :type-initializer "gtk_event_controller_scroll_flags_get_type")
  (:none 0)
  (:vertical 1)
  (:horizontal 2)
  (:discrete 4)
  (:kinetic 8)
  (:both-axes 16))

#+liber-documentation
(setf (liber:alias-for-symbol 'event-controller-scroll-flags)
      "GFlags"
      (liber:symbol-documentation 'event-controller-scroll-flags)
 "@version{#2022-8-23}
  @begin{short}
    Describes the behavior of a @class{gtk:event-controller-scroll} object.
  @end{short}
  @begin{pre}
(define-g-flags \"GtkEventControllerScrollFlags\" event-controller-scroll-flags
  (:export t
   :type-initializer \"gtk_event_controller_scroll_flags_get_type\")
  (:none 0)
  (:vertical 1)
  (:horizontal 2)
  (:discrete 4)
  (:kinetic 8)
  (:both-axes 16))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{Do not emit scroll.}
    @entry[:vertical]{Emit scroll with vertical deltas.}
    @entry[:horizontal]{Emit scroll with horizontal deltas.}
    @entry[:discrete]{Only emit deltas that are multiples of 1.}
    @entry[:kinetic]{Emit \"decelerate\" after continuous scroll finishes.}
    @entry[:both-axes]{Emit scroll on both axes.}
  @end{table}
  @see-class{gtk:event-controller-scroll}")

;;; ----------------------------------------------------------------------------
;;; struct GtkEventControllerScroll
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkEventControllerScroll" event-controller-scroll
  (:superclass event-controller
   :export t
   :interfaces nil
   :type-initializer "gtk_event_controller_scroll_get_type")
  ((flags
    event-controller-scroll-flags
    "flags" "GtkEventControllerScrollFlags" t t)))

#+liber-documentation
(setf (documentation 'event-controller-scroll 'type)
 "@version{#2022-8-23}
  @begin{short}
    The @sym{gtk:event-controller-scroll} object is an event controller is an
    event controller that handles scroll events.
  @end{short}
  It is capable of handling both discrete and continuous scroll events,
  abstracting them both with the \"scroll\" signal. Deltas in the discrete case
  are multiples of 1.

  In the case of continuous scroll events, the @sym{gtk:event-controller-scroll}
  object encloses all \"scroll\" events between two \"scroll-begin\" and
  \"scroll-end\" signals.

  The behavior of the event controller can be modified by the flags given at
  creation time, or modified at a later point through the
  @fun{gtk:event-controller-scroll-flags} function, e.g. because the scrolling
  conditions of the widget changed.

  The controller can be set up to emit motion for either/both vertical and
  horizontal scroll events through @code{:vertical}, @code{:horizontal} and
  @code{:both-axes}. If any axis is disabled, the respective \"scroll\" delta
  will be 0. Vertical scroll events will be translated to horizontal motion for
  the devices incapable of horizontal scrolling.

  The event controller can also be forced to emit discrete events on all devices
  through @code{:discrete}. This can be used to implement discrete actions
  triggered through scroll events, e.g. switching across combobox options.

  The @code{:kinetic} flag toggles the emission of the \"decelerate\" signal,
  emitted at the end of scrolling with two X/Y velocity arguments that are
  consistent with the motion that was received.
  @begin[Signal Details]{dictionary}
    @subheading{The \"decelerate\" signal}
      @begin{pre}
lambda (controller vel-x vel-y)    :run-first
      @end{pre}
      Emitted after scroll is finished if the @code{:kinetic} flag is set.
      @code{vel-x} and @code{vel-y} express the initial velocity that was
      imprinted by the scroll events. @code{vel-x} and @code{vel-y} are
      expressed in pixels/ms.
      @begin[code]{table}
        @entry[controller]{The @sym{gtk:event-controller-scroll} object which
          received the signal.}
        @entry[vel-x]{a double float with the x velocity}
        @entry[vel-y]{a double float with the y velocity}
      @end{table}
    @subheading{The \"scroll\" signal}
      @begin{pre}
lambda (controller dx dy)    :run-first
      @end{pre}
      Signals that the widget should scroll by the amount specified by
      @code{dx} and @code{dy}.
      @begin[code]{table}
        @entry[controller]{The @sym{gtk:event-controller-scroll} object which
          received the signal.}
        @entry[dx]{a double float with the x delta}
        @entry[dy]{a double float with the y delta}
      @end{table}
    @subheading{The \"scroll-begin\" signal}
      @begin{pre}
lambda (controller)    :run-first
      @end{pre}
      Signals that a new scrolling operation has begun. It will only be emitted
      on devices capable of it.
      @begin[code]{table}
        @entry[controller]{The @sym{gtk:event-controller-scroll} object which
          received the signal.}
      @end{table}
    @subheading{The \"scroll-end\" signal}
      @begin{pre}
lambda (controller)    :run-first
      @end{pre}
      Signals that a new scrolling operation has finished. It will only be
      emitted on devices capable of it.
      @begin[code]{table}
        @entry[controller]{The @sym{gtk:event-controller-scroll} object which
          received the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:event-controller-scroll-flags}
  @see-constructor{gtk:event-controller-scroll-new}
  @see-class{gtk:event-controller}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- event-controller-scroll-flags --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "flags"
                                               'event-controller-scroll) t)
 "The @code{flags} property of type
  @symbol{gtk:event-controller-scroll-flags} (Read / Write) @br{}
  The flags affecting event controller behavior.")

#+liber-documentation
(setf (liber:alias-for-function 'event-controller-scroll-flags)
      "Accessor"
      (documentation 'event-controller-scroll-flags 'function)
 "@version{#2022-8-23}
  @syntax[]{(gtk:event-controller-scroll-flags object) => flags)}
  @syntax[]{(setf (gtk:event-controller-scroll-flags object) flags)}
  @argument[object]{a @class{gtk:event-controller-scroll} object}
  @argument[flags]{a @symbol{gtk:event-controller-scroll-flags} value}
  @begin{short}
    Accessor of the @slot[gtk:event-controller-scroll]{flags} slot of the
    @class{gtk:event-controller-scroll} class.
  @end{short}
  The @sym{gtk:event-controller-scroll-flags} function gets the flags
  conditioning the scroll controller behavior. The
  @sym{(setf gtk:event-controller-scroll-flags)} function sets the flags.
  @see-class{gtk:event-controller-scroll}
  @see-symbol{gtk:event-controller-scroll-flags}")

;;; ----------------------------------------------------------------------------
;;; gtk_event_controller_scroll_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline event-controller-scroll-new))

(defun event-controller-scroll-new (flags)
 #+liber-documentation
 "@version{#2022-8-23}
  @argument[flags]{a @symbol{gtk:event-controller-scroll-flags} value
    affecting the controller behaviour}
  @return{The new @class{gtk:event-controller-scroll} object.}
  @begin{short}
    Creates a new event controller that will handle scroll events.
  @end{short}
  @see-class{gtk:event-controller-scroll}
  @see-symbol{gtk:event-controller-scroll-flags}"
  (make-instance 'event-controller-scroll
                 :flags flags))

(export 'event-controller-scroll-new)

;;; ----------------------------------------------------------------------------
;;; gtk_event_controller_scroll_get_unit ()
;;;
;;; GdkScrollUnit
;;; gtk_event_controller_scroll_get_unit (GtkEventControllerScroll* scroll)
;;;
;;; Gets the scroll unit of the last GtkEventControllerScroll::scroll signal
;;; received.
;;;
;;; Always returns GDK_SCROLL_UNIT_WHEEL if the
;;; GTK_EVENT_CONTROLLER_SCROLL_DISCRETE flag is set.
;;;
;;; Available since: 4.8
;;;
;;; scroll :
;;;     a GtkEventControllerScroll object
;;;
;;; Returns :
;;;     The scroll unit.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.event-controller-scroll.lisp ---------------------------
