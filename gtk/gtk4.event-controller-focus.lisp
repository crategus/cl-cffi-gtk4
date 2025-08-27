;;; ----------------------------------------------------------------------------
;;; gtk4.event-controller-focus.lisp
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
;;; GtkEventControllerFocus
;;;
;;;     Event controller for focus
;;;
;;; Types and Values
;;;
;;;     GtkEventControllerFocus
;;;
;;; Accessors
;;;
;;;     gtk_event_controller_focus_contains_focus
;;;     gtk_event_controller_focus_is_focus
;;;
;;; Functions
;;;
;;;     gtk_event_controller_focus_new
;;;
;;; Properties
;;;
;;;     contains-focus
;;;     is-focus
;;;
;;; Signals
;;;
;;;     enter
;;;     leave
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkEventControllerFocus
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkEventControllerFocus
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkEventControllerFocus" event-controller-focus
  (:superclass event-controller
   :export t
   :interfaces nil
   :type-initializer "gtk_event_controller_focus_get_type")
  ((contains-focus
    event-controller-focus-contains-focus
    "contains-focus" "gboolean" t nil)
   (is-focus
    event-controller-focus-is-focus
    "is-focus" "gboolean" t nil)))

#+liber-documentation
(setf (documentation 'event-controller-focus 'type)
 "@version{2025-07-23}
  @begin{short}
    The @class{gtk:event-controller-focus} class is an event controller to keep
    track of keyboard focus.
  @end{short}

  The event controller offers the @sig[gtk:event-controller-focus]{enter} and
  @sig[gtk:event-controller-focus]{leave} signals, as well as the
  @slot[gtk:event-controller-focus]{is-focus} and
  @slot[gtk:event-controller-focus]{contains-focus} properties which are updated
  to reflect focus changes inside the widget hierarchy that is rooted at the
  controllers widget.
  @begin[Signal Details]{dictionary}
    @begin[event-controller-focus::enter]{signal}
      @begin{pre}
lambda (controller)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[controller]{The @class{gtk:event-controller-focus} object that
          received the signal.}
      @end{simple-table}
      The signal is emitted whenever the focus enters into the widget or one of
      its descendents. Note that this means you may not get an
      @sig[gtk:event-controller-focus]{enter} signal even though the widget
      becomes the focus location, in certain cases, such as when the focus moves
      from a descendent of the widget to the widget itself. If you are
      interested in these cases, you can monitor the
      @slot[gtk:event-controller-focus]{is-focus} property for changes.
    @end{signal}
    @begin[event-controller-focus::leave]{signal}
      @begin{pre}
lambda (controller)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[controller]{The @class{gtk:event-controller-focus} object that
          received the signal.}
      @end{simple-table}
      The signal is emitted whenever the focus leaves the widget hierarchy that
      is rooted at the widget that the controller is attached to. Note that this
      means you may not get a @sig[gtk:event-controller-focus]{leave} signal
      even though the focus moves away from the widget, in certain cases, such
      as when the focus moves from the widget to a descendent. If you are
      interested in these cases, you can monitor the
      @slot[gtk:event-controller-focus]{is-focus} property for changes.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:event-controller-focus-new}
  @see-slot{gtk:event-controller-focus-contains-focus}
  @see-slot{gtk:event-controller-focus-is-focus}
  @see-class{gtk:event-controller}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:event-controller-focus-contains-focus ------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "contains-focus"
                                               'event-controller-focus) t)
 "The @code{contains-focus} property of type @code{:boolean} (Read) @br{}
  @em{True} if focus is contained in the controllers widget. See the
  @slot[gtk:event-controller-focus]{is-focus} property for whether the focus is
  in the widget itself or inside a descendent. When handling focus events, this
  property is updated before the @sig[gtk:event-controller-focus]{enter} or
  @sig[gtk:event-controller-focus]{leave} signals are emitted. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'event-controller-focus-contains-focus)
      "Accessor"
      (documentation 'event-controller-focus-contains-focus 'function)
 "@version{2025-08-17}
  @syntax{(gtk:event-controller-focus-contains-focus object) => contains}
  @argument[object]{a @class{gtk:event-controller-focus} object}
  @argument[contains]{a boolean whether focus is within @arg{object}}
  @begin{short}
    The accessor for the @slot[gtk:event-controller-focus]{contains-focus} slot
    of the @class{gtk:event-controller-focus} class returns @em{true} if focus
    is within the controllers widget or one of its children.
  @end{short}
  @see-class{gtk:event-controller-focus}")

;;; --- gtk:event-controller-focus-is-focus ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "is-focus"
                                               'event-controller-focus) t)
 "The @code{is-focus} property of type @code{:boolean} (Read) @br{}
  @em{True} if focus is contained in the controllers widget, as opposed to in a
  descendent widget. When handling focus events, this property is updated before
  the @sig[gtk:event-controller-focus]{enter} or
  @sig[gtk:event-controller-focus]{leave} signals are emitted. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'event-controller-focus-is-focus)
      "Accessor"
      (documentation 'event-controller-focus-is-focus 'function)
 "@version{2025-08-17}
  @syntax{(gtk:event-controller-focus-is-focus object) => is-focus}
  @argument[object]{a @class{gtk:event-controller-focus} object}
  @argument[is-focus]{a boolean whether focus is within the controllers widget,
    but not one of its children}
  @begin{short}
    The accessor for the @slot[gtk:event-controller-focus]{is-focus} slot of the
    @class{gtk:event-controller-focus} class returns @em{true} if focus is
    within the controllers widget, but not one of its children.
  @end{short}
  @see-class{gtk:event-controller-focus}")

;;; ----------------------------------------------------------------------------
;;; gtk_event_controller_focus_new
;;; ----------------------------------------------------------------------------

(declaim (inline event-controller-focus-new))

(defun event-controller-focus-new ()
 #+liber-documentation
 "@version{2024-07-26}
  @return{The new @class{gtk:event-controller-focus} object.}
  @short{Creates a new event controller that will handle focus events.}
  @see-class{gtk:event-controller-focus}"
  (make-instance 'event-controller-focus))

(export 'event-controller-focus-new)

;;; --- End of file gtk4.event-controller-focus.lisp ---------------------------
