;;; ----------------------------------------------------------------------------
;;; gtk.gesture-click.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 Dieter Kaiser
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
;;; GtkGestureClick
;;;
;;;     Multipress gesture
;;;
;;; Types and Values
;;;
;;;     GtkGestureClick
;;;
;;; Functions
;;;
;;;     gtk_gesture_click_new
;;;
;;; Signals
;;;
;;;     pressed
;;;     released
;;;     stopped
;;;     unpaired-release
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkGesture
;;;             ╰── GtkGestureSingle
;;;                 ╰── GtkGestureClick
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkGestureClick
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkGestureClick" gesture-click
  (:superclass gesture-single
   :export t
   :interfaces ()
   :type-initializer "gtk_gesture_click_get_type")
  nil)

#+liber-documentation
(setf (documentation 'gesture-click 'type)
 "@version{#2022-8-23}
  @begin{short}
    The @sym{gtk:gesture-click} object is a @class{gtk:gesture} implementation
    for clicks.
  @end{short}
  It is able to recognize multiple clicks on a nearby zone, which can be
  listened for through the \"pressed\" signal. Whenever time or distance between
  clicks exceed the GTK defaults, the \"stopped\" signal is emitted, and the
  click counter is reset.
  @begin[Signal Details]{dictionary}
    @subheading{The \"pressed\" signal}
      @begin{pre}
lambda (gesture n x y)    :run-last
      @end{pre}
      The signal is emitted whenever a button or touch press happens.
      @begin[code]{table}
        @entry[gesture]{The @sym{gtk:gesture-click} object which received the
          signal.}
        @entry[n]{An integer with how many touch/button press happened with
          this one.}
        @entry[x]{A double float with the X coordinate, in widget allocation
          coordinates.}
        @entry[y]{A double float with the Y coordinate, in widget allocation
          coordinates.}
      @end{table}
    @subheading{The \"released\" signal}
      @begin{pre}
lambda (gesture n x y)    :run-last
      @end{pre}
      The signal is emitted when a button or touch is released. The @arg{n}
      argument will report the number of press that is paired to this event,
      note that the \"stopped\" signal may have been emitted between the press
      and its release, @arg{n} will only start over at the next press.
      @begin[code]{table}
        @entry[gesture]{The @sym{gtk:gesture-click} object which received the
          signal.}
        @entry[n]{An integer with the number of presses that is paired with
          this release.}
        @entry[x]{A double float with the X coordinate, in widget allocation
          coordinates.}
        @entry[y]{A double float with the Y coordinate, in widget allocation
          coordinates.}
      @end{table}
    @subheading{The \"stopped\" signal}
      @begin{pre}
lambda (gesture)    :run-last
      @end{pre}
      The signal is emitted whenever any time/distance threshold has been
      exceeded.
      @begin[code]{table}
        @entry[gesture]{The @sym{gtk:gesture-click} object which received the
          signal.}
      @end{table}
    @subheading{The \"unpaired-release\" signal}
      @begin{pre}
lambda (gesture x y button sequence)    :run-last
      @end{pre}
      The signal is emitted whenever the gesture receives a release event that
      had no previous corresponding press. Due to implicit grabs, this can only
      happen on situations where input is grabbed elsewhere mid-press or the
      pressed widget voluntarily relinquishes its implicit grab.
      @begin[code]{table}
        @entry[gesture]{The @sym{gtk:gesture-click} object which received the
          signal.}
        @entry[x]{A double float with the X coordinate of the event.}
        @entry[y]{A double float with the Y coordinate of the event.}
        @entry[button]{An unsigned integer with the button being released.}
        @entry[sequence]{A @class{gdk:event-sequence} instance being released.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:gesture-click-new}
  @see-class{gtk:gesture}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_click_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline gesture-click-new))

(defun gesture-click-new ()
 #+liber-documentation
 "@version{#2022-8-23}
  @return{A newly created @class{gtk:gesture-click} object.}
  @begin{short}
    Returns a newly created gesture that recognizes single and multiple presses.
  @end{short}
  @see-class{gtk:gesture-click}"
  (make-instance 'gesture-click))

(export 'gesture-click-new)

;;; ---- End of file gtk.gesture-click.lisp ------------------------------------
