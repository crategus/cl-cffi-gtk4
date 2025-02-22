;;; ----------------------------------------------------------------------------
;;; gtk4.gesture-click.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2025 Dieter Kaiser
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

(gobject:define-gobject "GtkGestureClick" gesture-click
  (:superclass gesture-single
   :export t
   :interfaces ()
   :type-initializer "gtk_gesture_click_get_type")
  nil)

#+liber-documentation
(setf (documentation 'gesture-click 'type)
 "@version{2025-2-22}
  @begin{short}
    The @class{gtk:gesture-click} class is a @class{gtk:gesture} implementation
    for clicks.
  @end{short}
  It is able to recognize multiple clicks on a nearby zone, which can be
  listened for through the @code{\"pressed\"} signal. Whenever time or distance
  between clicks exceed the GTK defaults, the @code{\"stopped\"} signal is
  emitted, and the click counter is reset.
  @begin[Signal Details]{dictionary}
    @subheading{The \"pressed\" signal}
      @begin{pre}
lambda (gesture n x y)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[gesture]{The @class{gtk:gesture-click} object which received the
          signal.}
        @entry[n]{The integer of how many touch/button press happened with this
          one.}
        @entry[x]{The double float with the x coordinate, in widget allocation
          coordinates.}
        @entry[y]{The double float with the y coordinate, in widget allocation
          coordinates.}
      @end{table}
      The signal is emitted whenever a button or touch press happens.
    @subheading{The \"released\" signal}
      @begin{pre}
lambda (gesture n x y)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[gesture]{The @class{gtk:gesture-click} object which received the
          signal.}
        @entry[n]{The integer with the number of presses that is paired with
          this release.}
        @entry[x]{The double float with the x coordinate, in widget allocation
          coordinates.}
        @entry[y]{The double float with the y coordinate, in widget allocation
          coordinates.}
      @end{table}
      The signal is emitted when a button or touch is released. The @arg{n}
      argument will report the number of press that is paired to this event,
      note that the @code{\"stopped\"} signal may have been emitted between the
      press and its release, @arg{n} will only start over at the next press.
    @subheading{The \"stopped\" signal}
      @begin{pre}
lambda (gesture)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[gesture]{The @class{gtk:gesture-click} object which received the
          signal.}
      @end{table}
      The signal is emitted whenever any time/distance threshold has been
      exceeded.
    @subheading{The \"unpaired-release\" signal}
      @begin{pre}
lambda (gesture x y button sequence)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[gesture]{The @class{gtk:gesture-click} object which received the
          signal.}
        @entry[x]{The double float with the x coordinate of the event.}
        @entry[y]{The double float with the y coordinate of the event.}
        @entry[button]{The unsigned integer with the button being released.}
        @entry[sequence]{The @class{gdk:event-sequence} instance being
          released.}
      @end{table}
      The signal is emitted whenever the gesture receives a release event that
      had no previous corresponding press. Due to implicit grabs, this can only
      happen on situations where input is grabbed elsewhere mid-press or the
      pressed widget voluntarily relinquishes its implicit grab.
  @end{dictionary}
  @see-constructor{gtk:gesture-click-new}
  @see-class{gtk:gesture}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_click_new
;;; ----------------------------------------------------------------------------

(declaim (inline gesture-click-new))

(defun gesture-click-new ()
 #+liber-documentation
 "@version{2025-2-22}
  @return{The newly created @class{gtk:gesture-click} object.}
  @begin{short}
    Returns a newly created gesture that recognizes single and multiple presses.
  @end{short}
  @see-class{gtk:gesture-click}"
  (make-instance 'gesture-click))

(export 'gesture-click-new)

;;; ---- End of file gtk4.gesture-click.lisp -----------------------------------
