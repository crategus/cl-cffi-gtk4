;;; ----------------------------------------------------------------------------
;;; gtk4.gesture-click.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
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
 "@version{2025-07-24}
  @begin{short}
    The @class{gtk:gesture-click} class is a @class{gtk:gesture} implementation
    for clicks.
  @end{short}
  It is able to recognize multiple clicks on a nearby zone, which can be
  listened for through the @sig[gtk:gesture-click]{pressed} signal. Whenever
  time or distance between clicks exceed the GTK defaults, the
  @sig[gtk:gesture-click]{stopped} signal is emitted, and the click counter is
  reset.
  @begin[Signal Details]{dictionary}
    @begin[gesture-click::pressed]{signal}
      @begin{pre}
lambda (gesture n x y)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[gesture]{The @class{gtk:gesture-click} object that received the
          signal.}
        @entry[n]{The integer of how many touch/button press happened with this
          one.}
        @entry[x]{The double float for the x coordinate, in widget allocation
          coordinates.}
        @entry[y]{The double float for the y coordinate, in widget allocation
          coordinates.}
      @end{simple-table}
      The signal is emitted whenever a button or touch press happens.
    @end{signal}
    @begin[gesture-click::released]{signal}
      @begin{pre}
lambda (gesture n x y)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[gesture]{The @class{gtk:gesture-click} object that received the
          signal.}
        @entry[n]{The integer for the number of presses that is paired with
          this release.}
        @entry[x]{The double float for the x coordinate, in widget allocation
          coordinates.}
        @entry[y]{The double float for the y coordinate, in widget allocation
          coordinates.}
      @end{simple-table}
      The signal is emitted when a button or touch is released. The @arg{n}
      argument will report the number of press that is paired to this event,
      note that the @sig[gtk:gesture-click]{stopped} signal may have been
      emitted between the press and its release, @arg{n} will only start over
      at the next press.
    @end{signal}
    @begin[gesture-click::stopped]{signal}
      @begin{pre}
lambda (gesture)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[gesture]{The @class{gtk:gesture-click} object that received the
          signal.}
      @end{simple-table}
      The signal is emitted whenever any time/distance threshold has been
      exceeded.
    @end{signal}
    @begin[gesture-click::unpaired-release]{signal}
      @begin{pre}
lambda (gesture x y button sequence)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[gesture]{The @class{gtk:gesture-click} object that received the
          signal.}
        @entry[x]{The double float for the x coordinate of the event.}
        @entry[y]{The double float for the y coordinate of the event.}
        @entry[button]{The unsigned integer for the button being released.}
        @entry[sequence]{The @class{gdk:event-sequence} instance being
          released.}
      @end{simple-table}
      The signal is emitted whenever the gesture receives a release event that
      had no previous corresponding press. Due to implicit grabs, this can only
      happen on situations where input is grabbed elsewhere mid-press or the
      pressed widget voluntarily relinquishes its implicit grab.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:gesture-click-new}
  @see-class{gtk:gesture}")

;;; ----------------------------------------------------------------------------
;;; gtk_gesture_click_new
;;; ----------------------------------------------------------------------------

(declaim (inline gesture-click-new))

(defun gesture-click-new ()
 #+liber-documentation
 "@version{2025-02-22}
  @return{The newly created @class{gtk:gesture-click} object.}
  @begin{short}
    Returns a newly created gesture that recognizes single and multiple presses.
  @end{short}
  @see-class{gtk:gesture-click}"
  (make-instance 'gesture-click))

(export 'gesture-click-new)

;;; ---- End of file gtk4.gesture-click.lisp -----------------------------------
