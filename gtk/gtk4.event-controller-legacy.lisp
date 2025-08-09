;;; ----------------------------------------------------------------------------
;;; gtk4.event-controller-legacy.lisp
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
;;; GtkEventControllerLegacy
;;;
;;;     Event controller for miscellaneous events
;;;
;;; Types and Values
;;;
;;;     GtkEventControllerLegacy
;;;
;;; Functions
;;;
;;;     gtk_event_controller_legacy_new
;;;
;;; Signals
;;;
;;;     event
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkEventControllerLegacy
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkEventControllerLegacy
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkEventControllerLegacy" event-controller-legacy
  (:superclass event-controller
   :export t
   :interfaces nil
   :type-initializer "gtk_event_controller_legacy_get_type")
  nil)

#+liber-documentation
(setf (documentation 'event-controller-legacy 'type)
 "@version{2025-07-20}
  @begin{short}
    The @class{gtk:event-controller-legacy} object is an event controller that
    gives you direct access to the event stream.
  @end{short}
  It should only be used as a last resort if none of the other event controllers
  or gestures do the job.
  @begin[Signal Details]{dictionary}
    @begin[event-controller-legacy::event]{signal}
      @begin{pre}
lambda (controller event)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[controller]{The @class{gtk:event-controller-legacy} object that
          received the signal.}
        @entry[event]{The @class{gdk:event} instance that triggered the
          signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event and the emission of this signal. @em{False} to propagate
          the event further.}
      @end{simple-table}
      The signal is emitted for each GDK event delivered to @arg{controller}.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:event-controller-legacy-new}
  @see-class{gtk:event-controller}")

;;; ----------------------------------------------------------------------------
;;;gtk_event_controller_legacy_new
;;; ----------------------------------------------------------------------------

(defun event-controller-legacy-new ()
 #+liber-documentation
 "@version{2024-07-26}
  @return{The newly created @class{gtk:event-controller-legacy} object.}
  @short{Creates a new legacy event controller.}
  @see-class{gtk:event-controller-legacy}"
  (make-instance 'event-controller-legacy))

(export 'event-controller-legacy-new)

;;; --- End of file gtk4.event-controller-legacy.lisp --------------------------
