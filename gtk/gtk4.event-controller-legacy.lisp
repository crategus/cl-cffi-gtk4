;;; ----------------------------------------------------------------------------
;;; gtk.event-controller-legacy.lisp
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
;;; struct GtkEventControllerLegacy
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkEventControllerLegacy" event-controller-legacy
  (:superclass event-controller
   :export t
   :interfaces nil
   :type-initializer "gtk_event_controller_legacy_get_type")
  nil)

#+liber-documentation
(setf (documentation 'event-controller-legacy 'type)
 "@version{#2022-8-23}
  @begin{short}
    The @sym{gtk:event-controller-legacy} object is an event controller that
    gives you direct access to the event stream.
  @end{short}
  It should only be used as a last resort if none of the other event controllers
  or gestures do the job.
  @begin[Signal Details]{dictionary}
    @subheading{The \"event\" signal}
      @begin{pre}
lambda (controller event)    :run-last
      @end{pre}
      The signal is emitted for each GDK event delivered to @arg{controller}.
      @begin{table}
        @entry[controller]{The @sym{gtk:event-controller-legacy} object which
          received the signal.}
        @entry[event]{The @class{gdk:event} event which triggered the signal.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event and the emission of this signal. @em{False} to propagate
          the event further.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:event-controller-legacy-new}
  @see-class{gtk:event-controller}")

;;; ----------------------------------------------------------------------------
;;;gtk_event_controller_legacy_new ()
;;; ----------------------------------------------------------------------------

(defun event-controller-legacy-new ()
 #+liber-documentation
 "@version{#2022-8-23}
  @return{The newly created @class{gtk:event-controller-legacy} object.}
  @short{Creates a new legacy event controller.}
  @see-class{gtk:event-controller-legacy}"
  (make-instance 'event-controller-legacy))

(export 'event-controller-legacy-new)

;;; --- End of file gtk.event-controller-legacy.lisp ---------------------------
