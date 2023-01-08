;;; ----------------------------------------------------------------------------
;;; gtk.at-context.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
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
;;; GtkATContext
;;;
;;;     An object communicating to Assistive Technologies
;;;
;;; Types and Values
;;;
;;;     GtkATContext
;;;
;;; Accessors
;;;
;;;     gtk_at_context_get_accessible
;;;     gtk_at_context_get_accessible_role
;;;
;;; Functions
;;;
;;;     gtk_at_context_create
;;;
;;; Properties
;;;
;;;     accessible
;;;     accessible-role
;;;     display
;;;
;;; Signals
;;;
;;;     state-change
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkATContext
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkATContext
;;;
;;; GtkATContext is an abstract class provided by GTK to communicate to
;;; platform-specific assistive technologies API.
;;;
;;; Each platform supported by GTK implements a GtkATContext subclass, and is
;;; responsible for updating the accessible state in response to state changes
;;; in GtkAccessible.
;;;
;;; Signal Details
;;;
;;; The “state-change” signal
;;;
;;; void
;;; user_function (GtkATContext *self,
;;;                gpointer      user_data)
;;; Emitted when the attributes of the accessible for the GtkATContext instance
;;; change.
;;;
;;; self :
;;;     the GtkATContext
;;;
;;; user_data :
;;;     user data set when the signal handler was connected.
;;;
;;; Flags: Run First
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkATContext" at-context
  (:superclass g:object
   :export t
   :interfaces ()
   :type-initializer "gtk_at_context_get_type")
  ((accessible
    at-context-accessible
    "accessible" "GtkAccessible" t nil)
   (accessible-role
    at-context-accessible-role
    "accessible-role" "GtkAccessibleRole" t t)
   (display
    at-context-display
    "display" "GdkDidplay" t t)))

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- at-context-accessible ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accessible" 'at-context) t)
 "The @code{accessible} property of type @class{gtk:accessible}
  (Read / Write / Construct only) @br{}
  The accessible that created the AT context.")

;;; --- at-context-accessible-role -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accessible-role"
                                               'at-context) t)
 "The @code{accessible-role} property of type @symbol{gtk:accessible-role}
  (Read / Write / Construct) @br{}
  The accessible role used by the AT context. Depending on the given role,
  different states and properties can be set or retrieved. @br{}
  Default value: @code{:none}")

;;; --- at-context-display -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "display" 'at-context) t)
 "The @code{display} property of type @class{gdk:display} (Read / Write) @br{}
  The display for the AT context.")

;;; ----------------------------------------------------------------------------
;;; gtk_at_context_get_accessible ()
;;;
;;; GtkAccessible *
;;; gtk_at_context_get_accessible (GtkATContext *self);
;;;
;;; Retrieves the GtkAccessible using this context.
;;;
;;; self :
;;;     a GtkATContext
;;;
;;; Returns :
;;;     a GtkAccessible.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_at_context_get_accessible_role ()
;;;
;;; GtkAccessibleRole
;;; gtk_at_context_get_accessible_role (GtkATContext *self);
;;;
;;; Retrieves the accessible role of this context.
;;;
;;; self :
;;;     a GtkATContext
;;;
;;; Returns :
;;;     a GtkAccessibleRole
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_at_context_create ()
;;;
;;; GtkATContext *
;;; gtk_at_context_create (GtkAccessibleRole accessible_role,
;;;                        GtkAccessible *accessible,
;;;                        GdkDisplay *display);
;;;
;;; Creates a new GtkATContext instance for the given accessible role,
;;; accessible instance, and display connection.
;;;
;;; The GtkATContext implementation being instantiated will depend on the
;;; platform.
;;;
;;; accessible_role :
;;;     the accessible role used by the GtkATContext
;;;
;;; accessible :
;;;     the GtkAccessible implementation using the GtkATContext
;;;
;;; display :
;;;     the GdkDisplay used by the GtkATContext
;;;
;;; Returns :
;;;     the GtkATContext.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.at-context.lisp ----------------------------------------
