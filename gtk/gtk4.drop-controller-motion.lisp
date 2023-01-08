;;; ----------------------------------------------------------------------------
;;; gtk.drop-controller-motion.lisp
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
;;; GtkDropControllerMotion
;;;
;;;     Event controller for motion events during a drop
;;;
;;; Types and Values
;;;
;;;     GtkDropControllerMotion
;;;
;;; Functions
;;;
;;;     gtk_drop_controller_motion_new
;;;     gtk_drop_controller_motion_contains_pointer
;;;     gtk_drop_controller_motion_is_pointer
;;;     gtk_drop_controller_motion_get_drop ()
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkDropControllerMotion
;;;
;;; GtkDropControllerMotion is an event controller meant for tracking the
;;; pointer hovering over a widget during a drag and drop operation.
;;;
;;; It is modeled after GtkEventControllerMotion so if you have used that, this
;;; should feel really familiar.
;;;
;;; The drop controller is not able to accept drops, use GtkDropTarget for that
;;; purpose.
;;;
;;; See Also
;;;
;;;     GtkDropControllerMotion, GdkDrop, GtkDropTarget
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; gtk_drop_controller_motion_new ()
;;;
;;; GtkEventController *
;;; gtk_drop_controller_motion_new (void);
;;;
;;; Creates a new event controller that will handle pointer motion events during
;;; drag and drop.
;;;
;;; Returns :
;;;     a new GtkDropControllerMotion
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_controller_motion_contains_pointer ()
;;;
;;; gboolean
;;; gtk_drop_controller_motion_contains_pointer
;;;                                (GtkDropControllerMotion *self);
;;;
;;; Returns the value of the GtkDropControllerMotion:contains-pointer property.
;;;
;;; self :
;;;     a GtkDropControllerMotion
;;;
;;; Returns :
;;;     TRUE if a dragging pointer is within self or one of its children.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_controller_motion_is_pointer ()
;;;
;;; gboolean
;;; gtk_drop_controller_motion_is_pointer (GtkDropControllerMotion *self);
;;;
;;; Returns the value of the GtkDropControllerMotion:is-pointer property.
;;;
;;; self :
;;;     a GtkEventControllerKey
;;;
;;; Returns :
;;;     TRUE if a dragging pointer is within self but not one of its children
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_controller_motion_get_drop ()
;;;
;;; GdkDrop *
;;; gtk_drop_controller_motion_get_drop (GtkDropControllerMotion *self);
;;;
;;; Returns the value of the GtkDropControllerMotion:drop property.
;;;
;;; self :
;;;     a GtkDropControllerMotion
;;;
;;; Returns :
;;;     The GdkDrop currently happening within self or NULL if none.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.drop-controller-motion.lisp ----------------------------
