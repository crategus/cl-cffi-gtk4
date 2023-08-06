;;; ----------------------------------------------------------------------------
;;; gtk4.drop-controller-motion.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 Dieter Kaiser
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
;;;     gtk_drop_controller_motion_get_drop
;;;
;;; Properties
;;;
;;;     contains-pointer
;;;     drop
;;;     is-pointer
;;;
;;; Signals
;;;
;;;     enter
;;;     leave
;;;     move
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkDropControllerMotion
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

;;; --- End of file gtk4.drop-controller-motion.lisp ---------------------------
