;;; ----------------------------------------------------------------------------
;;; gtk.drag-icon.lisp
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
;;; GtkDragIcon
;;;
;;;     A toplevel to use as drag icon
;;;
;;; Types and Values
;;;
;;;     GtkDragIcon
;;;
;;; Accessors
;;;
;;;     gtk_drag_icon_set_child
;;;     gtk_drag_icon_get_child
;;;
;;; Functions
;;;
;;;     gtk_drag_icon_get_for_drag
;;;     gtk_drag_icon_set_from_paintable
;;;     gtk_drag_icon_create_widget_for_value
;;;
;;; Properties
;;;
;;;     child
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkDragIcon
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkNative
;;;     GtkRoot
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkDragIcon
;;;
;;; GtkDragIcon is a GtkRoot implementation with the sole purpose to serve as a
;;; drag icon during DND operations. A drag icon moves with the pointer during
;;; a drag operation and is destroyed when the drag ends.
;;;
;;; To set up a drag icon and associate it with an ongoing drag operation, use
;;; gtk_drag_icon_get_for_drag() to get the icon for a drag. You can then use
;;; it like any other widget and use gtk_drag_icon_set_child() to set whatever
;;; widget should be used for the drag icon.
;;;
;;; Keep in mind that drag icons do not allow user input.
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “child” property
;;;
;;;   “child”                    GtkWidget *
;;;
;;; The widget to display as drag icon.
;;;
;;; Owner: GtkDragIcon
;;;
;;; Flags: Read / Write
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_icon_set_child ()
;;;
;;; void
;;; gtk_drag_icon_set_child (GtkDragIcon *self,
;;;                          GtkWidget *child);
;;;
;;; Sets the widget to display as the drag icon.
;;;
;;; self :
;;;     a GtkDragIcon
;;;
;;; child :
;;;     a GtkWidget or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;;    gtk_drag_icon_get_child ()
;;;
;;; GtkWidget *
;;; gtk_drag_icon_get_child (GtkDragIcon *self);
;;;
;;; Gets the widget currently used as drag icon.
;;;
;;; self :
;;;     a GtkDragIcon
;;;
;;; Returns :
;;;     The drag icon or NULL if none.
;;; ----------------------------------------------------------------------------


;;; ----------------------------------------------------------------------------
;;; gtk_drag_icon_get_for_drag ()
;;;
;;; GtkWidget *
;;; gtk_drag_icon_get_for_drag (GdkDrag *drag);
;;;
;;; Gets the GtkDragIcon in use with drag .
;;;
;;; If no drag icon exists yet, a new one will be created and shown.
;;;
;;; drag :
;;;     a GdkDrag
;;;
;;; Returns :
;;;     the GtkDragIcon.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_icon_set_from_paintable ()
;;;
;;; void
;;; gtk_drag_icon_set_from_paintable (GdkDrag *drag,
;;;                                   GdkPaintable *paintable,
;;;                                   int hot_x,
;;;                                   int hot_y);
;;;
;;; Creates a GtkDragIcon that shows paintable , and associates it with the
;;; drag operation. The hotspot position on the paintable is aligned with the
;;; hotspot of the cursor.
;;;
;;; drag :
;;;     a GdkDrag
;;;
;;; paintable :
;;;     a GdkPaintable to display
;;;
;;; hot_x :
;;;     X coordinate of the hotspot
;;;
;;; hot_y :
;;;     Y coordinate of the hotspot
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drag_icon_create_widget_for_value ()
;;;
;;; GtkWidget *
;;; gtk_drag_icon_create_widget_for_value (const GValue *value);
;;;
;;; Creates a widget that can be used as a drag icon for the given value .
;;;
;;; Supported types include strings, GdkRGBA and GtkTextBuffer. If GTK does not
;;; know how to create a widget for a given value, it will return NULL.
;;;
;;; This method is used to set the default drag icon on drag'n'drop operations
;;; started by GtkDragSource, so you don't need to set a drag icon using this
;;; function there.
;;;
;;; value :
;;;     a GValue
;;;
;;; Returns :
;;;     A new GtkWidget for displaying value as a drag icon.
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.drag-icon.lisp -----------------------------------------
