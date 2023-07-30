;;; ----------------------------------------------------------------------------
;;; gtk4.drag-icon.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.10 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2023 Dieter Kaiser
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

(gobject:define-g-object-class "GtkDragIcon" drag-icon
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkNative"
                "GtkRoot")
   :type-initializer "gtk_drag_icon_get_type")
  ((child
    drag-icon-child
    "child" "GtkWidget" t t)))

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

(cffi:defcfun ("gtk_drag_icon_get_for_drag" drag-icon-for-drag)
    (g:object widget)
  (drag (g:object gdk:drag)))

(export 'drag-icon-for-drag)

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

(cffi:defcfun ("gtk_drag_icon_set_from_paintable" drag-icon-set-from-paintable)
    :void
  (drag (g:object gdk:drag))
  (paintable (g:object gdk:paintable))
  (xhot :int)
  (yhot :int))

(export 'drag-icon-set-from-paintable)
  
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

(cffi:defcfun ("gtk_drag_icon_create_widget_for_value"
               drag-icon-create-widget-for-value) (g:object widget)
  (value (:pointer (:struct g:value))))

(export 'drag-icon-create-widget-for-value)

;;; --- End of file gtk4.drag-icon.lisp ----------------------------------------
