;;; ----------------------------------------------------------------------------
;;; gtk4.drag-icon.lisp
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
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkDragIcon" drag-icon
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

#+liber-documentation
(setf (documentation 'drag-icon 'type)
 "@version{2023-07-31}
  @begin{short}
    The @class{gtk:drag-icon} widget is a @class{gtk:root} implementation with
    the sole purpose to serve as a drag icon during DND operations.
  @end{short}
  A drag icon moves with the pointer during a drag operation and is destroyed
  when the drag ends.

  To set up a drag icon and associate it with an ongoing drag operation, use
  the @fun{gtk:drag-icon-for-drag} function to get the icon for a drag. You can
  then use it like any other widget and use the @fun{gtk:drag-icon-child}
  function to set whatever widget should be used for the drag icon.

  Keep in mind that drag icons do not allow user input.
  @see-slot{gtk:drag-icon-child}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:drag-icon-child ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'drag-icon) t)
 "The @code{child} property of type @class{gtk:widget} (Read / Write) @br{}
  The widget to display as drag icon.")

#+liber-documentation
(setf (liber:alias-for-function 'drag-icon-child)
      "Accessor"
      (documentation 'drag-icon-child 'function)
 "@version{#2023-07-31}
  @syntax{(gtk:drag-icon-child object) => child}
  @syntax{(setf (gtk:drag-icon-child object) child)}
  @argument[object]{a @class{gtk:drag-icon} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Accessor of the @slot[gtk:drag-icon]{child} slot of the
    @class{gtk:drag-icon} class.
  @end{short}
  The @fun{gtk:drag-icon-child} function gets the widget currently used as drag
  icon. The @setf{gtk:drag-icon-child} function sets the widget to display as
  the drag icon.
  @see-class{gtk:drag-icon}
  @see-class{gtk:widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_drag_icon_get_for_drag
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_drag_icon_get_for_drag" drag-icon-for-drag)
    (g:object widget)
 #+liber-documentation
 "@version{#2025-07-27}
  @argument[drag]{a @class{gdk:drag} object}
  @return{The @class{gtk:widget} object for the drag icon.}
  @begin{short}
    Gets the drag icon in use with @arg{drag}.
  @end{short}
  If no drag icon exists yet, a new one will be created and shown.
  @see-class{gtk:drag-icon}
  @see-class{gdk:drag}
  @see-class{gtk:widget}"
  (drag (g:object gdk:drag)))

(export 'drag-icon-for-drag)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_icon_set_from_paintable
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_drag_icon_set_from_paintable" drag-icon-set-from-paintable)
    :void
 #+liber-documentation
 "@version{#2025-07-19}
  @argument[drag]{a @class{gdk:drag} object}
  @argument[paintable]{a @class{gdk:paintable} object to display}
  @argument[xhot]{an integer for the x coordinate of the hotspot}
  @argument[yhot]{an integer for the y coordinate of the hotspot}
  @begin{short}
    Creates a drag icon that shows @arg{paintable}, and associates it with the
    drag operation.
  @end{short}
  The hotspot position on the paintable is aligned with the hotspot of the
  cursor.
  @see-class{gtk:drag-icon}
  @see-class{gdk:paintable}"
  (drag (g:object gdk:drag))
  (paintable (g:object gdk:paintable))
  (xhot :int)
  (yhot :int))

(export 'drag-icon-set-from-paintable)

;;; ----------------------------------------------------------------------------
;;; gtk_drag_icon_create_widget_for_value
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_drag_icon_create_widget_for_value"
               drag-icon-create-widget-for-value) (g:object widget)
 #+liber-documentation
 "@version{#2025-07-11}
  @argument[gvalue]{a @sym{g:value} instance}
  @begin{return}
    The new @class{gtk:widget} object for displaying @arg{gvalue} as a drag
    icon.
  @end{return}
  @begin{short}
    Creates a widget that can be used as a drag icon for the given @arg{gvalue}.
  @end{short}
  Supported types include strings, @class{gdk:rgba} instances and
  @class{gtk:text-buffer} objects. If GTK does not know how to create a widget
  for a given value, it will return @code{nil}.

  This method is used to set the default drag icon on drag and drop operations
  started by the @class{gtk:drag-source} object, so you do not need to set a
  drag icon using this function there.
  @see-class{gtk:drag-icon}
  @see-class{gtk:widget}
  @see-symbol{g:value}"
  (value (:pointer (:struct g:value))))

(export 'drag-icon-create-widget-for-value)

;;; --- End of file gtk4.drag-icon.lisp ----------------------------------------
