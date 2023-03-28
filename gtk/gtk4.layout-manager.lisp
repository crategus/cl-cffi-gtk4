;;; ----------------------------------------------------------------------------
;;; gtk.layout-manager.lisp
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
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License. If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; GtkLayoutManager
;;;
;;;     Base class for layout manager
;;;
;;; Types and Values
;;;
;;;     GtkLayoutManager
;;;
;;; Functions
;;;
;;;     gtk_layout_manager_measure
;;;     gtk_layout_manager_allocate
;;;     gtk_layout_manager_get_request_mode
;;;     gtk_layout_manager_get_widget
;;;     gtk_layout_manager_get_layout_child
;;;     gtk_layout_manager_layout_changed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkLayoutManager
;;;         ├── GtkBinLayout
;;;         ├── GtkBoxLayout
;;;         ├── GtkCenterLayout
;;;         ├── GtkConstraintLayout
;;;         ├── GtkFixedLayout
;;;         ├── GtkGridLayout
;;;         ╰── GtkOverlayLayout
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkLayoutManager
;;;
;;; Layout managers are delegate classes that handle the preferred size and the
;;; allocation of a container widget.
;;;
;;; You typically subclass GtkLayoutManager if you want to implement a layout
;;; policy for the children of a widget, or if you want to determine the size
;;; of a widget depending on its contents.
;;;
;;; Each GtkWidget can only have a GtkLayoutManager instance associated to it
;;; at any given time; it is possible, though, to replace the layout manager
;;; instance using gtk_widget_set_layout_manager().
;;;
;;; Layout properties
;;; A layout manager can expose properties for controlling the layout of each
;;; child, by creating an object type derived from GtkLayoutChild and installing
;;; the properties on it as normal GObject properties.
;;;
;;; Each GtkLayoutChild instance storing the layout properties for a specific
;;; child is created through the gtk_layout_manager_get_layout_child() method;
;;; a GtkLayoutManager controls the creation of its GtkLayoutChild instances by
;;; overriding the GtkLayoutManagerClass.create_layout_child() virtual function.
;;; The typical implementation should look like:
;;;
;;; static GtkLayoutChild *
;;; create_layout_child (GtkLayoutManager *manager,
;;;                      GtkWidget        *container,
;;;                      GtkWidget        *child)
;;; {
;;;   return g_object_new (your_layout_child_get_type (),
;;;                        "layout-manager", manager,
;;;                        "child-widget", child,
;;;                        NULL);
;;; }
;;;
;;; The “layout-manager” and “child-widget” properties on the newly created
;;; GtkLayoutChild instance are mandatory. The GtkLayoutManager will cache the
;;; newly created GtkLayoutChild instance until the widget is removed from its
;;; parent, or the parent removes the layout manager.
;;;
;;; Each GtkLayoutManager instance creating a GtkLayoutChild should use
;;; gtk_layout_manager_get_layout_child() every time it needs to query the
;;; layout properties; each GtkLayoutChild instance should call
;;; gtk_layout_manager_layout_changed() every time a property is updated, in
;;; order to queue a new size measuring and allocation.
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkLayoutManager" layout-manager
  (:superclass g:object
   :export t
   :interfaces ()
   :type-initializer "gtk_layout_manager_get_type")
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_manager_measure ()
;;;
;;; void
;;; gtk_layout_manager_measure (GtkLayoutManager *manager,
;;;                             GtkWidget *widget,
;;;                             GtkOrientation orientation,
;;;                             int for_size,
;;;                             int *minimum,
;;;                             int *natural,
;;;                             int *minimum_baseline,
;;;                             int *natural_baseline);
;;;
;;; Measures the size of the widget using manager , for the given orientation
;;; and size.
;;;
;;; See GtkWidget's geometry management section for more details.
;;;
;;; manager :
;;;     a GtkLayoutManager
;;;
;;; widget :
;;;     the GtkWidget using manager
;;;
;;; orientation :
;;;     the orientation to measure
;;;
;;; for_size :
;;;     Size for the opposite of orientation ; for instance, if the orientation
;;;     is GTK_ORIENTATION_HORIZONTAL, this is the height of the widget; if the
;;;     orientation is GTK_ORIENTATION_VERTICAL, this is the width of the
;;;     widget. This allows to measure the height for the given width, and the
;;;     width for the given height. Use -1 if the size is not known
;;;
;;; minimum :
;;;     the minimum size for the given size and orientation.
;;;
;;; natural :
;;;     the natural, or preferred size for the given size and orientation.
;;;
;;; minimum_baseline :
;;;     the baseline position for the minimum size.
;;;
;;; natural_baseline :
;;;     the baseline position for the natural size.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_layout_manager_measure" %layout-manager-measure) :void
  (manager (g:object layout-manager))
  (widget (g:object widget))
  (orientation orientation)
  (for-size :int)
  (minimum (:pointer :int))
  (natural (:pointer :int))
  (minimum-baseline (:pointer :int))
  (natural-baseline (:pointer :int)))

(defun layout-manager-measure (manager widget orientation for-size)
  (with-foreign-objects ((minimum :int)
                         (natural :int)
                         (minimum-baseline :int)
                         (natural-baseline :int))
    (%layout-manager-measure manager
                             widget
                             orientation
                             for-size
                             minimum
                             natural
                             minimum-baseline
                             natural-baseline)
    (values (cffi:mem-ref minimum :int)
            (cffi:mem-ref natural :int)
            (cffi:mem-ref minimum-baseline :int)
            (cffi:mem-ref natural-baseline :int))))

(export 'layout-manager-measure)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_manager_allocate ()
;;;
;;; void
;;; gtk_layout_manager_allocate (GtkLayoutManager *manager,
;;;                              GtkWidget *widget,
;;;                              int width,
;;;                              int height,
;;;                              int baseline);
;;;
;;; This function assigns the given width , height , and baseline to a widget ,
;;; and computes the position and sizes of the children of the widget using the
;;; layout management policy of manager .
;;;
;;; manager :
;;;     a GtkLayoutManager
;;;
;;; widget :
;;;     the GtkWidget using manager
;;;
;;; width :
;;;     the new width of the widget
;;;
;;; height :
;;;     the new height of the widget
;;;
;;; baseline :
;;;     the baseline position of the widget , or -1
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_layout_manager_allocate" layout-manager-allocate) :void
  (manager (g:object layout-manager))
  (widget (g:object widget))
  (width :int)
  (height :int)
  (baseline :int))

(export 'layout-manager-allocate)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_manager_get_request_mode ()
;;;
;;; GtkSizeRequestMode
;;; gtk_layout_manager_get_request_mode (GtkLayoutManager *manager);
;;;
;;; Retrieves the request mode of manager .
;;;
;;; manager :
;;;     a GtkLayoutManager
;;;
;;; Returns :
;;;     a GtkSizeRequestMode
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_layout_manager_get_request_mode" layout-manager-request-mode)
    size-request-mode
  (manager (g:object layout-manager)))

(export 'layout-manager-request-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_manager_get_widget ()
;;;
;;; GtkWidget *
;;; gtk_layout_manager_get_widget (GtkLayoutManager *manager);
;;;
;;; Retrieves the GtkWidget using the given GtkLayoutManager.
;;;
;;; manager :
;;;     a GtkLayoutManager
;;;
;;; Returns :
;;;     a GtkWidget.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_layout_manager_get_widget" layout-manager-widget)
    (g:object widget)
  (manager (g:object layout-manager)))

(export 'layout-manager-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_manager_get_layout_child ()
;;;
;;; GtkLayoutChild *
;;; gtk_layout_manager_get_layout_child (GtkLayoutManager *manager,
;;;                                      GtkWidget *child);
;;;
;;; Retrieves a GtkLayoutChild instance for the GtkLayoutManager, creating one
;;; if necessary.
;;;
;;; The child widget must be a child of the widget using manager .
;;;
;;; The GtkLayoutChild instance is owned by the GtkLayoutManager, and is
;;; guaranteed to exist as long as child is a child of the GtkWidget using the
;;; given GtkLayoutManager.
;;;
;;; manager :
;;;     a GtkLayoutManager
;;;
;;; child :
;;;     a GtkWidget
;;;
;;; Returns :
;;;     a GtkLayoutChild.
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_layout_manager_get_layout_child" layout-manager-layout-child)
    (g:object layout-child)
  (manager (g:object layout-manager))
  (child (g:object widget)))

(export 'layout-manager-layout-child)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_manager_layout_changed ()
;;;
;;; void
;;; gtk_layout_manager_layout_changed (GtkLayoutManager *manager);
;;;
;;; Queues a resize on the GtkWidget using manager , if any.
;;;
;;; This function should be called by subclasses of GtkLayoutManager in response
;;; to changes to their layout management policies.
;;;
;;; manager :
;;;     a GtkLayoutManager
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_layout_manager_layout_changed" layout-manager-layout-changed)
    :void
  (manager (g:object layout-manager)))

(export 'layout-manager-layout-changed)

;;; --- End of file gtk.layout-manager.lisp ------------------------------------
