;;; ----------------------------------------------------------------------------
;;; gtk4.layout-manager.lisp
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
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkLayoutManager" layout-manager
  (:superclass g:object
   :export t
   :interfaces ()
   :type-initializer "gtk_layout_manager_get_type")
  nil)

#+liber-documentation
(setf (documentation 'layout-manager 'type)
 "@version{#2023-4-16}
  @begin{short}
    Layout managers are delegate classes that handle the preferred size and the
    allocation of a container widget.
  @end{short}

  You typically subclass the @sym{gtk:layout-manager} class if you want to
  implement a layout policy for the children of a widget, or if you want to
  determine the size of a widget depending on its contents.

  Each @class{gtk:widget} widget can only have a @sym{gtk:layout-manager}
  instance associated to it at any given time. It is possible, though, to
  replace the layout manager instance using the @fun{gtk:widget-layout-manager}
  function.

  @subheading{Layout properties}
  A layout manager can expose properties for controlling the layout of each
  child, by creating an object type derived from the @class{gtk:layout-child}
  class and installing the properties on it as normal @class{g:object}
  properties.

  Each the @class{gtk:layout-child} instance storing the layout properties for
  a specific child widget is created through the
  @fun{gtk:layout-manager-layout-child} method. A GtkLayoutManager controls the
  creation of its @class{gtk:layout-child} instances by overriding the
  @code{GtkLayoutManagerClass.create_layout_child()} virtual function. The
  typical implementation should look like:
  @begin{pre}
static GtkLayoutChild *
create_layout_child (GtkLayoutManager *manager,
                     GtkWidget        *container,
                     GtkWidget        *child)
{
  return g_object_new (your_layout_child_get_type (),
                       \"layout-manager\", manager,
                       \"child-widget\", child,
                       NULL);
@}
  @end{pre}
  The @slot[gtk:layout-child]{layout-manager} and
  @slot[gtk:layout-child]{child-widget} properties on the newly created
  @class{gtk:layout-child} instance are mandatory. The @sym{gtk:layout-manager}
  instance will cache the newly created @class{gtk:layout-child} instance until
  the widget is removed from its parent, or the parent removes the layout
  manager.

  Each @sym{gtk:-ayout-manager} instance creating a @class{gtk:layout-child}
  instance should use the @fun{gtk:layout-manager-layout-child} function every
  time it needs to query the layout properties. Each @class{gtk:layout-child}
  instance should call the @fun{gtk:layout-manager-layout-changed} function
  every time a property is updated, in order to queue a new size measuring and
  allocation.
  @see-class{gtk:layout-child}")

;;; ----------------------------------------------------------------------------
;;; gtk_layout_manager_measure ()
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
 #+liber-documentation
 "@version{#2023-4-16}
  @argument[manager]{a @class{gtk:layout-manager} object}
  @argument[widget]{a @class{gtk:widget} widget using @arg{manager}}
  @argument[orientation]{a @symbol{gtk:orientation} value to measure}
  @argument[for-size]{an integer with the size for the opposite of
    @arg{orientation}; for instance, if the orientation is @code{:horizontal},
    this is the height of the widget; if the orientation is @code{:vertical},
    this is the width of the widget; this allows to measure the height for the
    given width, and the width for the given height; use -1 if the size is not
    known}
  @begin{return}
    @arg{minimum} -- an integer with the minimum size for the given size and
      orientation @br{}
    @arg{natual} -- an integer with the natural, or preferred size for the
      given size and orientation @br{}
    @arg{minimum-baseline} -- an integer with the baseline position for the
      minimum size @br{}
    @arg{natural-baseline} -- an integer with the baseline position for the
      natural size
  @end{return}
  @begin{short}
    Measures the size of the widget using @arg{manager}, for the given
    orientation and size.
  @end{short}
  See the @class{gtk:widget} widgets geometry management section for more
  details.
  @see-class{gtk:layout-manager}
  @see-class{gtk:widget}
  @see-symbol{gtk:orientation}"
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
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_layout_manager_allocate" layout-manager-allocate) :void
 #+liber-documentation
 "@version{#2023-4-16}
  @argument[manager]{a @class{gtk:layout-manager} object}
  @argument[widget]{a @class{gtk:widget} widget using @arg{manager}}
  @argument[width]{an integer with the new width of the widget}
  @argument[height]{an integer with the new height of the widget}
  @argument[baseline]{an integer with the baseline position of the widget,
    or -1}
  @begin{short}
    This function assigns the given width, height, and baseline to a widget,
    and computes the position and sizes of the children of the widget using the
    layout management policy of manager .
  @end{short}
  @see-class{gtk:layout-manager}"
  (manager (g:object layout-manager))
  (widget (g:object widget))
  (width :int)
  (height :int)
  (baseline :int))

(export 'layout-manager-allocate)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_manager_get_request_mode ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_layout_manager_get_request_mode" layout-manager-request-mode)
    size-request-mode
 #+liber-documentation
 "@version{#2023-4-16}
  @argument[manager]{a @class{gtk:layout-manager} object}
  @begin{short}
    Retrieves the request mode of @arg{manager}.
  @end{short}
  @see-class{gtk:layout-manager}"
  (manager (g:object layout-manager)))

(export 'layout-manager-request-mode)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_manager_get_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_layout_manager_get_widget" layout-manager-widget)
    (g:object widget)
 #+liber-documentation
 "@version{#2023-4-16}
  @argument[manager]{a @class{gtk:layout-manager} object}
  @return{A @class{gtk:widget} widget.}
  @begin{short}
    Retrieves the widget using the given @class{gtk:layout-manager} object.
  @end{short}
  @see-class{gtk:layout-manager}
  @see-class{gtk:widget}"
  (manager (g:object layout-manager)))

(export 'layout-manager-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_manager_get_layout_child ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_layout_manager_get_layout_child" layout-manager-layout-child)
    (g:object layout-child)
 #+liber-documentation
 "@version{#2023-4-16}
  @argument[manager]{a @class{gtk:layout-manager} object}
  @argument[child]{a @class{gtk:widget} child widget}
  @return{A @class{gtk:layout-child} object.}
  @begin{short}
    Retrieves a @class{gtk:layout-child} instance for the
    @class{gtk:layout-manager} object, creating one if necessary.
  @end{short}
  The child widget must be a child of the widget using @arg{manager}.

  The @class{gtk:layout-child} object is owned by the @class{gtk:layout-manager}
  object, and is guaranteed to exist as long @arg{child} is a child of the
  @class{gtk:widget} object using the given @arg{manager}.
  @see-class{gtk:layout-manager}
  @see-class{gtk:layout-child}"
  (manager (g:object layout-manager))
  (child (g:object widget)))

(export 'layout-manager-layout-child)

;;; ----------------------------------------------------------------------------
;;; gtk_layout_manager_layout_changed ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_layout_manager_layout_changed" layout-manager-layout-changed)
    :void
 #+liber-documentation
 "@version{#2023-4-16}
  @argument[manager]{a @class{gtk:layout-manager} object}
  @begin{short}
    Queues a resize on the @class{gtk:widget} widget using @arg{manager}, if
    any.
  @end{short}
  This function should be called by subclasses of the @class{gtk:layout-manager}
  object in response to changes to their layout management policies.
  @see-class{gtk:layout-manager}
  @see-class{gtk:widget}"
  (manager (g:object layout-manager)))

(export 'layout-manager-layout-changed)

;;; --- End of file gtk4.layout-manager.lisp -----------------------------------
