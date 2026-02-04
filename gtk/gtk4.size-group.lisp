;;; ----------------------------------------------------------------------------
;;; gtk4.size-group.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2026 Dieter Kaiser
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
;;; GtkSizeGroup
;;;
;;;     Grouping widgets so they request the same size
;;;
;;; Types and Values
;;;
;;;     GtkSizeGroup
;;;     GtkSizeGroupMode
;;;
;;; Accessors
;;;
;;;     gtk_size_group_set_mode
;;;     gtk_size_group_get_mode
;;;
;;; Functions
;;;
;;;     gtk_size_group_new
;;;     gtk_size_group_add_widget
;;;     gtk_size_group_remove_widget
;;;     gtk_size_group_get_widgets
;;;
;;; Properties
;;;
;;;     mode
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkSizeGroup
;;;
;;; Implemented Interfaces
;;;
;;;     GtkBuildable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkSizeGroupMode
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkSizeGroupMode" size-group-mode
  (:export t
   :type-initializer "gtk_size_group_mode_get_type")
  (:none 0)
  (:horizontal 1)
  (:vertical 2)
  (:both 3))

#+liber-documentation
(setf (liber:alias-for-symbol 'size-group-mode)
      "GEnum"
      (liber:symbol-documentation 'size-group-mode)
 "@version{2025-07-22}
  @begin{declaration}
(gobject:define-genum \"GtkSizeGroupMode\" size-group-mode
  (:export t
   :type-initializer \"gtk_size_group_mode_get_type\")
  (:none 0)
  (:horizontal 1)
  (:vertical 2)
  (:both 3))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:none]{Group has no effect.}
      @entry[:horizontal]{Group affects horizontal requisition.}
      @entry[:vertical]{Group affects vertical requisition.}
      @entry[:both]{Group affects both horizontal and vertical requisition.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The mode of the size group determines the directions in which the
    @class{gtk:size-group} widget affects the requested sizes of its component
    widgets.
  @end{short}
  @see-class{gtk:size-group}")

;;; ----------------------------------------------------------------------------
;;; GtkSizeGroup
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkSizeGroup" size-group
  (:superclass g:object
    :export t
    :interfaces ("GtkBuildable")
    :type-initializer "gtk_size_group_get_type")
  ((mode
    size-group-mode
    "mode" "GtkSizeGroupMode" t t)))

#+liber-documentation
(setf (documentation 'size-group 'type)
 "@version{2025-07-26}
  @begin{short}
    The @class{gtk:size-group} object provides a mechanism for grouping a number
    of widgets together so they all request the same amount of space.
  @end{short}
  This is typically useful when you want a column of widgets to have the same
  size, but you cannot use a @class{gtk:grid} widget.

  In detail, the size requested for each widget in a size group is the maximum
  of the sizes that would have been requested for each widget in the size group
  if they were not in the size group. The mode of the size group, see the
  @fun{gtk:size-group-mode} function, determines whether this applies to the
  horizontal size, the vertical size, or both sizes.

  Note that size groups only affect the amount of space requested, not the
  size that the widgets finally receive. If you want the widgets in a size
  group to actually be the same size, you need to pack them in such a way that
  they get the size they request and not more. In particular it does not make a
  lot of sense to set the expand flags on the widgets that are members of a
  size group.

  Widgets can be part of multiple size groups. GTK will compute the horizontal
  size of a widget from the horizontal requisition of all widgets that can be
  reached from the widget by a chain of size groups of type @code{:horizontal}
  or @code{:both}, and the vertical size from the vertical requisition of all
  widgets that can be reached from the widget by a chain of size groups of type
  @code{:vertical} or @code{:both}.

  @subheading{Size groups and trading height-for-width}
  @b{Warning:}
    Generally, size groups do not interact well with widgets that trade height
    for width (or width for height), such as wrappable labels. Avoid using size
    groups with such widgets.

  A size group with @code{:horizontal} or @code{:vertival} mode only consults
  non-contextual sizes of widgets other than the one being measured, since it
  has no knowledge of what size a widget will get allocated in the other
  orientation. This can lead to widgets in a size group actually requesting
  different contextual sizes, contrary to the purpose of the
  @class{gtk:size-group} widget.

  In contrast, a size group with @code{:both} mode can properly propagate the
  available size in the opposite orientation when measuring widgets in the
  size group, which results in consistent and accurate measurements.

  In case some mechanism other than a size group is already used to ensure that
  widgets in a size group all get the same size in one orientation, for example,
  some common ancestor is known to allocate the same width to all its children,
  and the size group is only really needed to also make the widgets request the
  same size in the other orientation, it is beneficial to still set the mode of
  the size group to the @val[gtk:size-group-mode]{:both} value. This lets the
  size group assume and count on sizes of the widgets in the former orientation
  being the same, which enables it to propagate the available size as described
  above.

  @subheading{Alternatives to size groups}
  Size groups have many limitations, such as only influencing size requests but
  not allocations, and poor height-for-width support. When possible, prefer
  using dedicated mechanisms that can properly ensure that the widgets get the
  same size.

  Various container widgets and layout managers support a homogeneous layout
  mode, where they will explicitly give the same size to their children,
  see the @slot[gtk:box]{homogeneous} property. Using homogeneous mode can also
  have large performance benefits compared to either the same container in
  non-homogeneous mode, or to size groups.

  The @class{gtk:grid} widget can be used to position widgets into rows and
  columns. Members of each column will have the same width among them. Likewise,
  members of each row will have the same height. On top of that, the heights
  can be made equal between all rows with the @slot[gtk:grid]{row-homogeneous}
  property, and the widths can be made equal between all columns with the
  @slot[gtk:grid]{column-homogeneous} property.
  @begin[GtkSizeGroup as GtkBuildable]{dictionary}
    Size groups can be specified in a UI definition by placing an
    @code{<object>} element with @code{\"GtkSizeGroup\"} class somewhere in the
    UI definition. The widgets that belong to the size group are specified by a
    @code{<widgets>} element that may contain multiple @code{<widget>} elements,
    one for each member of the size group. The name attribute gives the ID of
    the widget.
  @end{dictionary}
  @begin[Examples]{dictionary}
    A UI definition fragment with the @class{gtk:size-group} widget.
    @begin{pre}
<object class=\"GtkSizeGroup\">
  <property name=\"mode\">GTK_SIZE_GROUP_HORIZONTAL</property>
  <widgets>
    <widget name=\"radio1\"/>
    <widget name=\"radio2\"/>
  </widgets>
</object>
    @end{pre}
  @end{dictionary}
  @see-constructor{gtk:size-group-new}
  @see-slot{gtk:size-group-mode}
  @see-symbol{gtk:size-group-mode}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:size-group-mode ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "mode" 'size-group) t)
 "The @code{mode} property of type @sym{gtk:size-group-mode} (Read / Write)
  @br{}
  The directions in which the size group affects the requested sizes of its
  component widgets. @br{}
  Default value: @val[gtk:size-group-mode]{:horizontal}")

#+liber-documentation
(setf (liber:alias-for-function 'size-group-mode)
      "Accessor"
      (documentation 'size-group-mode 'function)
 "@version{2025-09-27}
  @syntax{(gtk:size-group-mode object) => mode}
  @syntax{(setf (gtk:size-group-mode object) mode)}
  @argument[object]{a @class{gtk:size-group} object}
  @argument[mode]{a @sym{gtk:size-group-mode} value to set for the size group}
  @begin{short}
    The accessor for the @slot[gtk:size-group]{mode} slot of the
    @class{gtk:size-group} class gets or sets the mode of the size group.
  @end{short}

  The mode of the size group determines whether the widgets in the size group
  should all have the same horizontal requisition, @code{:horizontal}, all have
  the same vertical requisition, @code{:vertical}, or should all have the same
  requisition in both directions, @code{:both}.
  @see-class{gtk:size-group}
  @see-symbol{gtk:size-group-mode}")

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_new
;;; ----------------------------------------------------------------------------

(declaim (inline size-group-new))

(defun size-group-new (mode)
 #+liber-documentation
 "@version{2025-07-26}
  @argument[mode]{a @sym{gtk:size-group-mode} value for the new size group}
  @return{The newly created @class{gtk:size-group} object.}
  @begin{short}
    Create a new size group.
  @end{short}
  @see-class{gtk:size-group}
  @see-symbol{gtk:size-group-mode}"
  (make-instance 'size-group
                 :mode mode))

(export 'size-group-new)

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_add_widget
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_size_group_add_widget" size-group-add-widget) :void
 #+liber-documentation
 "@version{2025-02-23}
  @argument[group]{a @class{gtk:size-group} object}
  @argument[widget]{a @class{gtk:widget} object to add}
  @begin{short}
    Adds a widget to a size group.
  @end{short}
  In the future, the requisition of the widget will be determined as the maximum
  of its requisition and the requisition of the other widgets in the size group.
  Whether this applies horizontally, vertically, or in both directions depends
  on the mode of the size group. See the @fun{gtk:size-group-mode} function.

  When the widget is destroyed or no longer referenced elsewhere, it will be
  removed from the size group.
  @see-class{gtk:size-group}
  @see-class{gtk:widget}
  @see-function{gtk:size-group-mode}
  @see-function{gtk:size-group-remove-widget}"
  (group (g:object size-group))
  (widget (g:object widget)))

(export 'size-group-add-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_remove_widget
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_size_group_remove_widget" size-group-remove-widget) :void
 #+liber-documentation
 "@version{2025-02-23}
  @argument[group]{a @class{gtk:size-group} object}
  @argument[widget]{a @class{gtk:widget} object to remove}
  @begin{short}
    Removes a widget from a size group.
  @end{short}
  @see-class{gtk:size-group}
  @see-class{gtk:widget}
  @see-function{gtk:size-group-add-widget}"
  (group (g:object size-group))
  (widget (g:object widget)))

(export 'size-group-remove-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_size_group_get_widgets
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_size_group_get_widgets" size-group-widgets)
    (g:slist-t g:object :free-from-foreign nil)
 #+liber-documentation
 "@version{2025-02-23}
  @argument[group]{a @class{gtk:size-group} object}
  @begin{return}
    The list of @class{gtk:widget} objects.
  @end{return}
  @begin{short}
    Returns the list of widgets associated with the size group.
  @end{short}
  @see-class{gtk:size-group}
  @see-class{gtk:widget}"
  (group (g:object size-group)))

(export 'size-group-widgets)

;;; --- End of file gtk4.size-group.lisp ---------------------------------------
