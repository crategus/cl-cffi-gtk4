;;; ----------------------------------------------------------------------------
;;; gtk4.size-group.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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
;;;     GtkSizeGroupMode                         -> gtk.enumerations.lisp
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
 "@version{2023-3-18}
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
  they get the size they request and not more.

  Widgets can be part of multiple size groups. GTK will compute the horizontal
  size of a widget from the horizontal requisition of all widgets that can be
  reached from the widget by a chain of size groups of type @code{:horizontal}
  or @code{:both}, and the vertical size from the vertical requisition of all
  widgets that can be reached from the widget by a chain of size groups of type
  @code{:vertical} or @code{:both}.

  Note that only non-contextual sizes of every widget are ever consulted by
  size groups, since size groups have no knowledge of what size a widget will
  be allocated in one dimension, it cannot derive how much height a widget
  will receive for a given width. When grouping widgets that trade height for
  width in mode @code{:vertical} or @code{:both}: the height for the minimum
  width will be the requested height for all widgets in the group. The same is
  of course true when horizontally grouping width for height widgets.

  Widgets that trade height-for-width should set a reasonably large minimum
  width by way of \"width-chars\" for instance. Widgets with static sizes as
  well as widgets that grow (such as ellipsizing text) need no such
  considerations.
  @begin[GtkSizeGroup as GtkBuildable]{dictionary}
    Size groups can be specified in a UI definition by placing an
    @code{<object>} element with class \"GtkSizeGroup\" somewhere in the UI
    definition. The widgets that belong to the size group are specified by a
    @code{<widgets>} element that may contain multiple @code{<widget>} elements,
    one for each member of the size group. The name attribute gives the ID of
    the widget.

    @b{Example:} A UI definition fragment with the @class{gtk:size-group}
    widget.
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
  @see-slot{gtk:size-group-ignore-hidden}
  @see-slot{gtk:size-group-mode}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:size-group-mode ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "mode" 'size-group) t)
 "The @code{mode} property of type @symbol{gtk:size-group-mode} (Read / Write)
  @br{}
  The directions in which the size group affects the requested sizes of its
  component widgets. @br{}
  Default value: @code{:horizontal}")

#+liber-documentation
(setf (liber:alias-for-function 'size-group-mode)
      "Accessor"
      (documentation 'size-group-mode 'function)
 "@version{2023-3-18}
  @syntax{(gtk:size-group-mode object) => mode}
  @syntax{(setf (gtk:size-group-mode object) mode)}
  @argument[object]{a @class{gtk:size-group} object}
  @argument[mode]{a @symbol{gtk:size-group-mode} value to set for the size
    group}
  @begin{short}
    Accessor of the @slot[gtk:size-group]{mode} slot of the
    @class{gtk:size-group} class.
  @end{short}
  The @fun{gtk:size-group-mode} function gets the current mode of the size
  group. The @setf{gtk:size-group-mode} function sets the mode.

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
 "@version{2023-3-18}
  @argument[mode]{a @symbol{gtk:size-group-mode} value for the new size group}
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
 "@version{2023-3-18}
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
 "@version{2023-3-18}
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
 "@version{2023-3-18}
  @argument[group]{a @class{gtk:size-group} object}
  @begin{return}
    A list of @class{gtk:widget} objects.
  @end{return}
  @begin{short}
    Returns the list of widgets associated with the size group.
  @end{short}
  @see-class{gtk:size-group}
  @see-class{gtk:widget}"
  (group (g:object size-group)))

(export 'size-group-widgets)

;;; --- End of file gtk4.size-group.lisp ---------------------------------------
