;;; ----------------------------------------------------------------------------
;;; gtk4.column-view-cell.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2024 Dieter Kaiser
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
;;; Types and Values
;;;
;;;     GtkColumnViewCell
;;;
;;; Accessors
;;;
;;;     gtk_column_view_cell_get_child
;;;     gtk_column_view_cell_set_child
;;;     gtk_column_view_cell_get_focusable
;;;     gtk_column_view_cell_set_focusable
;;;     gtk_column_view_cell_get_item
;;;     gtk_column_view_cell_get_position
;;;     gtk_column_view_cell_get_selected
;;;
;;; Properties
;;;
;;;     child
;;;     focusable
;;;     item
;;;     position
;;;     selected
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkListItem
;;;         ╰── GtkColumnViewCell
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkColumnViewCell
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkColumnViewCell" column-view-cell
  (:superclass list-item
   :export t
   :interfaces nil
   :type-initializer "gtk_column_view_cell_get_type")
  ((child
    column-view-cell-child
    "child" "GtkWidget" t t)
   (focusable
    column-view-cell-focusable
    "focusable" "gboolean" t t)
   (item
    column-view-cell-item
    "item" "GObject" t nil)
   (position
    column-view-cell-position
    "position" "guint" t nil)
   (selected
    column-view-cell-selected
    "selected" "gboolean" t nil)))

#+liber-documentation
(setf (documentation 'column-view-cell 'type)
 "@version{2023-11-27}
  @begin{short}
    The @class{gtk:column-view-cell} object is used by the
    @class{gtk:column-view-column} object to represent items in a cell in
    @class{gtk:column-view} widget.
  @end{short}

  The @class{gtk:column-view-cell} objects are managed by the
  @class{gtk:column-view} widget with its factory and cannot be created by
  applications, but they need to be populated by application code. This is done
  by calling the @fun{gtk:column-view-cell-child} function.

  The @class{gtk:column-view-cell} objects exist in 2 stages:
  @begin{enumerate}
    @item{The unbound stage where the listitem is not currently connected to an
      item in the list. In that case, the @slot[gtk:column-view-cell]{item}
      property is set to @code{nil}.}
    @item{The bound stage where the listitem references an item from the list.
      The @slot[gtk:column-view-cell]{item} property is not @code{nil}.}
  @end{enumerate}
  Since 4.12
  @see-slot{gtk:column-view-cell-child}
  @see-slot{gtk:column-view-cell-focusable}
  @see-slot{gtk:column-view-cell-item}
  @see-slot{gtk:column-view-cell-position}
  @see-slot{gtk:column-view-cell-selected}
  @see-class{gtk:column-view}
  @see-class{gtk:column-view-column}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:column-view-cell-child ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'column-view-cell) t)
 "The @code{child} property of type @class{gtk:widget} (Read / Write) @br{}
  Widget used for display.")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-cell-child)
      "Accessor"
      (documentation 'column-view-cell-child 'function)
 "@version{2023-11-27}
  @syntax{(gtk:column-view-cell-child object) => child}
  @syntax{(setf (gtk:column-view-cell-child object) child)}
  @argument[object]{a @class{gtk:column-view-cell} object}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Accessor of the @slot[gtk:column-view-cell]{child} slot of the
    @class{gtk:column-view-cell} class.
  @end{short}
  The @fun{gtk:column-view-cell-child} function gets the child widget previously
  set or @code{nil} if none was set. The @setf{gtk:column-view-cell-child}
  function sets the child widget to be used for this listitem.

  This function is typically called by applications when setting up a listitem
  so that the widget can be reused when binding it multiple times.

  Since 4.12
  @see-class{gtk:column-view-cell}
  @see-class{gtk:widget}")

;;; --- gtk:column-view-cell-focusable -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "focusable" 'column-view-cell) t)
 "The @code{focusable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the item can be focused with the keyboard.")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-cell-focusable)
      "Accessor"
      (documentation 'column-view-cell-focusable 'function)
 "@version{2023-11-27}
  @syntax{(gtk:column-view-cell-focusable object) => focusable}
  @syntax{(setf (gtk:column-view-cell-focusable object) focusable)}
  @argument[object]{a @class{gtk:column-view-cell} object}
  @argument[focusable]{a boolean whether the item should be focusable}
  @begin{short}
    Accessor of the @slot[gtk:column-view-cell]{focusable} slot of the
    @class{gtk:column-view-cell} class.
  @end{short}
  The @fun{gtk:column-view-cell-focusable} function checks if a list item has
  been set to be focusable. The @setf{gtk:column-view-cell-focusable} function
  sets @arg{object} to be focusable. If an item is focusable, it can be focused
  using the keyboard. This works similar to the @fun{gtk:widget-focusable}
  function.

  Note that if items are not focusable, the keyboard cannot be used to activate
  them and selecting only works if one of the children of the listitem is
  focusable. By default, list items are focusable.

  Since 4.12
  @see-class{gtk:column-view-cell}
  @see-function{gtk:widget-focusable}")

;;; --- gtk:column-view-cell-item ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "item" 'column-view-cell) t)
 "The @code{item} property of type @class{g:object} (Read) @br{}
  Displayed item.")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-cell-item)
      "Accessor"
      (documentation 'column-view-cell-item 'function)
 "@version{2023-11-27}
  @syntax{(gtk:column-view-cell-item object) => item}
  @argument[object]{a @class{gtk:column-view-cell} object}
  @argument[item]{a @class{g:object} instance with the item displayed}
  @begin{short}
    Accessor of the @slot[gtk:column-view-cell]{item} slot of the
    @class{gtk:column-view-cell} class.
  @end{short}
  The @fun{gtk:column-view-cell-item} function gets the model item that
  associated with @arg{object}. If @arg{object} is unbound, this function
  returns @code{nil}.

  Since 4.12
  @see-class{gtk:column-view-cell}")

;;; --- gtk:column-view-cell-position ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "position" 'column-view-cell) t)
 "The @code{position} property of type @code{:uint} (Read) @br{}
  Position of the item. @br{}
  Default value: @var{gtk:+invalid-list-position+}")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-cell-position)
      "Accessor"
      (documentation 'column-view-cell-position 'function)
 "@version{2023-11-27}
  @syntax{(gtk:column-view-cell-position object) => position}
  @argument[object]{a @class{gtk:column-view-cell} object}
  @argument[position]{an unsigned integer with the position of the item}
  @begin{short}
    Accessor of the @slot[gtk:column-view-cell]{position} slot of the
    @class{gtk:column-view-cell} class.
  @end{short}
  The @fun{gtk:column-view-cell-position} function gets the position in the
  model that @arg{object} currently displays. If @arg{object} is unbound, the
  @var{gtk:+invalid-list-position+} value is returned.

  Since 4.12
  @see-class{gtk:column-view-cell}")

;;; --- gtk:column-view-cell-selected ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "selected" 'column-view-cell) t)
 "The @code{selected} property of type @code{:boolean} (Read) @br{}
  Whether the item is currently selected. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-cell-selected)
      "Accessor"
      (documentation 'column-view-cell-selected 'function)
 "@version{2023-11-27}
  @syntax{(gtk:column-view-cell-selected object) => selected}
  @argument[object]{a @class{gtk:column-view-cell} object}
  @argument[selected]{a boolean whether the item is currently selected}
  @begin{short}
    Accessor of the @slot[gtk:column-view-cell]{selected} slot of the
    @class{gtk:column-view-cell} class.
  @end{short}
  The @fun{gtk:column-view-cell-selected} function checks whether the item is
  displayed as selected. The selected state is maintained by the list widget
  and its model and cannot be set otherwise.

  Since 4.12
  @see-class{gtk:column-view-cell}")

;;; --- End of file gtk4.column-view-cell.lisp ---------------------------------

