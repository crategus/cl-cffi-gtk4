;;; ----------------------------------------------------------------------------
;;; gtk4.list-item.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2025 Dieter Kaiser
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
;;; GtkListItem
;;;
;;;     Object used to represent items of a list model
;;;
;;; Types and Values
;;;
;;;     GtkListItem
;;;
;;; Accessors
;;;
;;;     gtk_list_item_get_item
;;;     gtk_list_item_get_position
;;;     gtk_list_item_get_child
;;;     gtk_list_item_set_child
;;;     gtk_list_item_get_selected
;;;     gtk_list_item_get_selectable
;;;     gtk_list_item_set_selectable
;;;     gtk_list_item_get_activatable
;;;     gtk_list_item_set_activatable
;;;
;;; Properties
;;;
;;;     accessible-description                              Since 4.12
;;;     accessible-label                                    Since 4.12
;;;     activatable
;;;     child
;;;     focusable                                           Since 4.12
;;;     item
;;;     position
;;;     selectable
;;;     selected
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkListItem
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkListItem
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkListItem" list-item
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_list_item_get_type")
  (#+gtk-4-12
   (accessible-description
    list-item-accessible-description
    "accessible-description" "gchararray" t t)
   #+gtk-4-12
   (accessible-label
    list-item-accessible-label
    "accessible-label" "gchararray" t t)
   (activatable
    list-item-activatable
    "activatable" "gboolean" t t)
   (child
    list-item-child
    "child" "GtkWidget" t t)
   #+gtk-4-12
   (focusable
    list-item-focusable
    "focusable" "gboolean" t t)
   (item
    list-item-item
    "item" "GObject" t nil)
   (position
    list-item-position
    "position" "guint" t nil)
   (selectable
    list-item-selectable
    "selectable" "gboolean" t t)
   (selected
    list-item-selected
    "selected" "gboolean" t nil)))

#+liber-documentation
(setf (documentation 'list-item 'type)
 "@version{2025-03-15}
  @begin{short}
    The @class{gtk:list-item} object is the object that list-handling containers
    such as the @class{gtk:list-view} widget use to represent list items in a
    @class{g:list-model} object.
  @end{short}

  The @class{gtk:list-item} objects are managed by the list widget, with its
  factory, and cannot be created by applications, but they need to be populated
  by application code. This is done by calling the @fun{gtk:list-item-child}
  function.

  The @class{gtk:list-item} objects exist in 2 stages:
  @begin{enumerate}
    @item{The unbound stage where the list item is not currently connected to
      an item in the list. In that case, the @slot[gtk:list-item]{item} property
      is set to @code{nil}.}
    @item{The bound stage where the list item references an item from the list.
      The @slot[gtk:list-item]{item} property is not @code{nil}.}
  @end{enumerate}
  @see-slot{gtk:list-item-accessible-description}
  @see-slot{gtk:list-item-accessible-label}
  @see-slot{gtk:list-item-activatable}
  @see-slot{gtk:list-item-child}
  @see-slot{gtk:list-item-focusable}
  @see-slot{gtk:list-item-item}
  @see-slot{gtk:list-item-position}
  @see-slot{gtk:list-item-selectable}
  @see-slot{gtk:list-item-selected}
  @see-class{gtk:list-view}
  @see-class{g:list-model}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:list-item-accessible-description -----------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "accessible-description"
                                               'list-item) t)
 "The @code{accessible-description} property of type @code{:string}
  (Read / Write) @br{}
  The accessible description to set on the list item. Since 4.12 @br{}
  Default value: @code{nil}")

#+(and gtk-4-12 liber-documentation)
(setf (liber:alias-for-function 'list-item-accessible-description)
      "Accessor"
      (documentation 'list-item-accessible-description 'function)
 "@version{2025-09-29}
  @syntax{(gtk:list-item-accessible-description object) => description}
  @syntax{(setf (gtk:list-item-accessible-description object) description)}
  @argument[object]{a @class{gtk:list-item} object}
  @argument[description]{a string for the description}
  @begin{short}
    The accessor for the @slot[gtk:list-item]{accessible-description} slot of
    the @class{gtk:list-item} class gets or sets the accessible description for
    the list item.
  @end{short}
  The accessible description may be used by screen readers, for example.

  Since 4.12
  @see-class{gtk:list-item}")

;;; --- gtk:list-item-accessible-label -----------------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "accessible-label" 'list-item) t)
 "The @code{accessible-label} property of type @code{:string} (Read / Write)
  @br{}
  The accessible label to set on the list item. Since 4.12 @br{}
  Default value: @code{nil}")

#+(and gtk-4-12 liber-documentation)
(setf (liber:alias-for-function 'list-item-accessible-label)
      "Accessor"
      (documentation 'list-item-accessible-label 'function)
 "@version{2025-09-29}
  @syntax{(gtk:list-item-accessible-label object) => label}
  @syntax{(setf (gtk:list-item-accessible-label object) label)}
  @argument[object]{a @class{gtk:list-item} object}
  @argument[label]{a string for the label}
  @begin{short}
    The accessor for the @slot[gtk:list-item]{accessible-label} slot of the
    @class{gtk:list-item} class gets or sets the accessible label for the list
    item.
  @end{short}
  The accessible label may be used by screen readers, for example.

  Since 4.12
  @see-class{gtk:list-item}")

;;; --- gtk:list-item-activatable ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "activatable" 'list-item) t)
 "The @code{activatable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the list item can be activated by the user. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'list-item-activatable)
      "Accessor"
      (documentation 'list-item-activatable 'function)
 "@version{2025-09-29}
  @syntax{(gtk:list-item-activatable object) => activatable}
  @syntax{(setf (gtk:list-item-activatable object) activatable)}
  @argument[object]{a @class{gtk:list-item} object}
  @argument[activatable]{a boolean whether the list item should be activatable}
  @begin{short}
    The accessor for the @slot[gtk:list-item]{activatable} slot of the
    @class{gtk:list-item} class gets or sets whether the list item has been set
    to be activatable.
  @end{short}

  If a list item is activatable, double-clicking on the list item, using the
  @kbd{Return} key or calling the @fun{gtk:widget-activate} function will
  activate the list item. Activating instructs the containing view to handle
  activation. The @class{gtk:list-view} widget for example will be emitting the
  @sig[gtk:list-view]{activate} signal.

  By default, list items are activatable
  @see-class{gtk:list-item}
  @see-class{gtk:list-view}
  @see-function{gtk:widget-activate}")

;;; --- gtk:list-item-child ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'list-item) t)
 "The @code{child} property of type @class{gtk:widget} (Read / Write) @br{}
  The child widget used for display.")

#+liber-documentation
(setf (liber:alias-for-function 'list-item-child)
      "Accessor"
      (documentation 'list-item-child 'function)
 "@version{2025-09-29}
  @syntax{(gtk:list-item-child object) => child}
  @syntax{(setf (gtk:list-item-child object) child)}
  @argument[object]{a @class{gtk:list-item} object}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    The accessor for the @slot[gtk:list-item]{child} slot of the
    @class{gtk:list-item} class gets or sets the child widget.
  @end{short}
  Returns @code{nil} if none was set. This function is typically called by
  applications when setting up a list item so that the widget can be reused when
  binding it multiple times.
  @see-class{gtk:list-item}
  @see-class{gtk:widget}")

;;; --- gtk:list-item-focusable ------------------------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "focusable" 'list-item) t)
 "The @code{focusable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the item can be focused with the keyboard. Since 4.12 @br{}
  Default value: @em{true}")

#+(and gtk-4-12 liber-documentation)
(setf (liber:alias-for-function 'list-item-focusable)
      "Accessor"
      (documentation 'list-item-focusable 'function)
 "@version{2025-09-29}
  @syntax{(gtk:list-item-focusable object) => focusable}
  @syntax{(setf (gtk:list-item-focusable object) focusable)}
  @argument[object]{a @class{gtk:list-item} object}
  @argument[focusable]{a boolean whether the list item can be focused}
  @begin{short}
    The accessor for the @slot[gtk:list-item]{focusable} slot of the
    @class{gtk:list-item} class gets or sets whether the list item has been set
    to be focusable.
  @end{short}
  If a list item is focusable, it can be focused using the keyboard. This works
  similar to the @fun{gtk:widget-focusable} function.

  Note that if list items are not focusable, the keyboard cannot be used to
  activate them and selecting only works if one of the children of the list item
  is focusable. By default, list items are focusable.

  Since 4.12
  @see-class{gtk:list-item}
  @see-function{gtk:widget-focusable}")

;;; --- gtk:list-item-item -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "item" 'list-item) t)
 "The @code{item} property of type @class{g:object} (Read) @br{}
  The displayed item.")

#+liber-documentation
(setf (liber:alias-for-function 'list-item-item)
      "Accessor"
      (documentation 'list-item-item 'function)
 "@version{2025-09-29}
  @syntax{(gtk:list-item-item object) => item}
  @argument[object]{a @class{gtk:list-item} object}
  @argument[item]{a @class{g:object} instance for the list item displayed}
  @begin{short}
    The accessor for the @slot[gtk:list-item]{item} slot of the
    @class{gtk:list-item} class returns the list item that is currently
    displayed in the model that @arg{object} is currently bound to or @code{nil}
    if @arg{object} is unbound.
  @end{short}
  @see-class{gtk:list-item}
  @see-class{g:object}")

;;; --- gtk:list-item-position -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "position" 'list-item) t)
 "The @code{position} property of type @code{:uint} (Read) @br{}
  The position of the item. @br{}
  Default Value: @var{gtk:+invalid-list-position+}")

#+liber-documentation
(setf (liber:alias-for-function 'list-item-position)
      "Accessor"
      (documentation 'list-item-position 'function)
 "@version{2025-09-29}
  @syntax{(gtk:list-item-position object) => position}
  @argument[object]{a @class{gtk:list-item} object}
  @argument[position]{an unsigned integer for the position of the item.}
  @begin{short}
    The accessor for the @slot[gtk:list-item]{position} slot of the
    @class{gtk:list-item} class returns the position in the model that
    @arg{object} currently displays.
  @end{short}
  If @arg{object} is unbound, the @var{gtk:+invalid-list-position+} value is
  returned.
  @see-class{gtk:list-item}
  @see-variable{gtk:+invalid-list-position+}")

;;; --- gtk:list-item-selectable -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "selectable" 'list-item) t)
 "The @code{selectable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the item can be selected by the user. @br{}
  Default Value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'list-item-selectable)
      "Accessor"
      (documentation 'list-item-selectable 'function)
 "@version{2025-09-29}
  @syntax{(gtk:list-item-selectable object) => selectable}
  @syntax{(setf (gtk:list-item-selectable object) selectable)}
  @argument[object]{a @class{gtk:list-item} object}
  @argument[selectable]{a boolean whether the item should be selectable}
  @begin{short}
    The accessor for the @slot[gtk:list-item]{selectable} slot of the
    @class{gtk:list-item} class gets or sets whether the list item has been set
    to be selectable.
  @end{short}
  If an item is selectable, clicking on the item or using the keyboard will try
  to select or unselect the item. If this succeeds is up to the model to
  determine, as it is managing the selected state.

  Note that this means that making an item non-selectable has no influence on
  the selected state at all. A non-selectable item may still be selected.

  By default, list items are selectable. When rebinding them to a new item,
  they will also be reset to be selectable by GTK.
  @see-class{gtk:list-item}")

;;; --- gtk:list-item-selected -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "selected" 'list-item) t)
 "The @code{selected} property of type @code{:boolean} (Read) @br{}
  Whether the item is currently selected @br{}
  Default Value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'list-item-selected)
      "Accessor"
      (documentation 'list-item-selected 'function)
 "@version{2025-09-29}
  @syntax{(gtk:list-item-selected object) => selected}
  @argument[object]{a @class{gtk:list-item} object}
  @argument[selected]{a boolean whether the item is selected}
  @begin{short}
    The accessor for the @slot[gtk:list-item]{selected} slot of the
    @class{gtk:list-item} class returns whether the list item is displayed as
    selected.
  @end{short}
  The selected state is maintained by the container and its list model and
  cannot be set otherwise.
  @see-class{gtk:list-item}")

;;; --- End of file gtk4.list-item.lisp ----------------------------------------
