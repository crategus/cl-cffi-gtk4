;;; ----------------------------------------------------------------------------
;;; gtk4.list-item.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 Dieter Kaiser
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
;;; Functions
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
;;;     accessible-description                             Since 4.12
;;;     accessible-label                                   Since 4.12
;;;     activatable
;;;     child
;;;     focusable                                          Since 4.12
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

(gobject:define-g-object-class "GtkListItem" list-item
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_list_item_get_type")
  (#+gtk-4-12
   (accessible-description
    list-item-accessible-description
    "accessible-descripton" "gchararray" t t)
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
 "@version{2023-8-13}
  @begin{short}
    The @class{gtk:list-item} object is the object that list-handling containers
    such as the @class{gtk:list-view} widget use to represent items in a
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

;;; --- list-item-accessible-description ---------------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "accessible-description"
                                               'list-item) t)
 "The @code{accessible-description} property of type @code{:string}
  (Read / Write) @br{}
  The accessible description to set on the list item. @br{}
  Default value: @code{nil}")

;;; --- list-item-accessible-label ---------------------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "accessible-label" 'list-item) t)
 "The @code{accessible-label} property of type @code{:string} (Read / Write)
  @br{}
  The accessible label to set on the list item. @br{}
  Default value: @code{nil}")

;;; --- list-item-activatable --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "activatable" 'list-item) t)
 "The @code{activatable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the item can be activated by the user. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'list-item-activatable)
      "Accessor"
      (documentation 'list-item-activatable 'function)
 "@version{2023-8-13}
  @syntax[]{gtk:list-item-activatable object) => activatable}
  @syntax[]{(setf gtk:list-item-activatable object) activatable)}
  @argument[object]{a @class{gtk:list-item} object}
  @argument[activatable]{a boolean whether the item should be activatable}
  @begin{short}
    Accessor of the @slot[gtk:list-item]{activatable} slot of the
    @class{gtk:list-item} class.
  @end{short}
  The @fun{gtk:list-item-activatable} function checks if a list item has been
  set to be activatable. The @setf{gtk:list-item-activatable} function sets
  @arg{object} to be activatable.

  If an item is activatable, double-clicking on the item, using the @kbd{Return}
  key or calling the @fun{gtk:widget-activate} function will activate the item.
  Activating instructs the containing view to handle activation. The
  @class{gtk:list-view} widget for example will be emitting the \"activate\"
  signal.

  By default, list items are activatable
  @see-class{gtk:list-item}
  @see-class{gtk:list-view}
  @see-function{gtk:widget-activate}")

;;; --- list-item-child --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'list-item) t)
 "The @code{child} property of type @class{gtk:widget} (Read / Write) @br{}
  Child widget used for display.")

#+liber-documentation
(setf (liber:alias-for-function 'list-item-child)
      "Accessor"
      (documentation 'list-item-child 'function)
 "@version{2023-8-13}
  @syntax[]{gtk:list-item-child object) => child}
  @syntax[]{(setf gtk:list-item-child object) child)}
  @argument[object]{a @class{gtk:list-item} object}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Accessor of the @slot[gtk:list-item]{child} slot of the
    @class{gtk:list-item} class.
  @end{short}
  The @fun{gtk:list-item-child} function gets the child widget or @code{nil}
  if none was set. The @setf{gtk:list-item-child} function sets the child widget
  to be used for this list item.

  This function is typically called by applications when setting up a list item
  so that the widget can be reused when binding it multiple times.
  @see-class{gtk:list-item}
  @see-class{gtk:widget}")

;;; --- list-item-focusable ----------------------------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "focusable" 'list-item) t)
 "The @code{focusable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the item can be focused with the keyboard. @br{}
  Default value: @em{true}")

;;; --- list-item-item ---------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "item" 'list-item) t)
 "The @code{item} property of type @class{g:object} (Read) @br{}
  Displayed item.")

#+liber-documentation
(setf (liber:alias-for-function 'list-item-item)
      "Accessor"
      (documentation 'list-item-item 'function)
 "@version{2023-8-13}
  @syntax[]{gtk:list-item-item object) => item}
  @argument[object]{a @class{gtk:list-item} object}
  @argument[item]{a @class{g:object} object with the item displayed}
  @begin{short}
    Accessor of the @slot[gtk:list-item]{item} slot of the
    @class{gtk:list-item} class.
  @end{short}
  Gets the item that is currently displayed in the model that @arg{object} is
  currently bound to or @code{nil} if @arg{object} is unbound.
  @see-class{gtk:list-item}
  @see-class{g:object}")

;;; --- list-item-position -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "position" 'list-item) t)
 "The @code{position} property of type @code{:uint} (Read) @br{}
  Position of the item. @br{}
  Default Value: @var{gtk:+invalid-list-position+}")

#+liber-documentation
(setf (liber:alias-for-function 'list-item-position)
      "Accessor"
      (documentation 'list-item-position 'function)
 "@version{2023-8-13}
  @syntax[]{gtk:list-item-position object) => position}
  @argument[object]{a @class{gtk:list-item} object}
  @argument[position]{An unsigned integer with the position of the item.}
  @begin{short}
    Accessor of the @slot[gtk:list-item]{item} slot of the
    @class{gtk:list-item} class.
  @end{short}
  Gets the position in the model that @arg{object} currently displays. If
  @arg{object} is unbound, the @var{gtk:+invalid-list-position+} value is
  returned.
  @see-class{gtk:list-item}
  @see-variable{gtk:+invalid-list-position+}")

;;; --- list-item-selectable ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "selectable" 'list-item) t)
 "The @code{selectable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the item can be selected by the user. @br{}
  Default Value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'list-item-selectable)
      "Accessor"
      (documentation 'list-item-selectable 'function)
 "@version{2023-8-13}
  @syntax[]{gtk:list-item-selectable object) => selectable}
  @syntax[]{(setf gtk:list-item-selectable object) selectable)}
  @argument[object]{a @class{gtk:list-item} object}
  @argument[selectable]{a boolean whether the item should b selectable}
  @begin{short}
    Accessor of the @slot[gtk:list-item]{selectable} slot of the
    @class{gtk:list-item} class.
  @end{short}
  The @fun{gtk:list-item-selectable} function checks if a list item has been
  set to be selectable. The @setf{gtk:list-item-selectable} function sets
  @arg{object} to be selectable. If an item is selectable, clicking on the item
  or using the keyboard will try to select or unselect the item. If this
  succeeds is up to the model to determine, as it is managing the selected
  state.

  Note that this means that making an item non-selectable has no influence on
  the selected state at all. A non-selectable item may still be selected.

  By default, list items are selectable. When rebinding them to a new item,
  they will also be reset to be selectable by GTK.
  @see-class{gtk:list-item}")

;;; --- list-item-selected -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "selected" 'list-item) t)
 "The @code{selected} property of type @code{:boolean} (Read) @br{}
  Whether the item is currently selected @br{}
  Default Value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'list-item-selected)
      "Accessor"
      (documentation 'list-item-selected 'function)
 "@version{2023-8-13}
  @syntax[]{gtk:list-item-selected object) => selected}
  @argument[object]{a @class{gtk:list-item} object}
  @argument[selected]{a boolean whether the item is selected}
  @begin{short}
    Accessor of the @slot[gtk:list-item]{selected} slot of the
    @class{gtk:list-item} class.
  @end{short}
  Checks if the item is displayed as selected. The selected state is maintained
  by the container and its list model and cannot be set otherwise.
  @see-class{gtk:list-item}")

;;; --- End of file gtk4.list-item.lisp ----------------------------------------
