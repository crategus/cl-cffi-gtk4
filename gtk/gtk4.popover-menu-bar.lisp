;;; ----------------------------------------------------------------------------
;;; gtk4.popover-menu-bar.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2024 Dieter Kaiser
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
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------
;;;
;;; GtkPopoverMenuBar
;;;
;;;     A menu bar with popovers
;;;
;;; Types and Values
;;;
;;;     GtkPopoverMenuBar
;;;
;;; Accessors
;;;
;;;     gtk_popover_menu_bar_set_menu_model
;;;     gtk_popover_menu_bar_get_menu_model
;;;
;;; Functions
;;;
;;;     gtk_popover_menu_bar_new_from_model
;;;     gtk_popover_menu_bar_add_child
;;;     gtk_popover_menu_bar_remove_child
;;;
;;; Properties
;;;
;;;     menu-model
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkPopoverMenuBar
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkPopoverMenuBar
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkPopoverMenuBar" popover-menu-bar
  (:superclass widget
    :export t
    :interfaces ("GtkAccessible"
                 "GtkBuildable"
                 "GtkConstraintTarget")
    :type-initializer "gtk_popover_menu_bar_get_type")
  ((menu-model
    popover-menu-bar-menu-model
    "menu-model" "GMenuModel" t t)))

#+liber-documentation
(setf (documentation 'popover-menu-bar 'type)
 "@version{2024-10-26}
  @begin{short}
    The @class{gtk:popover-menu-bar} widget presents a horizontal bar of items
    that pop up popover menus when clicked.
  @end{short}
  The only way to create instances of the @class{gtk:popover-menu-bar} widget is
  from a @class{g:menu-model} object.

  @image[popover-menu-bar]{Figure: GtkPopoverMenuBar}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
menubar
├── item[.active]
┊   ╰── popover
╰── item
    ╰── popover
    @end{pre}
    The @class{gtk:popover-menu-bar} implementation has a single CSS node with
    name @code{menubar}, below which each item has its CSS node, and below that
    the corresponding popover. The item whose popover is currently open gets
    the @code{.active} style class.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:popover-menu-bar} implementation uses the @code{:menu-bar}
    role of the @symbol{gtk:accessible-role} enumeration. The menu items use
    the @code{:menu-item} role and the menus use the @code{:menu} role.
  @end{dictionary}
  @see-constructor{gtk:popover-menu-bar-new-from-model}
  @see-slot{gtk:popover-menu-bar-menu-model}
  @see-class{gtk:popover}
  @see-class{gtk:popover-menu}
  @see-class{g:menu-model}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:popover-menu-bar-menu-model ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "menu-model"
                                               'popover-menu-bar) t)
 "The @code{menu-model} property of type @class{g:menu-model} (Read / Write)
  @br{}
  The model from which the menu bar is created. The model should only contain
  submenus as toplevel elements.")

#+liber-documentation
(setf (liber:alias-for-function 'popover-menu-bar-menu-model)
      "Accessor"
      (documentation 'popover-menu-bar-menu-model 'function)
 "@version{2024-10-26}
  @syntax{(gtk:popover-menu-bar-menu-model object) => model}
  @syntax{(setf (gtk:popover-menu-bar-menu-model object) model)}
  @argument[object]{a @class{gtk:popover-menu} widget}
  @argument[model]{a @class{g:menu-model} object}
  @begin{short}
    Accessor of the @slot[gtk:popover-menu-bar]{menu-model} slot of the
    @class{gtk:popover-menu} class.
  @end{short}
  The @fun{gtk:popover-menu-bar-menu-model} function returns the model from
  which the contents of the menu bar are taken. The
  @setf{gtk:popover-menu-bar-menu-model} function sets a menu model.
  @see-class{gtk:popover-menu-bar}
  @see-class{g:menu-model}")

;;; ----------------------------------------------------------------------------
;;; gtk_popover_menu_bar_new_from_model
;;; ----------------------------------------------------------------------------

(defun popover-menu-bar-new-from-model (model)
 #+liber-documentation
 "@version{#2024-10-26}
  @argument[model]{a @class{g:menu-model} object, or @code{nil}}
  @return{The new @class{gtk:popover-menu-bar} widget.}
  @begin{short}
    Creates a @class{gtk:popover-menu-bar} widget from a @class{g:menu-model}
    object.
  @end{short}
  @see-class{gtk:popover-menu-bar}
  @see-class{g:menu-model}"
  (if model
      (make-instance 'popover-menu-bar
                     :menu-model model)
      (make-instance 'popover-menu-bar)))

(export 'popover-menu-bar-new-from-model)

;;; ----------------------------------------------------------------------------
;;; gtk_popover_menu_bar_add_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_popover_menu_bar_add_child" popover-menu-bar-add-child)
    :boolean
 #+liber-documentation
 "@version{#2024-10-26}
  @argument[menubar]{a @class{gtk:popover-menu-bar} widget}
  @argument[child]{a @class{gtk:widget} object to add}
  @argument[id]{a string with the ID to insert @arg{child} at}
  @return{@em{True} if the ID was found and the widget added.}
  @begin{short}
    Adds a custom widget to a generated menu bar.
  @end{short}
  For this to work, the menu model of the menu bar must have an item with a
  custom attribute that matches the ID.
  @see-class{gtk:popover-menu-bar}
  @see-class{gtk:widget}
  @see-function{gtk:popover-menu-bar-remove-child}"
  (menubar (g:object popover-menu-bar))
  (child (g:object widget))
  (id :string))

(export 'popover-menu-bar-add-child)

;;; ----------------------------------------------------------------------------
;;; gtk_popover_menu_bar_remove_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_popover_menu_bar_remove_child"
               popover-menu-bar-remove-child) :boolean
 #+liber-documentation
 "@version{#2024-10-26}
  @argument[menubar]{a @class{gtk:popover-menu-bar} widget}
  @argument[child]{a @class{gtk:widget} object to remove}
  @return{@em{True} if the widget was removed.}
  @begin{short}
    Removes a widget that has previously been added with the
    @fun{gtk:popover-menu-bar-add-child} function.
  @end{short}
  @see-class{gtk:popover-menu-bar}
  @see-class{gtk:widget}
  @see-function{gtk:popover-menu-bar-add-child}"
  (menubar (g:object popover-menu-bar))
  (child (g:object widget)))

(export 'popover-menu-bar-remove-child)

;;; --- End of file gtk4.popover-menu-bar.lisp ---------------------------------
