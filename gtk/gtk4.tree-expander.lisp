;;; ----------------------------------------------------------------------------
;;; gtk4.tree-expander.lisp
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
;;; GtkTreeExpander
;;;
;;;     An indenting expander button for use in a tree list
;;;
;;; Types and Values
;;;
;;;     GtkTreeExpander
;;;
;;; Accessors
;;;
;;;     gtk_tree_expander_get_child
;;;     gtk_tree_expander_set_child
;;;     gtk_tree_expander_get_hide_expander                 Since 4.10
;;;     gtk_tree_expander_set_hide_expander                 Since 4.10
;;;     gtk_tree_expander_get_indent-for-depth              Since 4.10
;;;     gtk_tree_expander_set_indent-for-depth              Since 4.10
;;;     gtk_tree_expander_get_indent-for-icon               Since 4.6
;;;     gtk_tree_expander_set_indent-for-icon               Since 4.6
;;;     gtk_tree_expander_get_item
;;;     gtk_tree_expander_get_list_row
;;;     gtk_tree_expander_set_list_row
;;;
;;; Functions
;;;
;;;     gtk_tree_expander_new
;;;
;;; Properties
;;;
;;;     child
;;;     hide-expander                                       Since 4.10
;;;     indent-for-depth                                    Since 4.10
;;;     indent-for-icon                                     Since 4.6
;;;     item
;;;     list-row
;;;
;;; Actions
;;;
;;;     listitem.toggle-expand
;;;     listitem.collapse
;;;     listitem.expand
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkTreeExpander
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTreeExpander
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkTreeExpander" tree-expander
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_tree_expander_get_type")
  ((child
    tree-expander-child
    "child" "GtkWidget" t t)
   #+gtk-4-10
   (hide-expander
    tree-expander-hide-expander
    "hide-expander" "gboolean" t t)
   #+gtk-4-10
   (indent-for-depth
    tree-expander-indent-for-depth
    "indent-for-depth" "gboolean" t t)
   #+gtk-4-6
   (indent-for-icon
    tree-expander-indent-for-icon
    "indent-for-icon" "gboolean" t t)
   (item
    tree-expander-item
    "item" "GObject" t nil)
   (list-row
    tree-expander-list-row
    "list-row" "GtkTreeListRow" t t)))

#+liber-documentation
(setf (documentation 'tree-expander 'type)
 "@version{2025-4-16}
  @begin{short}
    The @class{gtk:tree-expander} widget is a widget that provides an expander
    for a list.
  @end{short}
  It is typically placed as a bottommost child into a @class{gtk:list-view}
  widget to allow users to expand and collapse children in a list with a
  @class{gtk:tree-list-model} object. The @class{gtk:tree-expander} widget
  provides the common UI elements, gestures and keybindings for this purpose.

  On top of this, the @code{\"listitem.expand\"}, @code{\"listitem.collapse\"}
  and @code{\"listitem.toggle-expand\"} actions are provided to allow adding
  custom UI for managing expanded state.

  It is important to mention that you want to set the
  @slot[gtk:list-item]{focusable} property to @em{false} when using this widget,
  as you want the keyboard focus to be in the treexpander, and not inside the
  list to make use of the keybindings.

  The @class{gtk:tree-list-model} object must be set to not be passthrough.
  Then it will provide @class{gtk:tree-list-row} items which can be set via the
  @fun{gtk:tree-expander-list-row} function on the expander. The expander will
  then watch that row item automatically. The @fun{gtk:tree-expander-child}
  function sets the widget that displays the actual row contents.

  The @class{gtk:tree-expander} widget can be modified with properties such as
  @slot[gtk:tree-expander]{indent-for-icon},
  @slot[gtk:tree-expander]{indent-for-depth}, and
  @slot[gtk:tree-expander]{hide-expander} properties to achieve a different
  appearance. This can even be done to influence individual rows, for example
  by binding the @slot[gtk:tree-expander]{hide-expander} property to the item
  count of the model of the treelistrow, to hide the expander for rows without
  children, even if the row is expandable.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:tree-expander} implementation has zero or one CSS nodes with
    the name @code{expander} that should display the expander icon. The node
    will be @code{:checked} when it is expanded. If the node is not expandable,
    an @code{indent} node will be displayed instead. For every level of depth,
    another @code{indent} node is prepended.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    Until GTK 4.10, the @class{gtk:tree-expander} implementation used the
    @code{:group} role. Since GTK 4.12, the @class{gtk:tree-expander}
    implementation uses the @code{:button} role of the
    @symbol{gtk:accessible-role} enumeration. Toggling it will change the
    @code{:expanded} value of the @symbol{gtk:accessible-state} enumeration.
  @end{dictionary}
  @see-constructor{gtk:tree-expander-new}
  @see-slot{gtk:tree-expander-child}
  @see-slot{gtk:tree-expander-hide-expander}
  @see-slot{gtk:tree-expander-indent-for-depth}
  @see-slot{gtk:tree-expander-indent-for-icon}
  @see-slot{gtk:tree-expander-item}
  @see-slot{gtk:tree-expander-list-row}
  @see-class{gtk:tree-list-model}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:tree-expander-child ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'tree-expander) t)
 "The @code{child} property of type @class{gtk:widget} (Read / Write) @br{}
  The child widget with the actual contents.")

#+liber-documentation
(setf (liber:alias-for-function 'tree-expander-child)
      "Accessor"
      (documentation 'tree-expander-child 'function)
 "@version{2025-4-16}
  @syntax{(gtk:tree-expander-child object) => child}
  @syntax{(setf (gtk:tree-expander-child object) child)}
  @argument[object]{a @class{gtk:tree-expander} object}
  @argument[child]{a @class{gtk:widget} object}
  @begin{short}
    Accessor of the @slot[gtk:tree-expander]{child} slot of the
    @class{gtk:tree-expander} class.
  @end{short}
  The @fun{gtk:tree-expander-child} function gets the child widget displayed by
  @arg{object}. The @setf{gtk:tree-expander-child} function sets the content
  widget to display.
  @see-class{gtk:tree-expander}
  @see-class{gtk:widget}")

;;; --- gtk:tree-expander-hide-expander ----------------------------------------

#+(and gtk-4-10 liber-documentation)
(setf (documentation (liber:slot-documentation "hide-expander"
                                               'tree-expander) t)
 "The @code{hide-expander} property of type @code{:boolean} (Read / Write) @br{}
  Whether the expander icon should be hidden in a @class{gtk:tree-list-row}
  object. Note that this property simply hides the icon. The actions and
  keybinding, that is, collapse and expand, are not affected by this property.
  A common use for this property would be to bind to the number of children in
  the model of a @class{gtk:tree-list-row} object, to hide the expander when
  a row has no children. Since 4.10 @br{}
  Default value: @em{false}")

#+(and gtk-4-10 liber-documentation)
(setf (liber:alias-for-function 'tree-expander-hide-expander)
      "Accessor"
      (documentation 'tree-expander-hide-expander 'function)
 "@version{2025-4-16}
  @syntax{(gtk:tree-expander-hide-expander object) => setting}
  @syntax{(setf (gtk:tree-expander-hide-expander object) setting)}
  @argument[object]{a @class{gtk:tree-expander} object}
  @argument[setting]{a boolean whether the expander icon should be hidden}
  @begin{short}
    Accessor of the @slot[gtk:tree-expander]{hide-expander} slot of the
    @class{gtk:tree-expander} class.
  @end{short}
  The @fun{gtk:tree-expander-hide-expander} function gets whether the
  tree expander should be hidden in a @class{gtk:tree-list-row} object. The
  @setf{gtk:tree-expander-hide-expander} function sets whether the expander icon
  should be visible in a @class{gtk:tree-list-row} object.

  Since 4.10
  @see-class{gtk:tree-expander}
  @see-class{gtk:tree-list-row}")

;;; --- gtk:tree-expander-indent-for-depth -------------------------------------

#+(and gtk-4-10 liber-documentation)
(setf (documentation (liber:slot-documentation "indent-for-depth"
                                               'tree-expander) t)
 "The @code{indent-for-depth} property of type @code{:boolean} (Read / Write)
  @br{}
  The tree expander indents the child according to its depth. Since 4.10 @br{}
  Default value: @em{true}")

#+(and gtk-4-10 liber-documentation)
(setf (liber:alias-for-function 'tree-expander-indent-for-depth)
      "Accessor"
      (documentation 'tree-expander-indent-for-depth 'function)
 "@version{2025-4-16}
  @syntax{(gtk:tree-expander-indent-for-depth object) => setting}
  @syntax{(setf (gtk:tree-expander-indent-for-depth object) setting)}
  @argument[object]{a @class{gtk:tree-expander} object}
  @argument[setting]{a boolean whether the expander indents the child according
    to its depth}
  @begin{short}
    Accessor of the @slot[gtk:tree-expander]{indent-for-depth} slot of the
    @class{gtk:tree-expander} class.
  @end{short}
  The @fun{gtk:tree-expander-indent-for-depth} function gets whether the tree
  expander indents each level of depth with an additional indent. The
  @setf{gtk:tree-expander-indent-for-depth} function sets if the tree expander
  should indent the child according to its depth.

  Since 4.10
  @see-class{gtk:tree-expander}")

;;; --- gtk:tree-expander-indent-for-icon --------------------------------------

#+(and gtk-4-6 liber-documentation)
(setf (documentation (liber:slot-documentation "indent-for-icon"
                                               'tree-expander) t)
 "The @code{indent-for-icon} property of type @code{:boolean} (Read / Write)
  @br{}
  The tree expander indents the child by the width of an expander-icon if it is
  not expandable. Since 4.6 @br{}
  Default value: @em{true}")

#+(and gtk-4-6 liber-documentation)
(setf (liber:alias-for-function 'tree-expander-indent-for-icon)
      "Accessor"
      (documentation 'tree-expander-indent-for-icon 'function)
 "@version{2025-4-16}
  @syntax{(gtk:tree-expander-indent-for-icon object) => setting}
  @syntax{(setf (gtk:tree-expander-indent-for-icon object) setting)}
  @argument[object]{a @class{gtk:tree-expander} object}
  @argument[setting]{@em{true} if the child should be indented without expander,
    otherwise @em{false}}
  @begin{short}
    Accessor of the @slot[gtk:tree-expander]{indent-for-icon} slot of the
    @class{gtk:tree-expander} class.
  @end{short}
  The @fun{gtk:tree-expander-indent-for-icon} function gets whether the tree
  expander indents the child by the width of an expander-icon if it is not
  expandable. The @setf{gtk:tree-expander-indent-for-icon} function sets whether
  the tree expander should indent the child by the width of an expander-icon
  when it is not expandable.

  Since 4.6
  @see-class{gtk:tree-expander}")

;;; --- gtk:tree-expander-item -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "item" 'tree-expander) t)
 "The @code{item} property of type @class{g:object} (Read) @br{}
  The item held by this expander’s row.")

#+liber-documentation
(setf (liber:alias-for-function 'tree-expander-item)
      "Accessor"
      (documentation 'tree-expander-item 'function)
 "@version{2025-4-16}
  @syntax{(gtk:tree-expander-item object) => item}
  @argument[object]{a @class{gtk:tree-expander} object}
  @argument[item]{a @class{g:object} instance for the item of the row}
  @begin{short}
    Accessor of the @slot[gtk:tree-expander]{item} slot of the
    @class{gtk:tree-expander} class.
  @end{short}
  The @fun{gtk:tree-expander-item} function forwards the item set on the
  @class{gtk:tree-list-row} object that @arg{object} is managing. This call is
  essentially equivalent to calling:
  @begin{pre}
(gtk:tree-list-row-item (gtk:tree-expander-list-row object))
  @end{pre}
  @see-class{gtk:tree-expander}
  @see-class{g:object}")

;;; --- gtk:tree-expander-list-row ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "list-row" 'tree-expander) t)
 "The @code{list-row} property of type @class{gtk:tree-list-row}
  (Read / Write) @br{}
  The list row to track for expander state.")

#+liber-documentation
(setf (liber:alias-for-function 'tree-expander-list-row)
      "Accessor"
      (documentation 'tree-expander-list-row 'function)
 "@version{2025-4-16}
  @syntax{(gtk:tree-expander-list-row object) => row}
  @syntax{(setf (gtk:tree-expander-list-row object) row)}
  @argument[object]{a @class{gtk:tree-expander} object}
  @argument[row]{a @class{gtk:tree-list-row} object}
  @begin{short}
    Accessor of the @slot[gtk:tree-expander]{list-row} slot of the
    @class{gtk:tree-expander} class.
  @end{short}
  The @fun{gtk:tree-expander-list-row} function gets the list row managed by
  @arg{object}. The @setf{gtk:tree-expander-list-row} function sets the tree
  list row that this expander should manage.
  @see-class{gtk:tree-expander}
  @see-class{gtk:tree-list-row}")

;;; ----------------------------------------------------------------------------
;;; gtk_tree_expander_new
;;; ----------------------------------------------------------------------------

(declaim (inline tree-expander-new))

(defun tree-expander-new ()
 #+liber-documentation
 "@version{2025-4-16}
  @return{The new @class{gtk:tree-expander} widget.}
  @short{Creates a new tree expander.}
  @see-class{gtk:tree-expander}"
  (make-instance 'tree-expander))

(export 'tree-expander-new)

;;; --- End of file gtk4.tree-expander.lisp ------------------------------------
