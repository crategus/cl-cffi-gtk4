;;; ----------------------------------------------------------------------------
;;; gtk4.tree-expander.lisp
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
;;;     gtk_tree_expander_get_hide_expander                Since 4.10
;;;     gtk_tree_expander_set_hide_expander                Since 4.10
;;;     gtk_tree_expander_get_indent-for-depth             Since 4.10
;;;     gtk_tree_expander_set_indent-for-depth             Since 4.10
;;;     gtk_tree_expander_get_indent-for-icon              Since 4.6
;;;     gtk_tree_expander_set_indent-for-icon              Since 4.6
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
;;;     hide-expander                                      Since 4.10
;;;     indent-for-depth                                   Since 4.10
;;;     indent-for-icon                                    Since 4.6
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

(gobject:define-g-object-class "GtkTreeExpander" tree-expander
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
 "@version{#2023-9-10}
  @begin{short}
    The @class{gtk:tree-expander} widget is a widget that provides an expander
    for a list.
  @end{short}
  It is typically placed as a bottommost child into a @class{gtk:list-view}
  widget to allow users to expand and collapse children in a list with a
  @class{gtk:tree-list-model} object. The @class{gtk:tree-expander} widget
  provides the common UI elements, gestures and keybindings for this purpose.

  On top of this, the \"listitem.expand\", \"listitem.collapse\" and
  \"listitem.toggle-expand\" actions are provided to allow adding custom UI for
  managing expanded state.

  It is important to mention that you want to set the
  @slot[gkt:list-item]{focusable} property to @em{false} when using this widget,
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

;;; --- tree-expander-child ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'tree-expander) t)
 "The @code{child} property of type @class{gtk:widget} (Read / Write) @br{}
  The child widget with the actual contents.")


;;; --- tree-expander-hide-expander --------------------------------------------

#+(and gtk-4-10 liber-documentation)
(setf (documentation (liber:slot-documentation "hide-expander"
                                               'tree-expander) t)
 "The @code{hide-expander} property of type @code{:boolean} (Read / Write) @br{}
  Whether the expander icon should be hidden in a @class{gtk:tree-list-row}
  object. Note that this property simply hides the icon. The actions and
  keybinding (i.e. collapse and expand) are not affected by this property. A
  common use for this property would be to bind to the number of children in a
  @class{gtk:tree-list-row} object’s model in order to hide the expander when a
  row has no children. Since 4.10 @br{}
  Default value: @em{false}")


;;; --- tree-expander-indent-for-depth -----------------------------------------

#+(and gtk-4-10 liber-documentation)
(setf (documentation (liber:slot-documentation "indent-for-depth"
                                               'tree-expander) t)
 "The @code{indent-for-depth} property of type @code{:boolean} (Read / Write)
  @br{}
  The tree expander indents the child according to its depth. Since 4.10 @br{}
  Default value: @em{true}")


;;; --- tree-expander-indent-for-icon ------------------------------------------

#+(and gtk-4-6 liber-documentation)
(setf (documentation (liber:slot-documentation "indent-for-icon"
                                               'tree-expander) t)
 "The @code{indent-for-icon} property of type @code{:boolean} (Read / Write)
  @br{}
  The tree expander indents the child by the width of an expander-icon if it is
  not expandable. Since 4.6 @br{}
  Default value: @em{true}")


;;; --- tree-expander-item -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "item" 'tree-expander) t)
 "The @code{item} property of type @class{g:object} (Read) @br{}
  The item held by this expander’s row.")


;;; --- tree-expander-list-row -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "list-row" 'tree-expander) t)
 "The @code{list-row} property of type @class{gtk:tree-list-row}
  (Read / Write) @br{}
  The list row to track for expander state.")


;;; ----------------------------------------------------------------------------
;;; gtk_tree_expander_new ()
;;;
;;; GtkWidget *
;;; gtk_tree_expander_new (void);
;;;
;;; Creates a new GtkTreeExpander
;;;
;;; Returns :
;;;     a new GtkTreeExpander
;;; ----------------------------------------------------------------------------


;;;gtk_tree_expander_get_child ()
;;;GtkWidget *
;;;gtk_tree_expander_get_child (GtkTreeExpander *self);
;;;Gets the child widget displayed by self .

;;;Parameters
;;;self

;;;a GtkTreeExpander

;;;Returns
;;;The child displayed by self .


;;;gtk_tree_expander_set_child ()
;;;void
;;;gtk_tree_expander_set_child (GtkTreeExpander *self,
;;;                             GtkWidget *child);
;;;Sets the content widget to display.

;;;self
;;;a GtkTreeExpander widget

;;;child
;;;a GtkWidget, or NULL.


;;;gtk_tree_expander_get_item ()
;;;gpointer
;;;gtk_tree_expander_get_item (GtkTreeExpander *self);
;;;Forwards the item set on the GtkTreeListRow that self is managing.

;;;This call is essentially equivalent to calling:

;;;treeexpander
;;;├── [indent]*
;;;├── [expander]
;;;╰── <child>
;;;Parameters
;;;self

;;;a GtkTreeExpander

;;;Returns
;;;The item of the row.


;;;gtk_tree_expander_get_list_row ()
;;;GtkTreeListRow *
;;;gtk_tree_expander_get_list_row (GtkTreeExpander *self);
;;;Gets the list row managed by self .

;;;self
;;;a GtkTreeExpander

;;;Returns
;;;The list row displayed by self .


;;;gtk_tree_expander_set_list_row ()
;;;void
;;;gtk_tree_expander_set_list_row (GtkTreeExpander *self,
;;;                                GtkTreeListRow *list_row);
;;;Sets the tree list row that this expander should manage.

;;;self
;;;a GtkTreeExpander widget

;;;list_row
;;;a GtkTreeListRow, or NULL.


;;; --- End of file gtk4.tree-expander.lisp ------------------------------------
