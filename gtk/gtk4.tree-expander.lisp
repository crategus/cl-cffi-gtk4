;;; ----------------------------------------------------------------------------
;;; gtk4.tree-expander.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
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
;;; Functions
;;;
;;;     gtk_tree_expander_new 
;;;     gtk_tree_expander_get_child 
;;;     gtk_tree_expander_set_child 
;;;     gtk_tree_expander_get_item 
;;;     gtk_tree_expander_get_list_row 
;;;     gtk_tree_expander_set_list_row 
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
    

;;;Property Details
;;;The “child” property
;;;  “child”                    GtkWidget *
;;;The child widget with the actual contents

;;;Owner: GtkTreeExpander

;;;Flags: Read / Write

;;;The “item” property
;;;  “item”                     GObject *
;;;The item held by this expander's row

;;;Owner: GtkTreeExpander

;;;Flags: Read

;;;The “list-row” property
;;;  “list-row”                 GtkTreeListRow *
;;;The list row to track for expander state

;;;Owner: GtkTreeExpander

;;;Flags: Read / Write

;;;Action Details
;;;The “listitem.toggle-expand” action
;;;Tries to expand the expander if it was collapsed or collapses it if it was expanded.

;;;The “listitem.collapse” action
;;;Collapses the expander.

;;;The “listitem.expand” action
;;;Expands the expander if it can be expanded.

;;;See Also
;;;GtkTreeListModel


;;;Description
;;;GtkTreeExpander is a widget that provides an expander for a list.

;;;It is typically placed as a bottommost child into a GtkListView to allow users to expand and collapse children in a list with a GtkTreeListModel. It will provide the common UI elements, gestures and keybindings for this purpose.

;;;On top of this, the "listitem.expand", "listitem.collapse" and "listitem.toggle-expand" actions are provided to allow adding custom UI for managing expanded state.

;;;The GtkTreeListModel must be set to not be passthrough. Then it will provide GtkTreeListRow items which can be set via gtk_tree_expander_set_list_row() on the expander. The expander will then watch that row item automatically. gtk_tree_expander_set_child() sets the widget that displays the actual row contents.

;;;CSS nodes
;;;GtkTreeExpander has zero or one CSS nodes with the name "expander" that should display the expander icon. The node will be :checked when it is expanded. If the node is not expandable, an "indent" node will be displayed instead.

;;;For every level of depth, another "indent" node is prepended.

;;;Accessibility
;;;GtkTreeExpander uses the GTK_ACCESSIBLE_ROLE_GROUP role. The expander icon is represented as a GTK_ACCESSIBLE_ROLE_BUTTON, labelled by the expander's child, and toggling it will change the GTK_ACCESSIBLE_STATE_EXPANDED state.

;;;Functions
;;;gtk_tree_expander_new ()
;;;GtkWidget *
;;;gtk_tree_expander_new (void);
;;;Creates a new GtkTreeExpander

;;;Returns
;;;a new GtkTreeExpander

;;;gtk_tree_expander_get_child ()
;;;GtkWidget *
;;;gtk_tree_expander_get_child (GtkTreeExpander *self);
;;;Gets the child widget displayed by self .

;;;Parameters
;;;self

;;;a GtkTreeExpander

;;;Returns
;;;The child displayed by self .

;;;[nullable][transfer none]

;;;gtk_tree_expander_set_child ()
;;;void
;;;gtk_tree_expander_set_child (GtkTreeExpander *self,
;;;                             GtkWidget *child);
;;;Sets the content widget to display.

;;;Parameters
;;;self

;;;a GtkTreeExpander widget

;;;child

;;;a GtkWidget, or NULL.

;;;[nullable]
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

;;;[nullable][transfer full][type GObject]

;;;gtk_tree_expander_get_list_row ()
;;;GtkTreeListRow *
;;;gtk_tree_expander_get_list_row (GtkTreeExpander *self);
;;;Gets the list row managed by self .

;;;Parameters
;;;self

;;;a GtkTreeExpander

;;;Returns
;;;The list row displayed by self .

;;;[nullable][transfer none]

;;;gtk_tree_expander_set_list_row ()
;;;void
;;;gtk_tree_expander_set_list_row (GtkTreeExpander *self,
;;;                                GtkTreeListRow *list_row);
;;;Sets the tree list row that this expander should manage.

;;;Parameters
;;;self

;;;a GtkTreeExpander widget

;;;list_row

;;;a GtkTreeListRow, or NULL.

;;;[nullable]

;;; --- End of file gtk4.tree-expander.lisp ------------------------------------
