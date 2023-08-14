;;; ----------------------------------------------------------------------------
;;; gtk4.drop-down.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
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
;;; GtkDropDown
;;;
;;;     Choose an item from a list
;;;
;;; Types and Values
;;;
;;;     GtkDropDown
;;;
;;; Accessors
;;;
;;;     gtk_drop_down_set_enable_search
;;;     gtk_drop_down_get_enable_search
;;;     gtk_drop_down_set_expression
;;;     gtk_drop_down_get_expression
;;;     gtk_drop_down_set_factory
;;;     gtk_drop_down_get_factory
;;;     gtk_drop_down_set_list_factory
;;;     gtk_drop_down_get_list_factory
;;;     gtk_drop_down_set_model
;;;     gtk_drop_down_get_model
;;;     gtk_drop_down_set_selected
;;;     gtk_drop_down_get_selected
;;;     gtk_drop_down_get_selected_item
;;;     gtk_drop_down_set_show_arrow
;;;     gtk_drop_down_get_show_arrow
;;;
;;; Functions
;;;
;;;     gtk_drop_down_new
;;;     gtk_drop_down_new_from_strings
;;;
;;; Properties
;;;
;;;     enable-search
;;;     expression
;;;     factory
;;;     header-factory                                     Since 4.12
;;;     list-factory
;;;     model
;;;     search-match-mode                                  Since 4.12
;;;     selected
;;;     selected-item
;;;     show-arrow                                         Since 4.6
;;;
;;; Signals
;;;
;;;     activate                                           Since 4.6
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkDropDown
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkDropDown
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkDropDown" drop-down
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_drop_down_get_type")
  ((enable-search
    drop-down-enable-search
    "enable-search" "gboolean" t t)
   (expression
    drop-down-expression
    "expression" "GtkExpression" t t)
   (factory
    drop-down-factory
    "factory" "GtkListItemFactory" t t)
   #+gtk-4-12
   (header-factory
    drop-down-header-factory
    "header-factory" "GtkListItemFactory" t t)
   (list-factory
    drop-down-list-factory
    "list-factory" "GtkListItemFactory" t t)
   (model
    drop-down-model
    "model" "GListModel" t t)
   #+gtk-4-12
   (search-match-mode
    drop-down-search-match-mode
    "search-match-mode" "GtkStringFilterMatchMode" t t)
   (selected
    drop-down-selected
    "selected" "guint" t t)
   (selected-item
    drop-down-selected-item
    "selected-item" "GObject" t nil)
   #+gtk-4-6
   (show-arrow
    drop-down-show-arrow
    "show-arrow" "gboolean" t t)))

;;;Description

;;;GtkDropDown is a widget that allows the user to choose an item from a list of options. The GtkDropDown displays the selected choice.

;;;The options are given to GtkDropDown in the form of GListModel, and how the individual options are represented is determined by a GtkListItemFactory. The default factory displays simple strings.

;;;GtkDropDown knows how to obtain strings from the items in a GtkStringList; for other models, you have to provide an expression to find the strings via gtk_drop_down_set_expression().

;;;GtkDropDown can optionally allow search in the popup, which is useful if the list of options is long. To enable the search entry, use gtk_drop_down_set_enable_search().

;;;CSS nodes
;;;GtkDropDown has a single CSS node with name dropdown, with the button and popover nodes as children.

;;;Accessibility
;;;GtkDropDown uses the GTK_ACCESSIBLE_ROLE_COMBO_BOX role.

;;; Signals

;;;Gtk
;;;DropDown
;;;::activate
;;;[−]

;;;Declaration
;;;void
;;;activate (
;;;  GtkDropDown* self,
;;;  gpointer user_data
;;;)


;;;Emitted to when the drop down is activated.

;;;The ::activate signal on GtkDropDown is an action signal and emitting it causes the drop down to pop up its dropdown.

;;;Default handler:
;;;The default handler is called before the handlers added via g_signal_connect().

;;;The signal can be emitted directly
;;;Available since:

;;;See Also
;;;GtkComboBox



;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;;Property
;;;Gtk
;;;DropDown
;;;:show-arrow
;;;[−]
;;;Declaration
;;;property show-arrow: gboolean [ read, write ]
;;;[−]
;;;Description
;;;[src]
;;;Whether to show an arrow within the GtkDropDown widget.

;;;Type:	gboolean
;;;Available since:	4.6
;;;Getter method	gtk_drop_down_get_show_arrow()
;;;Setter method	gtk_drop_down_set_show_arrow()
;;;[−]
;;;Flags
;;;Readable	yes
;;;Writable	yes
;;;Construct	no
;;;Construct only	no


;;; ----------------------------------------------------------------------------
;;;The “enable-search” property
;;;
;;;  “enable-search”            gboolean
;;;
;;; Whether to show a search entry in the popup.
;;;
;;; Note that search requires “expression” to be set.
;;;
;;; Owner: GtkDropDown
;;;
;;; Flags: Read / Write
;;;
;;; Default value: FALSE
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “expression” property
;;;
;;;  “expression”               GtkExpression *
;;;
;;; An expression to evaluate to obtain strings to match against the search term
;;; (see “enable-search”). If “factory” is not set, the expression is also used
;;; to bind strings to labels produced by a default factory.
;;;
;;;
;;; Owner: GtkDropDown
;;;
;;; Flags: Read / Write
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “factory” property
;;;
;;;  “factory”                  GtkListItemFactory *
;;;
;;; Factory for populating list items.
;;;
;;; Owner: GtkDropDown
;;;
;;; Flags: Read / Write
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “list-factory” property
;;;
;;;  “list-factory”             GtkListItemFactory *
;;;
;;; The factory for populating list items in the popup.
;;;
;;; If this is not set, “factory” is used.
;;;
;;; Owner: GtkDropDown
;;;
;;; Flags: Read / Write
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “model” property
;;;
;;;  “model”                    GListModel *
;;;
;;; Model for the displayed items.
;;;
;;; Owner: GtkDropDown
;;;
;;; Flags: Read / Write
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “selected” property
;;;
;;;  “selected”                 guint
;;;
;;; The position of the selected item in “model”, or GTK_INVALID_LIST_POSITION
;;; if no item is selected.
;;;
;;; Owner: GtkDropDown
;;;
;;; Flags: Read / Write
;;;
;;; Default value: 4294967295
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; The “selected-item” property
;;;
;;;  “selected-item”            GObject *
;;;
;;; The selected item.
;;;
;;; Owner: GtkDropDown
;;;
;;; Flags: Read
;;; ----------------------------------------------------------------------------




;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_new ()
;;;
;;; GtkWidget *
;;; gtk_drop_down_new (GListModel *model,
;;;                    GtkExpression *expression);
;;;
;;; Creates a new GtkDropDown.
;;;
;;; You may want to call gtk_drop_down_set_factory() to set up a way to map its
;;; items to widgets.
;;;
;;; model :
;;;     the model to use or NULL for none.
;;;
;;; expression :
;;;     the expression to use or NULL for none.
;;;
;;; Returns :
;;;     a new GtkDropDown
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_new_from_strings ()
;;;
;;; GtkWidget *
;;; gtk_drop_down_new_from_strings (const char * const *strings);
;;;
;;; Creates a new GtkDropDown that is populated with the strings in strings .
;;;
;;; strings :
;;;     The strings to put in the dropdown.
;;;
;;; Returns :
;;;     a new GtkDropDown
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_set_model ()
;;;
;;; void
;;; gtk_drop_down_set_model (GtkDropDown *self,
;;;                          GListModel *model);
;;;
;;; Sets the GListModel to use.
;;;
;;; self :
;;;     a GtkDropDown
;;;
;;; model :
;;;     the model to use or NULL for none.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_get_model ()
;;;
;;; GListModel *
;;; gtk_drop_down_get_model (GtkDropDown *self);
;;;
;;; Gets the model that provides the displayed items.
;;;
;;; self :
;;;     a GtkDropDown
;;;
;;; Returns :
;;;     The model in use.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_set_selected ()
;;;
;;; void
;;; gtk_drop_down_set_selected (GtkDropDown *self,
;;;                             guint position);
;;;
;;; Selects the item at the given position.
;;;
;;; self :
;;;     a GtkDropDown
;;;
;;; position :
;;;     the position of the item to select, or GTK_INVALID_LIST_POSITION
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_get_selected ()
;;;
;;; guint
;;; gtk_drop_down_get_selected (GtkDropDown *self);
;;;
;;; Gets the position of the selected item.
;;;
;;; self :
;;;     a GtkDropDown
;;;
;;; Returns :
;;;     the position of the selected item, or GTK_INVALID_LIST_POSITION if not
;;;     item is selected
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_get_selected_item ()
;;;
;;; gpointer
;;; gtk_drop_down_get_selected_item (GtkDropDown *self);
;;;
;;; Gets the selected item. If no item is selected, NULL is returned.
;;;
;;; self :
;;;     a GtkDropDown
;;;
;;; Returns :
;;;     The selected item.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_set_factory ()
;;;
;;; void
;;; gtk_drop_down_set_factory (GtkDropDown *self,
;;;                            GtkListItemFactory *factory);
;;;
;;; Sets the GtkListItemFactory to use for populating list items.
;;;
;;; self :
;;;     a GtkDropDown
;;;
;;; factory :
;;;     the factory to use or NULL for none.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_get_factory ()
;;;
;;; GtkListItemFactory *
;;; gtk_drop_down_get_factory (GtkDropDown *self);
;;;
;;; Gets the factory that's currently used to populate list items.
;;;
;;; The factory returned by this function is always used for the item in the
;;; button. It is also used for items in the popup if “list-factory” is not set.
;;;
;;; self :
;;;     a GtkDropDown
;;;
;;; Returns :
;;;     The factory in use.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_set_list_factory ()
;;;
;;; void
;;; gtk_drop_down_set_list_factory (GtkDropDown *self,
;;;                                 GtkListItemFactory *factory);
;;;
;;; Sets the GtkListItemFactory to use for populating list items in the popup.
;;;
;;; self :
;;;     a GtkDropDown
;;;
;;; factory :
;;;     the factory to use or NULL for none.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_get_list_factory ()
;;;
;;; GtkListItemFactory *
;;; gtk_drop_down_get_list_factory (GtkDropDown *self);
;;;
;;; Gets the factory that's currently used to populate list items in the popup.
;;;
;;; self :
;;;     a GtkDropDown
;;;
;;; Returns :
;;;     The factory in use.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_set_expression ()
;;;
;;; void
;;; gtk_drop_down_set_expression (GtkDropDown *self,
;;;                               GtkExpression *expression);
;;;
;;; Sets the expression that gets evaluated to obtain strings from items when
;;; searching in the popup. The expression must have a value type of
;;; G_TYPE_STRING.
;;;
;;; self :
;;;     a GtkDropDown
;;;
;;; expression :
;;;     a GtkExpression, or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_get_expression ()
;;;
;;; GtkExpression *
;;; gtk_drop_down_get_expression (GtkDropDown *self);
;;;
;;; Gets the expression set with gtk_drop_down_set_expression().
;;;
;;; self :
;;;     a GtkDropDown
;;;
;;; Returns :
;;;     a GtkExpression or NULL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_set_enable_search ()
;;;
;;; void
;;; gtk_drop_down_set_enable_search (GtkDropDown *self,
;;;                                  gboolean enable_search);
;;;
;;; Sets whether a search entry will be shown in the popup that allows to search
;;; for items in the list.
;;;
;;; Note that “expression” must be set for search to work.
;;;
;;; self :
;;;     a GtkDropDown
;;;
;;; enable_search :
;;;     whether to enable search
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_drop_down_get_enable_search ()
;;;
;;; gboolean
;;; gtk_drop_down_get_enable_search (GtkDropDown *self);
;;;
;;; Returns whether search is enabled.
;;;
;;; self :
;;;     a GtkDropDown
;;;
;;; Returns :
;;;     TRUE if the popup includes a search entry
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk3.drop-down.lisp ----------------------------------------
