;;; ----------------------------------------------------------------------------
;;; gtk4.grid-view.lisp
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
;;; GtkGridView
;;;
;;;     A widget for displaying grids
;;;
;;; Types and Values
;;;
;;;     GtkGridView
;;;
;;; Functions
;;;
;;;     gtk_grid_view_new
;;;     gtk_grid_view_set_model
;;;     gtk_grid_view_get_model
;;;     gtk_grid_view_set_max_columns
;;;     gtk_grid_view_get_max_columns
;;;     gtk_grid_view_set_min_columns
;;;     gtk_grid_view_get_min_columns
;;;     gtk_grid_view_set_single_click_activate
;;;     gtk_grid_view_get_single_click_activate
;;;     gtk_grid_view_set_enable_rubberband
;;;     gtk_grid_view_get_enable_rubberband
;;;     gtk_grid_view_set_factory
;;;     gtk_grid_view_get_factory
;;;
;;; Properties
;;;
;;;     enable-rubberband
;;;     factory
;;;     max-columns
;;;     min-columns
;;;     model
;;;     single-click-activate
;;;
;;; Signals
;;;
;;;     activate
;;;
;;; Actions
;;;
;;;     list.activate-item
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkListBase
;;;                 ╰── GtkGridView
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkOrientable
;;;     GtkScrollable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkGridView
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkGridView" grid-view
  (:superclass list-base
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkOrientable"
                "GtkScrollable")
   :type-initializer "gtk_grid_view_get_type")
  ((enable-rubberband
    grid-view-enable-rubberband
    "enable-rubberband" "gboolean" t t)
   (factory
    grid-view-factory
    "factory" "GtkListItemFactory" t t)
   (max-columns
    grid-view-max-columns
    "max-columns" "guint" t t)
   (min-columns
    grid-view-min-columns
    "min-columns" "guint" t t)
   (model
    grid-view-model
    "model" "GtkSelectionModel"  t t)
   (single-click-activate
    grid-view-single-click-activate
    "single-click-activate" "gboolean" t t)
   #+gtk-4-12
   (tab-behavior
    grid-view-tab-behavior
    "tab-behavior" "GtkListTabBehavior" t t)))



;;;GtkGridView is a list widget implementation that arranges its items in a grid.

;;;Description
;;;GtkGridView is a widget to present a view into a large dynamic grid of items.

;;;GtkGridView uses its factory to generate one child widget for each visible item and shows them in a grid. The orientation of the grid view determines if the grid reflows vertically or horizontally.

;;;GtkGridView allows the user to select items according to the selection characteristics of the model. For models that allow multiple selected items, it is possible to turn on _rubberband selection_, using “enable-rubberband”.

;;;To learn more about the list widget framework, see the overview.

;;;CSS nodes
;;;gridview
;;;├── child
;;;│
;;;├── child
;;;│
;;;┊
;;;╰── [rubberband]
;;;GtkGridView uses a single CSS node with name gridview. Each child uses a single CSS node with name child. For rubberband selection, a subnode with name rubberband is used.

;;;Accessibility
;;;GtkGridView uses the GTK_ACCESSIBLE_ROLE_GRID role, and the items use the GTK_ACCESSIBLE_ROLE_GRID_CELL role.


;;;Property Details
;;;The “enable-rubberband” property
;;;  “enable-rubberband”        gboolean
;;;Allow rubberband selection

;;;Owner: GtkGridView

;;;Flags: Read / Write

;;;Default value: FALSE

;;;The “factory” property
;;;  “factory”                  GtkListItemFactory *
;;;Factory for populating list items

;;;Owner: GtkGridView

;;;Flags: Read / Write

;;;The “max-columns” property
;;;  “max-columns”              guint
;;;Maximum number of columns per row

;;;If this number is smaller than GtkGridView:min-columns, that value is used instead.

;;;Owner: GtkGridView

;;;Flags: Read / Write

;;;Allowed values: >= 1

;;;Default value: 7

;;;The “min-columns” property
;;;  “min-columns”              guint
;;;Minimum number of columns per row

;;;Owner: GtkGridView

;;;Flags: Read / Write

;;;Allowed values: >= 1

;;;Default value: 1

;;;The “model” property
;;;  “model”                    GtkSelectionModel *
;;;Model for the items displayed

;;;Owner: GtkGridView

;;;Flags: Read / Write

;;;The “single-click-activate” property
;;;  “single-click-activate”    gboolean
;;;Activate rows on single click and select them on hover

;;;Owner: GtkGridView

;;;Flags: Read / Write

;;;Default value: FALSE

;;;Signal Details
;;;The “activate” signal
;;;void
;;;user_function (GtkGridView *self,
;;;               guint        position,
;;;               gpointer     user_data)
;;;The ::activate signal is emitted when a cell has been activated by the user, usually via activating the GtkGridView|list.activate-item action.

;;;This allows for a convenient way to handle activation in a gridview. See GtkListItem:activatable for details on how to use this signal.

;;;Parameters
;;;self

;;;The GtkGridView

;;;position

;;;position of item to activate

;;;user_data

;;;user data set when the signal handler was connected.

;;;Flags: Run Last

;;;Action Details
;;;The “list.activate-item” action
;;;Activates the item given in position by emitting the GtkGridView::activate signal.

;;;Parameter type: u

;;;Parameters
;;;position

;;;position of item to activate

;;;See Also
;;;GtkSelectionModel, GtkListView, GtkColumnView


;;;Functions
;;;gtk_grid_view_new ()
;;;GtkWidget *
;;;gtk_grid_view_new (GtkSelectionModel *model,
;;;                   GtkListItemFactory *factory);
;;;Creates a new GtkGridView that uses the given factory for mapping items to widgets.

;;;The function takes ownership of the arguments, so you can write code like
;;;  grid_view = gtk_grid_view_new (create_model(),
;;;    gtk_builder_list_item_factory_new_from_resource ("/resource.ui"));

;;;Parameters
;;;model

;;;the model to use, or NULL.

;;;[allow-none][transfer full]
;;;factory

;;;The factory to populate items with, or NULL.

;;;[allow-none][transfer full]
;;;Returns
;;;a new GtkGridView using the given model and factory

;;;gtk_grid_view_set_model ()
;;;void
;;;gtk_grid_view_set_model (GtkGridView *self,
;;;                         GtkSelectionModel *model);
;;;Sets the GtkSelectionModel to use for

;;;Parameters
;;;self

;;;a GtkGridView

;;;model

;;;the model to use or NULL for none.

;;;[allow-none][transfer none]
;;;gtk_grid_view_get_model ()
;;;GtkSelectionModel *
;;;gtk_grid_view_get_model (GtkGridView *self);
;;;Gets the model that's currently used to read the items displayed.

;;;Parameters
;;;self

;;;a GtkGridView

;;;Returns
;;;The model in use.

;;;[nullable][transfer none]

;;;gtk_grid_view_set_max_columns ()
;;;void
;;;gtk_grid_view_set_max_columns (GtkGridView *self,
;;;                               guint max_columns);
;;;Sets the maximum number of columns to use. This number must be at least 1.

;;;If max_columns is smaller than the minimum set via gtk_grid_view_set_min_columns(), that value is used instead.

;;;Parameters
;;;self

;;;a GtkGridView

;;;max_columns

;;;The maximum number of columns

;;;gtk_grid_view_get_max_columns ()
;;;guint
;;;gtk_grid_view_get_max_columns (GtkGridView *self);
;;;Gets the maximum number of columns that the grid will use.

;;;Parameters
;;;self

;;;a GtkGridView

;;;Returns
;;;The maximum number of columns

;;;gtk_grid_view_set_min_columns ()
;;;void
;;;gtk_grid_view_set_min_columns (GtkGridView *self,
;;;                               guint min_columns);
;;;Sets the minimum number of columns to use. This number must be at least 1.

;;;If min_columns is smaller than the minimum set via gtk_grid_view_set_max_columns(), that value is ignored.

;;;Parameters
;;;self

;;;a GtkGridView

;;;min_columns

;;;The minimum number of columns

;;;gtk_grid_view_get_min_columns ()
;;;guint
;;;gtk_grid_view_get_min_columns (GtkGridView *self);
;;;Gets the minimum number of columns that the grid will use.

;;;Parameters
;;;self

;;;a GtkGridView

;;;Returns
;;;The minimum number of columns

;;;gtk_grid_view_set_single_click_activate ()
;;;void
;;;gtk_grid_view_set_single_click_activate
;;;                               (GtkGridView *self,
;;;                                gboolean single_click_activate);
;;;Sets whether items should be activated on single click and selected on hover.

;;;Parameters
;;;self

;;;a GtkGridView

;;;single_click_activate

;;;TRUE to activate items on single click

;;;gtk_grid_view_get_single_click_activate ()
;;;gboolean
;;;gtk_grid_view_get_single_click_activate
;;;                               (GtkGridView *self);
;;;Returns whether items will be activated on single click and selected on hover.

;;;Parameters
;;;self

;;;a GtkListView

;;;Returns
;;;TRUE if items are activated on single click

;;;gtk_grid_view_set_enable_rubberband ()
;;;void
;;;gtk_grid_view_set_enable_rubberband (GtkGridView *self,
;;;                                     gboolean enable_rubberband);
;;;Sets whether selections can be changed by dragging with the mouse.

;;;Parameters
;;;self

;;;a GtkGridView

;;;enable_rubberband

;;;TRUE to enable rubberband selection

;;;gtk_grid_view_get_enable_rubberband ()
;;;gboolean
;;;gtk_grid_view_get_enable_rubberband (GtkGridView *self);
;;;Returns whether rows can be selected by dragging with the mouse.

;;;Parameters
;;;self

;;;a GtkGridView

;;;Returns
;;;TRUE if rubberband selection is enabled

;;;gtk_grid_view_set_factory ()
;;;void
;;;gtk_grid_view_set_factory (GtkGridView *self,
;;;                           GtkListItemFactory *factory);
;;;Sets the GtkListItemFactory to use for populating list items.

;;;Parameters
;;;self

;;;a GtkGridView

;;;factory

;;;the factory to use or NULL for none.

;;;[allow-none][transfer none]
;;;gtk_grid_view_get_factory ()
;;;GtkListItemFactory *
;;;gtk_grid_view_get_factory (GtkGridView *self);
;;;Gets the factory that's currently used to populate list items.

;;;Parameters
;;;self

;;;a GtkGridView

;;;Returns
;;;The factory in use.

;;;[nullable][transfer none]


;;; --- End of file gtk4.grid-view.lisp ----------------------------------------
