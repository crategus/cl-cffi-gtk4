;;; ----------------------------------------------------------------------------
;;; gtk4.list-view.lisp
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
;;; GtkListView
;;;
;;;     A widget for displaying lists
;;;
;;; Types and Values
;;;
;;;     GtkListBase
;;;     GtkListView
;;;
;;; Functions
;;;
;;;     gtk_list_view_new
;;;     gtk_list_view_set_factory
;;;     gtk_list_view_get_factory
;;;     gtk_list_view_set_model
;;;     gtk_list_view_get_model
;;;     gtk_list_view_set_show_separators
;;;     gtk_list_view_get_show_separators
;;;     gtk_list_view_set_single_click_activate
;;;     gtk_list_view_get_single_click_activate
;;;     gtk_list_view_set_enable_rubberband
;;;     gtk_list_view_get_enable_rubberband
;;;
;;; Properties (GtkListBase)
;;;
;;;     orientation
;;;
;;; Properties (GtkListView)
;;;
;;;     enable-rubberband
;;;     factory
;;;     model
;;;     show-separators
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
;;;                 ╰── GtkListView
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
;;; GtkListBase
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkListBase" list-base
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkOrientable"
                "GtkScrollable")
   :type-initializer "gtk_list_base_get_type")
  ((orientation
    list-base-orientation
    "orientation" "GtkOrientation" t t)))

;;; ----------------------------------------------------------------------------
;;; GtkListView
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkListView" list-view
  (:superclass list-base
   :export t
   :interfaces nil
   :type-initializer "gtk_list_view_get_type")
  ((enable-rubberband
    list-view-enable-rubberband
    "enable-rubberband" "gboolean" t t)
   (factory
    list-view-factory
    "factory" "GtkListItemFactory" t t)
   #+gtk-4-12
   (header-factory
    list-view-header-factory
    "header-factory" "GtkListItemFactory" t t)
   (model
    list-view-model
    "model" "GtkSelectionModel" t t)
   (show-separators
    list-view-show-separators
    "show-separators" "gboolean" t t)
   (single-click-activate
    list-view-single-click-activate
    "single-click-activate" "gboolean" t t)
   #+gtk-4-12
   (tab-behavior
    list-view-tab-behavior
    "tab-behavior" "GtkListTabBehavior" t t)))


;;;typedef struct _GtkListView GtkListView;
;;;GtkListView is the simple list implementation for GTK's list widgets.

;;;Property Details
;;;The “enable-rubberband” property
;;;  “enable-rubberband”        gboolean
;;;Allow rubberband selection

;;;Owner: GtkListView

;;;Flags: Read / Write

;;;Default value: FALSE

;;;The “factory” property
;;;  “factory”                  GtkListItemFactory *
;;;Factory for populating list items

;;;Owner: GtkListView

;;;Flags: Read / Write

;;;The “model” property
;;;  “model”                    GtkSelectionModel *
;;;Model for the items displayed

;;;Owner: GtkListView

;;;Flags: Read / Write

;;;The “show-separators” property
;;;  “show-separators”          gboolean
;;;Show separators between rows

;;;Owner: GtkListView

;;;Flags: Read / Write

;;;Default value: FALSE

;;;The “single-click-activate” property
;;;  “single-click-activate”    gboolean
;;;Activate rows on single click and select them on hover

;;;Owner: GtkListView

;;;Flags: Read / Write

;;;Default value: FALSE

;;;Signal Details
;;;The “activate” signal
;;;void
;;;user_function (GtkListView *self,
;;;               guint        position,
;;;               gpointer     user_data)
;;;The ::activate signal is emitted when a row has been activated by the user, usually via activating the GtkListView|list.activate-item action.

;;;This allows for a convenient way to handle activation in a listview. See gtk_list_item_set_activatable() for details on how to use this signal.

;;;Parameters
;;;self

;;;The GtkListView

;;;position

;;;position of item to activate

;;;user_data

;;;user data set when the signal handler was connected.

;;;Flags: Run Last

;;;Action Details
;;;The “list.activate-item” action
;;;Activates the item given in position by emitting the GtkListView::activate signal.

;;;Parameter type: u

;;;Parameters
;;;position

;;;position of item to activate

;;;See Also
;;;GtkSelectionModel, GtkColumnView, GtkGridView


;;;Includes
;;;#include <gtk/gtk.h>
;;;Description
;;;GtkListView is a widget to present a view into a large dynamic list of items.

;;;GtkListView uses its factory to generate one row widget for each visible item and shows them in a linear display, either vertically or horizontally. The “show-separators” property offers a simple way to display separators between the rows.

;;;GtkListView allows the user to select items according to the selection characteristics of the model. For models that allow multiple selected items, it is possible to turn on _rubberband selection_, using “enable-rubberband”.

;;;If you need multiple columns with headers, see GtkColumnView.

;;;To learn more about the list widget framework, see the overview.

;;;An example of using GtkListView:

;;;CSS nodes
;;;static void
;;;setup_listitem_cb (GtkListItemFactory *factory,
;;;                   GtkListItem        *list_item)
;;;{
;;;  GtkWidget *image;

;;;  image = gtk_image_new ();
;;;  gtk_image_set_icon_size (GTK_IMAGE (image), GTK_ICON_SIZE_LARGE);
;;;  gtk_list_item_set_child (list_item, image);
;;;}

;;;static void
;;;bind_listitem_cb (GtkListItemFactory *factory,
;;;                  GtkListItem        *list_item)
;;;{
;;;  GtkWidget *image;
;;;  GAppInfo *app_info;

;;;  image = gtk_list_item_get_child (list_item);
;;;  app_info = gtk_list_item_get_item (list_item);
;;;  gtk_image_set_from_gicon (GTK_IMAGE (image), g_app_info_get_icon (app_info));
;;;}

;;;static void
;;;activate_cb (GtkListView  *list,
;;;             guint         position,
;;;             gpointer      unused)
;;;{
;;;  GAppInfo *app_info;

;;;  app_info = g_list_model_get_item (G_LIST_MODEL (gtk_list_view_get_model (list)), position);
;;;  g_app_info_launch (app_info, NULL, NULL, NULL);
;;;  g_object_unref (app_info);
;;;}

;;;...

;;;  model = create_application_list ();

;;;  factory = gtk_signal_list_item_factory_new ();
;;;  g_signal_connect (factory, "setup", G_CALLBACK (setup_listitem_cb), NULL);
;;;  g_signal_connect (factory, "bind", G_CALLBACK (bind_listitem_cb), NULL);

;;;  list = gtk_list_view_new (GTK_SELECTION_MODEL (gtk_single_selection_new (model)), factory);

;;;  g_signal_connect (list, "activate", G_CALLBACK (activate_cb), NULL);

;;;  gtk_scrolled_window_set_child (GTK_SCROLLED_WINDOW (sw), list);
;;;GtkListView uses a single CSS node named listview. It may carry the .separators style class, when “show-separators” property is set. Each child widget uses a single CSS node named row. For rubberband selection, a node with name rubberband is used.

;;;The main listview node may also carry style classes to select the style of list presentation: .rich-list, .navigation-sidebar or .data-table.

;;;Accessibility
;;;GtkListView uses the GTK_ACCESSIBLE_ROLE_LIST role, and the list items use the GTK_ACCESSIBLE_ROLE_LIST_ITEM role.

;;; ----------------------------------------------------------------------------
;;; gtk_list_view_new ()
;;;
;;; GtkWidget *
;;; gtk_list_view_new (GtkSelectionModel *model,
;;;                    GtkListItemFactory *factory);
;;;
;;; Creates a new GtkListView that uses the given factory for mapping items to 
;;; widgets.
;;;
;;; The function takes ownership of the arguments, so you can write code like
;;;
;;;  list_view = gtk_list_view_new (create_model(),
;;;    gtk_builder_list_item_factory_new_from_resource ("/resource.ui"));
;;;
;;; model :
;;;     the model to use, or NULL.
;;;
;;; factory :
;;;     The factory to populate items with, or NULL.
;;;
;;; Returns :
;;;     a new GtkListView using the given model and factory
;;; ----------------------------------------------------------------------------

(declaim (inline list-view-new))

(defun list-view-new (model factory)
  (make-instance 'list-view
                 :model model
                 :factory factory))

(export 'list-view-new)

;;; ----------------------------------------------------------------------------
;;;gtk_list_view_set_factory ()
;;;void
;;;gtk_list_view_set_factory (GtkListView *self,
;;;                           GtkListItemFactory *factory);
;;;Sets the GtkListItemFactory to use for populating list items.

;;;Parameters
;;;self

;;;a GtkListView

;;;factory

;;;the factory to use or NULL for none.

;;;[allow-none][transfer none]
;;;gtk_list_view_get_factory ()
;;;GtkListItemFactory *
;;;gtk_list_view_get_factory (GtkListView *self);
;;;Gets the factory that's currently used to populate list items.

;;;Parameters
;;;self

;;;a GtkListView

;;;Returns
;;;The factory in use.

;;;[nullable][transfer none]

;;;gtk_list_view_set_model ()
;;;void
;;;gtk_list_view_set_model (GtkListView *self,
;;;                         GtkSelectionModel *model);
;;;Sets the GtkSelectionModel to use.

;;;Parameters
;;;self

;;;a GtkListView

;;;model

;;;the model to use or NULL for none.

;;;[allow-none][transfer none]
;;;gtk_list_view_get_model ()
;;;GtkSelectionModel *
;;;gtk_list_view_get_model (GtkListView *self);
;;;Gets the model that's currently used to read the items displayed.

;;;Parameters
;;;self

;;;a GtkListView

;;;Returns
;;;The model in use.

;;;[nullable][transfer none]

;;;gtk_list_view_set_show_separators ()
;;;void
;;;gtk_list_view_set_show_separators (GtkListView *self,
;;;                                   gboolean show_separators);
;;;Sets whether the list box should show separators between rows.

;;;Parameters
;;;self

;;;a GtkListView

;;;show_separators

;;;TRUE to show separators

;;;gtk_list_view_get_show_separators ()
;;;gboolean
;;;gtk_list_view_get_show_separators (GtkListView *self);
;;;Returns whether the list box should show separators between rows.

;;;Parameters
;;;self

;;;a GtkListView

;;;Returns
;;;TRUE if the list box shows separators

;;;gtk_list_view_set_single_click_activate ()
;;;void
;;;gtk_list_view_set_single_click_activate
;;;                               (GtkListView *self,
;;;                                gboolean single_click_activate);
;;;Sets whether rows should be activated on single click and selected on hover.

;;;Parameters
;;;self

;;;a GtkListView

;;;single_click_activate

;;;TRUE to activate items on single click

;;;gtk_list_view_get_single_click_activate ()
;;;gboolean
;;;gtk_list_view_get_single_click_activate
;;;                               (GtkListView *self);
;;;Returns whether rows will be activated on single click and selected on hover.

;;;Parameters
;;;self

;;;a GtkListView

;;;Returns
;;;TRUE if rows are activated on single click

;;;gtk_list_view_set_enable_rubberband ()
;;;void
;;;gtk_list_view_set_enable_rubberband (GtkListView *self,
;;;                                     gboolean enable_rubberband);
;;;Sets whether selections can be changed by dragging with the mouse.

;;;Parameters
;;;self

;;;a GtkListView

;;;enable_rubberband

;;;TRUE to enable rubberband selection

;;;gtk_list_view_get_enable_rubberband ()
;;;gboolean
;;;gtk_list_view_get_enable_rubberband (GtkListView *self);
;;;Returns whether rows can be selected by dragging with the mouse.

;;;Parameters
;;;self

;;;a GtkListView

;;;Returns
;;;TRUE if rubberband selection is enabled


;;; --- End of file gtk4.list-view.lisp ----------------------------------------
