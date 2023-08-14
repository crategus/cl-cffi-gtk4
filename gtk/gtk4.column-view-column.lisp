;;; ----------------------------------------------------------------------------
;;; gtk4.column-view.lisp
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
;;; GtkColumnViewColumn
;;;
;;;     The column added to GtkColumnView
;;;
;;; Types and Values
;;;
;;;     GtkColumnViewColumn
;;;
;;; Functions
;;;
;;;     gtk_column_view_column_new
;;;     gtk_column_view_column_get_column_view
;;;     gtk_column_view_column_set_factory
;;;     gtk_column_view_column_get_factory
;;;     gtk_column_view_column_set_title
;;;     gtk_column_view_column_get_title
;;;     gtk_column_view_column_set_sorter
;;;     gtk_column_view_column_get_sorter
;;;     gtk_column_view_column_set_visible
;;;     gtk_column_view_column_get_visible
;;;     gtk_column_view_column_set_resizable
;;;     gtk_column_view_column_get_resizable
;;;     gtk_column_view_column_set_header_menu
;;;     gtk_column_view_column_get_header_menu
;;;     gtk_column_view_column_set_fixed_width
;;;     gtk_column_view_column_get_fixed_width
;;;     gtk_column_view_column_set_expand
;;;     gtk_column_view_column_get_expand
;;;
;;; Properties
;;;
;;;     column-view
;;;     expand
;;;     factory
;;;     fixed-width
;;;     header-menu
;;;     resizable
;;;     sorter
;;;     title
;;;     visible
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkColumnViewColumn
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkColumnViewColumn
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkColumnViewColumn" column-view-column
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_column_view_column_get_type")
  ((column-view
    column-view-column-column-view
    "column-view" "GtkColumnView" t nil)
   (expand
    column-view-column-expand
    "expand" "gboolean" t t)
   (factory
    column-view-column-factory
    "factory" "GtkListItemFactory" t t)
   (fixed-width
    column-view-column-fixed-width
    "fixed-width" "gint" t t)
   (header-menu
    column-view-column-header-menu
    "header-menu" "GMenuModel" t t)
   #+gtk-4-10
   (id
    column-view-column-id
    "id" "gchararray" t t)
   (resizable
    column-view-column-resizable
    "resizable" "gboolean" t t)
   (sorter
    column-view-column-sorter
    "sorter" "GtkSorter" t t)
   (title
    column-view-column-title
    "title" "gchararray" t t)
   (visible
    column-view-column-visible
    "visible" "gboolean" t t)))



;;;GtkColumnViewColumns are added to GtkColumnViews.

;;;Property Details
;;;The “column-view” property
;;;  “column-view”              GtkColumnView *
;;;GtkColumnView this column is a part of

;;;Owner: GtkColumnViewColumn

;;;Flags: Read

;;;The “expand” property
;;;  “expand”                   gboolean
;;;Column gets share of extra width allocated to the view

;;;Owner: GtkColumnViewColumn

;;;Flags: Read / Write

;;;Default value: FALSE

;;;The “factory” property
;;;  “factory”                  GtkListItemFactory *
;;;Factory for populating list items

;;;Owner: GtkColumnViewColumn

;;;Flags: Read / Write

;;;The “fixed-width” property
;;;  “fixed-width”              int
;;;If not -1, this is the width that the column is allocated, regardless of the size of its content.

;;;Owner: GtkColumnViewColumn

;;;Flags: Read / Write

;;;Allowed values: >= -1

;;;Default value: -1

;;;The “header-menu” property
;;;  “header-menu”              GMenuModel *
;;;Menu model used to create the context menu for the column header.

;;;Owner: GtkColumnViewColumn

;;;Flags: Read / Write

;;;The “resizable” property
;;;  “resizable”                gboolean
;;;Whether this column is resizable

;;;Owner: GtkColumnViewColumn

;;;Flags: Read / Write

;;;Default value: FALSE

;;;The “sorter” property
;;;  “sorter”                   GtkSorter *
;;;Sorter for sorting items according to this column

;;;Owner: GtkColumnViewColumn

;;;Flags: Read / Write

;;;The “title” property
;;;  “title”                    char *
;;;Title displayed in the header

;;;Owner: GtkColumnViewColumn

;;;Flags: Read / Write

;;;Default value: NULL

;;;The “visible” property
;;;  “visible”                  gboolean
;;;Whether this column is visible

;;;Owner: GtkColumnViewColumn

;;;Flags: Read / Write

;;;Default value: TRUE

;;;See Also
;;;GtkColumnView


;;;Description
;;;GtkColumnViewColumn represents the columns being added to GtkColumnView.

;;;Columns have a title, and can optionally have a header menu set with gtk_column_view_column_set_header_menu().

;;;A sorter can be associated with a column using gtk_column_view_column_set_sorter(), to let users influence sorting by clicking on the column header.

;;;Functions
;;;gtk_column_view_column_new ()
;;;GtkColumnViewColumn *
;;;gtk_column_view_column_new (const char *title,
;;;                            GtkListItemFactory *factory);
;;;Creates a new GtkColumnViewColumn that uses the given factory for mapping items to widgets.

;;;You most likely want to call gtk_column_add_column() next.

;;;The function takes ownership of the argument, so you can write code like
;;;  column = gtk_column_view_column_new (_("Name"),
;;;    gtk_builder_list_item_factory_new_from_resource ("/name.ui"));

;;;Parameters
;;;title

;;;Title to use for this column.

;;;[nullable]
;;;factory

;;;The factory to populate items with.

;;;[transfer full][nullable]
;;;Returns
;;;a new GtkColumnViewColumn using the given factory

;;;gtk_column_view_column_get_column_view ()
;;;GtkColumnView *
;;;gtk_column_view_column_get_column_view
;;;                               (GtkColumnViewColumn *self);
;;;Gets the column view that's currently displaying this column.

;;;If self has not been added to a column view yet, NULL is returned.

;;;Parameters
;;;self

;;;a GtkColumnViewColumn

;;;Returns
;;;The column view displaying self .

;;;[nullable][transfer none]

;;;gtk_column_view_column_set_factory ()
;;;void
;;;gtk_column_view_column_set_factory (GtkColumnViewColumn *self,
;;;                                    GtkListItemFactory *factory);
;;;Sets the GtkListItemFactory to use for populating list items for this column.

;;;Parameters
;;;self

;;;a GtkColumnViewColumn

;;;factory

;;;the factory to use or NULL for none.

;;;[allow-none][transfer none]
;;;gtk_column_view_column_get_factory ()
;;;GtkListItemFactory *
;;;gtk_column_view_column_get_factory (GtkColumnViewColumn *self);
;;;Gets the factory that's currently used to populate list items for this column.

;;;Parameters
;;;self

;;;a GtkColumnViewColumn

;;;Returns
;;;The factory in use.

;;;[nullable][transfer none]

;;;gtk_column_view_column_set_title ()
;;;void
;;;gtk_column_view_column_set_title (GtkColumnViewColumn *self,
;;;                                  const char *title);
;;;Sets the title of this column. The title is displayed in the header of a GtkColumnView for this column and is therefore user-facing text that should be translated.

;;;Parameters
;;;self

;;;a GtkColumnViewColumn

;;;title

;;;Title to use for this column.

;;;[nullable]
;;;gtk_column_view_column_get_title ()
;;;const char *
;;;gtk_column_view_column_get_title (GtkColumnViewColumn *self);
;;;Returns the title set with gtk_column_view_column_set_title().

;;;Parameters
;;;self

;;;a GtkColumnViewColumn

;;;Returns
;;;The column's title.

;;;[nullable]

;;;gtk_column_view_column_set_sorter ()
;;;void
;;;gtk_column_view_column_set_sorter (GtkColumnViewColumn *self,
;;;                                   GtkSorter *sorter);
;;;Associates a sorter with the column.

;;;If sorter is NULL, the column will not let users change the sorting by clicking on its header.

;;;This sorter can be made active by clicking on the column header, or by calling gtk_column_view_sort_by_column().

;;;See gtk_column_view_get_sorter() for the necessary steps for setting up customizable sorting for GtkColumnView.

;;;Parameters
;;;self

;;;a GtkColumnViewColumn

;;;sorter

;;;the GtkSorter to associate with column .

;;;[nullable]
;;;gtk_column_view_column_get_sorter ()
;;;GtkSorter *
;;;gtk_column_view_column_get_sorter (GtkColumnViewColumn *self);
;;;Returns the sorter that is associated with the column.

;;;Parameters
;;;self

;;;a GtkColumnViewColumn

;;;Returns
;;;the GtkSorter of self .

;;;[nullable][transfer none]

;;;gtk_column_view_column_set_visible ()
;;;void
;;;gtk_column_view_column_set_visible (GtkColumnViewColumn *self,
;;;                                    gboolean visible);
;;;Sets whether this column should be visible in views.

;;;Parameters
;;;self

;;;a GtkColumnViewColumn

;;;visible

;;;whether this column should be visible

;;;gtk_column_view_column_get_visible ()
;;;gboolean
;;;gtk_column_view_column_get_visible (GtkColumnViewColumn *self);
;;;Returns whether this column is visible.

;;;Parameters
;;;self

;;;a GtkColumnViewColumn

;;;Returns
;;;TRUE if this column is visible

;;;gtk_column_view_column_set_resizable ()
;;;void
;;;gtk_column_view_column_set_resizable (GtkColumnViewColumn *self,
;;;                                      gboolean resizable);
;;;Sets whether this column should be resizable by dragging.

;;;Parameters
;;;self

;;;a GtkColumnViewColumn

;;;resizable

;;;whether this column should be resizable

;;;gtk_column_view_column_get_resizable ()
;;;gboolean
;;;gtk_column_view_column_get_resizable (GtkColumnViewColumn *self);
;;;Returns whether this column is resizable.

;;;Parameters
;;;self

;;;a GtkColumnView

;;;Returns
;;;TRUE if this column is resizable

;;;gtk_column_view_column_set_header_menu ()
;;;void
;;;gtk_column_view_column_set_header_menu
;;;                               (GtkColumnViewColumn *self,
;;;                                GMenuModel *menu);
;;;Sets the menu model that is used to create the context menu for the column header.

;;;Parameters
;;;self

;;;a GtkColumnViewColumn

;;;menu

;;;a GMenuModel, or NULL.

;;;[allow-none]
;;;gtk_column_view_column_get_header_menu ()
;;;GMenuModel *
;;;gtk_column_view_column_get_header_menu
;;;                               (GtkColumnViewColumn *self);
;;;Gets the menu model that is used to create the context menu for the column header.

;;;Parameters
;;;self

;;;a GtkColumnViewColumn

;;;Returns
;;;the GMenuModel, or NULL.

;;;[transfer none][nullable]

;;;gtk_column_view_column_set_fixed_width ()
;;;void
;;;gtk_column_view_column_set_fixed_width
;;;                               (GtkColumnViewColumn *self,
;;;                                int fixed_width);
;;;If fixed_width is not -1, sets the fixed width of column ; otherwise unsets it.

;;;Setting a fixed width overrides the automatically calculated width. Interactive resizing also sets the “fixed-width” property.

;;;Parameters
;;;self

;;;a GtkColumnViewColumn

;;;fixed_width

;;;the new fixed width, or -1

;;;gtk_column_view_column_get_fixed_width ()
;;;int
;;;gtk_column_view_column_get_fixed_width
;;;                               (GtkColumnViewColumn *self);
;;;Gets the fixed width of the column.

;;;Parameters
;;;self

;;;a GtkColumnViewColumn

;;;Returns
;;;the fixed with of the column

;;;gtk_column_view_column_set_expand ()
;;;void
;;;gtk_column_view_column_set_expand (GtkColumnViewColumn *self,
;;;                                   gboolean expand);
;;;Sets the column to take available extra space.

;;;The extra space is shared equally amongst all columns that have the expand set to TRUE.

;;;Parameters
;;;self

;;;a GtkColumnViewColumn

;;;expand

;;;TRUE if this column should expand to fill available sace

;;;gtk_column_view_column_get_expand ()
;;;gboolean
;;;gtk_column_view_column_get_expand (GtkColumnViewColumn *self);
;;;Returns whether this column should expand.

;;;Parameters
;;;self

;;;a GtkColumnViewColumn

;;;Returns
;;;TRUE if this column expands


;;; --- End of file gtk4.column-view-column.lisp -------------------------------
