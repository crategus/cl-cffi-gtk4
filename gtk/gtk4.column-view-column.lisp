;;; ----------------------------------------------------------------------------
;;; gtk4.column-view.lisp
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
;;; GtkColumnViewColumn
;;;
;;;     The column added to GtkColumnView
;;;
;;; Types and Values
;;;
;;;     GtkColumnViewColumn
;;;
;;; Accessors
;;;
;;;     gtk_column_view_column_get_column_view
;;;     gtk_column_view_column_get_expand
;;;     gtk_column_view_column_set_expand
;;;     gtk_column_view_column_get_factory
;;;     gtk_column_view_column_set_factory
;;;     gtk_column_view_column_get_fixed_width
;;;     gtk_column_view_column_set_fixed_width
;;;     gtk_column_view_column_get_header_menu
;;;     gtk_column_view_column_set_header_menu
;;;     gtk_column_view_column_get_id                       Since 4.10
;;;     gtk_column_view_column_set_id                       Since 4.10
;;;     gtk_column_view_column_get_resizable
;;;     gtk_column_view_column_set_resizable
;;;     gtk_column_view_column_get_sorter
;;;     gtk_column_view_column_set_sorter
;;;     gtk_column_view_column_get_title
;;;     gtk_column_view_column_set_title
;;;     gtk_column_view_column_get_visible
;;;     gtk_column_view_column_set_visible
;;;
;;; Functions
;;;
;;;     gtk_column_view_column_new
;;;
;;; Properties
;;;
;;;     column-view
;;;     expand
;;;     factory
;;;     fixed-width
;;;     header-menu
;;;     id                                                  Since 4.10
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

(gobject:define-gobject "GtkColumnViewColumn" column-view-column
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

#+liber-documentation
(setf (documentation 'column-view-column 'type)
 "@version{2025-4-25}
  @begin{short}
    The @class{gtk:column-view-column} widget represents the columns being
    added to the @class{gtk:column-view} widget.
  @end{short}
  Columns have a title, and can optionally have a header menu set with the
  @fun{gtk:column-view-column-header-menu} function.

  A sorter can be associated with a column using the
  @fun{gtk:column-view-column-sorter} function, to let users influence sorting
  by clicking on the column header.
  @see-constructor{gtk:column-view-column-new}
  @see-slot{gtk:column-view-column-column-view}
  @see-slot{gtk:column-view-column-expand}
  @see-slot{gtk:column-view-column-factory}
  @see-slot{gtk:column-view-column-fixed-width}
  @see-slot{gtk:column-view-column-header-menu}
  @see-slot{gtk:column-view-column-id}
  @see-slot{gtk:column-view-column-resizable}
  @see-slot{gtk:column-view-column-sorter}
  @see-slot{gtk:column-view-column-title}
  @see-slot{gtk:column-view-column-visible}
  @see-class{gtk:column-view}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:column-view-column-column-view -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "column-view"
                                               'column-view-column) t)
 "The @code{column-view} property of type @class{gtk:column-view} (Read) @br{}
  The column view this column is a part of.")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-column-column-view)
      "Accessor"
      (documentation 'column-view-column-column-view 'function)
 "@version{2025-4-13}
  @syntax{(gtk:column-view-column-column-view object) => columnview}
  @argument[object]{a @class{gtk:column-view-column} object}
  @argument[columnview]{a @class{gtk:column-view} widget displaying
    @arg{object}}
  @begin{short}
    Accessor of the @slot[gtk:column-view-column]{column-view} slot of the
    @class{gtk:column-view-column} class.
  @end{short}
  The @fun{gtk:column-view-column-column-view} function gets the column view
  that is currently displaying this column. If @arg{object} has not been added
  to a column view yet, @code{nil} is returned.
  @see-class{gtk:column-view-column}
  @see-class{gtk:column-view}")

;;; --- gtk:column-view-column-expand ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "expand" 'column-view-column) t)
 "The @code{expand} property of type @code{:boolean} (Read / Write) @br{}
  Whether the column gets share of extra width allocated to the view. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-column-expand)
      "Accessor"
      (documentation 'column-view-column-expand 'function)
 "@version{2025-4-13}
  @syntax{(gtk:column-view-column-expand object) => expand}
  @syntax{(setf (gtk:column-view-column-expand object) expand)}
  @argument[object]{a @class{gtk:column-view-column} object}
  @argument[expand]{@em{true} if this column expands}
  @begin{short}
    Accessor of the @slot[gtk:column-view-column]{expand} slot of the
    @class{gtk:column-view-column} class.
  @end{short}
  The @fun{gtk:column-view-column-expand} function returns whether this column
  should expand. The @setf{gtk:column-view-column-expand} function sets the
  column to take available extra space. The extra space is shared equally
  amongst all columns that have the expand set to @em{true}.
  @see-class{gtk:column-view-column}")

;;; --- gtk:column-view-column-factory -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "factory" 'column-view-column) t)
 "The @code{factory} property of type @class{gtk:list-item-factory}
  (Read / Write) @br{}
  The factory for populating list items.")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-column-factory)
      "Accessor"
      (documentation 'column-view-column-factory 'function)
 "@version{2025-4-13}
  @syntax{(gtk:column-view-column-factory object) => factory}
  @syntax{(setf (gtk:column-view-column-factory object) factory)}
  @argument[object]{a @class{gtk:column-view-column} object}
  @argument[factory]{a @class{gtk:list-item-factory} object to use, or
    @code{nil} for none}
  @begin{short}
    Accessor of the @slot[gtk:column-view-column]{factory} slot of the
    @class{gtk:column-view-column} class.
  @end{short}
  The @fun{gtk:column-view-column-factory} function gets the factory that is
  currently used to populate list items. The
  @setf{gtk:column-view-column-factory} function sets the factory.
  @see-class{gtk:column-view-column}
  @see-class{gtk:list-item-factory}")

;;; --- gtk:column-view-column-fixed-width -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "fixed-width"
                                               'column-view-column) t)
 "The @code{fixed-width} property of type @code{:int} (Read / Write) @br{}
  If not -1, this is the width that the column is allocated, regardless of the
  size of its content. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-column-fixed-width)
      "Accessor"
      (documentation 'column-view-column-fixed-width 'function)
 "@version{2025-4-13}
  @syntax{(gtk:column-view-column-fixed-width object) => width}
  @syntax{(setf (gtk:column-view-column-fixed-width object) width)}
  @argument[object]{a @class{gtk:column-view-column} object}
  @argument[width]{an integer for the fixed width of the column}
  @begin{short}
    Accessor of the @slot[gtk:column-view-column]{fixed-width} slot of the
    @class{gtk:column-view-column} class.
  @end{short}
  The @fun{gtk:column-view-column-fixed-width} function gets the fixed width of
  the column. The @setf{gtk:column-view-column-fixed-width} function sets the
  fixed width. If @arg{width} is not -1, sets the fixed width of @arg{object},
  otherwise unsets it.

  Setting a fixed width overrides the automatically calculated width.
  Interactive resizing also sets the @slot[gtk:column-view-column]{fixed-width}
  property.
  @see-class{gtk:column-view-column}")

;;; --- gtk:column-view-column-header-menu -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "header-menu"
                                               'column-view-column) t)
 "The @code{header-menu} property of type @class{g:menu-model} (Read / Write)
  @br{}
  The menu model used to create the context menu for the column header.")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-column-header-menu)
      "Accessor"
      (documentation 'column-view-column-header-menu 'function)
 "@version{2025-4-13}
  @syntax{(gtk:column-view-column-header-menu object) => menu}
  @syntax{(setf (gtk:column-view-column-header-menu object) menu)}
  @argument[object]{a @class{gtk:column-view-column} object}
  @argument[menu]{a @class{g:menu-model} object, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:column-view-column]{header-menu} slot of the
    @class{gtk:column-view-column} class.
  @end{short}
  The @fun{gtk:column-view-column-header-menu} function gets the menu model
  that is used to create the context menu for the column header. The
  @setf{gtk:column-view-column-header-menu} function sets the menu model.
  @see-class{gtk:column-view-column}
  @see-class{g:menu-model}")

;;; --- gtk:column-view-column-id ----------------------------------------------

#+(and gtk-4-10 liber-documentation)
(setf (documentation (liber:slot-documentation "id" 'column-view-column) t)
 "The @code{id} property of type @code{:string} (Read / Write) @br{}
  The ID for the column. GTK is not currently using the ID for anything, but it
  can be used by applications when saving column view configurations. It is up
  to applications to ensure uniqueness of IDs. @br{}
  Default value: @code{nil}")

#+(and gtk-4-10 liber-documentation)
(setf (liber:alias-for-function 'column-view-column-id)
      "Accessor"
      (documentation 'column-view-column-id 'function)
 "@version{2025-4-13}
  @syntax{(gtk:column-view-column-id object) => id}
  @syntax{(setf (gtk:column-view-column-id object) id)}
  @argument[object]{a @class{gtk:column-view-column} object}
  @argument[id]{a string for the ID to use for this column}
  @begin{short}
    Accessor of the @slot[gtk:column-view-column]{id} slot of the
    @class{gtk:column-view-column} class.
  @end{short}
  The @fun{gtk:column-view-column-id} function returns the ID. The
  @setf{gtk:column-view-column-id} function sets the ID of this column. GTK
  makes no use of this, but applications can use it when storing column view
  configuration. It is up to callers to ensure uniqueness of IDs.

  Since 4.10
  @see-class{gtk:column-view-column}")

;;; --- gtk:column-view-column-resizable ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "resizable"
                                               'column-view-column) t)
 "The @code{resizable} property of type @code{:boolean} (Read / Write) @br{}
  Whether this column is resizable. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-column-resizable)
      "Accessor"
      (documentation 'column-view-column-resizable 'function)
 "@version{2025-4-13}
  @syntax{(gtk:column-view-column-resizable object) => resizable}
  @syntax{(setf (gtk:column-view-column-resizable object) resizable)}
  @argument[object]{a @class{gtk:column-view-column} object}
  @argument[resizable]{@em{true} if this column is resizable}
  @begin{short}
    Accessor of the @slot[gtk:column-view-column]{resizable} slot of the
    @class{gtk:column-view-column} class.
  @end{short}
  The @fun{gtk:column-view-column-resizable} function returns whether this
  column is resizable by dragging. The @setf{gtk:column-view-column-resizable}
  function sets the property.
  @see-class{gtk:column-view-column}")

;;; --- gtk:column-view-column-sorter ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "sorter" 'column-view-column) t)
 "The @code{sorter} property of type @class{gtk:sorter} (Read / Write) @br{}
  The sorter for sorting items according to this column.")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-column-sorter)
      "Accessor"
      (documentation 'column-view-column-sorter 'function)
 "@version{2025-4-13}
  @syntax{(gtk:column-view-column-sorter object) => sorter}
  @syntax{(setf (gtk:column-view-column-sorter object) sorter)}
  @argument[object]{a @class{gtk:column-view-column} object}
  @argument[sorter]{a @class{gtk:sorter} object}
  @begin{short}
    Accessor of the @slot[gtk:column-view-column]{sorter} slot of the
    @class{gtk:column-view-column} class.
  @end{short}
  The @fun{gtk:column-view-column-sorter} function returns the sorter that is
  associated with the column. The @setf{gtk:column-view-column-sorter} function
  associates a sorter with the column. If @arg{sorter} is @code{nil}, the column
  will not let users change the sorting by clicking on its header. This sorter
  can be made active by clicking on the column header, or by calling the
  @fun{gtk:column-view-sort-by-column} function.

  See the @fun{gtk:column-view-sorter} function for the necessary steps for
  setting up customizable sorting for the column view.
  @see-class{gtk:column-view-column}
  @see-class{gtk:sorter}
  @see-function{gtk:column-view-sort-by-column}
  @see-function{gtk:column-view-sorter}")

;;; --- gtk:column-view-column-title -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "title" 'column-view-column) t)
 "The @code{title} property of type @code{:string} (Read / Write) @br{}
  The title displayed in the header. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-column-title)
      "Accessor"
      (documentation 'column-view-column-title 'function)
 "@version{2025-4-13}
  @syntax{(gtk:column-view-column-title object) => title}
  @syntax{(setf (gtk:column-view-column-title object) title)}
  @argument[object]{a @class{gtk:column-view-column} object}
  @argument[title]{a string for the title of the column}
  @begin{short}
    Accessor of the @slot[gtk:column-view-column]{title} slot of the
    @class{gtk:column-view-column} class.
  @end{short}
  The @fun{gtk:column-view-column-title} function  returns the title of this
  column. The @setf{gtk:column-view-column-sorter} function sets the title.
  The title is displayed in the header of a @class{gtk:column-view} widget for
  this column and is therefore user-facing text that should be translated.
  @see-class{gtk:column-view-column}
  @see-class{gtk:column-view}")

;;; --- gtk:column-view-column-visible -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "visible" 'column-view-column) t)
 "The @code{visible} property of type @code{:boolean} (Read / Write) @br{}
  Whether this column is visible. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-column-visible)
      "Accessor"
      (documentation 'column-view-column-visible 'function)
 "@version{2025-4-13}
  @syntax{(gtk:column-view-column-visible object) => visible}
  @syntax{(setf (gtk:column-view-column-visible object) visible)}
  @argument[object]{a @class{gtk:column-view-column} object}
  @argument[visible]{@em{true} if this column is visible}
  @begin{short}
    Accessor of the @slot[gtk:column-view-column]{visible} slot of the
    @class{gtk:column-view-column} class.
  @end{short}
  The @fun{gtk:column-view-column-visible} function returns whether this column
  is visible in views. The @setf{gtk:column-view-column-visible} function sets
  the property.
  @see-class{gtk:column-view-column}")

;;; ----------------------------------------------------------------------------
;;; gtk_column_view_column_new
;;; ----------------------------------------------------------------------------

(declaim (inline column-view-column-new))

(defun column-view-column-new (&optional title factory)
 #+liber-documentation
 "@version{2025-4-13}
  @argument[title]{an optional string for the title to use for this column,
    or the default @code{nil} value}
  @argument[factory]{an optional @class{gtk:list-item-factory} object to
    populate items with, or the default @code{nil} value}
  @return{The new @class{gtk:column-view-column} object using the given
    @arg{factory}.}
  @begin{short}
    Creates a new column that uses the given @arg{factory} for mapping
    items to widgets.
  @end{short}
  You most likely want to call the @fun{gtk:column-view-append-column} function
  next.
  @see-class{gtk:column-view-column}
  @see-class{gtk:list-item-factory}
  @see-function{gtk:column-view-append-column}"
  (make-instance 'column-view-column
                 :title (or title (cffi:null-pointer))
                 :factory factory))

(export 'column-view-column-new)

;;; --- End of file gtk4.column-view-column.lisp -------------------------------
