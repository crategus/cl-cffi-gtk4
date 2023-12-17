;;; ----------------------------------------------------------------------------
;;; gtk4.column-view-row.lisp
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
;;; Types and Values
;;;
;;;     GtkColumnViewRow
;;;
;;; Accessors
;;;
;;;     gtk_column_view_row_get_accessible_description
;;;     gtk_column_view_row_set_accessible_description
;;;     gtk_column_view_row_get_accessible_label
;;;     gtk_column_view_row_set_accessible_label
;;;     gtk_column_view_row_get_activatable
;;;     gtk_column_view_row_set_activatable
;;;     gtk_column_view_row_get_focusable
;;;     gtk_column_view_row_set_focusable
;;;     gtk_column_view_row_get_item
;;;     gtk_column_view_row_get_position
;;;     gtk_column_view_row_get_selectable
;;;     gtk_column_view_row_set_selectable
;;;     gtk_column_view_row_get_selected
;;;
;;; Properties
;;;
;;;     accessible-description
;;;     accessible-label
;;;     activatable
;;;     focusable
;;;     item
;;;     position
;;;     selectable
;;;     selected
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkColumnViewrow
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkColumnViewRow
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkColumnViewRow" column-view-row
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_column_view_row_get_type")
  ((accessible-description
    column-view-row-accessible-description
    "accessible-description" "gchararray" t t)
   (accessible-label
    column-view-row-accessible-label
    "accessible-label" "gchararray" t t)
   (activatable
    column-view-row-activatable
    "activatable" "gboolean" t t)
   (focusable
    column-view-row-focusable
    "focusable" "gboolean" t t)
   (item
    column-view-row-item
    "item" "GObject" t nil)
   (position
    column-view-row-position
    "position" "guint" t nil)
   (selectable
    column-view-row-selectable
    "selectable" "gboolean" t t)
   (selected
    column-view-row-selected
    "selected" "gboolean" t nil)))

#+liber-documentation
(setf (documentation 'column-view-row 'type)
 "@version{2023-11-27}
  @begin{short}
    The @class{gtk:column-view-row} object is used by the
    @class{gtk:column-view} widget to allow configuring how rows are displayed.
  @end{short}
  It is not used to set the widgets displayed in the individual cells. For that
  see the @fun{gtk:column-view-column-factory} function and the
  @class{gtk:column-view-cell} object.

  Since 4.12
  @see-slot{gtk:column-view-row-accessible-description}
  @see-slot{gtk:column-view-row-accessible-label}
  @see-slot{gtk:column-view-row-activatable}
  @see-slot{gtk:column-view-row-focusable}
  @see-slot{gtk:column-view-row-item}
  @see-slot{gtk:column-view-row-position}
  @see-slot{gtk:column-view-row-selectable}
  @see-slot{gtk:column-view-row-selected}
  @see-class{gtk:column-view}
  @see-class{gtk:column-view-cell}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor details
;;; ----------------------------------------------------------------------------

;;; --- gtk:column-view-row-accessible-description -----------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accessible-description"
                                               'column-view-row) t)
 "The @code{accessible-description} property of type @code{:string}
  (Read / Write) @br{}
  The accessible description to set on the row. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-row-accessible-description)
      "Accessor"
      (documentation 'column-view-row-accessible-description 'function)
 "@version{2023-11-27}
  @syntax[]{(gtk:column-view-row-accessible-description object) => description}
  @syntax[]{(setf (gtk:column-view-row-accessible-description object) description)}
  @argument[object]{a @class{gtk:column-view-row} object}
  @argument[description]{a string with the accessible description}
  @begin{short}
    Accessor of the @slot[gtk:column-view-row]{accessible-description} slot of
    the @class{gtk:column-view-row} class.
  @end{short}
  The @fun{gtk:column-view-row-accessible-description} function gets the
  accessible description. The @setf{gtk:column-view-row-accessible-description}
  function sets the accessible description for the row, which may be used by
  e.g. screen readers.

  Since 4.12
  @see-class{gtk:column-view-row}")

;;; --- gtk:column-view-row-accessible-label -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accessible-label"
                                               'column-view-row) t)
 "The @code{accessible-label} property of type @code{:string} (Read / Write)
  @br{}
  The accessible label to set on the row. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-row-accessible-label)
      "Accessor"
      (documentation 'column-view-row-accessible-label 'function)
 "@version{2023-11-27}
  @syntax[]{(gtk:column-view-row-accessible-label object) => label}
  @syntax[]{(setf (gtk:column-view-row-accessible-label object) label)}
  @argument[object]{a @class{gtk:column-view-row} object}
  @argument[label]{a string with the accesible label}
  @begin{short}
    Accessor of the @slot[gtk:column-view-row]{accessible-label} slot of
    the @class{gtk:column-view-row} class.
  @end{short}
  The @fun{gtk:column-view-row-accessible-label} function gets the accessible
  label. The @setf{gtk:column-view-row-accessible-label} function sets the
  accessible label for the row, which may be used by e.g. screen readers.

  Since 4.12
  @see-class{gtk:column-view-row}")

;;; --- gtk:column-view-row-activatable ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "activatable"
                                               'column-view-row) t)
 "The @code{activatable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the row can be activated by the user. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-row-activatable)
      "Accessor"
      (documentation 'column-view-row-activatable 'function)
 "@version{2023-11-27}
  @syntax[]{(gtk:column-view-row-activatable object) => activatable}
  @syntax[]{(setf (gtk:column-view-row-activatable object) activatable)}
  @argument[object]{a @class{gtk:column-view-row} object}
  @argument[activatable]{a boolean whether the row can be activated}
  @begin{short}
    Accessor of the @slot[gtk:column-view-row]{activatable} slot of
    the @class{gtk:column-view-row} class.
  @end{short}
  The @fun{gtk:column-view-row-activatable} function checks if the row has been
  set to be activatable. The @setf{gtk:column-view-row-activatable} function
  sets @arg{object} to be activatable.

  If a row is activatable, double-clicking on the row, using the @kbd{Return}
  key or calling the @fun{gtk:widget-activate} function will activate the row.
  Activating instructs the containing columnview to emit the
  @code{\"GtkColumnView::activate\"} signal.

  By default, the rows are activatable.

  Since 4.12
  @see-class{gtk:column-view-row}
  @see-function{gtk:widget-activate}")

;;; --- gtk:column-view-row-focusable ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "focusable" 'column-view-row) t)
 "The @code{focusable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the row can be focused with the keyboard. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-row-focusable)
      "Accessor"
      (documentation 'column-view-row-focusable 'function)
 "@version{2023-11-27}
  @syntax[]{(gtk:column-view-row-focusable object) => focusable}
  @syntax[]{(setf (gtk:column-view-row-focusable object) focusable)}
  @argument[object]{a @class{gtk:column-view-row} object}
  @argument[focusable]{a boolean whether the row can be focused}
  @begin{short}
    Accessor of the @slot[gtk:column-view-row]{focusable} slot of
    the @class{gtk:column-view-row} class.
  @end{short}
  The @fun{gtk:column-view-row-focusable} function checks if a row item has been
  set to be focusable. The @setf{gtk:column-view-row-focusable} function sets
  @arg{object} to be focusable. If a row is focusable, it can be focused using
  the keyboard. This works similar to the @fun{gtk:widget-focusable} function.

  Note that if rows are not focusable, the contents of cells can still be
  focused if they are focusable.

  By default, rows are focusable.

  Since 4.12
  @see-class{gtk:column-view-row}
  @see-function{gtk:widget-focusable}")

;;; --- gtk:column-view-row-item -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "item" 'column-view-row) t)
 "The @code{item} property of type @class{g:object} (Read) @br{}
  The item for the row.")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-row-item)
      "Accessor"
      (documentation 'column-view-row-item 'function)
 "@version{2023-11-27}
  @syntax[]{(gtk:column-view-row-item object) => item}
  @argument[object]{a @class{gtk:column-view-row} object}
  @argument[item]{a @class{g:object} object with the item for the row}
  @begin{short}
    Accessor of the @slot[gtk:column-view-row]{item} slot of
    the @class{gtk:column-view-row} class.
  @end{short}
  The @fun{gtk:column-view-row-item} function gets the model item that
  associated with @arg{object}. If @arg{object} is unbound, this function
  returns @code{nil}.

  Since 4.12
  @see-class{gtk:column-view-row}
  @see-class{g:object}")

;;; --- gtk:column-view-row-position -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "position" 'column-view-row) t)
 "The @code{position} property of type @code{:uint} (Read) @br{}
  Position of the row. @br{}
  Default value: @var{+gtk-invalid-list-position+}")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-row-position)
      "Accessor"
      (documentation 'column-view-row-position 'function)
 "@version{2023-11-27}
  @syntax[]{(gtk:column-view-row-position object) => position}
  @argument[object]{a @class{gtk:column-view-row} object}
  @argument[position]{an unsigned integer with the position of the row}
  @begin{short}
    Accessor of the @slot[gtk:column-view-row]{position} slot of
    the @class{gtk:column-view-row} class.
  @end{short}
  The @fun{gtk:column-view-row-position} function gets the position in the
  model that @arg{object} currently displays. If @arg{object} is unbound, the
  @var{+gtk-invalid-list-position+} value is returned.

  Since 4.12
  @see-class{gtk:column-view-row}
  @see-class{g:object}")

;;; --- gtk:column-view-row-selectable -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "selectable" 'column-view-row) t)
 "The @code{selectable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the row can be selected by the user. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-row-selectable)
      "Accessor"
      (documentation 'column-view-row-selectable 'function)
 "@version{2023-11-27}
  @syntax[]{(gtk:column-view-row-selectable object) => selectable}
  @syntax[]{(setf (gtk:column-view-row-selectable object) selectable)}
  @argument[object]{a @class{gtk:column-view-row} object}
  @argument[selectable]{a boolean whether the row can be selected}
  @begin{short}
    Accessor of the @slot[gtk:column-view-row]{selectable} slot of
    the @class{gtk:column-view-row} class.
  @end{short}
  The @fun{gtk:column-view-row-selectable} function checks if the row has been
  set to be selectable. The @setf{gtk:column-view-row-selectable} function sets
  @arg{object} to be selectable.

  If a row is selectable, clicking on the row or using the keyboard will try to
  select or unselect the row. Whether this succeeds is up to the model to
  determine, as it is managing the selected state.

  Note that this means that making a row non-selectable has no influence on the
  selected state at all. A non-selectable row may still be selected.

  By default, rows are selectable.

  Since 4.12
  @see-class{gtk:column-view-row}")

;;; --- gtk:column-view-row-selected -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "selected" 'column-view-row) t)
 "The @code{selected} property of type @code{:boolean} (Read) @br{}
  Whether the item in the row is currently selected. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'column-view-row-selected)
      "Accessor"
      (documentation 'column-view-row-selected 'function)
 "@version{2023-11-27}
  @syntax[]{(gtk:column-view-row-selected object) => selected}
  @argument[object]{a @class{gtk:column-view-row} object}
  @argument[selected]{a boolean whether the item in the row is selected}
  @begin{short}
    Accessor of the @slot[gtk:column-view-row]{selected} slot of
    the @class{gtk:column-view-row} class.
  @end{short}
  The @fun{gtk:column-view-row-selected} function checks if the item is
  selected that this row corresponds to. The selected state is maintained by
  the list widget and its model and cannot be set otherwise.

  Since 4.12
  @see-class{gtk:column-view-row}")

;;; --- End of file gtk4.column-view-row.lisp ----------------------------------

