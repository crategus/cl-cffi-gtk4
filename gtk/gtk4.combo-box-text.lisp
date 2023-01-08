;;; ----------------------------------------------------------------------------
;;; gtk.combo-box-text.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2022 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
;;; ----------------------------------------------------------------------------
;;;
;;; GtkComboBoxText
;;;
;;;     A simple, text-only combo box
;;;
;;; Types and Values
;;;
;;;     GtkComboBoxText
;;;
;;; Functions
;;;
;;;     gtk_combo_box_text_new
;;;     gtk_combo_box_text_new_with_entry
;;;     gtk_combo_box_text_append
;;;     gtk_combo_box_text_prepend
;;;     gtk_combo_box_text_insert
;;;     gtk_combo_box_text_append_text
;;;     gtk_combo_box_text_prepend_text
;;;     gtk_combo_box_text_insert_text
;;;     gtk_combo_box_text_remove
;;;     gtk_combo_box_text_remove_all
;;;     gtk_combo_box_text_get_active_text
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkComboBox
;;;                 ╰── GtkComboBoxText
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkCellLayout
;;;     GtkCellEditable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkComboBoxText
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkComboBoxText" combo-box-text
  (:superclass combo-box
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkCellLayout"
                "GtkCellEditable")
   :type-initializer "gtk_combo_box_text_get_type")
  nil)

#+liber-documentation
(setf (documentation 'combo-box-text 'type)
 "@version{#2021-3-12}
  @begin{short}
    The @sym{gtk:combo-box-text} widget is a simple variant of the
    @class{gtk:combo-box} widget that hides the model-view complexity for
    simple text-only use cases.
  @end{short}

  To create a @sym{gtk:combo-box-text} widget, use the functions
  @fun{gtk:combo-box-text-new} or @fun{gtk:combo-box-text-new-with-entry}.

  You can add items to a @sym{gtk:combo-box-text} widget with the
  @fun{gtk:combo-box-text-append-text}, @fun{gtk:combo-box-text-insert-text} or
  @fun{gtk:combo-box-text-prepend-text} functions and remove options with the
  @fun{gtk:combo-box-text-remove} function.

  If the @sym{gtk:combo-box-text} widget contains an entry via the
  @slot[gtk:combo-box]{has-entry} property, its contents can be retrieved using
  the @fun{gtk:combo-box-text-active-text} function. The entry itself can be
  accessed by calling the @fun{gtk:combo-bpx-child} function on the combo box.

  You should not call the @fun{gtk:combo-box-model} function or attempt to pack
  more cells into this combo box via its @class{gtk:cell-layout} interface.
  @begin[GtkComboBoxText as GtkBuildable]{dictionary}
    The @sym{gtk:combo-box-text} implementation of the @class{gtk:buildable}
    interface supports adding items directly using the @code{<items>} element
    and specifying @code{<item>} elements for each item. Each @code{<item>}
    element supports the regular translation attributes \"translatable\",
    \"context\" and \"comments\".

    @b{Example:} A UI definition fragment specifying @sym{gtk:combo-box-text}
    items
    @begin{pre}
 <object class=\"GtkComboBoxText\">
   <items>
     <item translatable=\"yes\">Factory</item>
     <item translatable=\"yes\">Home</item>
     <item translatable=\"yes\">Subway</item>
   </items>
 </object>
    @end{pre}
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
 combobox
 ╰── box.linked
     ├── entry.combo
     ├── button.combo
     ╰── window.popup
    @end{pre}
    The @sym{gtk:combo-box-text} implementation has a single CSS node with name
    @code{combobox}. It adds the @code{.combo} style class to the main CSS
    nodes of its entry and button children, and the @code{.linked} class to the
    node of its internal box.
  @end{dictionary}
  @see-constructor{gtk:combo-box-text-new}
  @see-constructor{gtk:combo-box-text-new-with-entry}
  @see-class{gtk:combo-box}")

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline combo-box-text-new))

(defun combo-box-text-new ()
 #+liber-documentation
 "@version{#2021-3-12}
  @return{A new @class{gtk:combo-box-text} widget.}
  @begin{short}
    Creates a new combo box text widget, which is a @class{gtk:combo-box}
    widget just displaying strings.
  @end{short}
  @see-class{gtk:combo-box-text}
  @see-function{gtk:combo-box-text-new-with-entry}"
  (make-instance 'combo-box-text))

(export 'combo-box-text-new)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_new_with_entry ()
;;; ----------------------------------------------------------------------------

(declaim (inline combo-box-text-new-with-entry))

(defun combo-box-text-new-with-entry ()
 #+liber-documentation
 "@version{#2021-3-12}
  @return{A new @class{gtk:combo-box-text} widget.}
  @begin{short}
    Creates a new combo box text widget, which is a @class{gtk:combo-box}
    widget just displaying strings.
  @end{short}
  The combo box created by this function has an entry.
  @see-class{gtk:combo-box-text}
  @see-function{gtk:combo-box-text-new}"
  (make-instance 'combo-box-text
                 :has-entry t))

(export 'combo-box-text-new-with-entry)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_append ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_append" %combo-box-text-append) :void
  (combo (g:object combo-box-text))
  (id :string)
  (text :string))

(defun combo-box-text-append (combo id text)
 #+liber-documentation
 "@version{#2021-3-12}
  @argument[combo]{a @class{gtk:combo-box-text} widget}
  @argument[id]{a string ID for this value, or @code{nil}}
  @argument[text]{a string with the text}
  @begin{short}
    Appends @arg{text} to the list of strings stored in the combo box.
  @end{short}
  If the ID is non-@code{nil} then it is used as the ID of the row.

  This is the same as calling the function @fun{gtk:combo-box-text-insert} with
  a position of -1.
  @see-class{gtk:combo-box-text}
  @see-function{gtk:combo-box-text-insert}
  @see-function{gtk:combo-box-text-prepend}"
  (%combo-box-text-append combo
                              (if id id (null-pointer))
                              text))

(export 'combo-box-text-append)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_prepend ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_prepend" %combo-box-text-prepend) :void
  (combo (g:object combo-box-text))
  (id :string)
  (text :string))

(defun combo-box-text-prepend (combo id text)
 #+liber-documentation
 "@version{#2021-3-12}
  @argument[combo]{a @class{gtk:combo-box} widget}
  @argument[id]{a string ID for this value, or @code{nil}}
  @argument[text]{a string with the text}
  @begin{short}
    Prepends @arg{text} to the list of strings stored in the combo box.
  @end{short}
  If the ID is non-@code{nil} then it is used as the ID of the row.

  This is the same as calling the function @fun{gtk:combo-box-text-insert} with
  a position of 0.
  @see-class{gtk:combo-box-text}
  @see-function{gtk:combo-box-text-insert}"
  (%combo-box-text-prepend combo
                               (if id id (null-pointer))
                               text))

(export 'combo-box-text-prepend)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_insert ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_insert" %combo-box-text-insert) :void
  (combo (g:object combo-box-text))
  (position :int)
  (id :string)
  (text :string))

(defun combo-box-text-insert (combo position id text)
 #+liber-documentation
 "@version{#2021-3-12}
  @argument[combo]{a @class{gtk:combo-box-text} widget}
  @argument[position]{an integer with an index to insert text}
  @argument[id]{a string ID for this value, or @code{nil}}
  @argument[text]{a string with the text}
  @begin{short}
    Inserts @arg{text} at the given position in the list of strings stored in
    the combo box.
  @end{short}
  If the ID is non-@code{nil} then it is used as the ID of the row. See the
  @slot[gtk:combo-box]{id-column} property.

  If @arg{position} is negative then the text is appended.
  @see-class{gtk:combo-box-text}
  @see-function{gtk:combo-box-id-column}"
  (%combo-box-text-insert combo
                              position
                              (if id id (null-pointer))
                              text))

(export 'combo-box-text-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_append_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_append_text" combo-box-text-append-text) :void
 #+liber-documentation
 "@version{#2021-3-12}
  @argument[combo]{a @class{gtk:combo-box-text} widget}
  @argument[text]{a string with the text}
  @begin{short}
    Appends @arg{text} to the list of strings stored in the combo box.
  @end{short}

  This is the same as calling the function @fun{gtk:combo-box-text-insert-text}
  with a position of -1.
  @see-class{gtk:combo-box-text}
  @see-function{gtk:combo-box-text-insert-text}
  @see-function{gtk:combo-box-text-prepend}"
  (combo (g:object combo-box-text))
  (text :string))

(export 'combo-box-text-append-text)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_prepend_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_prepend_text" combo-box-text-prepend-text)
    :void
 #+liber-documentation
 "@version{#2021-3-12}
  @argument[combo]{a @class{gtk:combo-box} widget}
  @argument[text]{a string with the text}
  @begin{short}
    Prepends @arg{text} to the list of strings stored in the combo box.
  @end{short}

  This is the same as calling the function @fun{gtk:combo-box-text-insert-text}
  with a position of 0.
  @see-class{gtk:combo-box-text}
  @see-function{gtk:combo-box-text-insert-text}
  @see-function{gtk:combo-box-text-append-text}"
  (combo (g:object combo-box-text))
  (text :string))

(export 'combo-box-text-prepend-text)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_insert_text ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_insert_text" combo-box-text-insert-text) :void
 #+liber-documentation
 "@version{#2021-3-12}
  @argument[combo]{a @class{gtk:combo-box-text} widget}
  @argument[position]{an integer with an index to insert text}
  @argument[text]{a string with the text}
  @begin{short}
    Inserts @arg{text} at the given position in the list of strings stored in
    the combo box.
  @end{short}

  If @arg{position} is negative then the text is appended.

  This is the same as calling the function @fun{gtk:combo-box-text-insert} with
  a @code{nil} ID string.
  @see-class{gtk:combo-box-text}
  @see-function{gtk:combo-box-text-insert}"
  (combo (g:object combo-box-text))
  (position :int)
  (text :string))

(export 'combo-box-text-insert-text)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_remove ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_remove" combo-box-text-remove) :void
 #+liber-documentation
 "@version{#2021-3-12}
  @argument[combo]{a @class{gtk:combo-box} widget}
  @argument[position]{an integer with index of the item to remove}
  @begin{short}
    Removes the string at @arg{position} from the combo box.
  @end{short}
  @see-class{gtk:combo-box-text}
  @see-function{gtk:combo-box-text-remove-all}"
  (combo (g:object combo-box-text))
  (position :int))

(export 'combo-box-text-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_remove_all ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_remove_all" combo-box-text-remove-all) :void
 #+liber-documentation
 "@version{#2021-3-12}
  @argument[combo]{a @class{gtk:combo-box-text} widget}
  @begin{short}
    Removes all the text entries from the combo box.
  @end{short}
  @see-class{gtk:combo-box-text}
  @see-function{gtk:combo-box-text-remove}"
  (combo (g:object combo-box-text)))

(export 'combo-box-text-remove-all)

;;; ----------------------------------------------------------------------------
;;; gtk_combo_box_text_get_active_text () -> combo-box-text-active-text
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_combo_box_text_get_active_text"
           combo-box-text-active-text) :string
 #+liber-documentation
 "@version{#2021-3-12}
  @argument[combo]{a @class{gtk:combo-box-text} widget}
  @begin{return}
    A string containing the currently active text.
  @end{return}
  @begin{short}
    Returns the currently active text in the combo box, or @code{nil} if
    none is selected.
  @end{short}

  If the combo box contains an entry, this function will return its
  contents which will not necessarily be an item from the list.
  @see-class{gtk:combo-box-text}"
  (combo (g:object combo-box-text)))

(export 'combo-box-text-active-text)

;;; --- End of file gtk.combo-box-text.lisp ------------------------------------
