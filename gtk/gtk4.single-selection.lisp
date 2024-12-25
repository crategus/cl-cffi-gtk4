;;; ----------------------------------------------------------------------------
;;; gtk4.single-selection.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2023 - 2024 Dieter Kaiser
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
;;; GtkSingleSelection
;;;
;;;     A selection model that allows selecting a single item
;;;
;;; Types and Values
;;;
;;;     GtkSingleSelection
;;;     GTK_INVALID_LIST_POSITION
;;;
;;; Accessors
;;;
;;;     gtk_single_selection_get_model
;;;     gtk_single_selection_set_model
;;;     gtk_single_selection_get_selected
;;;     gtk_single_selection_set_selected
;;;     gtk_single_selection_get_selected_item
;;;     gtk_single_selection_get_autoselect
;;;     gtk_single_selection_set_autoselect
;;;     gtk_single_selection_get_can_unselect
;;;     gtk_single_selection_set_can_unselect
;;;
;;; Functions
;;;
;;;     gtk_single_selection_new
;;;
;;; Properties
;;;
;;;     autoselect
;;;     can-unselect
;;;     item-type                                          Since 4.8
;;;     model
;;;     n-items                                            Since 4.8
;;;     selected
;;;     selected-item
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkSingleSelection
;;;
;;; Implemented Interfaces
;;;
;;;     GListModel
;;;     GtkSelectionModel
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GTK_INVALID_LIST_POSITION
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-variable '+invalid-list-position+)
      "Constant")

(defconstant +invalid-list-position+ 4294967295
 #+liber-documentation
 "@version{2023-8-13}
  @variable-value{4294967295}
  @begin{short}
    The value used to refer to a guaranteed invalid position in a
    @class{g:list-model} object.
  @end{short}
  This value may be returned from some functions, others may accept it as input.
  Its interpretation may differ for different functions.

  Refer to each function's documentation for if this value is allowed and what
  it does.
  @see-class{g:list-model}")

(export '+invalid-list-position+)

;;; ----------------------------------------------------------------------------
;;; GtkSingleSelection
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkSingleSelection" single-selection
  (:superclass g:object
   :export t
   :interfaces ("GtkSelectionModel"
                #+gtk-4-12 "GtkSectionModel")
   :type-initializer "gtk_single_selection_get_type")
  ((autoselect
    single-selection-autoselect
    "autoselect" "gboolean" t t)
   (can-unselect
    single-selection-can-unselect
    "can-unselect" "gboolean" t t)
   #+gtk-4-8
   (item-type
    single-selection-item-type
    "item-type" "GType" t nil)
   (model
    single-selection-model
    "model" "GListModel" t t)
   #+gtk-4-8
   (n-items
    single-selection-n-items
    "n-items" "guint" t nil)
   (selected
    single-selection-selected
    "selected" "guint" t t)
   (selected-item
    single-selection-selected-item
    "selected-item" "GObject" t nil)))

#+liber-documentation
(setf (documentation 'single-selection 'type)
 "@version{2024-12-22}
  @begin{short}
    The @class{gtk:single-selection} class is an implementation of the
    @class{gtk:selection-model} interface that allows selecting a single
    element.
  @end{short}
  It is the default selection method used by list widgets in GTK.

  Note that the selection is *persistent* -- if the selected item is removed
  and re-added in the same @code{\"items-changed\"} signal emission, it stays
  selected. In particular, this means that changing the sort order of an
  underlying sort model will preserve the selection.
  @see-constructor{gtk:single-selection-new}
  @see-class{gtk:selection-model}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:single-selection-autoselect ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "autoselect"
                                               'single-selection) t)
 "The @code{autoselect} property of type @code{:boolean} (Read / Write) @br{}
  Whether the selection will always select an item. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'single-selection-autoselect)
      "Accessor"
      (documentation 'single-selection-autoselect 'function)
 "@version{2023-11-26}
  @syntax{(gtk:single-selection-autoselect object) => autoselect}
  @syntax{(setf (gtk:single-selection-autoselect object) autoselect)}
  @argument[object]{a @class{gtk:single-selection} object}
  @argument[autoselect]{@em{true} if autoselect is enabled}
  @begin{short}
    Accessor of the @slot[gtk:single-selection]{autoselect} slot of the
    @class{gtk:single-selection} class.
  @end{short}
  The @fun{gtk:single-selection-autoselect} function checks if autoselect has
  been enabled or disabled. The @setf{gtk:single-selection-autoselect} function
  sets the property.

  If @arg{autoselect} is @em{true}, @arg{object} will enforce that an item is
  always selected. It will select a new item when the currently selected item
  is deleted and it will disallow unselecting the current item.
  @see-class{gtk:single-selection}")

;;; --- gtk:single-selection-can-unselect --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "can-unselect"
                                               'single-selection) t)
 "The @code{can-unselect} property of type @code{:boolean} (Read / Write) @br{}
  Whether unselecting the selected item is allowed. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'single-selection-can-unselect)
      "Accessor"
      (documentation 'single-selection-can-unselect 'function)
 "@version{2023-11-26}
  @syntax{(gtk:single-selection-can-unselect object) => setting}
  @syntax{(setf (gtk:single-selection-can-unselect object) setting)}
  @argument[object]{a @class{gtk:single-selection} object}
  @argument[setting]{@em{true} to allow unselecting}
  @begin{short}
    Accessor of the @slot[gtk:single-selection]{can-unselect} slot of the
    @class{gtk:single-selection} class.
  @end{short}
  If @em{true}, unselecting the current item is supported. Note that setting
  @slot[gtk:single-selection]{autoselect} will cause the unselecting to not
  work, so it practically makes no sense to set both at the same time the same
  time.
  @see-class{gtk:single-selection}
  @see-function{gtk:single-selection-autoselect}")

;;; --- gtk:single-selection-item-type -----------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "item-type" 'single-selection) t)
 "The @code{item-type} property of type @class{g:type-t} (Read) @br{}
  The type of items. Since 4.8")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'single-selection-item-type)
      "Accessor"
      (documentation 'single-selection-item-type 'function)
 "@version{2024-11-22}
  @syntax{(gtk:single-selection-item-type object) => gtype}
  @argument[object]{a @class{gtk:single-selection} object}
  @argument[gtype]{a @class{g:type-t} type ID}
  @begin{short}
    Accessor of the @slot[gtk:single-selection]{item-type} slot of the
    @class{gtk:single-selection} class.
  @end{short}
  The type of items contained in the list model. Items must be subclasses of
  the @class{g:object} class.

  Since 4.8
  @begin[Notes]{dictionary}
    This function is equivalent to the @fun{g:list-model-item-type} function.
  @end{dictionary}
  @see-class{gtk:single-selection}
  @see-class{g:type-t}
  @see-class{g:object}
  @see-function{g:list-model-item-type}")

;;; --- gtk:single-selection-model ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model" 'single-selection) t)
 "The @code{model} property of type @class{g:list-model} (Read / Write) @br{}
  The model being managed.")

#+liber-documentation
(setf (liber:alias-for-function 'single-selection-model)
      "Accessor"
      (documentation 'single-selection-model 'function)
 "@version{2023-9-6}
  @syntax{(gtk:single-selection-model object) => model}
  @syntax{(setf (gtk:single-selection-model object) model)}
  @argument[object]{a @class{gtk:single-selection} object}
  @argument[model]{a @class{g:list-model} object to wrap}
  @begin{short}
    Accessor of the @slot[gtk:single-selection]{model} slot of the
    @class{gtk:single-selection} class.
  @end{short}
  The @fun{gtk:single-selection-model} function gets the model that @arg{object}
  is wrapping. The @setf{gtk:single-selection-model} function sets the model
  that @arg{object} should wrap. If @arg{model} is @code{nil}, @arg{object} will
  be empty.
  @see-class{gtk:single-selection}
  @see-class{g:list-model}")

;;; --- gtk:single-selection-n-items -------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "n-items" 'single-selection) t)
 "The @code{n-items} property of type @code{:uint} (Read / Write) @br{}
  The number of items. Since 4.8 @br{}
  Default value: 0")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'single-selection-n-items)
      "Accessor"
      (documentation 'single-selection-n-items 'function)
 "@version{2023-9-6}
  @syntax{(gtk:single-selection-n-items object) => n-items}
  @argument[object]{a @class{gtk:single-selection} object}
  @argument[n-items]{an unsigned integer with the number of items contained in
    the model}
  @begin{short}
    Accessor of the @slot[gtk:single-selection]{n-items} slot of the
    @class{gtk:single-selection} class.
  @end{short}
  @see-class{g:single-selection}
  @see-function{g:list-model-n-items}")

;;; --- gtk:single-selection-selected ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "selected" 'single-selection) t)
 "The @code{selected} property of type @code{:uint} (Read / Write) @br{}
  Position of the selected item. @br{}
  Default value: @var{gtk:+invalid-list-position+}")

#+liber-documentation
(setf (liber:alias-for-function 'single-selection-selected)
      "Accessor"
      (documentation 'single-selection-selected 'function)
 "@version{2023-11-26}
  @syntax{(gtk:single-selection-selected object) => position}
  @syntax{(setf (gtk:single-selection-selected object) position)}
  @argument[object]{a @class{gtk:single-selection} object}
  @argument[position]{an unsigned integer with the item to select or the
    @var{gtk:+invalid-list-position+} value}
  @begin{short}
    Accessor of the @slot[gtk:single-selection]{selected} slot of the
    @class{gtk:single-selection} class.
  @end{short}
  The @fun{gtk:single-selection-selected} function gets the position of the
  selected item. If no item is selected, the @var{gtk:+invalid-list-position+}
  value is returned. The @setf{gtk:single-selection-selected} function selects
  the item at the given @arg{position}.

  If the list does not have an item at @arg{position} or the
  @var{gtk:+invalid-list-position+} value is given, the behavior depends on
  the value of the @slot[gtk:single-selection]{autoselect} property. If it is
  set, no change will occur and the old item will stay selected. If it is unset,
  the selection will be unset and no item will be selected.
  @see-class{gtk:single-selection}
  @see-variable{gtk:+invalid-list-position+}")

;;; --- gtk:single-selection-selected-item -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "selected-item"
                                               'single-selection) t)
 "The @code{selected-item} property of type @class{g:object} (Read) @br{}
  The selected item.")

#+liber-documentation
(setf (liber:alias-for-function 'single-selection-selected-item)
      "Accessor"
      (documentation 'single-selection-selected-item 'function)
 "@version{2023-9-6}
  @syntax{(gtk:single-selection-selected object) => item}
  @argument[object]{a @class{gtk:single-selection} object}
  @argument[item]{a @code{:pointer} to the selected item}
  @begin{short}
    Accessor of the @slot[gtk:single-selection]{selected-item} slot of the
    @class{gtk:single-selection} class.
  @end{short}
  The @fun{gtk:single-selection-selected-item} function gets the selected item.
  If no item is selected, @code{nil} ist returned.
  @see-class{gtk:single-selection}")

;;; ----------------------------------------------------------------------------
;;; gtk_single_selection_new
;;; ----------------------------------------------------------------------------

(declaim (inline single-selection-new))

(defun single-selection-new (&optional model)
 #+liber-documentation
 "@version{2023-11-26}
  @argument[model]{an optional @class{g:list-model} object to manage, the
    default is @code{nil}}
  @return{The new @class{gtk:single-selection} object.}
  @short{Creates a new selection to handle @arg{model}.}
  @see-class{gtk:single-selection}
  @see-class{g:list-model}"
  (make-instance 'single-selection
                 :model model))

(export 'single-selection-new)

;;; --- End of file gtk4.single-selection.lisp ---------------------------------
