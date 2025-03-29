;;; ----------------------------------------------------------------------------
;;; gtk4.no-selection.lisp
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
;;; GtkNoSelection
;;;
;;;     A selection model that does not allow selecting anything
;;;
;;; Types and Values
;;;
;;;     GtkNoSelection
;;;
;;; Accessors
;;;
;;;     gtk_no_selection_get_model
;;;     gtk_no_selection_set_model
;;;
;;; Functions
;;;
;;;     gtk_no_selection_new
;;;
;;; Properties
;;;
;;;     item-type                                          Since 4.8
;;;     model
;;;     n-items                                            Since 4.8
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkNoSelection
;;;
;;; Implemented Interfaces
;;;
;;;     GListModel
;;;     GtkSelectionModel
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkNoSelection
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkNoSelection" no-selection
  (:superclass g:object
   :export t
   :interfaces ("GtkSelectionModel"
                #+gtk-4-12 "GtkSectionModel")
   :type-initializer "gtk_no_selection_get_type")
  (#+gtk-4-8
   (item-type
    no-selection-item-type
    "item-type" "GType" t nil)
   (model
    no-selection-model
    "model" "GListModel" t t)
   #+gtk-4-8
   (n-items
    no-selection-n-items
    "n-items" "guint" t nil)))

#+liber-documentation
(setf (documentation 'no-selection 'type)
 "@version{2024-12-22}
  @begin{short}
    The @class{gtk:no-selection} class is an implementation of the
    @class{gtk:selection-model} interface that does not allow selecting
    anything.
  @end{short}
  This model is meant to be used as a simple wrapper to @class{g:list-model}
  objects when a @class{gtk:selection-model} object is required. The
  @class{gtk:no-selection} object passes through sections from the underlying
  model.
  @see-constructor{gtk:no-selection-new}
  @see-class{gtk:selection-model}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:no-selection-item-type ---------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "item-type" 'no-selection) t)
 "The @code{item-type} property of type @class{g:type-t} (Read) @br{}
  The type of items. Since 4.8")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'no-selection-item-type)
      "Accessor"
      (documentation 'no-selection-item-type 'function)
 "@version{2024-12-22}
  @syntax{(gtk:no-selection-item-type object) => gtype}
  @argument[object]{a @class{gtk:no-selection} object}
  @argument[gtype]{a @class{g:type-t} type ID}
  @begin{short}
    Accessor of the @slot[gtk:no-selection]{item-type} slot of the
    @class{gtk:no-selection} class.
  @end{short}
  The type of items contained in the list model. Items must be subclasses of
  the @class{g:object} class.

  Since 4.8
  @begin[Notes]{dictionary}
    This function is equivalent to the @fun{g:list-model-item-type} function.
  @end{dictionary}
  @see-class{gtk:no-selection}
  @see-class{g:type-t}
  @see-class{g:object}
  @see-function{g:list-model-item-type}")

;;; --- gtk:no-selection-model -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model" 'no-selection) t)
 "The @code{model} property of type @class{g:list-model} (Read / Write) @br{}
  The model being managed.")

#+liber-documentation
(setf (liber:alias-for-function 'no-selection-model)
      "Accessor"
      (documentation 'no-selection-model 'function)
 "@version{2025-3-17}
  @syntax{(gtk:no-selection-model object) => model}
  @syntax{(setf (gtk:no-selection-model object) model)}
  @argument[object]{a @class{gtk:no-selection} object}
  @argument[model]{a @class{g:list-model} object to wrap}
  @begin{short}
    Accessor of the @slot[gtk:no-selection]{model} slot of the
    @class{gtk:no-selection} class.
  @end{short}
  The @fun{gtk:no-selection-model} function gets the model that @arg{object}
  is wrapping. The @setf{gtk:no-selection-model} function sets the model that
  @arg{object} should wrap. If @arg{model} is @code{nil}, this model will be
  empty.
  @see-class{gtk:no-selection}
  @see-class{g:list-model}")

;;; --- gtk:no-selection-n-items -----------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "n-items" 'no-selection) t)
 "The @code{n-items} property of type @code{:uint} (Read / Write) @br{}
  The number of items. Since 4.8 @br{}
  Default value: 0")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'no-selection-n-items)
      "Accessor"
      (documentation 'no-selection-n-items 'function)
 "@version{2025-3-17}
  @syntax{(gtk:no-selection-n-items object) => n-items}
  @argument[object]{a @class{gtk:no-selection} object}
  @argument[n-items]{an unsigned integer for the number of items contained in
    the model}
  @begin{short}
    Accessor of the @slot[gtk:no-selection]{n-items} slot of the
    @class{gtk:no-selection} class.
  @end{short}

  Since 4.8
  @see-class{gtk:no-selection}
  @see-function{g:list-model-n-items}")

;;; ----------------------------------------------------------------------------
;;; gtk_no_selection_new
;;; ----------------------------------------------------------------------------

(declaim (inline no-selection-new))

(defun no-selection-new (&optional model)
 #+liber-documentation
 "@version{2024-11-27}
  @argument[model]{an optional @class{g:list-model} object to manage, or
    the default @code{nil} value}
  @return{The new @class{gtk:no-selection} object.}
  @begin{short}
    Creates a new selection to handle the given @arg{model}.
  @end{short}
  @see-class{gtk:no-selection}"
  (make-instance 'no-selection
                 :model model))

(export 'no-selection-new)

;;; --- End of file gtk4.no-selection.lisp -------------------------------------
