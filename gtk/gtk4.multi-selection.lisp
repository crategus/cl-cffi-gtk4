;;; ----------------------------------------------------------------------------
;;; gtk4.multi-selection.lisp
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
;;; GtkMultiSelection
;;;
;;;     A selection model that allows selecting multiple items
;;;
;;; Types and Values
;;;
;;;     GtkMultiSelection
;;;
;;; Accessor
;;;
;;;     gtk_multi_selection_get_model
;;;     gtk_multi_selection_set_model
;;;
;;; Functions
;;;
;;;     gtk_multi_selection_new
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
;;;     ╰── GtkMultiSelection
;;;
;;; Implemented Interfaces
;;;
;;;     GListModel
;;;     GtkSelectionModel
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkMultiSelection
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkMultiSelection" multi-selection
  (:superclass g:object
   :export t
   :interfaces ("GtkSelectionModel"
                #+gtk-4-12 "GtkSectionModel")
   :type-initializer "gtk_multi_selection_get_type")
  (#+gtk-4-8
   (item-type
    %multi-selection-item-type
    "item-type" "GType" t nil)
   (model
    multi-selection-model
    "model" "GListModel" t t)
   #+gtk-4-8
   (n-items
    multi-selection-n-items
    "n-items" "guint" t nil)))

#+liber-documentation
(setf (documentation 'multi-selection 'type)
 "@version{2023-9-6}
  @begin{short}
    The @class{gtk:multi-selection} class is an implementation of the
    @class{gtk:selection-model} interface that allows selecting multiple
    elements.
  @end{short}
  @see-constructor{gtk:multi-selection-new}
  @see-slot{gtk:multi-selection-item-type}
  @see-slot{gtk:multi-selection-model}
  @see-slot{gtk:multi-selection-n-items}
  @see-class{gtk:selection-model}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:multi-selection-item-type ------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "item-type" 'multi-selection) t)
 "The @code{item-type} property of type @class{g:type-t} (Read) @br{}
  The type of items. Since 4.8")

#+gtk-4-8
(declaim (inline multi-selection-item-type))

#+gtk-4-8
(defun multi-selection-item-type (object)
  (g:list-model-item-type object))

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'multi-selection-item-type)
      "Accessor"
      (documentation 'multi-selection-item-type 'function)
 "@version{#2023-9-6}
  @syntax{(gtk:multi-selection-item-type object) => gtype}
  @argument[object]{a @class{gtk:multi-selection} object}
  @argument[gtype]{a @class{g:type-t} type ID}
  @begin{short}
    Accessor of the @slot[gtk:multi-selection]{item-type} slot of the
    @class{gtk:multi-selection} class.
  @end{short}
  The type of items contained in the list model. Items must be subclasses of
  the @class{g:object} class.
  @begin[Notes]{dictionary}
    This function is equivalent to the @fun{g:list-model-item-type} function.
  @end{dictionary}
  @see-class{gtk:multi-selection}
  @see-class{g:type-t}
  @see-class{g:object}
  @see-function{g:list-model-item-type}")

;;; --- gtk:multi-selection-model ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model" 'multi-selection) t)
 "The @code{model} property of type @class{g:list-model} (Read / Write) @br{}
  The model being managed by this selection.")

#+liber-documentation
(setf (liber:alias-for-function 'multi-selection-model)
      "Accessor"
      (documentation 'multi-selection-model 'function)
 "@version{#2023-9-6}
  @syntax{(gtk:multi-selection-model object) => model}
  @syntax{(setf (gtk:multi-selection-model object) model)}
  @argument[object]{a @class{gtk:multi-selection} object}
  @argument[model]{a @class{g:list-model} object to wrap}
  @begin{short}
    Accessor of the @slot[gtk:multi-selection]{model} slot of the
    @class{gtk:multi-selection} class.
  @end{short}
  The @fun{gtk:multi-selection-model} function returns the underlying model of
  @arg{object}. The @setf{gtk:multi-selection-model} function sets the model
  that @arg{object} should wrap. If @arg{model} is @code{nil}, @arg{object}
  will be empty.
  @see-class{gtk:multi-selection}
  @see-class{g:list-model}")

;;; --- gtk:multi-selection-n-items --------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "n-items" 'multi-selection) t)
 "The @code{n-items} property of type @code{:uint} (Read / Write) @br{}
  The number of items. Since 4.8 @br{}
  Default value: 0")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'multi-selection-n-items)
      "Accessor"
      (documentation 'multi-selection-n-items 'function)
 "@version{2023-9-6}
  @syntax{(gtk:multi-selection-n-items object) => n-items}
  @argument[object]{a @class{gtk:multi-selection} object}
  @argument[n-items]{an unsigned integer with the number of items contained in
    the model}
  @begin{short}
    Accessor of the @slot[gtk:multi-selection]{n-items} slot of the
    @class{gtk:multi-selection} class.
  @end{short}
  @see-class{g:multi-selection}
  @see-function{g:list-model-n-items}")

;;; ----------------------------------------------------------------------------
;;; gtk_multi_selection_new
;;; ----------------------------------------------------------------------------

(declaim (inline multi-selection-new))

(defun multi-selection-new (model)
 #+liber-documentation
 "@version{#2023-9-6}
  @argument[model]{a @class{g:list-model} object to manage, or @code{nil}}
  @return{The new @class{gtk:multi-selection} object.}
  @begin{short}
    Creates a new selection to handle @arg{model}.
  @end{short}
  @see-class{gtk:multi-selection}
  @see-class{g:list-model}"
  (make-instance 'multi-selection
                 :model model))

(export 'multi-selection-new)

;;; --- End of file gtk4.multi-selection.lisp ----------------------------------
