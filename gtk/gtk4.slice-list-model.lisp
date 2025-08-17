;;; ----------------------------------------------------------------------------
;;; gtk4.slice-list-model.lisp
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
;;; GtkSliceListModel
;;;
;;;     A list model that presents a slice out of a larger list
;;;
;;; Types and Values
;;;
;;;     GtkSliceListModel
;;;
;;; Accessors
;;;
;;;     gtk_slice_list_model_set_model
;;;     gtk_slice_list_model_get_model
;;;     gtk_slice_list_model_set_offset
;;;     gtk_slice_list_model_get_offset
;;;     gtk_slice_list_model_set_size
;;;     gtk_slice_list_model_get_size
;;;
;;; Functions
;;;
;;;     gtk_slice_list_model_new
;;;
;;; Properties
;;;
;;;     item-type                                           Since 4.8
;;;     model
;;;     n-items                                             Since 4.8
;;;     offset
;;;     size
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkSliceListModel
;;;
;;; Implemented Interfaces
;;;
;;;     GListModel
;;;     GtkSectionModel                                     Since 4.12
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkSliceListModel
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkSliceListModel" slice-list-model
  (:superclass g:object
   :export t
   :interfaces ("GListModel"
                #+gtk-4-12 "GtkSectionModel")
   :type-initializer "gtk_slice_list_model_get_type")
  (#+gtk-4-8
   (item-type
    slice-list-model-item-type
    "item-type" "GType" t nil)
   (model
    slice-list-model-model
    "model" "GListModel" t t)
   #+gtk-4-8
   (n-items
    slice-list-model-n-items
    "n-items" "guint" t nil)
   (offset
    slice-list-model-offset
    "offset" "guint" t t)
   (size
    slice-list-model-size
    "size" "guint" t t)))

#+liber-documentation
(setf (documentation 'slice-list-model 'type)
 "@version{2024-12-15}
  @begin{short}
    The @class{gtk:slice-list-model} object is a list model that takes a list
    model and presents a slice of that model.
  @end{short}
  This is useful when implementing paging by setting the size to the number of
  elements per page and updating the offset whenever a different page is opened.
  The @class{gtk:slice-list-model} object passes through sections from the
  underlying model.
  @see-constructor{gtk:slice-list-model-new}
  @see-slot{gtk:slice-list-model-item-type}
  @see-slot{gtk:slice-list-model-model}
  @see-slot{gtk:slice-list-model-n-items}
  @see-slot{gtk:slice-list-model-offset}
  @see-slot{gtk:slice-list-model-size}
  @see-class{g:list-model}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:slice-list-model-item-type -----------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "item-type" 'slice-list-model) t)
 "The @code{item-type} property of type @class{g:type-t} (Read) @br{}
  The type of items. Since 4.8")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'slice-list-model-item-type)
      "Accessor"
      (documentation 'slice-list-model-item-type 'function)
 "@version{2025-08-13}
  @syntax{(gtk:slice-list-model-item-type object) => gtype}
  @argument[object]{a @class{gtk:slice-list-model} object}
  @argument[gtype]{a @class{g:type-t} type ID}
  @begin{short}
    The accessor for the @slot[gtk:slice-list-model]{item-type} slot of the
    @class{gtk:slice-list-model} class gets or sets the type of items contained
    in the list model.
  @end{short}
  Items must be subclasses of the @class{g:object} class.

  Since 4.8
  @begin[Notes]{dictionary}
    This function is equivalent to the @fun{g:list-model-item-type} function.
  @end{dictionary}
  @see-class{gtk:slice-list-model}
  @see-class{g:type-t}
  @see-class{g:object}
  @see-function{g:list-model-item-type}")

;;; --- gtk:slice-list-model-model ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model" 'slice-list-model) t)
 "The @code{model} property of type @class{g:list-model} (Read / Write) @br{}
  The child model to take slice from.")

#+liber-documentation
(setf (liber:alias-for-function 'slice-list-model-model)
      "Accessor"
      (documentation 'slice-list-model-model 'function)
 "@version{2025-08-13}
  @syntax{(gtk:slice-list-model-model object) => model}
  @syntax{(setf (gtk:slice-list-model-model object) model)}
  @argument[object]{a @class{gtk:slice-list-model} object}
  @argument[model]{a @class{g:list-model} object to be sliced}
  @begin{short}
    The accessor for the @slot[gtk:slice-list-model]{model} slot of the
    @class{gtk:slice-list-model} class gets or sets the model that is  being
    used to take slice from.
  @end{short}
  Returns @code{nil} if none. The item type of @arg{model} must conform to the
  item type of @arg{object}.
  @see-class{gtk:slice-list-model}
  @see-class{g:list-model}")

;;; --- gtk:slice-list-model-n-items -------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "n-items" 'slice-list-model) t)
 "The @code{n-items} property of type @code{:uint} (Read) @br{}
  The number of items. @br{}
  Default value: 0")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'slice-list-model-n-items)
      "Accessor"
      (documentation 'slice-list-model-n-items 'function)
 "@version{2025-08-13}
  @syntax{(gtk:slice-list-model-n-items object) => n-items}
  @argument[object]{a @class{gtk:slice-list-model} object}
  @argument[n-items]{an unsigned integer for the number of items contained in
    the model}
  @begin{short}
    The accessor for the @slot[gtk:slice-list-model]{n-items} slot of the
    @class{gtk:slice-list-model} class gets the number of items.
  @end{short}
  @see-class{g:slice-list-model}
  @see-function{g:list-model-n-items}")

;;; --- gtk:slice-list-model-offset --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "offset" 'slice-list-model) t)
 "The @code{offset} property of type @code{:int} (Read / Write) @br{}
  The offset of slice. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'slice-list-model-offset)
      "Accessor"
      (documentation 'slice-list-model-offset 'function)
 "@version{2025-08-13}
  @syntax{(gtk:slice-list-model-offset object) => offset}
  @syntax{(setf (gtk:slice-list-model-offset object) offset)}
  @argument[object]{a @class{gtk:slice-list-model} object}
  @argument[offset]{an integer for the offset}
  @begin{short}
    The accessor for the @slot[gtk:slice-list-model]{offset} slot of the
    @class{gtk:slice-list-model} class gets or sets the offset into the original
    model for this slice.
  @end{short}
  If the offset is too large for the sliced model, @arg{object} will end up
  empty.
  @see-class{gtk:slice-list-model}")

;;; --- gtk:slice-list-model-size ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "size" 'slice-list-model) t)
 "The @code{size} property of type @code{:uint} (Read / Write) @br{}
  The maximum size of slice. @br{}
  Default value: 10")

#+liber-documentation
(setf (liber:alias-for-function 'slice-list-model-size)
      "Accessor"
      (documentation 'slice-list-model-size 'function)
 "@version{2025-08-13}
  @syntax{(gtk:slice-list-model-size object) => size}
  @syntax{(setf (gtk:slice-list-model-size object) size)}
  @argument[object]{a @class{gtk:slice-list-model} object}
  @argument[size]{an unsigned integer for the size}
  @begin{short}
    The accessor for the @slot[gtk:slice-list-model]{size} slot of the
    @class{gtk:slice-list-model} class gets or sets the maximum size of slice.
  @end{short}
  @arg{object} will never have more items than @arg{size}. It can however have
  fewer items if the offset is too large or the model sliced from does not have
  enough items.
  @see-class{gtk:slice-list-model}")

;;; ----------------------------------------------------------------------------
;;; gtk_slice_list_model_new
;;; ----------------------------------------------------------------------------

(declaim (inline slice-list-model-new))

(defun slice-list-model-new (model offset size)
 #+liber-documentation
 "@version{2025-03-18}
  @argument[model]{a @class{g:list-model} object to use, or @code{nil}}
  @argument[offset]{an unsigned integer for the offset of the slize}
  @argument[size]{an unsigned integer for the maximum size of the slize}
  @return{The new @class{gtk:slice-list-model} object.}
  @begin{short}
    Creates a new slice model that presents the slice from @arg{offset} to
    @arg{offset} + @arg{size} out of the given @arg{model}.
  @end{short}
  @see-class{gtk:slice-list-model}
  @see-class{g:list-model}"
  (make-instance 'slice-list-model
                 :model model
                 :offset offset
                 :size size))

(export 'slice-list-model-new)

;;; --- End of file gtk4.slice-list-model.lisp ---------------------------------
