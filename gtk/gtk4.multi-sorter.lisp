;;; ----------------------------------------------------------------------------
;;; gtk4.multi-sorter.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
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
;;; GtkMultiSorter
;;;
;;;     Combining multiple sorters
;;;
;;; Types and Values
;;;
;;;     GtkMultiSorter
;;;
;;; Functions
;;;
;;;     gtk_multi_sorter_new
;;;     gtk_multi_sorter_append
;;;     gtk_multi_sorter_remove
;;;
;;; Properties
;;;
;;;     item-type                                          Since 4.8
;;;     n-items                                            Since 4.8
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkSorter
;;;         ╰── GtkMultiSorter
;;;
;;; Implemented Interfaces
;;;
;;;     GListModel
;;;     GtkBuildable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkMultiSorter
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkMultiSorter" multi-sorter
  (:superclass sorter
   :export t
   :interfaces ("GListModel"
                "GtkBuildable")
   :type-initializer "gtk_multi_sorter_get_type")
  (#+gtk-4-8
   (item-type
    %multi-sorter-item-type
    "item-type" "GType" t nil)
   #+gtk-4-8
   (n-items
    multi-sorter-n-items
    "n-items" "guint" t nil)))

#+liber-documentation
(setf (documentation 'multi-sorter 'type)
 "@version{2023-9-12}
  @begin{short}
    The @class{gtk:multi-sorter} object combines multiple sorters by trying
    them in turn.
  @end{short}
  If the first sorter compares two items as equal, the second is tried next,
  and so on.
  @see-constructor{gtk:multi-sorter-new}
  @see-slot{gtk:multi-sorter-item-type}
  @see-slot{gtk:multi-sorter-n-items}
  @see-class{gtk:sorter}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:multi-sorter-item-type ---------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "item-type" 'multi-sorter) t)
 "The @code{item-type} property of type @class{g:type-t} (Read) @br{}
  The type of items. Since 4.8")

#+gtk-4-8
(declaim (inline multi-sorter-item-type))

#+gtk-4-8
(defun multi-sorter-item-type (object)
  (g:list-model-item-type object))

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'multi-sorter-item-type)
      "Accessor"
      (documentation 'multi-sorter-item-type 'function)
 "@version{#2023-9-5}
  @syntax{(gtk:multi-sorter-item-type object) => gtype}
  @argument[object]{a @class{gtk:multi-sorter} object}
  @argument[gtype]{a @class{g:type-t} type ID}
  @begin{short}
    Accessor of the @slot[gtk:multi-sorter]{item-type} slot of the
    @class{gtk:multi-sorter} class.
  @end{short}
  The type of items contained in the list model. Items must be subclasses of
  the @class{g:object} class.
  @begin[Note]{dictionary}
    This function is equivalent to the @fun{g:list-model-item-type} function.
  @end{dictionary}
  @see-class{gtk:multi-sorter}
  @see-class{g:type-t}
  @see-class{g:object}
  @see-function{g:list-model-item-type}")

;;; --- gtk:multi-sorter-n-items -----------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "n-items" 'multi-sorter) t)
 "The @code{n-items} property of type @code{:uint} (Read / Write) @br{}
  The number of items. Since 4.8 @br{}
  Default value: 0")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'multi-sorter-n-items)
      "Accessor"
      (documentation 'multi-sorter-n-items 'function)
 "@version{#2023-9-5}
  @syntax{(gtk:multi-sorter-n-items object) => n-items}
  @argument[object]{a @class{gtk:multi-sorter} object}
  @argument[n-items]{an unsigned integer with the number of items contained in
    the model}
  @begin{short}
    Accessor of the @slot[gtk:multi-sorter]{n-items} slot of the
    @class{gtk:multi-sorter} class.
  @end{short}
  @see-class{g:multi-sorter}
  @see-function{g:list-model-n-items}")

;;; ----------------------------------------------------------------------------
;;; gtk_multi_sorter_new
;;; ----------------------------------------------------------------------------

(declaim (inline multi-sorter-new))

(defun multi-sorter-new ()
 #+liber-documentation
 "@version{#2023-9-13}
  @return{The new @class{gtk:multi-sorter} object.}
  @begin{short}
    Creates a new multi sorter.
  @end{short}
  This sorter compares items by trying each of the sorters in turn, until one
  returns non-zero. In particular, if no sorter has been added to it, it will
  always compare items as equal.
  @see-class{gtk:multi-sorter}"
  (make-instance 'multi-sorter))

(export 'multi-sorter-new)

;;; ----------------------------------------------------------------------------
;;; gtk_multi_sorter_append
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_multi_sorter_append" multi-sorter-append) :void
 #+liber-documentation
 "@version{#2023-9-13}
  @argument[sorter]{a @class{gtk:multi-sorter} object}
  @argument[other]{another @class{gtk:sorter} object to add}
  @begin{short}
    Add @arg{other} to @arg{sorter} to use for sorting at the end.
  @end{short}
  The @arg{sorter} object will consult all existing sorters before it will sort
  with the given @arg{other}.
  @see-class{gtk:multi-sorter}
  @see-class{gtk:sorter}"
  (sorter (g:object multi-sorter))
  (other (g:object sorter)))

(export 'multi-sorter-append)

;;; ----------------------------------------------------------------------------
;;; gtk_multi_sorter_remove
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_multi_sorter_remove" multi-sorter-remove) :void
 #+liber-documentation
 "@version{#2023-9-13}
  @argument[sorter]{a @class{gtk:multi-sorter} object}
  @argument[position]{an unsigned integer with the position of the sorter
   to remove}
  @begin{short}
    Removes the sorter at the given position from the list of sorter used by
    @arg{sorter}.
  @end{short}
  If the @arg{position} parameter is larger than the number of sorters, nothing
  happens.
  @see-class{gtk:multi-sorter}"
  (object (g:object multi-sorter))
  (position :uint))

(export 'multi-sorter-remove)

;;; --- End of file gtk4.multi-sorter.lisp -------------------------------------
