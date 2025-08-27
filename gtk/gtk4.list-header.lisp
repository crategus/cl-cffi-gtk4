;;; ----------------------------------------------------------------------------
;;; gtk4.list-header.lisp
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
;;; Types and Values
;;;
;;;     GtkListHeader
;;;
;;; Accessors
;;;
;;;     gtk_list_header_get_child
;;;     gtk_list_header_set_child
;;;     gtk_list_header_get_end
;;;     gtk_list_header_get_item
;;;     gtk_list_header_get_n_items
;;;     gtk_list_header_get_start
;;;
;;; Properties
;;;
;;;     child
;;;     end
;;;     item
;;;     n-items
;;;     start
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkListHeader
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkListHeader
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkListHeader" list-header
  (:superclass g:object
   :export t
   :interfaces ()
   :type-initializer "gtk_list_header_get_type")
  ((child
    list-header-child
    "child" "GtkWidget" t t)
   (end
    list-header-end
    "end" "guint" t nil)
   (item
    list-header-item
    "item" "GObject" t nil)
   (n-items
    list-header-n-items
    "n-items" "gunit" t nil)
   (start
    list-header-start
    "start" "guint" t nil)))

#+liber-documentation
(setf (documentation 'list-header 'type)
 "@version{2025-04-07}
  @begin{short}
    The @class{gtk:list-header} object is used by list widgets to represent the
    headers they display.
  @end{short}

  The @class{gtk:list-header} objects are managed just like
  @class{gtk:list-item} objects through their factory, but provide a different
  set of properties suitable for managing the header instead of individual
  items.

  Since 4.12
  @see-slot{gtk:list-header-child}
  @see-slot{gtk:list-header-end}
  @see-slot{gtk:list-header-item}
  @see-slot{gtk:list-header-n-items}
  @see-slot{gtk:list-header-start}
  @see-class{gtk:list-item}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:list-header-child --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'list-header) t)
 "The @code{child} property of type @class{gtk:widget} (Read / Write) @br{}
  The widget used for display.")

#+liber-documentation
(setf (liber:alias-for-function 'list-header-child)
      "Accessor"
      (documentation 'list-header-child 'function)
 "@version{2025-04-11}
  @syntax{(gtk:list-header-child object) => child}
  @syntax{(setf (gtk:list-header-child object) child)}
  @argument[object]{a @class{gtk:list-header} object}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Accessor of the @slot[gtk:list-header]{child} slot of the
    @class{gtk:list-header} class.
  @end{short}
  The @fun{gtk:list-header-child} function gets the child widget previously set
  or @code{nil} if none was set. The @setf{gtk:list-header-child} function sets
  the child widget to be used for this list item.

  This function is typically called by applications when setting up a header so
  that the widget can be reused when binding it multiple times.

  Since 4.12
  @see-class{gtk:list-header}")

;;; --- gtk:list-header-end ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "end" 'list-header) t)
 "The @code{end} property of type @code{guint} (Read) @br{}
  The first position no longer part of the section. @br{}
  Default value: @var{gtk:+invalid-list-position+}")

#+liber-documentation
(setf (liber:alias-for-function 'list-header-end)
      "Accessor"
      (documentation 'list-header-end 'function)
 "@version{2025-04-07}
  @syntax{(gtk:list-header-end object) => end}
  @argument[object]{a @class{gtk:list-header} object}
  @argument[end]{an unsigned integer for the end position of the section}
  @begin{short}
    Accessor of the @slot[gtk:list-header]{end} slot of the
    @class{gtk:list-header} class.
  @end{short}
  The @fun{gtk:list-header-end} function gets the end position in the model of
  the section that @arg{object} is currently the header for. If @arg{object} is
  unbound, the @var{gtk:+invalid-list-position+} value is returned.

  Since 4.12
  @see-class{gtk:list-header}
  @see-variable{gtk:+invalid-list-position+}")

;;; --- gtk:list-header-item ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "item" 'list-header) t)
 "The @code{item} property of type @class{g:object} (Read) @br{}
  The item at the start of the section.")

#+liber-documentation
(setf (liber:alias-for-function 'list-header-item)
      "Accessor"
      (documentation 'list-header-item 'function)
 "@version{2025-04-07}
  @syntax{(gtk:list-header-item object) => item}
  @argument[object]{a @class{gtk:list-header} object}
  @argument[item]{a @class{g:object} instance for the item displayed}
  @begin{short}
    Accessor of the @slot[gtk:list-header]{item} slot of the
    @class{gtk:list-header} class.
  @end{short}
  The @fun{gtk:list-header-item} function gets the model item at the start of
  the section. This is the item that occupies the list model at the
  @slot[gtk:list-header]{start} position.

  If @arg{object} is unbound, this function returns @code{nil}.

  Since 4.12
  @see-class{gtk:list-header}
  @see-function{gtk:list-header-start}")

;;; --- gtk:list-header-n-items ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "n-items" 'list-header) t)
 "The @code{n-items} property of type @code{guint} (Read) @br{}
  The number of items in the section.")

#+liber-documentation
(setf (liber:alias-for-function 'list-header-n-items)
      "Accessor"
      (documentation 'list-header-n-items 'function)
 "@version{2025-08-17}
  @syntax{(gtk:list-header-n-items object) => n-items}
  @argument[object]{a @class{gtk:list-header} object}
  @argument[n-items]{an unsigned integer for the number of items in the
    section}
  @begin{short}
    The accessor for the @slot[gtk:list-header]{n-items} slot of the
    @class{gtk:list-header} class returns the the number of items in the
    section.
  @end{short}
  If @arg{object} is unbound, 0 is returned.

  Since 4.12
  @see-class{gtk:list-header}
  @see-function{gtk:list-header-start}")

;;; --- gtk:list-header-start --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "start" 'list-header) t)
 "The @code{start} property of type @code{guint} (Read) @br{}
  The first position of items in the section. @br{}
  Default value: @var{gtk:+invalid-list-position+}")

#+liber-documentation
(setf (liber:alias-for-function 'list-header-start)
      "Accessor"
      (documentation 'list-header-start 'function)
 "@version{2025-04-07}
  @syntax{(gtk:list-header-start object) => start}
  @argument[object]{a @class{gtk:list-header} object}
  @argument[start]{an unsigned integer for the start position of the section}
  @begin{short}
    Accessor of the @slot[gtk:list-header]{start} slot of the
    @class{gtk:list-header} class.
  @end{short}
  The @fun{gtk:list-header-start} function gets the start position in the model
  of the section that @arg{object} is currently the header for.

  If @arg{object} is unbound, the @var{gtk:+invalid-list-position+} value is
  returned.

  Since 4.12
  @see-class{gtk:list-header}
  @see-variable{gtk:+invalid-list-position+}")

;;; --- End of file gtk4.list-header.lisp --------------------------------------
