;;; ----------------------------------------------------------------------------
;;; gtk.text-mark.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
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
;;; GtkTextMark
;;;
;;;     A position in the buffer preserved across buffer modifications
;;;
;;; Types and Values
;;;
;;;     GtkTextMark
;;;
;;; Accessors
;;;
;;;     gtk_text_mark_get_left_gravity
;;;     gtk_text_mark_get_name
;;;
;;; Functions
;;;
;;;     gtk_text_mark_new
;;;     gtk_text_mark_set_visible
;;;     gtk_text_mark_get_visible
;;;     gtk_text_mark_get_deleted
;;;     gtk_text_mark_get_buffer
;;;
;;; Properties
;;;
;;;     left-gravity
;;;     name
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkTextMark
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTextMark
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkTextMark" text-mark
  (:superclass g:object
   :export t
   :interfaces ()
   :type-initializer "gtk_text_mark_get_type")
  ((left-gravity
    text-mark-left-gravity
    "left-gravity" "gboolean" t nil)
   (name
    text-mark-name
    "name" "gchararray" t nil)))

#+liber-documentation
(setf (documentation 'text-mark 'type)
 "@version{#2022-1-23}
  @begin{short}
    A @sym{gtk:text-mark} object is like a bookmark in a text buffer.
  @end{short}
  It preserves a position in the text.

  You can convert the text mark to an iterator using the
  @fun{gtk:text-buffer-iter-at-mark} function. Unlike iterators, text marks
  remain valid across buffer mutations, because their behavior is defined when
  text is inserted or deleted. When text containing a text mark is deleted, the
  text mark remains in the position originally occupied by the deleted text.
  When text is inserted at a text mark, a text mark with left gravity will be
  moved to the beginning of the newly inserted text, and a text mark with right
  gravity will be moved to the end.

  Note that \"left\" and \"right\" here refer to logical direction. Left is
  toward the start of the buffer. In some languages such as Hebrew the
  logically leftmost text is not actually on the left when displayed.

  Text marks are reference counted, but the reference count only controls the
  validity of the memory. Text marks can be deleted from the buffer at any time
  with the @fun{gtk:text-buffer-delete-mark} function. Once deleted from the
  buffer, a text mark is essentially useless.

  Text marks optionally have names. These can be convenient to avoid passing
  the @sym{gtk:text-mark} object around. Text marks are typically created using
  the @fun{gtk:text-buffer-create-mark} function.
  @see-slot{gtk:text-mark-left-gravity}
  @see-slot{gtk:text-mark-name}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-create-mark}
  @see-function{gtk:text-buffer-delete-mark}
  @see-function{gtk:text-buffer-iter-at-mark}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- text-mark-left-gravity ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "left-gravity"
                                               'text-mark) t)
 "The @code{left-gravity} property of type @code{:boolean}
  (Read / Write / Construct only) @br{}
  Whether the text mark has left gravity. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-mark-left-gravity)
      "Accessor"
      (documentation 'text-mark-left-gravity 'function)
 "@version{#2022-1-23}
  @syntax[]{(gtk:text-mark-left-gravity object) => gravity}
  @argument[object]{a @class{gtk:text-mark} object}
  @argument[gravity]{@em{true} if the text mark has left gravity}
  @begin{short}
    Accessor of the @slot[gtk:text-mark]{left-gravity} slot of the
    @class{gtk:text-mark} class.
  @end{short}

  The @sym{gtk:text-mark-left-gravity} function determines whether the text mark
  has left gravity. When text is inserted at the current location of the text
  mark, if the text mark has left gravity it will be moved to the left of the
  newly-inserted text, otherwise to the right.
  @see-class{gtk:text-mark}")

;;; --- text-mark-name -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "name" 'text-mark) t)
 "The @code{name} property of type @code{:string}
  (Read / Write / Construct only) @br{}
  The name of the text mark. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'text-mark-name)
      "Accessor"
      (documentation 'text-mark-name 'function)
 "@version{#2022-1-23}
  @syntax[]{(gtk:text-mark-name object) => name}
  @argument[object]{a @class{gtk:text-mark} object}
  @argument[name]{a string with the name of the text mark}
  @begin{short}
    Accessor of the @slot[gtk:text-mark]{name} slot of the
    @class{gtk:text-mark} class.
  @end{short}

  The @sym{gtk:text-mark-name} function returns the name of the text mark or
  @code{nil} for anonymous text marks.
  @see-class{gtk:text-mark}")

;;; ----------------------------------------------------------------------------
;;; gtk_text_mark_new
;;; ----------------------------------------------------------------------------

(defun text-mark-new (name left-gravity)
 #+liber-documentation
 "@version{#2022-1-23}
  @argument[name]{a string with the name of the text mark or @code{nil}}
  @argument[left-gravity]{a boolean whether the text mark should have left
    gravity}
  @return{New @class{gtk:text-mark} object.}
  @begin{short}
    Creates a text mark.
  @end{short}
  Add the text mark to a text buffer using the @fun{gtk:text-buffer-add-mark}
  function. If @arg{name} is @code{nil}, the text mark is anonymous. Otherwise,
  the text mark can be retrieved by @arg{name} using the
  @fun{gtk:text-buffer-mark} function. If a text mark has left gravity, and text
  is inserted at the text mark's current location, the text mark will be moved
  to the left of the newly inserted text. If the text mark has right gravity,
  @arg{left-gravity} is @em{false}, the text mark will end up on the right of
  newly inserted text. The standard left-to-right cursor is a text mark with
  right gravity, when you type, the cursor stays on the right side of the text
  you are typing.
  @see-class{gtk:text-mark}
  @see-function{gtk:text-buffer-mark}
  @see-function{gtk:text-buffer-add-mark}"
  (if name
      (make-instance 'text-mark
                     :name name
                     :left-gravity left-gravity)
      (make-instance 'text-mark
                     :left-gravity left-gravity)))

(export 'text-mark-new)

;;; ----------------------------------------------------------------------------
;;; gtk_text_mark_set_visible
;;; gtk_text_mark_get_visible -> text-mark-visible
;;; ----------------------------------------------------------------------------

(defun (setf text-mark-visible) (visibility mark)
  (foreign-funcall "gtk_text_mark_set_visible"
                   (g:object) mark
                   :boolean visibility
                   :void)
  visibility)

(defcfun ("gtk_text_mark_get_visible" text-mark-visible) :boolean
 #+liber-documentation
 "@version{#2022-1-23}
  @syntax[]{(gtk:text-mark-visible mark) => visibility}
  @syntax[]{(setf (gtk:text-mark-visible mark) visibility)}
  @argument[mark]{a @class{gtk:text-mark} object}
  @argument[visibility]{a boolean whether the text mark is visible}
  @return{@em{True} if the text mark is visible.}
  @begin{short}
    Accessor for the visibility of a text mark.
  @end{short}

  The @sym{gtk:text-mark-visible} function returns @em{true} if the text mark
  is visible, i.e. a cursor is displayed for it. The
  @sym{(setf gtk:text-mark-visible)} function sets the visibility.

  The insertion point is normally visible, i.e. you can see it as a vertical
  bar. Also, the text widget uses a visible text mark to indicate where a drop
  will occur when dragging-and-dropping text. Most other text marks are not
  visible. Text marks are not visible by default.
  @see-class{gtk:text-mark}"
  (mark (g:object text-mark)))

(export 'text-mark-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_text_mark_get_deleted -> text-mark-deleted
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_mark_get_deleted" text-mark-deleted) :boolean
 #+liber-documentation
 "@version{#2022-1-23}
  @argument[mark]{a @class{gtk:text-mark} object}
  @return{A boolean whether the text mark is deleted.}
  @begin{short}
    Returns @em{true} if the text mark has been removed from its text buffer
    with the @fun{gtk:text-buffer-delete-mark} function.
  @end{short}
  See the @fun{gtk:text-buffer-add-mark} function for a way to add the text
  mark to a text buffer again.
  @see-class{gtk:text-mark}
  @see-function{gtk:text-buffer-add-mark}
  @see-function{gtk:text-buffer-delete-mark}"
  (mark (g:object text-mark)))

(export 'text-mark-deleted)

;;; ----------------------------------------------------------------------------
;;; gtk_text_mark_get_buffer -> text-mark-buffer
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_text_mark_get_buffer" text-mark-buffer) (g:object text-buffer)
 #+liber-documentation
 "@version{#2022-1-23}
  @argument[mark]{a @class{gtk:text-mark} object}
  @return{The @class{gtk:text-buffer} object of the text mark.}
  @begin{short}
    Gets the text buffer this text mark is located inside, or @code{nil} if the
    mark is deleted.
  @end{short}
  @see-class{gtk:text-mark}
  @see-class{gtk:text-buffer}"
  (mark (g:object text-mark)))

(export 'text-mark-buffer)

;;; --- End of file gtk.text-mark.lisp -----------------------------------------
