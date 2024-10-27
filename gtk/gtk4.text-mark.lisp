;;; ----------------------------------------------------------------------------
;;; gtk4.text-mark.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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

(gobject:define-gobject "GtkTextMark" text-mark
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
 "@version{2024-7-2}
  @begin{short}
    The @class{gtk:text-mark} object is like a bookmark in a text buffer.
  @end{short}
  It preserves a position in the text buffer. You can convert the text mark to
  an iterator using the @fun{gtk:text-buffer-iter-at-mark} function. Unlike
  iterators, text marks remain valid across buffer mutations, because their
  behavior is defined when text is inserted or deleted. When text containing a
  text mark is deleted, the text mark remains in the position originally
  occupied by the deleted text. When text is inserted at a text mark, a text
  mark with left gravity will be moved to the beginning of the newly inserted
  text, and a text mark with right gravity will be moved to the end.

  Note that \"left\" and \"right\" here refer to logical direction. Left is
  toward the start of the buffer. In some languages such as Hebrew the
  logically leftmost text is not actually on the left when displayed.

  Text marks are reference counted, but the reference count only controls the
  validity of the memory. Text marks can be deleted from the buffer at any time
  with the @fun{gtk:text-buffer-delete-mark} function. Once deleted from the
  buffer, a text mark is essentially useless.

  Text marks optionally have names. These can be convenient to avoid passing
  the @class{gtk:text-mark} object around. Text marks are typically created
  using the @fun{gtk:text-buffer-create-mark} function.
  @see-constructor{gtk:text-mark-new}
  @see-slot{gtk:text-mark-left-gravity}
  @see-slot{gtk:text-mark-name}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-create-mark}
  @see-function{gtk:text-buffer-delete-mark}
  @see-function{gtk:text-buffer-iter-at-mark}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:text-mark-left-gravity ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "left-gravity" 'text-mark) t)
 "The @code{left-gravity} property of type @code{:boolean}
  (Read / Write / Construct only) @br{}
  Whether the text mark has left gravity. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-mark-left-gravity)
      "Accessor"
      (documentation 'text-mark-left-gravity 'function)
 "@version{2024-7-2}
  @syntax{(gtk:text-mark-left-gravity object) => gravity}
  @argument[object]{a @class{gtk:text-mark} object}
  @argument[gravity]{@em{true} if the text mark has left gravity}
  @begin{short}
    Accessor of the @slot[gtk:text-mark]{left-gravity} slot of the
    @class{gtk:text-mark} class.
  @end{short}
  The @fun{gtk:text-mark-left-gravity} function determines whether the text mark
  has left gravity. When text is inserted at the current location of the text
  mark, if the text mark has left gravity it will be moved to the left of the
  newly-inserted text, otherwise to the right.
  @see-class{gtk:text-mark}")

;;; --- gtk:text-mark-name -----------------------------------------------------

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
 "@version{2024-7-2}
  @syntax{(gtk:text-mark-name object) => name}
  @argument[object]{a @class{gtk:text-mark} object}
  @argument[name]{a string with the name of the text mark}
  @begin{short}
    Accessor of the @slot[gtk:text-mark]{name} slot of the
    @class{gtk:text-mark} class.
  @end{short}
  The @fun{gtk:text-mark-name} function returns the name of the text mark or
  @code{nil} for anonymous text marks.
  @see-class{gtk:text-mark}")

;;; ----------------------------------------------------------------------------
;;; gtk_text_mark_new
;;; ----------------------------------------------------------------------------

(defun text-mark-new (name &optional gravity)
 #+liber-documentation
 "@version{2024-7-2}
  @argument[name]{a string with the name of the text mark or @code{nil}}
  @argument[gravity]{an optional boolean whether the text mark should have left
    gravity, the default is @em{false}}
  @return{The new @class{gtk:text-mark} object.}
  @begin{short}
    Creates a text mark.
  @end{short}
  Add the text mark to a text buffer using the @fun{gtk:text-buffer-add-mark}
  function. If the @arg{name} argument is @code{nil}, the text mark is
  anonymous. Otherwise, the text mark can be retrieved by @arg{name} using the
  @fun{gtk:text-buffer-mark} function. If a text mark has left gravity, and text
  is inserted at the text current location of the text mark, the text mark will
  be moved to the left of the newly inserted text. If the text mark has right
  gravity, @arg{gravity} is @em{false}, the text mark will end up on the right
  of newly inserted text. The standard left-to-right cursor is a text mark with
  right gravity, when you type, the cursor stays on the right side of the text
  you are typing.
  @see-class{gtk:text-mark}
  @see-function{gtk:text-buffer-mark}
  @see-function{gtk:text-buffer-add-mark}"
  (if name
      (make-instance 'text-mark
                     :name name
                     :left-gravity gravity)
      (make-instance 'text-mark
                     :left-gravity gravity)))

(export 'text-mark-new)

;;; ----------------------------------------------------------------------------
;;; gtk_text_mark_set_visible
;;; gtk_text_mark_get_visible
;;; ----------------------------------------------------------------------------

(defun (setf text-mark-visible) (visibility mark)
  (cffi:foreign-funcall "gtk_text_mark_set_visible"
                        (g:object) mark
                        :boolean visibility
                        :void)
  visibility)

(cffi:defcfun ("gtk_text_mark_get_visible" text-mark-visible) :boolean
 #+liber-documentation
 "@version{2024-7-2}
  @syntax{(gtk:text-mark-visible mark) => visibility}
  @syntax{(setf (gtk:text-mark-visible mark) visibility)}
  @argument[mark]{a @class{gtk:text-mark} object}
  @argument[visibility]{a boolean whether the text mark is visible}
  @return{@em{True} if the text mark is visible.}
  @begin{short}
    The @fun{gtk:text-mark-visible} function returns @em{true} if the text mark
    is visible, that is a cursor is displayed for it.
  @end{short}
  The @setf{gtk:text-mark-visible} function sets the visibility.

  The insertion point is normally visible, that is, you can see it as a vertical
  bar. Also, the text widget uses a visible text mark to indicate where a drop
  will occur when dragging-and-dropping text. Most other text marks are not
  visible. Text marks are not visible by default.
  @see-class{gtk:text-mark}"
  (mark (g:object text-mark)))

(export 'text-mark-visible)

;;; ----------------------------------------------------------------------------
;;; gtk_text_mark_get_deleted
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_mark_get_deleted" text-mark-deleted) :boolean
 #+liber-documentation
 "@version{2024-7-2}
  @argument[mark]{a @class{gtk:text-mark} object}
  @return{@em{True} if the text mark is deleted.}
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
;;; gtk_text_mark_get_buffer
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_mark_get_buffer" text-mark-buffer)
    (g:object text-buffer)
 #+liber-documentation
 "@version{2024-7-2}
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

;;; --- End of file gtk4.text-mark.lisp ----------------------------------------
