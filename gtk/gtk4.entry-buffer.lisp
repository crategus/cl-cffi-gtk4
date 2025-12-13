;;; ----------------------------------------------------------------------------
;;; gtk4.entry-buffer.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2025 Dieter Kaiser
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
;;; GtkEntryBuffer
;;;
;;;     Text buffer for GtkEntry
;;;
;;; Types and Values
;;;
;;;     GtkEntryBuffer
;;;
;;; Accessors
;;;
;;;     gtk_entry_buffer_get_length
;;;     gtk_entry_buffer_get_max_length
;;;     gtk_entry_buffer_set_max_length
;;;     gtk_entry_buffer_get_text
;;;     gtk_entry_buffer_set_text
;;;
;;; Functions
;;;
;;;     gtk_entry_buffer_new
;;;     gtk_entry_buffer_get_bytes
;;;     gtk_entry_buffer_insert_text
;;;     gtk_entry_buffer_delete_text
;;;     gtk_entry_buffer_emit_deleted_text
;;;     gtk_entry_buffer_emit_inserted_text
;;;
;;; Properties
;;;
;;;     length
;;;     max-length
;;;     text
;;;
;;; Signals
;;;
;;;     deleted-text
;;;     inserted-text
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEntryBuffer
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkEntryBuffer
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkEntryBuffer" entry-buffer
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_entry_buffer_get_type")
  ((length
    entry-buffer-length
    "length" "guint" t nil)
   (max-length
    entry-buffer-max-length
    "max-length" "gint" t t)
   (text
    entry-buffer-text
    "text" "gchararray" t t)))

#+liber-documentation
(setf (documentation 'entry-buffer 'type)
 "@version{2025-07-15}
  @begin{short}
    The @class{gtk:entry-buffer} object contains the actual text displayed in a
    @class{gtk:entry} widget.
  @end{short}
  A single @class{gtk:entry-buffer} object can be shared by multiple
  @class{gtk:entry} widgets which will then share the same text content, but
  not the cursor position, visibility attributes, icon, and so on.

  The @class{gtk:entry-buffer} class may be derived from. Such a derived class
  might allow text to be stored in an alternate location, such as non-pageable
  memory, useful in the case of important passwords. Or a derived class could
  integrate with an undo/redo concept of the application.
  @begin[Signal Details]{dictionary}
    @begin[entry-buffer::deleted-text]{signal}
      @begin{pre}
lambda (buffer pos nchars)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:entry-buffer} object.}
        @entry[pos]{The integer for the position the text was deleted at.}
        @entry[nchars]{The integer for the number of characters that were
          deleted.}
      @end{simple-table}
      The signal is emitted after text is deleted from the entry buffer.
    @end{signal}
    @begin[entry-buffer::inserted-text]{signal}
      @begin{pre}
lambda (buffer pos chars nchars)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:entry-buffer} object.}
        @entry[pos]{The integer for the position the text was inserted at.}
        @entry[chars]{The string for the text that was inserted.}
        @entry[nchars]{The integer for the number of characters that were
          inserted.}
      @end{simple-table}
      The signal is emitted after text is inserted into the entry buffer.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:entry-buffer-new}
  @see-slot{gtk:entry-buffer-length}
  @see-slot{gtk:entry-buffer-max-length}
  @see-slot{gtk:entry-buffer-text}
  @see-class{gtk:entry}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:entry-buffer-length ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "length" 'entry-buffer) t)
 "The @code{length} property of type @code{:uint} (Read) @br{}
  The length, in characters, of the text in the entry buffer. @br{}
  Allowed values: <= 65535 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'entry-buffer-length)
      "Accessor"
      (documentation 'entry-buffer-length 'function)
 "@version{2025-08-09}
  @syntax{(gtk:entry-buffer-length object) => length}
  @argument[object]{a @class{gtk:entry-buffer} object}
  @argument[length]{an unsigned integer for the length of the text}
  @begin{short}
    The accessor for the @slot[gtk:entry-buffer]{length} slot of the
    @class{gtk:entry-buffer} class gets or sets the length in characters of
    the entry buffer.
  @end{short}
  @see-class{gtk:entry-buffer}
  @see-function{gtk:entry-buffer-bytes}")

;;; --- gtk:entry-buffer-max-length --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "max-length" 'entry-buffer) t)
 "The @code{max-length} property of type @code{:int} (Read / Write) @br{}
  The maximum length, in characters, of the text in the entry buffer. @br{}
  Allowed values: [0,65535] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'entry-buffer-max-length)
      "Accessor"
      (documentation 'entry-buffer-max-length 'function)
 "@version{2025-08-09}
  @syntax{(gtk:entry-buffer-max-length object) => length}
  @syntax{(setf (gtk:entry-buffer-max-length object) length)}
  @argument[object]{a @class{gtk:entry-buffer} object}
  @argument[length]{an integer for the maximum length of the entry buffer,
    or 0 for no maximum, the value passed in will be clamped to the range
    [0, 65536]}
  @begin{short}
    The accessor for the @slot[gtk:entry-buffer]{max-length} slot of the
    @class{gtk:entry-buffer} class gets or sets the maximum number of
    characters allowed in the entry buffer.
  @end{short}
  Returns 0 if there is no maximum. If the current content is longer than
  the given length, it will be truncated to fit.
  @see-class{gtk:entry-buffer}")

;;; --- gtk:entry-buffer-text --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "text" 'entry-buffer) t)
 "The @code{text} property of type @code{:string} (Read / Write) @br{}
  The contents of the entry buffer. @br{}
  Default value: \"\"")

#+liber-documentation
(setf (liber:alias-for-function 'entry-buffer-text)
      "Accessor"
      (documentation 'entry-buffer-text 'function)
 "@version{2025-08-09}
  @syntax{(gtk:entry-buffer-text object) => text}
  @syntax{(setf (gtk:entry-buffer-text object) text)}
  @argument[object]{a @class{gtk:entry-buffer} object}
  @argument[text]{a string for the contents of the entry buffer}
  @begin{short}
    The accessor for the @slot[gtk:entry-buffer]{text} slot of the
    @class{gtk:entry-buffer} class gets or sets the contents of the entry
    buffer.
  @end{short}
  This is roughly equivalent to calling the @fun{gtk:entry-buffer-delete-text}
  and @fun{gtk:entry-buffer-insert-text} functions.
  @see-class{gtk:entry-buffer}
  @see-function{gtk:entry-buffer-delete-text}
  @see-function{gtk:entry-buffer-insert-text}")

;;; ----------------------------------------------------------------------------
;;; gtk_entry_buffer_new
;;; ----------------------------------------------------------------------------

(defun entry-buffer-new (&optional text)
 #+liber-documentation
 "@version{2025-01-05}
  @argument[text]{an optional string for the initial entry buffer text}
  @return{The new @class{gtk:entry-buffer} object.}
  @begin{short}
    Create a new entry buffer.
  @end{short}
  Optionally, specify initial text to set in the entry buffer.
  @see-class{gtk:entry-buffer}"
  (let ((buffer (make-instance 'entry-buffer)))
    (when text
      (setf (entry-buffer-text buffer) text))
    buffer))

(export 'entry-buffer-new)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_buffer_get_bytes
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_buffer_get_bytes" entry-buffer-bytes) :size
 #+liber-documentation
 "@version{2025-07-17}
  @argument[buffer]{a @class{gtk:entry-buffer} object}
  @return{The unsigned integer for the byte length of @arg{buffer}.}
  @begin{short}
    Retrieves the length in bytes of the entry buffer.
  @end{short}
  See also the @fun{gtk:entry-buffer-length} function for the length in
  characters.
  @see-class{gtk:entry-buffer}
  @see-function{gtk:entry-buffer-length}"
  (buffer (g:object entry-buffer)))

(export 'entry-buffer-bytes)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_buffer_insert_text
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_buffer_insert_text" %entry-buffer-insert-text) :uint
  (buffer (g:object entry-buffer))
  (pos :uint)
  (text :string)
  (nchars :int))

(defun entry-buffer-insert-text (buffer pos text)
 #+liber-documentation
 "@version{2025-07-17}
  @argument[buffer]{a @class{gtk:entry-buffer} object}
  @argument[pos]{an integer for the position at which to insert text}
  @argument[text]{a string for the text to insert into the entry buffer}
  @return{The unsigned integer for the number of characters actually inserted.}
  @begin{short}
    Inserts text into the contents of the entry buffer, at the given position.
  @end{short}
  If the @arg{pos} argument or the length of the text are out of bounds, or the
  maximum text buffer length is exceeded, then they are coerced to sane values.
  Note that the position is specified in characters, not in bytes.
  @see-class{gtk:entry-buffer}
  @see-function{gtk:entry-buffer-delete-text}"
  (%entry-buffer-insert-text buffer pos text -1))

(export 'entry-buffer-insert-text)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_buffer_delete_text
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_buffer_delete_text" entry-buffer-delete-text) :uint
 #+liber-documentation
 "@version{2025-07-17}
  @argument[buffer]{a @class{gtk:entry-buffer} object}
  @argument[pos]{an unsigned integer for the position at which to delete text}
  @argument[nchars]{an integer for the number of characters to delete}
  @return{The unsigned integer for the number of characters deleted.}
  @begin{short}
    Deletes a sequence of characters from the entry buffer.
  @end{short}
  @arg{nchars} characters are deleted starting at @arg{pos}. If the @arg{nchars}
  argument is negative, then all characters up to the end of the entry buffer
  are deleted.

  If the @arg{pos} or @arg{nchars} arguments are out of bounds, then they are
  coerced to sane values. Note that the position is specified in characters,
  not bytes.
  @see-class{gtk:entry-buffer}
  @see-function{gtk:entry-buffer-insert-text}"
  (buffer (g:object entry-buffer))
  (pos :uint)
  (nchars :int))

(export 'entry-buffer-delete-text)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_buffer_emit_deleted_text
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_buffer_emit_deleted_text"
               entry-buffer-emit-deleted-text) :void
 #+liber-documentation
 "@version{2025-07-17}
  @argument[buffer]{a @class{gtk:entry-buffer} object}
  @argument[pos]{an unsigned integer for the position at which text was deleted}
  @argument[nchars]{an integer for the number of characters deleted}
  @begin{short}
    Emits the @sig[gtk:entry-buffer]{deleted-text} signal on the entry buffer.
  @end{short}
  Used when subclassing the @class{gtk:entry-buffer} class.
  @see-class{gtk:entry-buffer}
  @see-function{gtk:entry-buffer-emit-inserted-text}"
  (buffer (g:object entry-buffer))
  (pos :uint)
  (nchars :int))

(export 'entry-buffer-emit-deleted-text)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_buffer_emit_inserted_text
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_buffer_emit_inserted_text"
               entry-buffer-emit-inserted-text) :void
 #+liber-documentation
 "@version{2025-07-17}
  @argument[buffer]{a @class{gtk:entry-buffer} object}
  @argument[pos]{an unsigned integer for the position at which text was
    inserted}
  @argument[text]{a string for the text that was inserted}
  @argument[nchars]{an integer for the number of characters inserted}
  @begin{short}
    Emits the @sig[gtk:entry-buffer]{inserted-text} signal on the entry buffer.
  @end{short}
  Used when subclassing the @class{gtk:entry-buffer} class.
  @see-class{gtk:entry-buffer}
  @see-function{gtk:entry-buffer-emit-deleted-text}"
  (buffer (g:object entry-buffer))
  (pos :uint)
  (text :string)
  (nchars :int))

(export 'entry-buffer-emit-inserted-text)

;;; --- End of file gtk4.entry-buffer.lisp -------------------------------------
