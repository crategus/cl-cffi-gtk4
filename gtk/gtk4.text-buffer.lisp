;;; ----------------------------------------------------------------------------
;;; gtk4.text-buffer.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
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
;;; GtkTextBuffer
;;;
;;;     Stores attributed text for display in a GtkTextView
;;;
;;; Types and Values
;;;
;;;     GtkTextBuffer
;;;
;;;     GtkTextBufferNotifyFlags                            Since 4.16
;;;
;;; Accessors
;;;
;;;     gtk_text_buffer_get_can_redo
;;;     gtk_text_buffer_get_can_undo
;;;     gtk_text_buffer_get_enable_undo
;;;     gtk_text_buffer_set_enable_undo
;;;     gtk_text_buffer_get_has_selection
;;;     gtk_text_buffer_get_tag_table
;;;     gtk_text_buffer_set_text
;;;     gtk_text_buffer_get_text
;;;
;;; Functions
;;;
;;;     gtk_text_buffer_new
;;;     gtk_text_buffer_get_line_count
;;;     gtk_text_buffer_get_char_count
;;;     gtk_text_buffer_insert
;;;     gtk_text_buffer_insert_at_cursor
;;;     gtk_text_buffer_insert_interactive
;;;     gtk_text_buffer_insert_interactive_at_cursor
;;;     gtk_text_buffer_insert_range
;;;     gtk_text_buffer_insert_range_interactive
;;;     gtk_text_buffer_insert_with_tags
;;;     gtk_text_buffer_insert_with_tags_by_name
;;;     gtk_text_buffer_insert_markup
;;;     gtk_text_buffer_insert_paintable
;;;     gtk_text_buffer_delete
;;;     gtk_text_buffer_delete_interactive
;;;     gtk_text_buffer_backspace
;;;     gtk_text_buffer_get_slice
;;;     gtk_text_buffer_insert_child_anchor
;;;     gtk_text_buffer_create_child_anchor
;;;     gtk_text_buffer_create_mark
;;;     gtk_text_buffer_move_mark
;;;     gtk_text_buffer_move_mark_by_name
;;;     gtk_text_buffer_add_mark
;;;     gtk_text_buffer_delete_mark
;;;     gtk_text_buffer_delete_mark_by_name
;;;     gtk_text_buffer_get_mark
;;;     gtk_text_buffer_get_insert
;;;     gtk_text_buffer_get_selection_bound
;;;     gtk_text_buffer_place_cursor
;;;     gtk_text_buffer_select_range
;;;     gtk_text_buffer_apply_tag
;;;     gtk_text_buffer_remove_tag
;;;     gtk_text_buffer_apply_tag_by_name
;;;     gtk_text_buffer_remove_tag_by_name
;;;     gtk_text_buffer_remove_all_tags
;;;     gtk_text_buffer_create_tag
;;;     gtk_text_buffer_get_iter_at_line_offset
;;;     gtk_text_buffer_get_iter_at_offset
;;;     gtk_text_buffer_get_iter_at_line
;;;     gtk_text_buffer_get_iter_at_line_index
;;;     gtk_text_buffer_get_iter_at_mark
;;;     gtk_text_buffer_get_iter_at_child_anchor
;;;     gtk_text_buffer_get_start_iter
;;;     gtk_text_buffer_get_end_iter
;;;     gtk_text_buffer_get_bounds
;;;     gtk_text_buffer_get_modified
;;;     gtk_text_buffer_set_modified
;;;     gtk_text_buffer_delete_selection
;;;     gtk_text_buffer_paste_clipboard
;;;     gtk_text_buffer_copy_clipboard
;;;     gtk_text_buffer_cut_clipboard
;;;     gtk_text_buffer_get_selection_bounds
;;;     gtk_text_buffer_get_selection_content
;;;     gtk_text_buffer_begin_user_action
;;;     gtk_text_buffer_end_user_action
;;;     gtk_text_buffer_add_selection_clipboard
;;;     gtk_text_buffer_remove_selection_clipboard
;;;     gtk_text_buffer_get_max_undo_levels
;;;     gtk_text_buffer_set_max_undo_levels
;;;     gtk_text_buffer_undo
;;;     gtk_text_buffer_redo
;;;     gtk_text_buffer_begin_irreversible_action
;;;     gtk_text_buffer_end_irreversible_action
;;;
;;;     GtkTextBufferCommitNotify                           Since 4.16
;;;     gtk_text_buffer_add_commit_notify                   Since 4.16
;;;     gtk_text_buffer_remove_commit_notify                Since 4.16
;;;
;;; Properties
;;;
;;;     can-redo
;;;     can-undo
;;;     cursor-position
;;;     enable-undo
;;;     has-selection
;;;     tag-table
;;;     text
;;;
;;; Signals
;;;
;;;     apply-tag
;;;     begin-user-action
;;;     changed
;;;     delete-range
;;;     end-user-action
;;;     insert-child-anchor
;;;     insert-paintable
;;;     insert-text
;;;     mark-deleted
;;;     mark-set
;;;     modified-changed
;;;     paste-done
;;;     redo
;;;     remove-tag
;;;     undo
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkTextBuffer
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTextBufferNotifyFlags
;;; ----------------------------------------------------------------------------

#+gtk-4-16
(gobject:define-gflags "GtkTextBufferNotifyFlags" text-buffer-notify-flags
  (:export t
   :type-initializer "gtk_text_buffer_notify_flags_get_type")
  (:before-insert #.(ash 1 0))
  (:after-insert #.(ash 1 1))
  (:before-delete #.(ash 1 2))
  (:after-delete #.(ash 1 3)))

#+(and gtk-4-16 liber-documentation)
(setf (liber:alias-for-symbol 'text-buffer-notify-flags)
      "GFlags"
      (liber:symbol-documentation 'text-buffer-notify-flags)
 "@version{2025-07-27}
  @begin{declaration}
(gobject:define-gflags \"GtkTextBufferNotifyFlags\" text-buffer-notify-flags
  (:export t
   :type-initializer \"gtk_text_buffer_notify_flags_get_type\")
  (:before-insert #.(ash 1 0))
  (:after-insert #.(ash 1 1))
  (:before-delete #.(ash 1 2))
  (:after-delete #.(ash 1 3)))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:before-insert]{Be notified before text is inserted into the
        underlying text buffer.}
      @entry[:after-insert]{Be notified after text has been inserted into the
        underlying text buffer.}
      @entry[:before-delete]{Be notified before text is deleted from the
        underlying text buffer.}
      @entry[:after-delete]{Be notified after text has been deleted from the
        underlying text buffer.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Values for the @sym{gtk:text-buffer-commit-notify} callback function to
    denote the point of the notification.
  @end{short}
  Since 4.16
  @see-class{gtk:text-buffer}
  @see-symbol{gtk:text-buffer-commit-notify}")

;;; ----------------------------------------------------------------------------
;;; GtkTextBuffer
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkTextBuffer" text-buffer
  (:superclass g:object
    :export t
    :interfaces nil
    :type-initializer "gtk_text_buffer_get_type")
  ((can-redo
    text-buffer-can-redo
    "can-redo" "gboolean" t nil)
   (can-undo
    text-buffer-can-undo
    "can-undo" "gboolean" t nil)
   (cursor-position
    text-buffer-cursor-position
    "cursor-position" "gint" t nil)
   (enable-undo
    text-buffer-enable-undo
    "enable-undo" "gboolean" t t)
   (has-selection
    text-buffer-has-selection
    "has-selection" "gboolean" t nil)
   (tag-table
    text-buffer-tag-table
    "tag-table" "GtkTextTagTable" t nil)
   (text
    text-buffer-text
    "text" "gchararray" t t)))

#+liber-documentation
(setf (documentation 'text-buffer 'type)
 "@version{2025-07-21}
  @begin{short}
    Stores text and attributes for display in a @class{gtk:text-view} widget.
  @end{short}
  You may wish to begin by reading the text widget conceptual overview which
  gives an overview of all the objects and data types related to the text
  widget and how they work together.

  The @class{gtk:text-buffer} object can support undoing changes to the text
  buffer content, see the @fun{gtk:text-buffer-enable-undo} function.
  @begin[Signal Details]{dictionary}
    @begin[text-buffer::apply-tag]{signal}
      @begin{pre}
lambda (buffer tag start end)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object that received the
          signal.}
        @entry[tag]{The @class{gtk:text-tag} applied tag.}
        @entry[start]{The @class{gtk:text-iter} start iterator for the range
          the tag is applied to.}
        @entry[end]{The @class{gtk:text-iter} end iterator for the range
          the tag is applied to.}
      @end{simple-table}
      The signal is emitted to apply a tag to a range of text in a text buffer.
      Applying actually occurs in the default handler. Note that if your handler
      runs before the default handler it must not invalidate the @arg{start} and
      @arg{end} iterators, or has to revalidate them.
    @end{signal}
    @begin[text-buffer::begin-user-action]{signal}
      @begin{pre}
lambda (buffer)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object that received the
          signal.}
      @end{simple-table}
      The signal is emitted at the beginning of a single user visible operation
      on a text buffer.
    @end{signal}
    @begin[text-buffer::changed]{signal}
      @begin{pre}
lambda (buffer)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object that received the
          signal.}
      @end{simple-table}
      The signal is emitted when the content of a text buffer has changed.
    @end{signal}
    @begin[text-buffer::delete-range]{signal}
      @begin{pre}
lambda (buffer start end)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object that received the
          signal.}
        @entry[start]{The @class{gtk:text-iter} start iterator for the range
          to be deleted.}
        @entry[end]{The @class{gtk:text-iter} end iterator for the range
          to be deleted.}
      @end{simple-table}
      The signal is emitted to delete a range from a text buffer. Note that if
      your handler runs before the default handler it must not invalidate the
      @arg{start} and @arg{end} iterators, or has to revalidate them. The
      default signal handler revalidates the @arg{start} and @arg{end} iterators
      to both point to the location where text was deleted. Handlers which run
      after the default handler do not have access to the deleted text.
    @end{signal}
    @begin[text-buffer::end-user-action]{signal}
      @begin{pre}
lambda (buffer)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object that received the
          signal.}
      @end{simple-table}
      The signal is emitted at the end of a single user visible operation on
      the text buffer.
    @end{signal}
    @begin[text-buffer::insert-child-anchor]{signal}
      @begin{pre}
lambda (buffer location anchor)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object that received the
          signal.}
        @entry[location]{The @class{gtk:text-iter} position to insert
          @arg{anchor} in @arg{buffer}.}
        @entry[anchor]{The @class{gtk:text-child-anchor} object to be inserted.}
      @end{simple-table}
      The signal is emitted to insert a @class{gtk:text-child-anchor} object
      in a text buffer. Insertion actually occurs in the default handler. Note
      that if your handler runs before the default handler it must not
      invalidate the @arg{location} iterator, or has to revalidate it. The
      default signal handler revalidates it to be placed after the inserted
      @arg{anchor}.
    @end{signal}
    @begin[text-buffer::insert-paintable]{signal}
      @begin{pre}
lambda (buffer location paintable)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object that received the
          signal.}
        @entry[location]{The @class{gtk:text-iter} position to insert
          @arg{paintable} in @arg{buffer}.}
        @entry[paintable]{The @class{gdk:paintable} object to be inserted.}
      @end{simple-table}
      The signal is emitted to insert a @class{gdk:paintable} object in a text
      buffer. Insertion actually occurs in the default handler. Note that if
      your handler runs before the default handler it must not invalidate the
      @arg{location} iterator, or has to revalidate it. The default signal
      handler revalidates it to be placed after the inserted @arg{paintable}.
    @end{signal}
    @begin[text-buffer::insert-text]{signal}
      @begin{pre}
lambda (buffer location text len)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object that received the
          signal.}
        @entry[location]{The @class{gtk:text-iter} position to insert @arg{text}
          in @arg{buffer}.}
        @entry[text]{The string for the UTF-8 text to be inserted.}
        @entry[len]{The integer for the length of the inserted text in bytes.}
      @end{simple-table}
      The signal is emitted to insert text in a text buffer. Insertion actually
      occurs in the default handler. Note that if your handler runs before the
      default handler it must not invalidate the @arg{location} iterator, or has
      to revalidate it. The default signal handler revalidates it to point to
      the end of the inserted text.
    @end{signal}
    @begin[text-buffer::mark-deleted]{signal}
      @begin{pre}
lambda (buffer mark)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object that received the
          signal.}
        @entry[mark]{The @class{gtk:text-mark} object that was deleted.}
      @end{simple-table}
      The signal is emitted as notification after a @class{gtk:text-mark}
      object is deleted.
    @end{signal}
    @begin[text-buffer::mark-set]{signal}
      @begin{pre}
lambda (buffer location mark)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object that received the
          signal.}
        @entry[location]{The @class{gtk:text-iter} location of @arg{mark} in
          @arg{buffer}.}
        @entry[mark]{The @class{gtk:text-mark} object that is set.}
      @end{simple-table}
      The signal is emitted as notification after a @class{gtk:text-mark}
      object is set.
    @end{signal}
    @begin[text-buffer::modified-changed]{signal}
      @begin{pre}
lambda (buffer)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object that received the
          signal.}
      @end{simple-table}
      The signal is emitted when the modified bit of a text buffer flips.
    @end{signal}
    @begin[text-buffer::paste-done]{signal}
      @begin{pre}
lambda (buffer clipboard)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object that received the
          signal.}
        @entry[clipboard]{The @class{gdk:clipboard} object.}
      @end{simple-table}
      The signal is emitted after a paste operation has been completed. This is
      useful to properly scroll the view to the end of the pasted text.
    @end{signal}
    @begin[text-buffer::redo]{signal}
      @begin{pre}
lambda (buffer)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object that received the
          signal.}
      @end{simple-table}
      The signal is emitted when a request has been made to redo the previously
      undone operation.
    @end{signal}
    @begin[text-buffer::remove-tag]{signal}
      @begin{pre}
lambda (buffer tag start end)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object that received the
          signal.}
        @entry[tag]{The @class{gtk:text-tag} object to be removed.}
        @entry[start]{The @class{gtk:text-iter} start iterator for the range
          the tag is removed from.}
        @entry[end]{The @class{gtk:text-iter} end iterator for the range
          the tag is removed from.}
      @end{simple-table}
      The signal is emitted to remove all occurrences of @arg{tag} from a range
      of text in a text buffer. Removal actually occurs in the default handler.
      Note that if your handler runs before the default handler it must not
      invalidate the @arg{start} and @arg{end} iterators, or has to revalidate
      them.
    @end{signal}
    @begin[text-buffer::undo]{signal}
      @begin{pre}
lambda (buffer)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[buffer]{The @class{gtk:text-buffer} object that received the
          signal.}
      @end{simple-table}
      The signal is emitted when a request has been made to undo the previous
      operation or set of operations that have been grouped together.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:text-buffer-new}
  @see-slot{gtk:text-buffer-can-redo}
  @see-slot{gtk:text-buffer-can-undow}
  @see-slot{gtk:text-buffer-cursor-position}
  @see-slot{gtk:text-buffer-enable-undo}
  @see-slot{gtk:text-buffer-has-selection}
  @see-slot{gtk:text-buffer-tag-table}
  @see-slot{gtk:text-buffer-text}
  @see-class{gtk:text-view}
  @see-class{gtk:text-iter}
  @see-class{gtk:text-mark}
  @see-class{gtk:text-tag}
  @see-class{gtk:text-tag-table}
  @see-class{gtk:text-child-anchor}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:text-buffer-can-redo -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "can-redo" 'text-buffer) t)
 "The @code{can-redo} property of type @code{:boolean} (Read) @br{}
  The property denotes that the text buffer can reapply the last undone action.
  @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-buffer-can-redo)
      "Accessor"
      (documentation 'text-buffer-can-redo 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-buffer-can-redo object) => setting}
  @argument[object]{a @class{gtk:text-buffer} object}
  @argument[setting]{@em{true} if there is an redoable action}
  @begin{short}
    The accessor for the @slot[gtk:text-buffer]{can-redo} slot of the
    @class{gtk:text-buffer} class returns whether there is a redoable action in
    the history.
  @end{short}
  @see-class{gtk:text-buffer}")

;;; --- gtk:text-buffer-can-undo -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "can-undo" 'text-buffer) t)
 "The @code{can-undo} property of type @code{:boolean} (Read) @br{}
  The property denotes that the text buffer can undo the last applied action.
  @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-buffer-can-undo)
      "Accessor"
      (documentation 'text-buffer-can-undo 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-buffer-can-undo object) => setting}
  @argument[object]{a @class{gtk:text-buffer} object}
  @argument[setting]{@em{true} if there is an undoable action}
  @begin{short}
    The accessor for the @slot[gtk:text-buffer]{can-undo} slot of the
    @class{gtk:text-buffer} class returns whether there is a undoable action in
    the history.
  @end{short}
  @see-class{gtk:text-buffer}")

;;; --- gtk:text-buffer-cursor-position ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "cursor-position"
                                               'text-buffer) t)
 "The @code{cursor-position} property of type @code{:int} (Read) @br{}
  The position of the insert mark, as offset from the beginning of the text
  buffer. It is useful for getting notified when the cursor moves. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'text-buffer-cursor-position)
      "Accessor"
      (documentation 'text-buffer-cursor-position 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-buffer-cursor-position object) => position}
  @argument[object]{a @class{gtk:text-buffer} object}
  @argument[position]{an integer for the position of the insert mark}
  @begin{short}
    The accessor for the @slot[gtk:text-buffer]{cursor-position} slot of the
    @class{gtk:text-buffer} class returns the position of the insert mark, as
    offset from the beginning of the text buffer.
  @end{short}
  It is useful for getting notified when the cursor moves.
  @see-class{gtk:text-buffer}")

;;; --- gtk:text-buffer-enable-undo --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "enable-undo" 'text-buffer) t)
 "The @code{enable-undow} property of type @code{:boolean} (Read / Write) @br{}
  The property denotes if support for undoing and redoing changes to the text
  buffer is allowed. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'text-buffer-enable-undo)
      "Accessor"
      (documentation 'text-buffer-enable-undo 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-buffer-cursor-position object) => setting}
  @syntax{(setf (gtk:text-buffer-enable-undo object) setting)}
  @argument[object]{a @class{gtk:text-buffer} object}
  @argument[setting]{@em{true} to enable undo}
  @begin{short}
    The accessor for the @slot[gtk:text-buffer]{enable-undo} slot of the
    @class{gtk:text-buffer} class gets or sets whether the text buffer is saving
    modifications to the text buffer to allow for undo and redo actions.
  @end{short}
  If enabled, the user will be able to undo the last number of actions up to the
  @fun{gtk:text-buffer-max-undo-levels} value.

  See the @fun{gtk:text-buffer-begin-irreversible-action} and
  @fun{gtk:text-buffer-end-irreversible-action} functions to create changes to
  the text buffer that cannot be undone.
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-max-undo-levels}
  @see-function{gtk:text-buffer-begin-irreversible-action}
  @see-function{gtk:text-buffer-end-irreversible-action}")

;;; --- gtk:text-buffer-has-selection ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-selection" 'text-buffer) t)
 "The @code{has-selection} property of type @code{:boolean} (Read) @br{}
  Whether the text buffer has some text currently selected. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-buffer-has-selection)
      "Accessor"
      (documentation 'text-buffer-has-selection 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-buffer-has-selection object) => setting}
  @argument[object]{a @class{gtk:text-buffer} object}
  @argument[setting]{@em{true} if there is text selected}
  @begin{short}
    The accessor for the @slot[gtk:text-buffer]{has-selection} slot of the
    @class{gtk:text-buffer} class returns whether the text buffer has some text
    currently selected.
  @end{short}
  @see-class{gtk:text-buffer}")

;;; --- gtk:text-buffer-tag-table ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tag-table" 'text-buffer) t)
 "The @code{tag-table} property of type @class{gtk:text-tag-table}
  (Read / Write / Construct) @br{}
  The tag table associated with the text buffer.")

#+liber-documentation
(setf (liber:alias-for-function 'text-buffer-tag-table)
      "Accessor"
      (documentation 'text-buffer-tag-table 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-buffer-tag-table object) => table}
  @argument[object]{a @class{gtk:text-buffer} object}
  @argument[table]{a @class{gtk:text-tag-table} object}
  @begin{short}
    The accessor for the @slot[gtk:text-buffer]{tag-table} slot of the
    @class{gtk:text-buffer} class returns the tag table associated with the text
    buffer.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-tag-table}")

;;; --- gtk:text-buffer-text ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "text" 'text-buffer) t)
 "The @code{text} property of type @code{:string} (Read / Write) @br{}
  The text content of the text buffer, without child widgets and images. @br{}
  Default value: \"\"")

#+liber-documentation
(setf (liber:alias-for-function 'text-buffer-text)
      "Accessor"
      (documentation 'text-buffer-text 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-buffer-text object) => text}
  @syntax{(setf (gtk:text-buffer-text object) text)}
  @argument[object]{a @class{gtk:text-buffer} object}
  @argument[text]{a string for the UTF-8 text}
  @begin{short}
    The accessor for the @slot[gtk:text-buffer]{text} slot of the
    @class{gtk:text-buffer} class gets or sets the text of the text buffer,
    without child widgets and images.
  @end{short}
  The text must be valid UTF-8.
  @begin[Notes]{dictionary}
    Use the @fun{gtk:text-buffer-get-text} function to retrieve a range of text
    from the text buffer and the @fun{gtk:text-buffer-get-slice} function to
    include widgets and images.
  @end{dictionary}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-get-text}
  @see-function{gtk:text-buffer-get-slice}")

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_new
;;; ----------------------------------------------------------------------------

(declaim (inline text-buffer-new))

(defun text-buffer-new (&optional table)
 #+liber-documentation
 "@version{2024-07-03}
  @argument[table]{an optional @class{gtk:text-tag-table} object, or no
    argument to create a new one}
  @return{The new @class{gtk:text-buffer} object.}
  @short{Creates a new text buffer.}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-tag-table}"
  (make-instance 'text-buffer
                 :tag-table table))

(export 'text-buffer-new)

;;; ----------------------------------------------------------------------------
;;; gtk:text-buffer-load-file
;;; ----------------------------------------------------------------------------

(defun text-buffer-load-file (buffer path)
 #+liber-documentation
 "@version{2024-11-09}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[path]{a pathname or namestring for the file to load}
  @begin{short}
    Loads a file into the given text buffer.
  @end{short}
  @begin[Notes]{dictionary}
    This is a simple Lisp extension that does not exist in the C library.
  @end{dictionary}
  @see-class{gtk:text-buffer}"
  (let ((filename (namestring path)))
    (with-open-file (stream filename)
      (setf (text-buffer-text buffer) "")
      (do ((line (read-line stream nil)
                 (read-line stream nil)))
          ((null line))
        (text-buffer-insert buffer :cursor line)
        (text-buffer-insert buffer :cursor (format nil "~%"))))))

(export 'text-buffer-load-file)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_line_count
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_line_count" text-buffer-line-count) :int
 #+liber-documentation
 "@version{2025-07-27}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @return{The integer for the number of lines in the text buffer.}
  @begin{short}
    Obtains the number of lines in the text buffer.
  @end{short}
  This value is cached, so the function is very fast.
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-char-count}"
  (buffer (g:object text-buffer)))

(export 'text-buffer-line-count)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_char_count
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_char_count" text-buffer-char-count) :int
 #+liber-documentation
 "@version{2025-07-27}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @return{The integer for the number of characters in the text buffer.}
  @begin{short}
    Gets the number of characters in the text buffer.
  @end{short}
  Note that characters and bytes are not the same, you cannot, for example,
  expect the contents of the text buffer in string form to be this many bytes
  long. The character count is cached, so this function is very fast.
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-line-count}"
  (buffer (g:object text-buffer)))

(export 'text-buffer-char-count)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert
;;; gtk_text_buffer_insert_at_cursor                        not exported
;;; gtk_text_buffer_insert_interactive                      not exported
;;; gtk_text_buffer_insert_interactive_at_cursor            not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_insert" %text-buffer-insert) :void
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (text :string)
  (len :int))

(cffi:defcfun ("gtk_text_buffer_insert_at_cursor" %text-buffer-insert-at-cursor)
    :void
  (buffer (g:object text-buffer))
  (text :string)
  (len :int))

(cffi:defcfun ("gtk_text_buffer_insert_interactive"
               %text-buffer-insert-interactive) :boolean
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (text :string)
  (len :int)
  (editable :boolean))

(cffi:defcfun ("gtk_text_buffer_insert_interactive_at_cursor"
               %text-buffer-insert-interactive-at-cursor) :boolean
  (buffer (g:object text-buffer))
  (text :string)
  (len :int)
  (editable :boolean))

(defun text-buffer-insert (buffer pos text &key interactive editable)
 #+liber-documentation
 "@version{2025-07-27}
  @syntax{(gtk:text-buffer-insert buffer pos text) => t}
  @syntax{(gtk:text-buffer-insert buffer pos text :interactive t) => t}
  @syntax{(gtk:text-buffer-insert buffer pos text :interactive t :editable t) => t}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[pos]{a @class{gtk:text-iter} iterator or the @code{:cursor} value}
  @argument[text]{a string for the text in UTF-8 format}
  @argument[interactive]{a boolean whether the insertion is caused by user
    interaction, the default value is @em{false}}
  @argument[editable]{a boolean whether @arg{buffer} is editable by default,
    the default value is @em{false}}
  @return{@em{True} if the text was actually inserted.}
  @begin{short}
    Inserts text in the text buffer.
  @end{short}
  If the @arg{pos} argument has the @code{:cursor} value, inserts the text
  using the current cursor position as the insertion point.

  If the @arg{interactive} keyword argument is @em{true}, the insertion will
  not occur if the iterator is at a non-editable location in the text buffer.
  Usually you want to prevent insertions at ineditable locations if the
  insertion results from a user action (is interactive).

  The @arg{editable} keyword argument indicates the editability of text that
  does not have a tag affecting editability applied to it. Typically the result
  of the @fun{gtk:text-view-editable} function is appropriate here.

  Emits the @sig[gtk:text-buffer]{insert-text} signal. Insertion actually occurs
  in the default handler for the signal. The iterator is invalidated when
  insertion occurs, because the text buffer contents change, but the default
  signal handler revalidates it to point to the end of the inserted text.
  @begin[Notes]{dictionary}
    The @fun{gtk:text-buffer-insert} function combines the
    @code{gtk_text_buffer_insert()}, @code{gtk_text_buffer_insert_at_cursor()},
    @code{gtk_text_buffer_insert_interactive()}, and
    @code{gtk_text_buffer_insert_interactive_at_cursor()} functions into one
    function using the @arg{interactive}, and @arg{editable} keyword arguments.
    The corresponding Lisp functions except for the @fun{gtk:text-buffer-insert}
    function are not exported in the Lisp implementation.
  @end{dictionary}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-view-editable}"
  (assert (typep pos '(or text-iter (member :cursor))))
  (if interactive
      (if (eq pos :cursor)
          (%text-buffer-insert-interactive-at-cursor buffer
                                                     text
                                                     -1
                                                     editable)
          (%text-buffer-insert-interactive buffer
                                           pos
                                           text
                                           -1
                                           editable))
      (progn
        (if (eq pos :cursor)
            (%text-buffer-insert-at-cursor buffer text -1)
            (%text-buffer-insert buffer pos text -1))
        t)))

(export 'text-buffer-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_range
;;; gtk_text_buffer_insert_range_interactive                not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_insert_range" %text-buffer-insert-range) :void
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (start (g:boxed text-iter))
  (end (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_buffer_insert_range_interactive"
               %text-buffer-insert-range-interactive) :boolean
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (start (g:boxed text-iter))
  (end (g:boxed text-iter))
  (editable :boolean))

(defun text-buffer-insert-range (buffer iter start end &key interactive
                                                            editable)
 #+liber-documentation
 "@version{2025-07-23}
  @syntax{(gtk:text-buffer-insert-range buffer iter start end) => t}
  @syntax{(gtk:text-buffer-insert-range buffer iter start end :interactive t)
    => t}
  @syntax{(gtk:text-buffer-insert-range buffer iter start end :interactive t
    :editable t) => t}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[iter]{a @class{gtk:text-iter} position in text buffer}
  @argument[start]{a @class{gtk:text-iter} start position}
  @argument[end]{a @class{gtk:text-iter} end position}
  @argument[interactive]{a boolean whether the deletion is caused by user
    interaction}
  @argument[editable]{a boolean whether @arg{buffer} is editable by default}
  @return{@em{True} if the insertion was possible.}
  @begin{short}
    Copies text, tags, and paintables between the @arg{start} and @arg{end}
    iterators, the order of @arg{start} and @arg{end} does not matter, and
    inserts the copy at the @arg{iter} iterator.
  @end{short}

  Used instead of simply getting/inserting text because it preserves images and
  tags. If @arg{start} and @arg{end} are in a different text buffer from
  @arg{buffer}, the two buffers must share the same tag table.

  The @arg{interactive} keyword argument with the @em{true} value is the same,
  but does nothing if the insertion point is not editable. The @arg{editable}
  keyword argument indicates whether the text is editable at the
  iterator if no tags enclosing the iterator affect editability. Typically the
  result of the @fun{gtk:text-view-editable} function is appropriate here.

  Implemented via emissions of the @sig[gtk:text-buffer]{insert-text} and
  @sig[gtk:text-buffer]{apply-tag} signals, so expect those.
  @begin[Notes]{dictionary}
    The Lisp implementation combines the two
    @code{gtk_text_buffer_insert_range()} and
    @code{gtk_text_buffer_insert_range_interactive()} functions. The second
    function is not exported in the Lisp implementation,
  @end{dictionary}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-view-editable}
  @see-function{gtk:text-buffer-insert}"
  (if interactive
      (%text-buffer-insert-range-interactive buffer
                                             iter
                                             start
                                             end
                                             editable)
      (progn
        (%text-buffer-insert-range buffer iter start end)
        t)))

(export 'text-buffer-insert-range)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_with_tags
;;; gtk_text_buffer_insert_with_tags_by_name                not implemented
;;; ----------------------------------------------------------------------------

(defun text-buffer-insert-with-tags (buffer iter text &rest tags)
 #+liber-documentation
 "@version{2025-07-27}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[iter]{a @class{gtk:text-iter} iterator in the text buffer}
  @argument[text]{a string for the UTF-8 text}
  @argument[tags]{a list for @class{gtk:text-tag} objects or strings for the
    tag names to apply to @arg{text}}
  @begin{short}
    Inserts text into the text buffer at the position @arg{iter}, applying the
    list of tags to the newly inserted text.
  @end{short}
  Equivalent to calling the @fun{gtk:text-buffer-insert} function, then the
  @fun{gtk:text-buffer-apply-tag} function on the inserted text. The
  @fun{gtk:text-buffer-insert-with-tags} function is just a convenience
  function.
  @begin[Notes]{dictionary}
    The Lisp implementation does not call the
    @code{gtk_text_buffer_insert_with_tags()} function, but uses the
    @fun{gtk:text-buffer-insert} and @fun{gtk:text-buffer-apply-tag} functions.
    The @code{gtk_text_buffer_insert_with_tags_by_name()} function is included
    in this function and not implemented in the Lisp library.
  @end{dictionary}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-class{gtk:text-tag}
  @see-function{gtk:text-buffer-insert}
  @see-function{gtk:text-buffer-apply-tag}"
  (let ((offset (text-iter-offset iter)))
    (prog1
      (text-buffer-insert buffer iter text)
      (let ((start (text-buffer-iter-at-offset buffer offset)))
        (dolist (tag tags)
          (text-buffer-apply-tag buffer tag start iter))))))

(export 'text-buffer-insert-with-tags)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_markup
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_insert_markup" %text-buffer-insert-markup) :void
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (markup :string)
  (len :int))

(defun text-buffer-insert-markup (buffer iter markup)
 #+liber-documentation
 "@version{2025-07-27}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[iter]{a @class{gtk:text-iter} iterator for a position in the text
    buffer}
  @argument[markup]{a UTF-8 string containing Pango markup}
  @begin{short}
    Inserts the text in @arg{markup} at the position of the iterator.
  @end{short}
  The text in @arg{markup} will be inserted in its entirety and must be valid
  UTF-8. Emits the @sig[gtk:text-buffer]{insert-text} signal, possibly multiple
  times. Insertion actually occurs in the default handler for the signal. The
  iterator will point to the end of the inserted text on return.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-buffer-insert}"
  (%text-buffer-insert-markup buffer iter markup -1))

(export 'text-buffer-insert-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_paintable
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_insert_paintable" text-buffer-insert-paintable)
    :void
 #+liber-documentation
 "@version{2025-07-27}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[iter]{a @class{gtk:text-iter} iterator for the location to insert
    the paintable}
  @argument[paintable]{a @class{gdk:paintable} object}
  @begin{short}
    Inserts an image into the text buffer at @arg{iter}.
  @end{short}
  The image will be counted as one character in character counts, and when
  obtaining the text buffer contents as a string, will be represented by the
  Unicode “object replacement character” @code{0xFFFC}. Note that the \"slice\"
  variants for obtaining portions of the text buffer as a string include this
  character for the paintable, but the ¸\"text\" variants do not. See the
  @fun{gtk:text-buffer-get-slice} and @fun{gtk:text-buffer-get-text} functions.
  @see-class{gtk:text-buffer}
  @see-class{gdk:paintable}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-buffer-get-slice}
  @see-function{gtk:text-buffer-get-text}"
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (paintable (g:object gdk:paintable)))

(export 'text-buffer-insert-paintable)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_delete
;;; gtk_text_buffer_delete_interactive                      not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_delete_interactive"
               %text-buffer-delete-interactive) :boolean
  (buffer (g:object text-buffer))
  (start (g:boxed text-iter))
  (end (g:boxed text-iter))
  (editable :boolean))

(cffi:defcfun ("gtk_text_buffer_delete" %text-buffer-delete) :void
  (buffer (g:object text-buffer))
  (start (g:boxed text-iter))
  (end (g:boxed text-iter)))

(defun text-buffer-delete (buffer start end &key interactive editable)
 #+liber-documentation
 "@version{2025-07-23}
  @syntax{(gtk:text-buffer-delete buffer start end) => t}
  @syntax{(gtk:text-buffer-delete buffer start end :interactive t) => t}
  @syntax{(gtk:text-buffer-delete buffer start end :interactive t
    :editable t) => t}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[start]{a @class{gtk:text-iter} start position in the text buffer}
  @argument[end]{a @class{gtk:text-iter} end position in the text buffer}
  @argument[interactive]{a boolean whether the deletion is caused by user
    interaction, the default value is @code{nil}}
  @argument[editable]{a boolean whether the text buffer is editable by default,
    the default value is @code{nil}}
  @return{@em{True} if some text was actually deleted.}
  @begin{short}
    Deletes text between the @arg{start} and @arg{end} iterators.
  @end{short}
  The order of the @arg{start} and @arg{end} iterators is not actually relevant.
  The @fun{gtk:text-buffer-delete} function will reorder them. This function
  actually emits the @sig[gtk:text-buffer]{delete-range} signal, and the default
  handler of that signal deletes the text. Because the text buffer is modified,
  all outstanding iterators become invalid after calling this function. However,
  the @arg{start} and @arg{end} interators will be re-initialized to point to
  the location where text was deleted.

  If the @arg{interactive} keyword argument is @em{true} deletes all editable
  text for each editable sub range of [@arg{start}, @arg{end}). The @arg{start}
  and @arg{end} iterators are revalidated to point to the location of the last
  deleted range, or left untouched if no text was deleted. If the @arg{editable}
  keyword argument is @em{true} deletes in addition the not editable text.
  This case is equivalent to calling the @fun{gtk:text-buffer-delete} function
  with the @code{nil} value for the @arg{interactive} keyword argument.
  @begin[Notes]{dictionary}
    The @code{gtk_text_buffer_delete_interactive()} function is included in
    this function and not implemented in the Lisp libraray.
  @end{dictionary}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}"
  (if interactive
      (%text-buffer-delete-interactive buffer start end editable)
      (progn
        (%text-buffer-delete buffer start end)
        t)))

(export 'text-buffer-delete)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_backspace
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_backspace" %text-buffer-backspace) :boolean
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (interactive :boolean)
  (editable :boolean))

(defun text-buffer-backspace (buffer iter &key interactive editable)
 #+liber-documentation
 "@version{2024-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[iter]{a @class{gtk:text-iter} position in @arg{buffer}}
  @argument[interactive]{a boolean whether the deletion is caused by user
    interaction}
  @argument[editable]{a boolean whether @arg{buffer} is editable by default}
  @return{@em{True} if the text buffer was modified.}
  @begin{short}
    Performs the appropriate action as if the user hit the delete key with the
    cursor at the position specified by @arg{iter}.
  @end{short}
  In the normal case a single character will be deleted, but when combining
  accents are involved, more than one character can be deleted, and when
  precomposed character and accent combinations are involved, less than one
  character will be deleted.

  Because the text buffer is modified, all outstanding iterators become invalid
  after calling this function. However, the iterator will be re-initialized to
  point to the location where text was deleted.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}"
  (%text-buffer-backspace buffer iter interactive editable))

(export 'text-buffer-backspace)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_text
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_text" %text-buffer-get-text) :string
  (buffer (g:object text-buffer))
  (start (g:boxed text-iter))
  (end (g:boxed text-iter))
  (include :boolean))

(defun text-buffer-get-text (buffer start end &optional include)
 #+liber-documentation
 "@version{2024-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[start]{a @class{gtk:text-iter} start iterator of a range}
  @argument[end]{a @class{gtk:text-iter} end iterator of a range}
  @argument[include]{a boolean whether to include invisible text}
  @return{The allocated UTF-8 string.}
  @begin{short}
    Returns the text in the range [@arg{start}, @arg{end}).
  @end{short}
  Excludes undisplayed text, text marked with tags that set the invisibility
  attribute, if the @arg{include} argument is @em{false}. Does not include
  characters representing embedded images, so byte and character indexes into
  the returned string do not correspond to byte and character indexes into the
  text buffer. Contrast with the @fun{gtk:text-buffer-get-slice} function.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-buffer-text}
  @see-function{gtk:text-buffer-get-slice}"
  (%text-buffer-get-text buffer start end include))

(export 'text-buffer-get-text)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_slice
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_slice" %text-buffer-get-slice)
    (:string :free-from-foreign t)
  (buffer (g:object text-buffer))
  (start (g:boxed text-iter))
  (end (g:boxed text-iter))
  (include :boolean))

(defun text-buffer-get-slice (buffer start end &optional include)
 #+liber-documentation
 "@version{2024-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[start]{a @class{gtk:text-iter} start of a range}
  @argument[end]{a @class{gtk:text-iter} end of a range}
  @argument[include]{a boolean whether to include invisible text}
  @return{The allocated UTF-8 string.}
  @begin{short}
    Returns the text in the range [@arg{start}, @arg{end}).
  @end{short}
  Excludes undisplayed text, text marked with tags that set the invisibility
  attribute, if the @arg{include} argument is @em{false}.

  The returned string includes a @code{0xFFFC} character whenever the text
  buffer contains embedded images, so byte and character indexes into the
  returned string do correspond to byte and character indexes into the text
  buffer. Contrast with the @fun{gtk:text-buffer-get-text} function. Note that
  @code{0xFFFC} can occur in normal text as well, so it is not a reliable
  indicator that a paintable or widget is in the text buffer.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-buffer-get-text}"
  (%text-buffer-get-slice buffer start end include))

(export 'text-buffer-get-slice)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_insert_child_anchor
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_insert_child_anchor"
               %text-buffer-insert-child-anchor) :void
  (buffer (g:object text-buffer))
  (pos (g:boxed text-iter))
  (anchor (g:object text-child-anchor)))

(defun text-buffer-insert-child-anchor (buffer pos &optional anchor)
 #+liber-documentation
 "@version{2025-07-27}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[pos]{a @class{gtk:text-iter} iterator for the position to insert
    the anchor}
  @argument[anchor]{an optional @class{gtk:text-child-anchor} object}
  @return{The @class{gtk:text-child-anchor} child widget anchor.}
  @begin{short}
    Inserts a child widget anchor into the text buffer at @arg{pos}.
  @end{short}
  The anchor will be counted as one character in character counts, and when
  obtaining the buffer contents as a string, will be represented by the Unicode
  \"object replacement character\" @code{0xFFFC}. Note that the \"slice\"
  variants for obtaining portions of the text buffer as a string include this
  character for anchors, but the \"text\" variants do not, for example, see the
  @fun{gtk:text-buffer-get-slice} and @fun{gtk:text-buffer-get-text} functions.
  Consider the @fun{gtk:text-buffer-create-child-anchor} function as a more
  convenient alternative to this function. The text buffer will add a reference
  to the anchor, so you can unref it after insertion.

  If the @arg{anchor} argument is @code{nil} creates an anchor with the
  @fun{gtk:text-child-anchor-new} function and inserts it into @arg{buffer}
  with the @fun{gtk:text-buffer-insert-child-anchor} function. The new anchor
  is owned by the text buffer.
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-get-slice}
  @see-function{gtk:text-buffer-get-text}
  @see-function{gtk:text-buffer-child-anchor-new}
  @see-function{gtk:text-buffer-insert-child-anchor}
  @see-function{gtk:text-buffer-create-child-anchor}"
  (if anchor
      (progn
        (%text-buffer-insert-child-anchor buffer pos anchor)
        anchor)
      (text-buffer-create-child-anchor buffer pos)))

(export 'text-buffer-insert-child-anchor)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_create_child_anchor
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_create_child_anchor"
               text-buffer-create-child-anchor) (g:object text-child-anchor)
 #+liber-documentation
 "@version{2024-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[iter]{a @class{gtk:text-iter} location in the text buffer}
  @return{The created @class{gtk:text-child-anchor} anchor.}
  @begin{short}
    This is a convenience function which simply creates an anchor with the
    @fun{gtk:text-child-anchor-new} function and inserts it into the text buffer
    with the @fun{gtk:text-buffer-insert-child-anchor} function.
  @end{short}
  The new anchor is owned by the text buffer.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-class{gtk:text-child-anchor}
  @see-function{gtk:text-child-anchor-new}
  @see-function{gtk:text-buffer-insert-child-anchor}"
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter)))

(export 'text-buffer-create-child-anchor)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_create_mark
;;; ----------------------------------------------------------------------------

;; TODO: Check the memory management of the created mark

(cffi:defcfun ("gtk_text_buffer_create_mark" %text-buffer-create-mark)
    (g:object text-mark)
  (buffer (g:object text-buffer))
  (name :string)
  (where (g:boxed text-iter))
  (gravity :boolean))

(defun text-buffer-create-mark (buffer name pos &optional (gravity t))
 #+liber-documentation
 "@version{2025-07-27}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[name]{a string for the name for the mark, or @code{nil}}
  @argument[pos]{a @class{gtk:text-iter} iterator for the location to place
    the mark}
  @argument[gravity]{a boolean whether the mark has left gravity, the default
    is @em{true}}
  @return{The new @class{gtk:text-mark} object.}
  @begin{short}
    Creates a mark at position @arg{pos}.
  @end{short}
  If the @arg{mark} argument is @code{nil}, the mark is anonymous. Otherwise,
  the mark can be retrieved by name using the @fun{gtk:text-buffer-mark}
  function. If a mark has left gravity, and text is inserted at the current
  location of the mark, the mark will be moved to the left of the newly inserted
  text. If the mark has right gravity, the mark will end up on the right of
  newly inserted text. The standard left-to-right cursor is a mark with right
  gravity, when you type, the cursor stays on the right side of the text you are
  typing.

  The caller of this function does not own a reference to the returned
  @class{gtk:text-mark} object, so you can ignore the return value if you like.
  Marks are owned by the text buffer and go away when the text buffer does.

  Emits the @sig[gtk:text-buffer]{mark-set} signal as notification of the
  initial placement of the mark.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-class{gtk:text-mark}
  @see-function{gtk:text-buffer-mark}"
  (%text-buffer-create-mark buffer name pos gravity))

(export 'text-buffer-create-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_move_mark
;;; gtk_text_buffer_move_mark_by_name                       not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_move_mark_by_name"
               %text-buffer-move-mark-by-name) :void
  (buffer (g:object text-buffer))
  (name :string)
  (pos (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_buffer_move_mark" %text-buffer-move-mark) :void
  (buffer (g:object text-buffer))
  (mark (g:object text-mark))
  (pos (g:boxed text-iter)))

(defun text-buffer-move-mark (buffer mark pos)
 #+liber-documentation
 "@version{2025-07-27}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[mark]{a @class{gtk:text-mark} object, or a string for the name
    of the mark}
  @argument[pos]{a @class{gtk:text-iter} iterator for the new position for
    @arg{mark} in the text buffer}
  @begin{short}
    Moves the mark to the new location @arg{pos}.
  @end{short}
  Emits the @sig[gtk:text-buffer]{mark-set} signal as notification of the move.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-class{gtk:text-mark}"
  (if (stringp mark)
      (%text-buffer-move-mark-by-name buffer mark pos)
      (%text-buffer-move-mark buffer mark pos)))

(export 'text-buffer-move-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_add_mark
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_add_mark" text-buffer-add-mark) :void
 #+liber-documentation
 "@version{2025-07-23}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[mark]{a @class{gtk:text-mark} object for the mark to add}
  @argument[pos]{a @class{gtk:text-iter} iterator for the location to place
    the mark}
  @begin{short}
    Adds the mark at the given position.
  @end{short}
  The mark must not be added to another text buffer, and if its name is not
  @code{nil} then there must not be another mark in the text buffer with the
  same name.

  Emits the @sig[gtk:text-buffer]{mark-set} signal as notification of the
  initial placement of the mark.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-mark}
  @see-class{gtk:text-iter}"
  (buffer (g:object text-buffer))
  (mark (g:object text-mark))
  (pos (g:boxed text-iter)))

(export 'text-buffer-add-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_delete_mark
;;; gtk_text_buffer_delete_mark_by_name                     not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_delete_mark_by_name"
               %text-buffer-delete-mark-by-name) :void
  (buffer (g:object text-buffer))
  (name :string))

(cffi:defcfun ("gtk_text_buffer_delete_mark" %text-buffer-delete-mark) :void
  (buffer (g:object text-buffer))
  (mark (g:object text-mark)))

(defun text-buffer-delete-mark (buffer mark)
 #+liber-documentation
 "@version{2025-07-23}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[mark]{a @class{gtk:text-mark} object, or a string for the name
    of a mark in the text buffer}
  @begin{short}
    Deletes the mark, so that it is no longer located anywhere in the text
    buffer.
  @end{short}
  Removes the reference the text buffer holds to the mark. Most operations on
  the mark become invalid, until it gets added to a text buffer again with the
  @fun{gtk:text-buffer-add-mark} function. Use the @fun{gtk:text-mark-deleted}
  function to find out if a mark has been removed from its text buffer. The
  @sig[gtk:text-buffer]{mark-deleted} signal will be emitted as notification
  after the mark is deleted.
  @begin[Notes]{dictionary}
    The @code{gtk_text_buffer_delete_mark_by_name()} function is included in
    this function and not exported in the Lisp library.
  @end{dictionary}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-mark}
  @see-function{gtk:text-buffer-add-mark}
  @see-function{gtk:text-mark-deleted}"
  (if (stringp mark)
      (%text-buffer-delete-mark-by-name buffer mark)
      (%text-buffer-delete-mark buffer mark)))

(export 'text-buffer-delete-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_mark
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_mark" text-buffer-mark) (g:object text-mark)
 #+liber-documentation
 "@version{2025-07-27}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[name]{a string for a mark name}
  @return{The @class{gtk:text-mark} object, or @code{nil}.}
  @begin{short}
    Returns the mark named @arg{name} in the text buffer, or @code{nil} if
    no such mark exists in the text buffer.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-mark}"
  (buffer (g:object text-buffer))
  (name :string))

(export 'text-buffer-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_insert
;;; ----------------------------------------------------------------------------

;; It is wrong to implement this as text-buffer-insert, we have already a
;; function with this name.

(cffi:defcfun ("gtk_text_buffer_get_insert" text-buffer-get-insert)
    (g:object text-mark)
 #+liber-documentation
 "@version{2025-07-27}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @return{The @class{gtk:text-mark} object for the insertion point mark.}
  @begin{short}
    Returns the mark that represents the cursor (insertion point).
  @end{short}
  Equivalent to calling the @fun{gtk:text-buffer-mark} function to get the mark
  named @code{\"insert\"}, but more efficient.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-mark}
  @see-function{gtk:text-buffer-mark}"
  (buffer (g:object text-buffer)))

(export 'text-buffer-get-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_selection_bound
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_selection_bound"
               text-buffer-selection-bound) (g:object text-mark)
 #+liber-documentation
 "@version{2025-07-27}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @return{The @class{gtk:text-mark} object for the selection bound mark.}
  @begin{short}
    Returns the mark that represents the selection bound.
  @end{short}
  Equivalent to calling the @fun{gtk:text-buffer-mark} function to get the mark
  named @code{\"selection_bound\"}, but very slightly more efficient, and
  involves less typing.

  The currently selected text in the text buffer is the region between the
  @code{\"selection_bound\"} and @code{\"insert\"} marks. If the
  @code{\"selection_bound\"} and @code{\"insert\"} marks are in the same place,
  then there is no current selection. The @fun{gtk:text-buffer-selection-bounds}
  function is another convenient function for handling the selection, if you
  just want to know whether there is a selection and what its bounds are.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-mark}
  @see-function{gtk:text-buffer-mark}
  @see-function{gtk:text-buffer-selection-bounds}"
  (buffer (g:object text-buffer)))

(export 'text-buffer-selection-bound)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_place_cursor
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_place_cursor" text-buffer-place-cursor) :void
 #+liber-documentation
 "@version{2024-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[pos]{a @class{gtk:text-iter} iterator where to put the cursor}
  @begin{short}
    This function moves the @code{\"insert\"} and @code{\"selection_bound\"}
    marks simultaneously.
  @end{short}
  If you move them to the same place in two steps with the
  @fun{gtk:text-buffer-move-mark} function, you will temporarily select a region
  in between their old and new locations, which can be pretty inefficient since
  the temporarily selected region will force stuff to be recalculated. This
  function moves them as a unit, which can be optimized.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-buffer-move-mark}"
  (buffer (g:object text-buffer))
  (pos (g:boxed text-iter)))

(export 'text-buffer-place-cursor)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_select_range
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_select_range" text-buffer-select-range) :void
 #+liber-documentation
 "@version{2024-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[insertion]{a @class{gtk:text-iter} iterator where to put the
    @code{\"insert\"} mark}
  @argument[selection]{a @class{gtk:text-iter} iterator where to put the
    @code{\"selection_bound\"} mark}
  @begin{short}
    This function moves the @code{\"insert\"} and @code{\"selection_bound\"}
    marks simultaneously.
  @end{short}
  If you move them in two steps with the @fun{gtk:text-buffer-move-mark}
  function, you will temporarily select a region in between their old and new
  locations, which can be pretty inefficient since the temporarily selected
  region will force stuff to be recalculated. This function moves them as a
  unit, which can be optimized.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-buffer-move-mark}"
  (buffer (g:object text-buffer))
  (insertion (g:boxed text-iter))
  (selection (g:boxed text-iter)))

(export 'text-buffer-select-range)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_apply_tag
;;; gtk_text_buffer_apply_tag_by_name                       no exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_apply_tag_by_name"
               %text-buffer-apply-tag-by-name) :void
  (buffer (g:object text-buffer))
  (name :string)
  (start (g:boxed text-iter))
  (end (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_buffer_apply_tag" %text-buffer-apply-tag) :void
  (buffer (g:object text-buffer))
  (tag (g:object text-tag))
  (start (g:boxed text-iter))
  (end (g:boxed text-iter)))

(defun text-buffer-apply-tag (buffer tag start end)
 #+liber-documentation
 "@version{2024-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[tag]{a @class{gtk:text-tag} object, or a string for the tag name}
  @argument[start]{a @class{gtk:text-iter} iterator for the start bound of
    the range to be tagged}
  @argument[end]{a @class{gtk:text-iter} iterator for the end bound of the
    range to be tagged}
  @begin{short}
    Emits the @sig[gtk:text-buffer]{apply-tag} signal on the text buffer.
  @end{short}
  The default handler for the signal applies @arg{tag} to the given range.
  The @arg{start} and @arg{end} iterators do not have to be in order.
  @begin[Notes]{dictionary}
    The Lisp implementation combines the two @code{gtk_text_buffer_apply_tag()}
    and @code{gtk_text_buffer_apply_tag_by_name()} functions. The second
    funtion is not exported.
  @end{dictionary}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-tag}
  @see-class{gtk:text-iter}"
  (if (stringp tag)
      (%text-buffer-apply-tag-by-name buffer tag start end)
      (%text-buffer-apply-tag buffer tag start end)))

(export 'text-buffer-apply-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_remove_tag
;;; gtk_text_buffer_remove_tag_by_name                      not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_remove_tag_by_name"
               %text-buffer-remove-tag-by-name) :void
  (buffer (g:object text-buffer))
  (name :string)
  (start (g:boxed text-iter))
  (end (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_buffer_remove_tag" %text-buffer-remove-tag) :void
  (buffer (g:object text-buffer))
  (tag (g:object text-tag))
  (start (g:boxed text-iter))
  (end (g:boxed text-iter)))

(defun text-buffer-remove-tag (buffer tag start end)
 #+liber-documentation
 "@version{2025-07-23}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[tag]{a @class{gtk:text-tag} object, or a string for the tag name}
  @argument[start]{a @class{gtk:text-iter} iterator for the start bound of the
    range to be untagged}
  @argument[end]{a @class{gtk:text-iter} iterator for the end bound of the
    range to be untagged}
  @begin{short}
    Emits the @sig[gtk:text-buffer]{remove-tag} signal.
  @end{short}
  The default handler for the signal removes all occurrences of @arg{tag} from
  the given range. The @arg{start} and @arg{end} iterators do not have to be in
  order.
  @begin[Notes]{dictionary}
    The Lisp implementation combines the two @code{gtk_text_buffer_remove_tag()}
    and @code{gtk_text_buffer_remove_tag_by_name()} functions. The second
    funtion is not exported.
  @end{dictionary}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-tag}
  @see-class{gtk:text-iter}"
  (if (stringp tag)
      (%text-buffer-remove-tag-by-name buffer tag start end)
      (%text-buffer-remove-tag buffer tag start end)))

(export 'text-buffer-remove-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_remove_all_tags
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_remove_all_tags" text-buffer-remove-all-tags)
    :void
 #+liber-documentation
 "@version{2025-07-27}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[start]{a @class{gtk:text-iter} iterator for the start bound of the
    range to be untagged}
  @argument[end]{a @class{gtk:text-iter} iterator for the end bound of the
    range to be untagged}
  @begin{short}
    Removes all tags in the range between the @arg{start} and @arg{end}
    iterators.
  @end{short}
  Be careful with this function, it could remove tags added in code unrelated
  to the code you are currently writing. That is, using this function is
  probably a bad idea if you have two or more unrelated code sections that add
  tags.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}"
  (buffer (g:object text-buffer))
  (start (g:boxed text-iter))
  (end (g:boxed text-iter)))

(export 'text-buffer-remove-all-tags)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_create_tag
;;; ----------------------------------------------------------------------------

(defun text-buffer-create-tag (buffer name &rest args)
 #+liber-documentation
 "@version{2025-07-27}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[name]{a string for the name of the new tag, or @code{nil}}
  @argument[args]{a list for property keywords and values}
  @return{The new @class{gtk:text-tag} object.}
  @begin{short}
    Creates a tag and adds it to the tag table for the text buffer.
  @end{short}
  Equivalent to calling the @fun{gtk:text-tag-new} function and then adding the
  tag to the tag table of the text buffer.

  If the @arg{name} argument is @code{nil}, the tag is anonymous. If the
  @arg{name} argument is non-@code{nil}, a tag called @arg{name} must not
  already exist in the tag table for this text buffer.

  The @arg{args} argument is a list of properties and values to set on the
  tag.
  @begin[Examples]{dictionary}
    Create and add a tag with name \"font-italic\" to the text buffer.
    @begin{pre}
(defvar buffer (gtk:text-buffer-new)) => BUFFER
(gtk:text-buffer-create-tag buffer \"font-italic\"
                                   :font \"fixed\" :style :italic)
=> #<GTK:TEXT-TAG {1002193283@}>
    @end{pre}
  @end{dictionary}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-tag}
  @see-function{gtk:text-tag-new}"
  (let* ((name (or name (cffi:null-pointer)))
         (tag (apply #'make-instance 'text-tag :name name args)))
    (when (text-tag-table-add (text-buffer-tag-table buffer) tag)
      tag)))

(export 'text-buffer-create-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_iter_at_line_offset
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_iter_at_line_offset"
               %text-buffer-iter-at-line-offset) :void
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (line :int)
  (offset :int))

(defun text-buffer-iter-at-line-offset (buffer line offset)
 #+liber-documentation
 "@version{2025-07-27}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[line]{an integer for the line number counting from 0}
  @argument[offset]{an integer for the char offset from the start of the line}
  @return{The @class{gtk:text-iter} iterator.}
  @begin{short}
    Obtains an iterator pointing to @arg{offset} within the given @arg{line}.
  @end{short}
  Note characters, not bytes, UTF-8 may encode one character as multiple bytes.

  If the @arg{line} argument is greater than the number of lines in the text
  buffer, the end iterator is returned. And if the @arg{offset} argument is
  off the end of the line, the iterator at the end of the line is returned.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-buffer-iter-at-offset}"
  (let ((iter (make-instance 'text-iter)))
    (%text-buffer-iter-at-line-offset buffer iter line offset)
    iter))

(export 'text-buffer-iter-at-line-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_iter_at_offset
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_iter_at_offset"
               %text-buffer-iter-at-offset) :void
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (offset :int))

(defun text-buffer-iter-at-offset (buffer offset)
 #+liber-documentation
 "@version{2025-07-27}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[offset]{an integer for the char offset from the start of the text
    buffer, counting from 0, or -1}
  @return{The @class{gtk:text-iter} iterator.}
  @begin{short}
    Initializes the returned iterator to a position @arg{offset} chars from the
    start of the entire text buffer.
  @end{short}
  If the @arg{offset} argument is -1 or greater than the number of characters
  in the text buffer, the iterator is initialized to the end iterator, the
  iterator one past the last valid character in the text buffer.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}"
  (let ((iter (make-instance 'text-iter)))
    (%text-buffer-iter-at-offset buffer iter offset)
    iter))

(export 'text-buffer-iter-at-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_iter_at_line
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_iter_at_line"
               %text-buffer-iter-at-line) :void
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (line :int))

(defun text-buffer-iter-at-line (buffer line)
 #+liber-documentation
 "@version{2025-07-27}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[line]{an integer for the line number counting from 0}
  @return{The @class{gtk:text-iter} iterator.}
  @begin{short}
    Initializes the returned iterator to the start of the given line.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}"
  (let ((iter (make-instance 'text-iter)))
    (%text-buffer-iter-at-line buffer iter line)
    iter))

(export 'text-buffer-iter-at-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_iter_at_line_index
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_iter_at_line_index"
               %text-buffer-iter-at-line-index) :void
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (line :int)
  (index :int))

(defun text-buffer-iter-at-line-index (buffer line index)
 #+liber-documentation
 "@version{2025-07-27}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[line]{an integer for the line number counting from 0}
  @argument[index]{an integer for the byte index from the start of the line}
  @return{The @class{gtk:text-iter} iterator.}
  @begin{short}
    Obtains an iterator pointing to @arg{index} within the given line.
  @end{short}
  The @arg{index} argument must be the start of a UTF-8 character. Note bytes,
  not characters, UTF-8 may encode one character as multiple bytes. If the
  @arg{line} argument is greater than the number of lines in the text buffer,
  the end iterator is returned. And if the @arg{index} argument is off the end
  of the line, the iterator at the end of the line is returned.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}"
  (let ((iter (make-instance 'text-iter)))
    (%text-buffer-iter-at-line-index buffer iter line index)
    iter))

(export 'text-buffer-iter-at-line-index)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_iter_at_mark
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_iter_at_mark" %text-buffer-iter-at-mark)
    :void
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (mark (g:object text-mark)))

(defun text-buffer-iter-at-mark (buffer mark)
 #+liber-documentation
 "@version{2025-07-27}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[mark]{a @class{gtk:text-mark} object, or a string for the mark
    name in the text buffer}
  @return{The @class{gtk:text-iter} iterator.}
  @begin{short}
    Returns the iterator with the current position of @arg{mark}.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-mark}"
  (let ((iter (make-instance 'text-iter))
        (mark (if (stringp mark) (text-buffer-mark buffer mark) mark)))
    (%text-buffer-iter-at-mark buffer iter mark)
    iter))

(export 'text-buffer-iter-at-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_iter_at_child_anchor
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_iter_at_child_anchor"
               %text-buffer-iter-at-child-anchor) :void
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter))
  (anchor (g:object text-child-anchor)))

(defun text-buffer-iter-at-child-anchor (buffer anchor)
 #+liber-documentation
 "@version{2024-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[anchor]{a @class{gtk:text-child-anchor} anchor that appears in text
    buffer}
  @return{The @class{gtk:text-iter} iterator.}
  @begin{short}
    Obtains the location of @arg{anchor} within the text buffer.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-child-anchor}
  @see-class{gtk:text-iter}"
  (let ((iter (make-instance 'text-iter)))
    (%text-buffer-iter-at-child-anchor buffer iter anchor)
    iter))

(export 'text-buffer-iter-at-child-anchor)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_start_iter
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_start_iter" %text-buffer-start-iter) :void
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter)))

(defun text-buffer-start-iter (buffer)
 #+liber-documentation
 "@version{2024-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @return{The @class{gtk:text-iter} iterator.}
  @begin{short}
    Returns an iterator with the first position in the text buffer.
  @end{short}
  This is the same as using the @fun{gtk:text-buffer-iter-at-offset} function
  to get the itererator at character offset 0.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-buffer-iter-at-offset}"
  (let ((iter (make-instance 'text-iter)))
    (%text-buffer-start-iter buffer iter)
    iter))

(export 'text-buffer-start-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_end_iter
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_end_iter" %text-buffer-end-iter) :void
  (buffer (g:object text-buffer))
  (iter (g:boxed text-iter)))

(defun text-buffer-end-iter (buffer)
 #+liber-documentation
 "@version{2024-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @return{The @class{gtk:text-iter} iterator.}
  @begin{short}
    Returns an iterator with the \"end iterator\", one past the last valid
    character in the text buffer.
  @end{short}
  If dereferenced with the @fun{gtk:text-iter-char} function, the end iterator
  has a character value of 0. The entire text buffer lies in the range from the
  first position in the text buffer to the end iterator. Call the
  @fun{gtk:text-buffer-start-iter} function to get character position 0.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-buffer-start-iter}
  @see-function{gtk:text-iter-char}"
  (let ((iter (make-instance 'text-iter)))
    (%text-buffer-end-iter buffer iter)
    iter))

(export 'text-buffer-end-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_bounds
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_bounds" %text-buffer-bounds) :void
  (buffer (g:object text-buffer))
  (start (g:boxed text-iter))
  (end (g:boxed text-iter)))

(defun text-buffer-bounds (buffer)
 #+liber-documentation
 "@version{2025-07-27}
  @syntax{(gtk:text-buffer-bounds buffer) => start, end}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[start]{a @class{gtk:text-iter} iterator for the first position in
    the text buffer}
  @argument[end]{a @class{gtk:text-iter} iterator for the end position in the
    text buffer}
  @begin{short}
    Retrieves the first and last iterators in the text buffer.
  @end{short}
  The entire text buffer lies within the range [@arg{start}, @arg{end}).
  @begin[Examples]{dictionary}
    Get the start and end itererator for a text buffer and clear the text
    buffer.
    @begin{pre}
(defun clear-buffer (buffer)
  (multiple-value-bind (start end)
      (gtk:text-buffer-bounds buffer)
    (gtk:text-buffer-delete buffer start end)))
    @end{pre}
  @end{dictionary}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}"
  (let ((start (make-instance 'text-iter))
        (end (make-instance 'text-iter)))
    (%text-buffer-bounds buffer start end)
    (values start end)))

(export 'text-buffer-bounds)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_modified
;;; gtk_text_buffer_set_modified
;;; ----------------------------------------------------------------------------

(defun (setf text-buffer-modified) (setting buffer)
  (cffi:foreign-funcall "gtk_text_buffer_set_modified"
                        (g:object text-buffer) buffer
                        :boolean setting
                        :void)
  setting)

(cffi:defcfun ("gtk_text_buffer_get_modified" text-buffer-modified) :boolean
 "@version{2025-07-23}
  @syntax{(gtk:text-buffer-modified buffer) => setting}
  @syntax{(setf (gtk:text-buffer-modified buffer) setting)}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[setting]{a boolean for the modification flag setting}
  @begin{short}
    Returns @em{true} if the text buffer has been modified.
  @end{short}
  The @fun{gtk:text-buffer-modified} function indicates whether the text buffer
  has been modified since the last call to the @setf{gtk:text-buffer-modified}
  function.

  Used to keep track of whether the text buffer has been modified since the last
  time it was saved. Whenever the text buffer is saved to disk, call the
  @setf{gtk:text-buffer-modified} function with the @em{false} value. When the
  text buffer is modified, it will automatically toggle on the modified bit
  again. When the modified bit flips, the text buffer emits a
  @sig[gtk:text-buffer]{modified-changed} signal.
  @see-class{gtk:text-buffer}"
  (buffer (g:object text-buffer)))

(export 'text-buffer-modified)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_delete_selection
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_delete_selection" %text-buffer-delete-selection)
    :boolean
  (buffer (g:object text-buffer))
  (interactive :boolean)
  (editable :boolean))

(defun text-buffer-delete-selection (buffer &key interactive editable)
 #+liber-documentation
 "@version{#2024-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[interactive]{a boolean whether the deletion is caused by user
    interaction}
  @argument[editable]{a boolean whether the text buffer is editable by default}
  @return{The boolean whether there was a non-empty selection to delete.}
  @begin{short}
    Deletes the range between the @code{\"insert\"} and
    @code{\"selection_bound\"} marks, that is, the currently selected text.
  @end{short}
  If the @arg{interactive} argument is @em{true}, the editability of the
  selection will be considered, users cannot delete uneditable text.
  @see-class{gtk:text-buffer}"
  (%text-buffer-delete-selection buffer interactive editable))

(export 'text-buffer-delete-selection)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_paste_clipboard
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_paste_clipboard" %text-buffer-paste-clipboard)
    :void
  (buffer (g:object text-buffer))
  (clipboard (g:object gdk:clipboard))
  (override (g:boxed text-iter))
  (editable :boolean))

(defun text-buffer-paste-clipboard (buffer clipboard &key override editable)
 #+liber-documentation
 "@version{#2024-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[clipboard]{a @class{gdk:clipboard} object to paste from}
  @argument[override]{a @class{gtk:text-iter} location to insert pasted text,
    or @code{nil} to insert at the cursor}
  @argument[editable]{a boolean whether the text buffer is editable by default}
  @begin{short}
    Pastes the contents of a clipboard.
  @end{short}
  If the @arg{override} argument is @code{nil}, the pasted text will be inserted
  at the cursor position, or the buffer selection will be replaced if the
  selection is non-empty.
  @begin[Notes]{dictionary}
    Pasting is asynchronous, that is, we will ask for the paste data and return,
    and at some point later after the main loop runs, the paste data will be
    inserted.
  @end{dictionary}
  @see-class{gtk:text-buffer}
  @see-class{gdk:clipboard}
  @see-class{gtk:text-iter}"
  (%text-buffer-paste-clipboard buffer clipboard override editable))

(export 'text-buffer-paste-clipboard)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_copy_clipboard
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_copy_clipboard" text-buffer-copy-clipboard)
    :void
 #+liber-documentation
 "@version{#2024-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[clipboard]{a @class{gdk:clipboard} object to copy to}
  @begin{short}
    Copies the currently selected text to the clipboard.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-class{gdk:clipboard}"
  (buffer (g:object text-buffer))
  (clipboard (g:object gdk:clipboard)))

(export 'text-buffer-copy-clipboard)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_cut_clipboard
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_cut_clipboard" text-buffer-cut-clipboard) :void
 #+liber-documentation
 "@version{#2024-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[clipboard]{a @class{gdk:clipboard} object to cut to}
  @argument[editable]{a boolean whether the text buffer is editable by default}
  @begin{short}
    Copies the currently selected text to a clipboard, then deletes the text
    if it is editable.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-class{gdk:clipboard}"
  (buffer (g:object text-buffer))
  (clipboard (g:object gdk:clipboard))
  (editable :boolean))

(export 'text-buffer-cut-clipboard)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_selection_bounds
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_selection_bounds"
               %text-buffer-selection-bounds) :boolean
  (buffer (g:object text-buffer))
  (start (g:boxed text-iter))
  (end (g:boxed text-iter)))

(defun text-buffer-selection-bounds (buffer)
 #+liber-documentation
 "@version{2024-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @begin{return}
    @arg{start} -- a @class{gtk:text-iter} iterator for the selection start
      @br{}
    @arg{end} -- a @class{gtk:text-iter} iterator for the selection end @br{}
    @code{nil} -- if no text is selected
  @end{return}
  @begin{short}
    Returns the @arg{start} and @arg{end} iterators if some text is selected.
  @end{short}
  If the selection has length 0, then the @arg{start} and @arg{end} iterators
  are filled in with the same value. The @arg{start} and @arg{end} iterators
  will be in ascending order.
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-iter}"
  (let ((start (text-iter-new))
        (end (text-iter-new)))
    (when (%text-buffer-selection-bounds buffer start end)
      (values start end))))

(export 'text-buffer-selection-bounds)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_selection_content
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_get_selection_content"
               text-buffer-selection-content) (g:object gdk:content-provider)
 #+liber-documentation
 "@version{#2024-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @return{The @class{gdk:content-provider} object}
  @begin{short}
    Get a content provider for this text buffer.
  @end{short}
  It can be used to make the content of the text buffer available in a
  @class{gdk:clipboard} object, see the @fun{gdk:clipboard-content} function.
  @see-class{gtk:text-buffer}
  @see-class{gdk:content-provider}
  @see-class{gdk:clipboard}
  @see-function{gdk:clipboard-content}"
  (buffer (g:object text-buffer)))

(export 'text-buffer-selection-content)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_begin_user_action
;;; ----------------------------------------------------------------------------

;; Example implementation of a macro, not exported at this time.

(defmacro with-text-buffer-user-action ((buffer) &body body)
  (let ((g (gensym)))
    `(let ((,g ,buffer))
       (text-buffer-begin-user-action ,g)
       (unwind-protect
         (progn ,@body)
         (text-buffer-end-user-action ,g)))))

;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_begin_user_action"
               text-buffer-begin-user-action) :void
 #+liber-documentation
 "@version{#2025-07-23}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @begin{short}
    Called to indicate that the text buffer operations between here and a call
    to the @fun{gtk:text-buffer-end-user-action} function are part of a single
    user visible operation.
  @end{short}
  The operations between the @fun{gtk:text-buffer-begin-user-action} and
  @fun{gtk:text-buffer-end-user-action} functions can then be grouped when
  creating an undo stack. The text buffer maintains a count of calls to the
  @fun{gtk:text-buffer-begin-user-action} function that have not been closed
  with a call to the @fun{gtk:text-buffer-end-user-action} function, and emits
  the @sig[gtk:text-buffer]{begin-user-action} and
  @sig[gtk:text-buffer]{end-user-action} signals only for the outermost pair of
  calls. This allows you to build user actions from other user actions.

  The \"interactive\" text buffer mutation functions automatically call
  begin/end user action around the text buffer operations they perform, so there
  is no need to add extra calls if the user action consists solely of a single
  call to one of those functions.
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-end-user-action}"
  (buffer (g:object text-buffer)))

(export 'text-buffer-begin-user-action)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_end_user_action
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_end_user_action" text-buffer-end-user-action)
    :void
 #+liber-documentation
 "@version{#2024-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @begin{short}
    Should be paired with a call to the @fun{gtk:text-buffer-begin-user-action}
    function.
  @end{short}
  See that function for a full explanation.
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-begin-user-action}"
  (buffer (g:object text-buffer)))

(export 'text-buffer-end-user-action)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_add_selection_clipboard
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_add_selection_clipboard"
               text-buffer-add-selection-clipboard) :void
 #+liber-documentation
 "@version{#2024-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[clipboard]{a @class{gdk:clipboard} object}
  @begin{short}
    Adds a clipboard to the list of clipboards in which the selection contents
    of the text buffer are available.
  @end{short}
  In most cases, the clipboard will be of type \"PRIMARY\" for a view of the
  text buffer.
  @see-class{gtk:text-buffer}
  @see-class{gdk:clipboard}"
  (buffer (g:object text-buffer))
  (clipboard (g:object gdk:clipboard)))

(export 'text-buffer-add-selection-clipboard)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_remove_selection_clipboard
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_remove_selection_clipboard"
               text-buffer-remove-selection-clipboard) :void
 #+liber-documentation
 "@version{#2024-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[clipboard]{a @class{gdk:clipboard} object added to the text buffer}
  @begin{short}
    Removes a clipboard added with the
    @fun{gtk:text-buffer-add-selection-clipboard} function.
  @end{short}
  @see-class{gtk:text-buffer}
  @see-class{gdk:clipboard}
  @see-function{gtk:text-buffer-add-selection-clipboard}"
  (buffer (g:object text-buffer))
  (clipboard (g:object gdk:clipboard)))

(export 'text-buffer-remove-selection-clipboard)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_get_max_undo_levels
;;; gtk_text_buffer_set_max_undo_levels
;;; ----------------------------------------------------------------------------

(defun (setf text-buffer-max-undo-levels) (value buffer)
  (cffi:foreign-funcall "gtk_text_buffer_set_max_undo_levels"
                        (g:object text-buffer) buffer
                        :uint value
                        :void)
  value)

(cffi:defcfun ("gtk_text_buffer_get_max_undo_levels"
               text-buffer-max-undo-levels) :uint
 #+liber-documentation
 "@version{#2025-07-27}
  @syntax{(gtk:text-buffer-max-undo-levels buffer) => levels}
  @syntax{(setf (gtk:text-buffer-max-undo-levels buffer) levels)}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[levels]{an unsigned integer for the number of undo levels}
  @begin{short}
    The @fun{gtk:text-buffer-max-undo-levels} function gets the maximum number
    of undo levels to perform.
  @end{short}
  The @setf{gtk:text-buffer-max-undo-levels} function sets the maximum number.
  If 0, unlimited undo actions may be performed. Note that this may have a
  memory usage impact as it requires storing an additional copy of the inserted
  or removed text within the text buffer.
  @see-class{gtk:text-buffer}"
  (buffer (g:object text-buffer)))

(export 'text-buffer-max-undo-levels)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_undo
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_undo" text-buffer-undo) :void
 #+liber-documentation
 "@version{#2024-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @begin{short}
    Undoes the last undoable action on the text buffer, if there is one.
  @end{short}
  @see-class{gtk:text-buffer}"
  (buffer (g:object text-buffer)))

(export 'text-buffer-undo)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_redo
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_redo" text-buffer-redo) :void
 #+liber-documentation
 "@version{#2024-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @begin{short}
    Redoes the next redoable action on the text buffer, if there is one.
  @end{short}
  @see-class{gtk:text-buffer}"
  (buffer (g:object text-buffer)))

(export 'text-buffer-redo)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_begin_irreversible_action
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_begin_irreversible_action"
               text-buffer-begin-irreversible-action) :void
 #+liber-documentation
 "@version{2024-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @begin{short}
    Denotes the beginning of an action that may not be undone.
  @end{short}
  This will cause any previous operations in the undo/redo queue to be cleared.
  This should be paired with a call to the
  @fun{gtk:text-buffer-end-irreversible-action} function after the irreversible
  action has completed.

  You may nest calls to the @fun{gtk:text-buffer-begin-irreversible-action} and
  @fun{gtk:text-buffer-end-irreversible-action} function pairs.
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-end-irreversible-action}"
  (buffer (g:object text-buffer)))

(export 'text-buffer-begin-irreversible-action)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_end_irreversible_action
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_buffer_end_irreversible_action"
               text-buffer-end-irreversible-action) :void
 #+liber-documentation
 "@version{2024-07-03}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @begin{short}
    Denotes the end of an action that may not be undone.
  @end{short}
  This will cause any previous operations in the undo/redo queue to be cleared.
  This should be called after completing modifications to the text buffer
  after the @fun{gtk:text-buffer-begin-irreversible-action} function was called.

  You may nest calls to the @fun{gtk:text-buffer-begin-irreversible-action} and
  @fun{gtk:text-buffer-end-irreversible-action} function pairs.
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-begin-irreversible-action}"
  (buffer (g:object text-buffer)))

(export 'text-buffer-end-irreversible-action)

;;; ----------------------------------------------------------------------------
;;; GtkTextBufferCommitNotify                               Since 4.16
;;; ----------------------------------------------------------------------------

#+gtk-4-16
(cffi:defcallback text-buffer-commit-notify :void
    ((buffer (g:object text-buffer))
     (flags text-buffer-notify-flags)
     (pos :uint)
     (len :uint)
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (funcall func buffer flags pos len)))

#+(and gtk-4-16 liber-documentation)
(setf (liber:alias-for-symbol 'text-buffer-commit-notify)
      "Callback"
      (liber:symbol-documentation 'text-buffer-commit-notify)
 "@version{#2025-07-27}
  @syntax{lambda (buffer flags pos len)}
  @argument[buffer]{a @class{gtk:text-buffer} object being notified}
  @argument[flags]{a @sym{gtk:text-buffer-notify-flags} value for the type of
    commit notification}
  @argument[pos]{an unsigned integer for the position of the text operation}
  @argument[len]{an unsigned integer for the length of the text operation
    in characters}
  @begin{short}
    A notification callback used by the @fun{gtk:text-buffer-add-commit-notify}
    function.
  @end{short}
  You may not modify the @class{gtk:text-buffer} object from a
  @sym{gtk:text-buffer-commit-notify} callback function and that is enforced
  by the @class{gtk:text-buffer} API.

  The @sym{gtk:text-buffer-commit-notify} callback function may be used to be
  notified about changes to the underlying text buffer right before or after the
  changes are committed to the underlying B-Tree. This is useful if you want to
  observe changes to the text buffer without other signal handlers potentially
  modifying state on the way to the default signal handler.

  When the @arg{flags} argument is @code{:before-insert}, the @arg{pos}
  argument is set to the offset in characters from the start of the text buffer
  where the insertion will occur. The @arg{len} argument is set to the number
  of characters to be inserted. You may not yet retrieve the text until it has
  been inserted. You may access the text from @code{:after-insert} using the
  @fun{gtk:text-buffer-get-slice} function.

  When the @arg{flags} argument is @code{:after-insert}, the @arg{pos} argument
  is set to offset in characters where the insertion occurred and @arg{len} is
  set to the number of characters inserted.

  When the @arg{flags} argument is @code{:before-delete}, the @arg{pos} argument
  is set to offset in characters where the deletion will occur and @arg{len} is
  set to the number of characters that will be removed. You may still retrieve
  the text from this handler using @arg{pos} and @arg{len}.

  When the @arg{flags} argument is @code{:after-delete}, the @arg{len} argument
  is set to zero to denote that the delete-range has already been committed to
  the underlying B-Tree. You may no longer retrieve the text that has been
  deleted from the text buffer.

  Since 4.16
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-add-commit-notify}")

#+gtk-4-16
(export 'text-buffer-commit-notify)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_add_commit_notify                       Since 4.16
;;; ----------------------------------------------------------------------------

#+gtk-4-16
(cffi:defcfun ("gtk_text_buffer_add_commit_notify"
               %text-buffer-add-commit-notify) :uint
  (buffer (g:object text-buffer))
  (flags text-buffer-notify-flags)
  (notify :pointer)
  (data :pointer)
  (destroy :pointer))

#+gtk-4-16
(defun text-buffer-add-commit-notify (buffer flags notify)
 #+liber-documentation
 "@version{#2025-07-27}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[flags]{a @sym{gtk:text-buffer-notify-flags} value for which
    notifications should be dispatched to @arg{notify}}
  @argument[notify]{a @sym{gtk:text-buffer-commit-notify} callback function to
    call for commit notifications}
  @begin{return}
    The unsigned integer for a handler ID which may be used to remove the commit
    notify callback using the @fun{gtk:text-buffer-remove-commit-notify}
    function.
  @end{return}
  @begin{short}
    Adds a @sym{gtk:text-buffer-commit-notify} callback function to be called
    when a change is to be made to the text buffer.
  @end{short}
  Functions are explicitly forbidden from making changes to the text buffer from
  this callback. It is intended for tracking changes to the text buffer only.

  It may be advantageous to use the @sym{gtk:text-buffer-commit-notify} callback
  function over connecting to the @sig[gtk:text-buffer]{insert-text} signal or
  the @sig[gtk:text-buffer]{delete-range} signal to avoid ordering issues with
  other signal handlers which may further modify the text buffer.

  Since 4.16
  @see-class{gtk:text-buffer}
  @see-symbol{gtk:text-buffer-commit-notify}
  @see-function{gtk:text-buffer-remove-commit-notify}"
  (if notify
      (%text-buffer-add-commit-notify
              buffer
              flags
              (cffi:callback text-buffer-commit-notify)
              (glib:allocate-stable-pointer notify)
              (cffi:callback glib:stable-pointer-destroy-notify))
      (%text-buffer-add-commit-notify
              buffer
              flags
              (cffi:null-pointer)
              (cffi:null-pointer)
              (cffi:null-pointer))))

#+gtk-4-16
(export 'text-buffer-add-commit-notify)

;;; ----------------------------------------------------------------------------
;;; gtk_text_buffer_remove_commit_notify                    Since 4.16
;;; ----------------------------------------------------------------------------

#+gtk-4-16
(cffi:defcfun ("gtk_text_buffer_remove_commit_notify"
               text-buffer-remove-commit-notify) :void
 #+liber-documentation
 "@version{#2025-07-27}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @argument[notifyid]{an unsigned integer for the notify handler identifier}
  @begin{short}
    Removes the @sym{gtk:text-buffer-commit-notify} handler previously
    registered with the @fun{gtk:text-buffer-add-commit-notify} function.
  @end{short}

  Since 4.16
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-add-commit-notify}"
  (buffer (g:object text-buffer))
  (notifyid :uint))

#+gtk-4-16
(export 'text-buffer-remove-commit-notify)

;;; --- End of file gtk4.text-buffer.lisp --------------------------------------
