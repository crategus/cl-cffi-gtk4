;;; ----------------------------------------------------------------------------
;;; gtk4.text-iter.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
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
;;; GtkTextIter
;;;
;;;     Text buffer iterator
;;;
;;; Types and Values
;;;
;;;     GtkTextIter
;;;     GtkTextSearchFlags
;;;
;;; Functions
;;;
;;;     gtk_text_iter_get_buffer
;;;     gtk_text_iter_copy
;;;     gtk_text_iter_assign
;;;     gtk_text_iter_free
;;;     gtk_text_iter_get_offset
;;;     gtk_text_iter_get_line
;;;     gtk_text_iter_get_line_offset
;;;     gtk_text_iter_get_line_index
;;;     gtk_text_iter_get_visible_line_index
;;;     gtk_text_iter_get_visible_line_offset
;;;     gtk_text_iter_get_char
;;;     gtk_text_iter_get_slice
;;;     gtk_text_iter_get_text
;;;     gtk_text_iter_get_visible_slice
;;;     gtk_text_iter_get_visible_text
;;;     gtk_text_iter_get_paintable
;;;     gtk_text_iter_get_marks
;;;     gtk_text_iter_get_toggled_tags
;;;     gtk_text_iter_get_child_anchor

;;;     gtk_text_iter_starts_tag

;;;     gtk_text_iter_ends_tag
;;;     gtk_text_iter_toggles_tag
;;;     gtk_text_iter_has_tag
;;;     gtk_text_iter_get_tags
;;;     gtk_text_iter_editable
;;;     gtk_text_iter_can_insert
;;;     gtk_text_iter_starts_word
;;;     gtk_text_iter_ends_word
;;;     gtk_text_iter_inside_word
;;;     gtk_text_iter_starts_line
;;;     gtk_text_iter_ends_line
;;;     gtk_text_iter_starts_sentence
;;;     gtk_text_iter_ends_sentence
;;;     gtk_text_iter_inside_sentence
;;;     gtk_text_iter_is_cursor_position
;;;     gtk_text_iter_get_chars_in_line
;;;     gtk_text_iter_get_bytes_in_line
;;;     gtk_text_iter_get_language
;;;     gtk_text_iter_is_end
;;;     gtk_text_iter_is_start
;;;     gtk_text_iter_forward_char
;;;     gtk_text_iter_backward_char
;;;     gtk_text_iter_forward_chars
;;;     gtk_text_iter_backward_chars
;;;     gtk_text_iter_forward_line
;;;     gtk_text_iter_backward_line
;;;     gtk_text_iter_forward_lines
;;;     gtk_text_iter_backward_lines
;;;     gtk_text_iter_forward_word_ends
;;;     gtk_text_iter_backward_word_starts
;;;     gtk_text_iter_forward_word_end
;;;     gtk_text_iter_backward_word_start
;;;     gtk_text_iter_forward_cursor_position
;;;     gtk_text_iter_backward_cursor_position
;;;     gtk_text_iter_forward_cursor_positions
;;;     gtk_text_iter_backward_cursor_positions
;;;     gtk_text_iter_backward_sentence_start
;;;     gtk_text_iter_backward_sentence_starts
;;;     gtk_text_iter_forward_sentence_end
;;;     gtk_text_iter_forward_sentence_ends
;;;     gtk_text_iter_forward_visible_word_ends
;;;     gtk_text_iter_backward_visible_word_starts
;;;     gtk_text_iter_forward_visible_word_end
;;;     gtk_text_iter_backward_visible_word_start
;;;     gtk_text_iter_forward_visible_cursor_position
;;;     gtk_text_iter_backward_visible_cursor_position
;;;     gtk_text_iter_forward_visible_cursor_positions
;;;     gtk_text_iter_backward_visible_cursor_positions
;;;     gtk_text_iter_forward_visible_line
;;;     gtk_text_iter_backward_visible_line
;;;     gtk_text_iter_forward_visible_lines
;;;     gtk_text_iter_backward_visible_lines
;;;     gtk_text_iter_set_offset
;;;     gtk_text_iter_set_line
;;;     gtk_text_iter_set_line_offset
;;;     gtk_text_iter_set_line_index
;;;     gtk_text_iter_set_visible_line_index
;;;     gtk_text_iter_set_visible_line_offset
;;;     gtk_text_iter_forward_to_end
;;;     gtk_text_iter_forward_to_line_end
;;;     gtk_text_iter_forward_to_tag_toggle
;;;     gtk_text_iter_backward_to_tag_toggle
;;;
;;;     GtkTextCharPredicate
;;;
;;;     gtk_text_iter_forward_find_char
;;;     gtk_text_iter_backward_find_char
;;;     gtk_text_iter_forward_search
;;;     gtk_text_iter_backward_search
;;;     gtk_text_iter_equal
;;;     gtk_text_iter_compare
;;;     gtk_text_iter_in_range
;;;     gtk_text_iter_order
;;;
;;;  Hierarchy
;;;
;;;     GBoxed
;;;     ╰── GtkTextIter
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTextSearchFlags
;;; ----------------------------------------------------------------------------

(gobject:define-g-flags "GtkTextSearchFlags" text-search-flags
  (:export t
   :type-initializer "gtk_text_search_flags_get_type")
  (:visible-only 1)
  (:text-only 2)
  (:case-insensitive 4))

#+liber-documentation
(setf (liber:alias-for-symbol 'text-search-flags)
      "GFlags"
      (liber:symbol-documentation 'text-search-flags)
 "@version{2024-7-1}
  @begin{declaration}
(gobject:define-g-flags \"GtkTextSearchFlags\" text-search-flags
  (:export t
   :type-initializer \"gtk_text_search_flags_get_type\")
  (:visible-only 1)
  (:text-only 2)
  (:case-insensitive 4))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:visible-only]{Search only visible data. A search match may have
        invisible text interspersed.}
      @entry[:text-only]{Search only text. A match may have pixbufs or child
        widgets mixed inside the matched range.}
      @entry[:case-insensitive]{The text will be matched regardless of what case
        it is in.}
    @end{table}
  @end{values}
  @short{Flags affecting how a search is done.}
  If neither @code{:visible-only} nor @code{:text-only} are enabled, the match
  must be exact. The special @code{0xFFFC} character will match embedded pixbufs
  or child widgets.
  @see-class{gtk:text-iter}
  @see-function{gtk:text-iter-search}")

;;; ----------------------------------------------------------------------------
;;; GtkTextIter
;;; ----------------------------------------------------------------------------

(cffi:defcstruct %text-iter
  (dummy1 :pointer)
  (dummy2 :pointer)
  (dummy3 :int)
  (dummy4 :int)
  (dummy5 :int)
  (dummy6 :int)
  (dummy7 :int)
  (dummy8 :int)
  (dummy9 :pointer)
  (dummy10 :pointer)
  (dummy11 :int)
  (dummy12 :int)
  (dummy13 :int)
  (dummy14 :pointer))

(cffi:defcfun ("gtk_text_iter_copy" %text-iter-copy) :pointer
  (iter :pointer))

(defun %text-iter-alloc ()
  (cffi:with-foreign-object (iter '(:struct %text-iter))
    (%text-iter-copy iter)))

;;; ----------------------------------------------------------------------------

(glib:define-g-boxed-opaque text-iter "GtkTextIter"
  :type-initializer "gtk_text_iter_get_type"
  :alloc (%text-iter-alloc))

#+liber-documentation
(setf (liber:alias-for-class 'text-iter)
      "GBoxed"
      (documentation 'text-iter 'type)
 "@version{2024-7-1}
  @begin{declaration}
(glib:define-g-boxed-opaque text-iter \"GtkTextIter\"
  :export t
  :type-initializer \"gtk_text_iter_get_type\"
  :alloc (%text-iter-alloc))
  @end{declaration}
  @begin{short}
    Most text manipulation is accomplished with iterators, represented by a
    @class{gtk:text-iter} instance.
  @end{short}
  An iterator represents a position between two characters in the text buffer.

  Iterators are not valid indefinitely. Whenever the text buffer is modified in
  a way that affects the number of characters in the text buffer, all
  outstanding iterators become invalid. Note that deleting 5 characters and
  then reinserting 5 characters still invalidates iterators, though you end up
  with the same number of characters you pass through a state with a different
  number.
  @see-constructor{gtk:text-iter-new}
  @see-constructor{gtk:text-iter-copy}
  @see-class{gtk:text-buffer}")

(export 'text-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_buffer
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_get_buffer" text-iter-buffer)
    (g:object text-buffer)
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @return{The @class{gtk:text-buffer} object.}
  @short{Returns the text buffer this iterator is associated with.}
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}"
  (iter (g:boxed text-iter)))

(export 'text-iter-buffer)

;;; ----------------------------------------------------------------------------
;;; gtk:text-iter-new
;;; ----------------------------------------------------------------------------

(declaim (inline text-iter-new))

(defun text-iter-new ()
 "@version{2024-7-1}
  @return{The newly allocated @class{gtk:text-iter} instance.}
  @short{Returns a newly allocated iterator.}
  @see-class{gtk:text-iter}"
  (make-instance 'text-iter))

(export 'text-iter-new)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_copy
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_copy" text-iter-copy) (g:boxed text-iter :return)
 #+liber-documentation
 "@version{2024-6-24}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @return{The newly allocated @class{gtk:text-iter} instance with the copy of
    @arg{iter}.}
  @short{Creates a copy of an iterator.}
  @see-class{gtk:text-iter}"
  (iter (g:boxed text-iter)))

(export 'text-iter-copy)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_assign
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_assign" text-iter-assign) :void
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @argument[other]{another @class{gtk:text-iter} instance}
  @short{Assigns the value of @arg{other} to @arg{iter}.}
  @see-class{gtk:text-iter}"
  (iter (g:boxed text-iter))
  (other (g:boxed text-iter)))

(export 'text-iter-assign)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_free ()                                   not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_offset
;;; gtk_text_iter_set_offset
;;; ----------------------------------------------------------------------------

(defun (setf text-iter-offset) (offset iter)
  (cffi:foreign-funcall "gtk_text_iter_set_offset"
                        (g:boxed text-iter) iter
                        :int offset
                        :void)
  offset)

(cffi:defcfun ("gtk_text_iter_get_offset" text-iter-offset) :int
 #+liber-documentation
 "@version{2024-7-1}
  @syntax{(gtk:text-iter-offset iter) => offset}
  @syntax{(setf (gtk:text-iter-offset iter) offset)}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @argument[offset]{an integer with a character offset}
  @begin{short}
    The @fun{gtk:text-iter-offset} function returns the character offset of an
    iterator.
  @end{short}
  The @setf{gtk:text-iter-offset} function sets the iterator to point to the
  given character offset.

  Each character in a text buffer has an offset, starting with 0 for the first
  character in the text buffer. Use the @fun{gtk:text-buffer-iter-at-offset}
  function to convert an character offset back into an iterator.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-iter-at-offset}"
  (iter (g:boxed text-iter)))

(export 'text-iter-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_line
;;; gtk_text_iter_set_line
;;; ----------------------------------------------------------------------------

(defun (setf text-iter-line) (line iter)
  (cffi:foreign-funcall "gtk_text_iter_set_line"
                        (g:boxed text-iter) iter
                        :int line
                        :void)
  line)

(cffi:defcfun ("gtk_text_iter_get_line" text-iter-line) :int
 #+liber-documentation
 "@version{2024-7-1}
  @syntax{(gtk:text-iter-line iter) => line}
  @syntax{(setf (gtk:text-iter-line iter) line)}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @argument[line]{an integer with the line number, counted from 0}
  @begin{short}
    The @fun{gtk:text-iter-line} function returns the line number containing
    the iterator.
  @end{short}
  The @setf{gtk:text-iter-line} function moves the iterator to the start of the
  given line number.

  Lines in a text buffer are numbered beginning with 0 for the first line in
  the text buffer. If the given line number is negative or larger than the
  number of lines in the text buffer, moves the iterator to the start of the
  last line in the text buffer.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}"
  (iter (g:boxed text-iter)))

(export 'text-iter-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_line_offset
;;; gtk_text_iter_set_line_offset
;;;
;;; gtk_text_iter_get_visible_line_index
;;; gtk_text_iter_set_visible_line_index
;;; ----------------------------------------------------------------------------

(defun (setf text-iter-line-offset) (offset iter &key (visible nil))
  (if visible
      (cffi:foreign-funcall "gtk_text_iter_set_visible_line_offset"
                            (g:boxed text-iter) iter
                            :int offset
                            :void)
      (cffi:foreign-funcall "gtk_text_iter_set_line_offset"
                            (g:boxed text-iter) iter
                            :int offset
                            :void))
  offset)

(cffi:defcfun ("gtk_text_iter_get_line_offset" %text-iter-line-offset) :int
  (iter (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_iter_get_visible_line_offset"
               %text-iter-visible-line-offset) :int
  (iter (g:boxed text-iter)))

(defun text-iter-line-offset (iter &key (visible nil))
 #+liber-documentation
 "@version{2024-7-26}
  @syntax{(gtk:text-iter-line-offset iter) => offset}
  @syntax{(gtk:text-iter-line-offset iter :visible t) => offset}
  @syntax{(setf (gtk:text-iter-line-offset iter) offset)}
  @syntax{(setf (gtk:text-iter-line-offset iter :visible t) offset)}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @argument[offset]{an integer with a character offset relative to the start of
    the current line of the iterator}
  @argument[visible]{a boolean keyword argument, the default is @em{false}}
  @begin{short}
    The @fun{gtk:text-iter-line-offset} function returns the character offset
    of the iterator, counting from the start of a newline-terminated line.
  @end{short}
  The @setf{gtk:text-iter-line-offset} function moves the iterator within a
  line, to the new character offset. If the @arg{visible} keyword argument is
  @em{true}, the function does not count characters that are invisible due
  to tags with the @slot[gtk:text-tag]{invisible} attribute toggled on.

  The first character on the line has offset 0. The given character offset must
  be less than or equal to the number of characters in the line. If equal, the
  iterator moves to the start of the next line. See the
  @fun{gtk:text-iter-line-index} function if you have a byte index rather than
  a character offset.
  @begin{notes}
    This function combines the @code{*_line_offset()} and
    @code{*_visible_line_offset()} functions into one, using the @arg{visible}
    keyword argument.
  @end{notes}
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-iter-line-index}"
  (if visible
      (%text-iter-visible-line-offset iter)
      (%text-iter-line-offset iter)))

(export 'text-iter-line-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_line_index
;;; gtk_text_iter_set_line_index
;;;
;;; gtk_text_iter_get_visible_line_index
;;; gtk_text_iter_set_visible_line_index
;;; ----------------------------------------------------------------------------

(defun (setf text-iter-line-index) (index iter &key (visible nil))
  (if visible
      (cffi:foreign-funcall "gtk_text_iter_set_visible_line_index"
                            (g:boxed text-iter) iter
                            :int index
                            :void)
      (cffi:foreign-funcall "gtk_text_iter_set_line_index"
                            (g:boxed text-iter) iter
                            :int index
                            :void))
  index)

(cffi:defcfun ("gtk_text_iter_get_line_index" %text-iter-line-index) :int
  (iter (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_iter_get_visible_line_index"
               %text-iter-visible-line-index) :int
  (iter (g:boxed text-iter)))

(defun text-iter-line-index (iter &key (visible nil))
 #+liber-documentation
 "@version{2024-7-26}
  @syntax{(gtk:text-iter-line-index iter) => index}
  @syntax{(gtk:text-iter-line-index iter :visible t) => index}
  @syntax{(setf (gtk:text-iter-line-offset iter) index)}
  @syntax{(setf (gtk:text-iter-line-offset iter :visible t) index)}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @argument[index]{an integer with a byte index relative to the start of the
    current line of the iterator}
  @argument[visible]{a boolean keyword argument, the default is @em{false}}
  @begin{short}
    The @fun{gtk:text-iter-line-index} function returns the byte index of the
    iterator, counting from the start of a newline-terminated line.
  @end{short}
  The @setf{gtk:text-iter-line-index} function moves the iterator within a
  line, to the new byte index. If the @arg{visible} keyword argument is
  @em{true}, the function does not count bytes that are invisible due to tags
  with the @slot[gtk:text-tag]{invisible} attribute toggled on.

  Remember that the text buffer encodes text in UTF-8, and that characters can
  require a variable number of bytes to represent. The given byte index must be
  at the start of a character, it cannot be in the middle of a UTF-8 encoded
  character.
  @begin{notes}
    This function combines the @code{*_line_index()} and
    @code{*_visible_line_index()} functions into one, using the @arg{visible}
    keyword argument.
  @end{notes}
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-iter-line-offset}"
  (if visible
      (%text-iter-visible-line-index iter)
      (%text-iter-line-index iter)))

(export 'text-iter-line-index)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_char
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_get_char" text-iter-char) g:unichar
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @return{The Unicode character.}
  @begin{short}
    Returns a Unicode character at this iterator, or @code{#\\Nul} if the
    iterator is not dereferenceable.
  @end{short}
  If the element at this iterator is a non-character element, such as an image
  embedded in the text buffer, the Unicode @code{#\OBJECT_REPLACEMENT_CHARACTER}
  character with the @code{0xFFFC} char code is returned. If invoked on the end
  iterator, @code{#\\Nul} with the @code{0} char code is returned.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-iter-text}
  @see-function{gtk:text-iter-slice}"
  (iter (g:boxed text-iter)))

(export 'text-iter-char)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_slice
;;; gtk_text_iter_get_visible_slice
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_get_visible_slice" %text-iter-visible-slice)
    (:string :free-from-foreign t)
  (start (g:boxed text-iter))
  (end (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_iter_get_slice" %text-iter-slice)
    (:string :free-from-foreign t)
  (start (g:boxed text-iter))
  (end (g:boxed text-iter)))

(defun text-iter-slice (start end &key (visible nil))
 #+liber-documentation
 "@version{2024-7-26}
  @argument[start]{a @class{gtk:text-iter} instance with the start of a range}
  @argument[end]{a @class{gtk:text-iter} instance with the end of a range}
  @argument[visible]{a boolean keyword argument, the default is @em{false}}
  @return{The string with a slice of text from the text buffer.}
  @begin{short}
    Returns a string with the text in the given range.
  @end{short}
  If the @arg{visible} keyword argument is @em{true}, invisible text is not
  included. Invisible text is usually invisible because a @class{gtk:text-tag}
  object with the @slot[gtk:text-tag]{invisible} attribute turned on has been
  applied to it.

  A \"slice\" is a string of characters encoded in UTF-8 format, including the
  Unicode \"unknown\" character @code{0xFFFC} for iterable non-character
  elements in the text buffer, such as images. Because images are encoded in
  the slice, byte and character offsets in the returned string will correspond
  to byte offsets in the text buffer. Note that the character @code{0xFFFC} can
  occur in normal text as well, so it is not a reliable indicator that a pixbuf
  or widget is in the text buffer.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-tag}
  @see-function{gtk:text-iter-text}"
  (if visible
      (%text-iter-visible-slice start end)
      (%text-iter-slice start end)))

(export 'text-iter-slice)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_text
;;; gtk_text_iter_get_visible_text
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_get_visible_text" %text-iter-visible-text)
    (:string :free-from-foreign t)
  (start (g:boxed text-iter))
  (end (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_iter_get_text" %text-iter-text)
    (:string :free-from-foreign t)
  (start (g:boxed text-iter))
  (end (g:boxed text-iter)))

(defun text-iter-text (start end &key (visible nil))
 #+liber-documentation
 "@version{2024-7-26}
  @argument[start]{a @class{gtk:text-iter} instance with the start of a range}
  @argument[end]{a @class{gtk:text-iter} instance with the end of a range}
  @argument[visible]{a boolean keyword argument, the default is @em{false}}
  @return{The string with characters from the text buffer.}
  @begin{short}
    Returns a string with the text in the given range.
  @end{short}
  If the @arg{visible} keyword argument is @em{true}, invisible text is not
  included. Invisible text is usually invisible because a @class{gtk:text-tag}
  object with the @slot[gtk:text-tag]{invisible} attribute turned on has been
  applied to it.

  If the range contains non-text elements such as images, the character and
  byte offsets in the returned string will not correspond to character and byte
  offsets in the text buffer. If you want offsets to correspond, see the
  @fun{gtk:text-iter-slice} function.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-tag}
  @see-function{gtk:text-iter-slice}"
  (if visible
      (%text-iter-visible-text start end)
      (%text-iter-text start end)))

(export 'text-iter-text)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_paintable
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_get_paintable" text-iter-paintable)
    (g:object gdk:paintable)
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @return{The @class{gdk:paintable} object at the iterator.}
  @begin{short}
    If the element at the iterator is a paintable, the paintable is returned.
  @end{short}
  Otherwise, @code{nil} is returned.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-class{gdk:paintable}"
  (iter (g:boxed text-iter)))

(export 'text-iter-paintable)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_marks
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_get_marks" text-iter-marks)
    (g:slist-t (g:object text-mark))
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @return{List of @class{gtk:text-mark} objects.}
  @begin{short}
    Returns a list of all text marks at this location.
  @end{short}
  Because text marks are not iterable, they do not take up any \"space\" in the
  text buffer, they are just text marks in between iterable locations, multiple
  text marks can exist in the same place. The returned list is not in any
  meaningful order.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-mark}"
  (iter (g:boxed text-iter)))

(export 'text-iter-marks)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_toggled_tags
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_get_toggled_tags" text-iter-toggled-tags)
    (g:slist-t (g:object text-tag))
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @argument[toggled]{@em{true} to get toggled-on tags}
  @return{The list of @class{gtk:text-tag} objects toggled at this point.}
  @begin{short}
    Returns a list of tags that are toggled on or off at this point.
  @end{short}

  If @arg{toggled} is @em{true}, the list contains tags that are toggled on.
  If a tag is toggled on at the iterator, then some non-empty range of
  characters following the iterator has that tag applied to it. If a tag is
  toggled off, then some non-empty range following the iterator does not have
  the tag applied to it.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-tag}"
  (iter (g:boxed text-iter))
  (toggled :boolean))

(export 'text-iter-toggled-tags)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_child_anchor
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_get_child_anchor" text-iter-child-anchor)
    (g:object text-child-anchor)
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @return{The @class{gtk:text-child-anchor} object at the iterator.}
  @begin{short}
    If the location at the iterator contains an anchor, the anchor is returned.
  @end{short}
  Otherwise, @code{nil} is returned.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-child-anchor}"
  (iter (g:boxed text-iter)))

(export 'text-iter-child-anchor)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_starts_tag
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_starts_tag" text-iter-starts-tag) :boolean
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @argument[tag]{a @class{gtk:text-tag} object, or @code{nil}}
  @return{@em{True} if the iterator is the start of a range tagged with
    @arg{tag}.}
  @begin{short}
    Returns @em{true} if the tag is toggled on at exactly this point.
  @end{short}
  If @arg{tag} is @code{nil}, returns @em{true} if any tag is toggled on at
  this point.

  Note that if the @fun{gtk:text-iter-starts-tag} function returns @em{true},
  it means that the iterator is at the beginning of the tagged range, and that
  the character at the iterator is inside the tagged range. In other words,
  unlike the @fun{gtk:text-iter-ends-tag} function, if the
  @fun{gtk:text-iter-starts-tag} function returns @em{true}, the
  @fun{gtk:text-iter-has-tag} function will also return @em{true} for the same
  parameters.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-tag}
  @see-function{gtk:text-iter-ends-tag}
  @see-function{gtk:text-iter-has-tag}"
  (iter (g:boxed text-iter))
  (tag (g:object text-tag)))

(export 'text-iter-starts-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_ends_tag
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_ends_tag" text-iter-ends-tag) :boolean
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @argument[tag]{a @class{gtk:text-tag} object, or @code{nil}}
  @return{@em{True} if the iterator is the end of a range tagged with
    @arg{tag}.}
  @begin{short}
    Returns @em{true} if @arg{tag} is toggled off at exactly this point.
  @end{short}
  If @arg{tag} is @code{nil}, returns @em{true} if any tag is toggled off at
  this point. Note that the @fun{gtk:text-iter-ends-tag} function returns
  @em{true} if the iterator is the end of the tagged range. The
  @fun{gtk:text-iter-has-tag} function tells you whether an iterator is within
  a tagged range.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-tag}
  @see-function{gtk:text-iter-has-tag}
  @see-function{gtk:text-iter-starts-tag}"
  (iter (g:boxed text-iter))
  (tag (g:object text-tag)))


(export 'text-iter-ends-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_toggles_tag
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_toggles_tag" text-iter-toggles-tag) :boolean
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @argument[tag]{a @class{gtk:text-tag} object, or @code{nil}}
  @return{The boolean whether @arg{tag} is toggled on or off at the iterator.}
  @begin{short}
    Tells you whether a range with @arg{tag} applied to it begins or ends at
    the iterator.
  @end{short}
  This is equivalent to
  @begin{pre}
(or (gtk:text-iter-starts-tag iter tag)
    (gtk:text-iter-ends-tag iter tag))
  @end{pre}
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-tag}
  @see-function{gtk:text-iter-starts-tag}
  @see-function{gtk:text-iter-ends-tag}"
  (iter (g:boxed text-iter))
  (tag (g:object text-tag)))

(export 'text-iter-toggles-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_has_tag
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_has_tag" text-iter-has-tag) :boolean
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @argument[tag]{a @class{gtk:text-tag} object}
  @return{@em{True} if the iterator is tagged with @arg{tag}.}
  @begin{short}
    Returns @em{true} if the iterator is within a range tagged with @arg{tag}.
  @end{short}
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-tag}
  @see-function{gtk:text-iter-starts-tag}
  @see-function{gtk:text-iter-ends-tag}"
  (iter (g:boxed text-iter))
  (tag (g:object text-tag)))

(export 'text-iter-has-tag)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_tags
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_get_tags" text-iter-tags)
    (g:slist-t (g:object text-tag))
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @return{List of @class{gtk:text-tag} objects.}
  @begin{short}
    Returns a list of tags that apply to the iterator.
  @end{short}
  The list is in ascending order of priority, highest-priority tags are last.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-tag}"
  (iter (g:boxed text-iter)))

(export 'text-iter-tags)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_editable
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_editable" text-iter-editable) :boolean
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @argument[setting]{@em{true} if text is editable by default}
  @return{@em{True} if the iterator is inside an editable range.}
  @begin{short}
    Returns whether the character at the iterator is within an editable region
    of text.
  @end{short}
  Non-editable text is \"locked\" and cannot be changed by the user via the
  @class{gtk:text-view} widget. If no tags applied to this text affect
  editability, @arg{setting} will be returned.

  You do not want to use this function to decide whether text can be inserted
  at the iterator, because for insertion you do not want to know whether the
  char at the iterator is inside an editable range, you want to know whether a
  new character inserted at the iterator would be inside an editable range. Use
  the @fun{gtk:text-iter-can-insert} function to handle this case.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-class{gtk:text-view}
  @see-function{gtk:text-iter-can-insert}"
  (iter (g:boxed text-iter))
  (setting :boolean))

(export 'text-iter-editable)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_can_insert
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_can_insert" text-iter-can-insert) :boolean
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @argument[editabe]{@em{true} if text is editable by default}
  @return{@em{True} if the text inserted at the iterator would be editable.}
  @begin{short}
    Considering the default editability of the text buffer, and tags that affect
    editability, determines whether text inserted at the iterator would be
    editable.
  @end{short}

  If text inserted at the iterator would be editable then the user should be
  allowed to insert text at the iterator. The @fun{gtk:text-buffer-insert}
  function with the @em{true} value for the @arg{interactive} argument uses this
  function to decide whether insertions are allowed at a given position.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-insert}"
  (iter (g:boxed text-iter))
  (editable :boolean))

(export 'text-iter-can-insert)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_starts_word
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_starts_word" text-iter-starts-word) :boolean
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @return{@em{True} if the iterator is at the start of a word.}
  @begin{short}
    Determines whether the iterator begins a natural-language word.
  @end{short}
  Word breaks are determined by Pango and should be correct for nearly any
  language, if not, the correct fix would be to the Pango word break algorithms.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-iter-ends-word}"
  (iter (g:boxed text-iter)))

(export 'text-iter-starts-word)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_ends_word
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_ends_word" text-iter-ends-word) :boolean
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @return{@em{True} if the iterator is at the end of a word.}
  @begin{short}
    Determines whether the iterator ends a natural-language word.
  @end{short}
  Word breaks are determined by Pango and should be correct for nearly any
  language, if not, the correct fix would be to the Pango word break algorithms.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-iter-starts-word}"
  (iter (g:boxed text-iter)))

(export 'text-iter-ends-word)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_inside_word
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_inside_word" text-iter-inside-word) :boolean
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @return{@em{True} if the iterator is inside a word.}
  @begin{short}
    Determines whether the iterator is inside a natural-language word, as
    opposed to say inside some whitespace.
  @end{short}
  Word breaks are determined by Pango and should be correct for nearly any
  language, if not, the correct fix would be to the Pango word break algorithms.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-iter-starts-word}
  @see-function{gtk:text-iter-ends-word}"
  (iter (g:boxed text-iter)))

(export 'text-iter-inside-word)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_starts_line
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_starts_line" text-iter-starts-line) :boolean
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @return{@em{True} if the iterator begins a line.}
  @begin{short}
    Returns @em{true} if the iterator begins a paragraph, for example, if the
    @fun{gtk:text-iter-line-offset} function would return 0.
  @end{short}
  However this function is potentially more efficient than the
  @fun{gtk:text-iter-line-offset} function because it does not have to compute
  the offset, it just has to see whether it is 0.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-iter-ends-line}
  @see-function{gtk:text-iter-line-offset}"
  (iter (g:boxed text-iter)))

(export 'text-iter-starts-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_ends_line
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_ends_line" text-iter-ends-line) :boolean
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @return{@em{True} if the iterator is at the end of a line.}
  @begin{short}
    Returns @em{true} if the iterator points to the start of the paragraph
    delimiter characters for a line.
  @end{short}
  Delimiters will be either a newline, a carriage return, a carriage return
  followed by a newline, or a Unicode paragraph separator character.

  Note that an iterator pointing to the @code{\\n} of a @code{\\r\\n} pair will
  not be counted as the end of a line, the line ends before the @code{\\r}. The
  end iterator is considered to be at the end of a line, even though there are
  no paragraph delimiter chars there.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-iter-starts-line}"
  (iter (g:boxed text-iter)))

(export 'text-iter-ends-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_starts_sentence
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_starts_sentence" text-iter-starts-sentence)
    :boolean
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @return{@em{True} if the iterator is at the start of a sentence.}
  @begin{short}
    Determines whether the iterator begins a sentence.
  @end{short}
  Sentence boundaries are determined by Pango and should be correct for nearly
  any language, if not, the correct fix would be to the Pango text boundary
  algorithms.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-iter-ends-sentence}"
  (iter (g:boxed text-iter)))

(export 'text-iter-starts-sentence)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_ends_sentence
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_ends_sentence" text-iter-ends-sentence) :boolean
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @return{@em{True} if the iterator is at the end of a sentence.}
  @begin{short}
    Determines whether the iterator ends a sentence.
  @end{short}
  Sentence boundaries are determined by Pango and should be correct for nearly
  any language, if not, the correct fix would be to the Pango text boundary
  algorithms.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-iter-starts-sentence}"
  (iter (g:boxed text-iter)))

(export 'text-iter-ends-sentence)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_inside_sentence
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_inside_sentence" text-iter-inside-sentence)
    :boolean
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @return{@em{True} if the iterator is inside a sentence.}
  @begin{short}
    Determines whether the iterator is inside a sentence, as opposed to in
    between two sentences, for example, after a period and before the first
    letter of the next sentence.
  @end{short}
  Sentence boundaries are determined by Pango and should be correct for nearly
  any language, if not, the correct fix would be to the Pango text boundary
  algorithms.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-iter-starts-sentence}
  @see-function{gtk:text-iter-ends-sentence}"
  (iter (g:boxed text-iter)))

(export 'text-iter-inside-sentence)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_is_cursor_position
;;; ----------------------------------------------------------------------------

;; TODO: PANOG:LOG-ATTR and PANGO:DEFAULT-BREAK are not implemented.

(cffi:defcfun ("gtk_text_iter_is_cursor_position" text-iter-is-cursor-position)
    :boolean
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @return{@em{True} if the cursor can be placed at the iterator.}
  @begin{short}
    See the @fun{gtk:text-iter-move} function with the @code{:cursor-position}
    value for the @arg{by} keyword argument, the @symbol{pango:log-attr}
    structure or the @fun{pango:default-break} function for details on what a
    cursor position is.
  @end{short}
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-symbol{pango:log-attr}
  @see-function{pango:default-break}
  @see-function{gtk:text-iter-move}"
  (iter (g:boxed text-iter)))

(export 'text-iter-is-cursor-position)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_chars_in_line
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_get_chars_in_line" text-iter-chars-in-line) :int
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @return{The integer with the number of characters in the line.}
  @begin{short}
    Returns the number of characters in the line containing the iterator,
    including the paragraph delimiters.
  @end{short}
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-iter-bytes-in-line}"
  (iter (g:boxed text-iter)))

(export 'text-iter-chars-in-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_bytes_in_line
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_get_bytes_in_line" text-iter-bytes-in-line) :int
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @return{The integer with the number of bytes in the line.}
  @begin{short}
    Returns the number of bytes in the line containing the iterator, including
    the paragraph delimiters.
  @end{short}
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-iter-chars-in-line}"
  (iter (g:boxed text-iter)))

(export 'text-iter-bytes-in-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_get_language
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_get_language" text-iter-language)
    (g:boxed pango:language)
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @return{The @class{pange-language} instance with the language in effect at
    the iterator.}
  @begin{short}
    Returns the language in effect at the iterator.
  @end{short}
  If no tags affecting language apply to the iterator, the return value is
  identical to that of the @fun{gtk:default-language} function.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-class{pango:language}
  @see-function{gtk:default-language}"
  (iter (g:boxed text-iter)))

(export 'text-iter-language)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_is_end
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_is_end" text-iter-is-end) :boolean
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @return{@em{True} if the iterator is the end iterator.}
  @begin{short}
    Returns @em{true} if the iterator is the end iterator, i.e. one past the
    last dereferenceable iterator in the text buffer.
  @end{short}
  The @fun{gtk:text-iter-is-end} function is the most efficient way to check
  whether an iterator is the end iterator.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-iter-is-start}"
  (iter (g:boxed text-iter)))

(export 'text-iter-is-end)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_is_start
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_is_start" text-iter-is-start) :boolean
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @return{@em{True} if the iterator is the first in the text buffer.}
  @begin{short}
    Returns @em{true} if the iterator is the first iterator in the text buffer,
    that is if the iterator has a character offset of 0.
  @end{short}
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-iter-is-end}"
  (iter (g:boxed text-iter)))

(export 'text-iter-is-start)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_move
;;;
;;; gtk_text_iter_forward_char                              not exported
;;; gtk_text_iter_backward_char                             not exported
;;; gtk_text_iter_forward_chars                             not exported
;;; gtk_text_iter_backward_chars                            not exported
;;; gtk_text_iter_forward_line                              not exported
;;; gtk_text_iter_backward_line                             not exported
;;; gtk_text_iter_forward_lines                             not exported
;;; gtk_text_iter_backward_lines                            not exported
;;; gtk_text_iter_forward_word_ends                         not exported
;;; gtk_text_iter_backward_word_starts                      not exported
;;; gtk_text_iter_forward_word_end                          not exported
;;; gtk_text_iter_backward_word_start                       not exported
;;; gtk_text_iter_forward_cursor_position                   not exported
;;; gtk_text_iter_backward_cursor_position                  not exported
;;; gtk_text_iter_forward_cursor_positions                  not exported
;;; gtk_text_iter_backward_cursor_positions                 not exported
;;; gtk_text_iter_backward_sentence_start                   not exported
;;; gtk_text_iter_backward_sentence_starts                  not exported
;;; gtk_text_iter_forward_sentence_end                      not exported
;;; gtk_text_iter_forward_sentence_ends                     not exported
;;; gtk_text_iter_forward_visible_word_ends                 not exported
;;; gtk_text_iter_backward_visible_word_starts              not exported
;;; gtk_text_iter_forward_visible_word_end                  not exported
;;; gtk_text_iter_backward_visible_word_start               not exported
;;; gtk_text_iter_forward_visible_cursor_position           not exported
;;; gtk_text_iter_backward_visible_cursor_position          not exported
;;; gtk_text_iter_forward_visible_cursor_positions          not exported
;;; gtk_text_iter_backward_visible_cursor_positions         not exported
;;; gtk_text_iter_forward_visible_line                      not exported
;;; gtk_text_iter_backward_visible_line                     not exported
;;; gtk_text_iter_forward_visible_lines                     not exported
;;; gtk_text_iter_backward_visible_lines                    not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_forward_char" text-iter-forward-char) :boolean
  (iter (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_iter_backward_char" text-iter-backward-char) :boolean
  (iter (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_iter_forward_chars" text-iter-forward-chars) :boolean
  (iter (g:boxed text-iter))
  (count :int))

(cffi:defcfun ("gtk_text_iter_backward_chars" text-iter-backward-chars) :boolean
  (iter (g:boxed text-iter))
  (count :int))

(cffi:defcfun ("gtk_text_iter_forward_line" text-iter-forward-line) :boolean
  (iter (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_iter_backward_line" text-iter-backward-line) :boolean
  (iter (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_iter_forward_lines" text-iter-forward-lines) :boolean
  (iter (g:boxed text-iter))
  (count :int))

(cffi:defcfun ("gtk_text_iter_backward_lines" text-iter-backward-lines) :boolean
  (iter (g:boxed text-iter))
  (count :int))

(cffi:defcfun ("gtk_text_iter_forward_word_ends" text-iter-forward-word-ends)
    :boolean
  (iter (g:boxed text-iter))
  (count :int))

(cffi:defcfun ("gtk_text_iter_backward_word_starts"
               text-iter-backward-word-starts) :boolean
  (iter (g:boxed text-iter))
  (count :int))

(cffi:defcfun ("gtk_text_iter_forward_word_end" text-iter-forward-word-end)
    :boolean
  (iter (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_iter_backward_word_start"
               text-iter-backward-word-start) :boolean
  (iter (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_iter_forward_cursor_position"
               text-iter-forward-cursor-position) :boolean
  (iter (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_iter_backward_cursor_position"
               text-iter-backward-cursor-position) :boolean
  (iter (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_iter_forward_cursor_positions"
               text-iter-forward-cursor-positions) :boolean
  (iter (g:boxed text-iter))
  (count :int))

(cffi:defcfun ("gtk_text_iter_backward_cursor_positions"
               text-iter-backward-cursor-positions) :boolean
  (iter (g:boxed text-iter))
  (count :int))

(cffi:defcfun ("gtk_text_iter_backward_sentence_start"
               text-iter-backward-sentence-start) :boolean
  (iter (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_iter_backward_sentence_starts"
               text-iter-backward-sentence-starts) :boolean
  (iter (g:boxed text-iter))
  (count :int))

(cffi:defcfun ("gtk_text_iter_forward_sentence_end"
               text-iter-forward-sentence-end) :boolean
  (iter (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_iter_forward_sentence_ends"
               text-iter-forward-sentence-ends) :boolean
  (iter (g:boxed text-iter))
  (count :int))

(cffi:defcfun ("gtk_text_iter_forward_visible_word_ends"
               text-iter-forward-visible-word-ends) :boolean
  (iter (g:boxed text-iter))
  (count :int))

(cffi:defcfun ("gtk_text_iter_backward_visible_word_starts"
               text-iter-backward-visible-word-starts) :boolean
  (iter (g:boxed text-iter))
  (count :int))

(cffi:defcfun ("gtk_text_iter_forward_visible_word_end"
               text-iter-forward-visible-word-end) :boolean
  (iter (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_iter_backward_visible_word_start"
               text-iter-backward-visible-word-start) :boolean
  (iter (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_iter_forward_visible_cursor_position"
               text-iter-forward-visible-cursor-position) :boolean
  (iter (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_iter_backward_visible_cursor_position"
               text-iter-backward-visible-cursor-position) :boolean
  (iter (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_iter_forward_visible_cursor_positions"
               text-iter-forward-visible-cursor-positions) :boolean
  (iter (g:boxed text-iter))
  (count :int))

(cffi:defcfun ("gtk_text_iter_backward_visible_cursor_positions"
               text-iter-backward-visible-cursor-positions) :boolean
  (iter (g:boxed text-iter))
  (count :int))

(cffi:defcfun ("gtk_text_iter_forward_visible_line"
               text-iter-forward-visible-line) :boolean
  (iter (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_iter_backward_visible_line"
               text-iter-backward-visible-line) :boolean
  (iter (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_iter_forward_visible_lines"
               text-iter-forward-visible-lines) :boolean
  (iter (g:boxed text-iter))
  (count :int))

(cffi:defcfun ("gtk_text_iter_backward_visible_lines"
               text-iter-backward-visible-lines) :boolean
  (iter (g:boxed text-iter))
  (count :int))

;;; ----------------------------------------------------------------------------

(defun text-iter-move (iter &key (count 1) (by :char) (direction :forward))
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @argument[count]{an integer with the default value 1}
  @argument[by]{a keyword which determines the operation to perform, the
    default value is @code{:char}}
  @argument[direction]{a keyword for the direction, the default value is
    @code{:forward}}
  @begin{short}
    This is a convenience function of the Lisp implementation, which replaces
    the functions to move the iterator in the text buffer.
  @end{short}

  The following operations are performed depending on the @arg{by} keyword
  argument:
  @begin[code]{table}
    @begin[:char]{entry}
      Moves @arg{count} characters if possible in the given direction, which
      is @code{:forward} or @code{:backward}. If @arg{count} would move past
      the start or end of the text buffer, moves to the start or end of the
      text buffer.

      The return value indicates whether the new position of the iterator is
      different from its original position, and dereferenceable, the last
      iterator in the text buffer is not dereferenceable. If count is 0, the
      function does nothing and returns @em{false}.

      This replaces the functions:
      @begin{itemize}
        @item{@code{gtk_text_iter_forward_char()}}
        @item{@code{gtk_text_iter_backward_char()}}
        @item{@code{gtk_text_iter_forward_chars()}}
        @item{@code{gtk_text_iter_backward_chars()}}
      @end{itemize}
    @end{entry}
    @begin[:line]{entry}
      Moves the iterator to the start of the next line for the @code{:forward}
      direction or to the start of the previous line for the @code{:backward}
      direction.

      If the the iterator is already on the last line of the text buffer for
      a @code{:forward} direction, moves the iterator to the end of the current
      line. If after the operation, the iterator is at the end of the text
      buffer and not dereferencable, returns @em{false}. Otherwise, returns
      @em{true}.

      For the @code{:backward} direction returns @em{true} if the iterator could
      be moved, for example, if the iterator was at character offset 0, this
      function returns @em{false}. Therefore if the iterator was already on line
      0, but not at the start of the line, the iterator is snapped to the start
      of the line and the function returns @em{true}. Note that this implies
      that in a loop calling this function, the line number may not change on
      every iteration, if your first iteration is on line 0.

      This replaces the functions:
      @begin{itemize}
        @item{@code{gtk_text_iter_forward_line()}}
        @item{@code{gtk_text_iter_backward_line()}}
        @item{@code{gtk_text_iter_forward_lines()}}
        @item{@code{gtk_text_iter_backward_lines()}}
      @end{itemize}
    @end{entry}
    @begin[:word]{entry}
      Moves forward up to @arg{count} times for the @code{:forward} direction
      to the next word end. If the iterator is currently on a word end, moves
      forward to the next one after that.

      Moves backward up to @arg{count} times for the @code{:backward} direction
      to the previous word start. If the iterator is currently on a word start,
      moves backward to the next one after that.

      Word breaks are determined by Pango and should be correct for nearly any
      language, if not, the correct fix would be to the Pango word break
      algorithms.

      Returns @em{true} if the iterator moved and is not the end iterator.

      This replaces the functions:
      @begin{itemize}
        @item{@code{gtk_text_iter_forward_word_end()}}
        @item{@code{gtk_text_iter_backward_word_start()}}
        @item{@code{gtk_text_iter_forward_word_ends()}}
        @item{@code{gtk_text_iter_backward_word_starts()}}
      @end{itemize}
    @end{entry}
    @begin[:cursor-position]{entry}
      Moves the iterator up to @arg{count} cursor positions forward or backward.

      Cursor positions are (unsurprisingly) positions where the cursor can
      appear. Perhaps surprisingly, there may not be a cursor position between
      all characters. The most common example for European languages would be
      a carriage return/newline sequence. For some Unicode characters, the
      equivalent of say the letter \"a\" with an accent mark will be represented
      as two characters, first the letter then a \"combining mark\" that causes
      the accent to be rendered. So the cursor cannot go between those two
      characters. See also the @class{pango:log-attr} structure and the
      @fun{pango:default-break} function.

      Returns @em{true} if we moved and the new position is dereferenceable.

      This replaces the functions:
      @begin{itemize}
        @item{@code{gtk_text_iter_forward_cursor_position()}}
        @item{@code{gtk_text_iter_backward_cursor_position()}}
        @item{@code{gtk_text_iter_forward_cursor_positions()}}
        @item{@code{gtk_text_iter_backward_cursor_positions()}}
      @end{itemize}
    @end{entry}
    @begin[:sentence]{entry}
      Moves backward to the previous sentence start or forward to the next
      sentence end. If the iterator is already at the start of a sentence, moves
      backward to the next one. If the iterator is at the end of a sentence,
      moves to the next end of sentence.

      Sentence boundaries are determined by Pango and should be correct for
      nearly any language, if not, the correct fix would be to the Pango text
      boundary algorithms.

      This replaces the functions:
      @begin{itemize}
        @item{@code{gtk_text_iter_forward_sentence_end()}}
        @item{@code{gtk_text_iter_backward_sentence_start()}}
        @item{@code{gtk_text_iter_forward_sentences_ends()}}
        @item{@code{gtk_text_iter_backward_sentence_starts()}}
      @end{itemize}
    @end{entry}
    @begin[:visible-word]{entry}
      Moves forward to the next visible word end or backward to the previous
      visible word start.

      If the iterator is currently on a word start, moves backward to the next
      one after that. Word breaks are determined by Pango and should be correct
      for nearly any language. If not, the correct fix would be to the Pango
      word break algorithms.

      This replaces the functions:
      @begin{itemize}
        @item{@code{gtk_text_iter_forward_visible_word_end()}}
        @item{@code{gtk_text_iter_backward_visible_word_start()}}
        @item{@code{gtk_text_iter_forward_visible_word_ends()}}
        @item{@code{gtk_text_iter_backward_visible_word_starts()}}
      @end{itemize}
    @end{entry}
    @begin[:visible-line]{entry}
      Moves the iterator to the start of the next visible line or to the start
      of the previous visible line.

      The return value indicates whether the iterator moved onto a
      dereferenceable position. If the iterator did not move, or moved onto the
      end iterator, then @em{false} is returned. If @arg{count} is 0, the
      function does nothing and returns @em{false}.

      This replaces the functions:
      @begin{itemize}
        @item{@code{gtk_text_iter_forward_visible_line()}}
        @item{@code{gtk_text_iter_backward_visible_line()}}
        @item{@code{gtk_text_iter_forward_visible_lines()}}
        @item{@code{gtk_text_iter_backward_visible_lines()}}
      @end{itemize}
    @end{entry}
    @begin[:visible-cursor-position]{entry}
      Moves the iterator forward to the next visible cursor position or forward
      to the previous visible cursor position.

      Returns @em{true} if we moved and the new position is dereferenceable.

      This replaces the functions:
      @begin{itemize}
        @item{@code{gtk_text_iter_forward_visible_cursor_position()}}
        @item{@code{gtk_text_iter_backward_visible_cursor_position()}}
        @item{@code{gtk_text_iter_forward_visible_cursor_positions()}}
        @item{@code{gtk_text_iter_backward_visible_cursor_positions()}}
      @end{itemize}
    @end{entry}
  @end{table}
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}"
  (assert (typep by '(member :char :line :word :cursor-position :sentence
                             :visible-word :visible-line
                             :visible-cursor-position)))
  (assert (typep direction '(member :forward :backward)))
  (when (and (member by '(:char :line :cursor-position :visible-line
                           :visible-cursor-position))
             (eq direction :backward))
    (setf count (- count)))
  (ecase by
    (:char
     (text-iter-forward-chars iter count))
    (:line
     (text-iter-forward-lines iter count))
    (:word
     (if (eq direction :forward)
         (text-iter-forward-word-ends iter count)
         (text-iter-backward-word-starts iter count)))
    (:cursor-position
     (text-iter-forward-cursor-positions iter count))
    (:sentence
     (if (eq direction :forward)
         (text-iter-forward-sentence-ends iter count)
         (text-iter-backward-sentence-starts iter count)))
    (:visible-word
     (if (eq direction :forward)
         (text-iter-forward-visible-word-ends iter count)
         (text-iter-backward-visible-word-starts iter count)))
    (:visible-line
     (text-iter-forward-visible-lines iter count))
    (:visible-cursor-position
     (text-iter-forward-visible-cursor-positions iter count))))

(export 'text-iter-move)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_to_end
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_forward_to_end" text-iter-forward-to-end) :void
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @begin{short}
    Moves the iterator forward to the \"end iterator\", which points one past
    the last valid character in the text buffer.
  @end{short}
  The @fun{gtk:text-iter-char} function called on the end iterator returns
  @code{#\\Nul}, which is convenient for writing loops.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-iter-char}"
  (iter (g:boxed text-iter)))

(export 'text-iter-forward-to-end)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_to_line_end
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_forward_to_line_end"
               text-iter-forward-to-line-end) :boolean
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @return{@em{True} if we moved and the new location is not the end iterator.}
  @begin{short}
    Moves the iterator to point to the paragraph delimiter characters, which
    will be either a newline, a carriage return, a carriage return/newline in
    sequence, or the Unicode paragraph separator character.
  @end{short}

  If the iterator is already at the paragraph delimiter characters, moves to the
  paragraph delimiter characters for the next line. If the iterator is on the
  last line in the text buffer, which does not end in paragraph delimiters,
  moves to the end iterator (end of the last line), and returns @em{false}.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}"
  (iter (g:boxed text-iter)))

(export 'text-iter-forward-to-line-end)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_forward_to_tag_toggle
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_forward_to_tag_toggle"
               text-iter-forward-to-tag-toggle) :boolean
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @argument[tag]{a @class{gtk:text-tag} object, or @code{nil}}
  @return{@em{True} if we found a tag toggle after the iterator.}
  @begin{short}
    Moves forward to the next toggle (on or off) of @arg{tag}, or to the next
    toggle of any tag if @arg{tag} is @code{nil}.
  @end{short}
  If no matching tag toggles are found, returns @em{false}, otherwise @em{true}.
  Does not return toggles located at the iterator, only toggles after the
  iterator. Sets the iterator to the location of the toggle, or to the end of
  the text buffer if no toggle is found.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}"
  (iter (g:boxed text-iter))
  (tag (g:object text-tag)))

(export 'text-iter-forward-to-tag-toggle)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_backward_to_tag_toggle
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_backward_to_tag_toggle"
               text-iter-backward-to-tag-toggle) :boolean
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @argument[tag]{a @class{gtk:text-tag} object, or @code{nil}}
  @return{@em{True} if we found a tag toggle before the iterator.}
  @begin{short}
    Moves backward to the next toggle (on or off) of the @arg{tag}, or to the
    next toggle of any tag if @arg{tag} is @code{nil}.
  @end{short}
  If no matching tag toggles are found, returns @em{false}, otherwise @em{true}.
  Does not return toggles located at the iterator, only toggles before the
  iterator. Sets the iterator to the location of the toggle, or the start of
  the text buffer if no toggle is found.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}"
  (iter (g:boxed text-iter))
  (tag (g:object text-tag)))

(export '(text-iter-backward-to-tag-toggle))

;;; ----------------------------------------------------------------------------
;;; GtkTextCharPredicate
;;; ----------------------------------------------------------------------------

(cffi:defcallback text-char-predicate :boolean
    ((char g:unichar)
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (restart-case
      (funcall func char)
      (return-true () :report "Return T" t)
      (return-false () :report "Return NIL" nil))))

#+liber-documentation
(setf (liber:alias-for-symbol 'text-char-predicate)
      "Callback"
      (liber:symbol-documentation 'text-char-predicate)
 "@version{2024-7-1}
  @syntax{lambda (ch) => result}
  @argument[ch]{a Unichar character}
  @argument[result]{@em{true} if the character was found}
  @begin{short}
    A callback function used by the @fun{gtk:text-iter-find-char} function to
    search a char in the text buffer.
  @end{short}
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-iter-find-char}")

(export 'text-char-predicate)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_find_char
;;;
;;; gtk_text_iter_forward_find_char                         not exported
;;; gtk_text_iter_backward_find_char                        not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_forward_find_char"
               %text-iter-forward-find-char) :boolean
  (iter (g:boxed text-iter))
  (pred :pointer)
  (user-data :pointer)
  (limit (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_iter_backward_find_char" %text-iter-backward-find-char)
    :boolean
  (iter (g:boxed text-iter))
  (pred :pointer)
  (user-data :pointer)
  (limit (g:boxed text-iter)))

(defun text-iter-find-char (iter predicate &key limit (direction :forward))
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @argument[pred]{a @symbol{gtk:text-char-predicate} callback function to be
    called on each character}
  @argument[limit]{a @class{gtk:text-iter} instance with a search limit, or
    @code{nil} for none, the default is @code{nil}}
  @argument[direction]{a @code{:forward} value indicates forward search and
    a @code{:backward} value backward search, the default is @code{:forward}}
  @return{@em{True} if a match was found.}
  @begin{short}
    Advances the iterator, calling the @arg{pred} function on each character.
  @end{short}
  If @arg{direction} is @code{:backward} goes backward from the iterator.

  If the @arg{pred} callback function returns @em{true}, returns @em{true} and
  stops scanning. If the @arg{pred} callback function never returns @em{true},
  the iterator is set to @arg{limit} if @arg{limit} is non-@code{nil}, otherwise
  to the end iterator. The @arg{limit} keyword argument has the @code{nil}
  default value.
  @begin{notes}
    This function combines the @code{gtk_text_iter_forward_find_char()} and
    @code{gtk_text_iter_backward_find_char()} functions into one single
    function using the @arg{direction} keyword argument.
  @end{notes}
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-symbol{gtk:text-char-predicate}"
  (assert (typep direction '(member :forward :backward)))
  (glib:with-stable-pointer (ptr predicate)
    (if (eq direction :forward)
        (%text-iter-forward-find-char iter
                                      (cffi:callback text-char-predicate)
                                      ptr
                                      limit)
        (%text-iter-backward-find-char iter
                                       (cffi:callback text-char-predicate)
                                       ptr
                                       limit))))

(export 'text-iter-find-char)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_search
;;;
;;; gtk_text_iter_forward_search                            not exported
;;; gtk_text_iter_backward_search                           not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_forward_search" %text-iter-forward-search)
    :boolean
  (iter (g:boxed text-iter))
  (str :string)
  (flags text-search-flags)
  (match-start (g:boxed text-iter))
  (match-end (g:boxed text-iter))
  (limit (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_iter_backward_search" %text-iter-backward-search)
    :boolean
  (iter (g:boxed text-iter))
  (str :string)
  (flags text-search-flags)
  (match-start (g:boxed text-iter))
  (match-end (g:boxed text-iter))
  (limit (g:boxed text-iter)))

(defun text-iter-search (iter str &key flags limit (direction :forward))
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} iterator with the start of the search}
  @argument[str]{a search string}
  @argument[flags]{a @symbol{gtk:text-search-flags} value with the flags
    affecting how the search is done}
  @argument[limit]{a @class{gtk:text-iter} iterator with the bound for the
    search, or @code{nil} for the end of the text buffer}
  @argument[direction]{a @code{:forward} value indicates forward search and
    a @code{:backward} value backward search,  the default is @code{:forward}}
  @begin{return}
    @arg{start} -- a @class{gtk:text-iter} iterator with the start of the
      match @br{}
    @arg{end} -- a @class{gtk:text-iter} iterator with the end of the match
  @end{return}
  @begin{short}
    Searches for @arg{str} in the text buffer that is associated with
    @arg{iter}.
  @end{short}
  The direction of the search is indicated with the @arg{direction} keyword
  argument which has a @code{:forward} default value for forward
  search. For backward search the @arg{direction} argument takes the
  @code{:backward} value. The @arg{flags} and @arg{limit} arguments are keyword
  arguments with a @code{nil} default value.

  Any match is returned by returning @arg{start} to the first character of the
  match and @arg{end} to the first character after the match. The search will
  not continue past @arg{limit}. Note that a search is a linear or O(n)
  operation, so you may wish to use @arg{limit} to avoid locking up your
  UI on large text buffers.

  The @arg{start} value will never be set to an iterator located before
  @arg{iter}, even if there is a possible @arg{end} after or at @arg{iter}.
  @begin{notes}
  This function combines the @code{gtk_text_iter_forward_search()} and
  @code{gtk_text_iter_backward_search()} functions into one function.
  @end{notes}
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-symbol{gtk:text-search-flags}"
  (assert (typep direction '(member :forward :backward)))
  (let ((start (make-instance 'text-iter))
        (end (make-instance 'text-iter)))
    (when (if (eq direction :forward)
              (%text-iter-forward-search iter
                                         str
                                         flags
                                         start
                                         end
                                         limit)
              (%text-iter-backward-search iter
                                          str
                                          flags
                                          start
                                          end
                                          limit))
      (values start end))))

(export 'text-iter-search)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_equal" text-iter-equal) :boolean
 #+liber-documentation
 "@version{2024-7-1}
  @argument[lhs]{a @class{gtk:text-iter} instance}
  @argument[rhs]{another @class{gtk:text-iter} instance}
  @return{@em{True} if the iterators point to the same place in the text
    buffer.}
  @begin{short}
    Tests whether two iterators are equal, using the fastest possible mechanism.
  @end{short}
  This function is very fast. You can expect it to perform better than for
  example, getting the character offset for each iterator and comparing the
  offsets yourself. Also, it is a bit faster than the
  @fun{gtk:text-iter-compare} function.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-iter-compare}"
  (lhs (g:boxed text-iter))
  (rhs (g:boxed text-iter)))

(export 'text-iter-equal)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_compare
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_compare" text-iter-compare) :int
 #+liber-documentation
 "@version{2024-7-1}
  @argument[lhs]{a @class{gtk:text-iter} instance}
  @argument[rhs]{another @class{gtk:text-iter} instance}
  @return{-1 if @arg{lhs} is less than @arg{rhs}, 1 if @arg{lhs} is greater, 0
    if they are equal.}
  @begin{short}
    A @code{qsort()}-style function that returns negative if @arg{lhs} is less
    than @arg{rhs}, positive if @arg{lhs} is greater than @arg{rhs}, and 0 if
    they are equal.
  @end{short}
  Ordering is in character offset order, for example, the first character in
  the text buffer is less than the second character in the text buffer.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-iter-equal}"
  (lhs (g:boxed text-iter))
  (rhs (g:boxed text-iter)))

(export 'text-iter-compare)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_in_range
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_in_range" text-iter-in-range) :boolean
 #+liber-documentation
 "@version{2024-7-1}
  @argument[iter]{a @class{gtk:text-iter} instance}
  @argument[start]{a @class{gtk:text-iter} instance with the start of range}
  @argument[end]{a @class{gtk:text-iter} instance with the end of range}
  @return{@em{True} if the iterator is in the range.}
  @begin{short}
    Checks whether the iterator falls in the range [@arg{start}, @arg{end}).
  @end{short}
  The @arg{start} and @arg{end} iterators must be in ascending order.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}"
  (iter (g:boxed text-iter))
  (start (g:boxed text-iter))
  (end (g:boxed text-iter)))

(export 'text-iter-in-range)

;;; ----------------------------------------------------------------------------
;;; gtk_text_iter_order
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_iter_order" text-iter-order) :void
 #+liber-documentation
 "@version{2024-7-1}
  @argument[first]{a @class{gtk:text-iter} instance}
  @argument[second]{another @class{gtk:text-iter} instance}
  @begin{short}
    Swaps the value of @arg{first} and @arg{second} if @arg{second} comes
    before @arg{first} in the text buffer.
  @end{short}
  That is, ensures that @arg{first} and @arg{second} are in sequence. Most text
  buffer functions that take a range call this automatically on your behalf, so
  there is no real reason to call it yourself in those cases. There are some
  exceptions, such as the @fun{gtk:text-iter-in-range} functions, that expect a
  pre-sorted range.
  @see-class{gtk:text-iter}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-iter-in-range}"
  (first (g:boxed text-iter))
  (second (g:boxed text-iter)))

(export 'text-iter-order)

;;; --- End of file gtk4.text-iter.lisp ----------------------------------------
