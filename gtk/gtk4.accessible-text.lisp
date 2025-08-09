;;; ----------------------------------------------------------------------------
;;; gtk4.accessible-text.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2024 - 2025 Dieter Kaiser
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
;;;     GtkAccessibleText
;;;     GtkAccessibleTextRange
;;;
;;;     GtkAccessibleTextContentChange
;;;     GtkAccessibleTextGranularity
;;;
;;; Functions
;;;
;;;     gtk_accessible_text_get_attributes
;;;     gtk_accessible_text_get_caret_position
;;;     gtk_accessible_text_get_contents
;;;     gtk_accessible_text_get_contents_at
;;;     gtk_accessible_text_get_default_attributes
;;;     gtk_accessible_text_get_extents                     Since 4.16
;;;     gtk_accessible_text_get_offset                      Since 4.16
;;;     gtk_accessible_text_get_selection
;;;
;;;     gtk_accessible_text_update_caret_position
;;;     gtk_accessible_text_update_contents
;;;     gtk_accessible_text_update_selection_bound
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkAccessibleTextContentChange
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkAccessibleTextContentChange"
                      accessible-text-content-change
  (:export t
   :type-initializer "gtk_accessible_text_content_change_get_type")
  (:insert 0)
  (:remove 1))

#+liber-documentation
(setf (liber:alias-for-symbol 'accessible-text-content-change)
      "GEnum"
      (liber:symbol-documentation 'accessible-text-content-change)
 "@version{2025-07-22}
  @begin{declaration}
(gobject:define-genum \"GtkAccessibleTextContentChange\"
                      accessible-text-content-change
  (:export t
   :type-initializer \"gtk_accessible_text_content_change_get_type\")
  (:insert 0)
  (:remove 1))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:insert]{Contents change as the result of an insert operation.}
      @entry[:remvve]{Contents change as the result of a remove operation.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The type of contents change operation.
  @end{short}
  Since 4.14
  @see-class{gtk:accessible-text}")

;;; ----------------------------------------------------------------------------
;;; GtkAccessibleTextGranularity
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkAccessibleTextGranularity" accessible-text-granularity
  (:export t
   :type-initializer "gtk_accessible_text_granularity_get_type")
  (:character 0)
  (:word 1)
  (:sentence 2)
  (:line 3)
  (:paragraph 4))

#+liber-documentation
(setf (liber:alias-for-symbol 'accessible-text-granularity)
      "GEnum"
      (liber:symbol-documentation 'accessible-text-granularity)
 "@version{2024-11-05}
  @begin{declaration}
(gobject:define-genum \"GtkAccessibleTextGranularity\" accessible-text-granularity
  (:export t
   :type-initializer \"gtk_accessible_text_granularity_get_type\")
  (:character 0)
  (:word 1)
  (:sentence 2)
  (:line 3)
  (:paragraph 4))
  @end{declaration}
  @begin{values}
    @begin{tables}
      @entry[:character]{Use the boundary between characters, including
        non-printing characters.}
      @entry[:word]{Use the boundary between words, starting from the beginning
        of the current word and ending at the beginning of the next word.}
      @entry[:sentence]{Use the boundary between sentences, starting from the
        beginning of the current sentence and ending at the beginning of the
        next sentence.}
      @entry[:line]{Use the boundary between lines, starting from the beginning
        of the current line and ending at the beginning of the next line.}
      @entry[:paragraph]{Use the boundary between paragraphs, starting from the
        beginning of the current paragraph and ending at the beginning of the
        next paragraph.}
    @end{tables}
  @end{values}
  @begin{short}
    The granularity for queries about the text contents of a
    @class{gtk:accessible-text} implementation.
  @end{short}
  Since 4.14
  @see-class{gtk:accessible-text}")

;;; ----------------------------------------------------------------------------
;;; GtkAccessibleTextRange
;;;
;;; struct GtkAccessibleTextRange {
;;;   gsize start;
;;;   gsize length;
;;; }
;;;
;;; start
;;;     The start of the range, in characters.
;;;
;;; length
;;;     The length of the range, in characters.
;;;
;;; A range inside the text of an accessible object.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkAccessibleText
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GtkAccessibleText" accessible-text
  (:superclass accessible
   :export t
   :type-initializer "gtk_accessible_text_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'accessible-text)
      "Interface"
      (documentation 'accessible-text 'type)
 "@version{2025-07-25}
  @begin{short}
    The @class{gtk:accessible-text} interface is an interface for accessible
    objects containing formatted text.
  @end{short}
  It is meant to be implemented by accessible objects that have text formatted
  with attributes, or non-trivial text contents.

  You should use the @val[gtk:accessible-property]{:label} or the
  @val[gtk:accessible-property]{:description} values of the
  @sym{gtk:accessible-property} enumeration for accessible objects containing
  simple, unformatted text.

  Since 4.14
  @see-class{gtk:accessible}
  @see-symbol{gtk:accessible-property}")

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_text_get_attributes
;;;
;;; Retrieves the text attributes inside the accessible object.
;;;
;;; Since 4.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_text_get_caret_position
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accessible_text_get_caret_position"
               accessible-text-caret-position) :uint
 #+liber-documentation
 "@version{#2025-07-19}
  @argument[accessible]{a @class{gtk:accessible-text} object}
  @return{The unsigned integer for the position of the caret, in characters.}
  @begin{short}
    Retrieves the position of the caret inside the accessible object.
  @end{short}
  Since 4.14
  @see-class{gtk:accessible-text}"
  (accessible (g:object accessible-text)))

(export 'accessible-text-caret-position)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_text_get_contents
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accessible_text_get_contents" accessible-text-contents)
    (g:boxed g:bytes :return)
 #+liber-documentation
 "@version{#2025-07-13}
  @argument[accessible]{a @class{gtk:accessible-text} object}
  @argument[start]{an unsigned integer for the beginning of the range, in
    characters}
  @argument[end]{an unsigned integer for the end of the range, in characters}
  @begin{return}
    The @class{g:bytes} instance for the requested slice of the contents
    of the accessible object, as UTF-8.
  @end{return}
  @begin{short}
    Retrieve the current contents of the accessible object within the given
    range.
  @end{short}
  If end is @code{G_MAXUINT}, the end of the range is the full content of the
  accessible object.

  Since 4.14
  @see-class{gtk:accessible-text}"
  (accessible (g:object accessible-text))
  (start :uint)
  (end :uint))

(export 'accessible-text-contents)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_text_get_contents_at
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accessible_text_get_contents_at"
               %accessible-text-contents-at) (g:boxed g:bytes)
  (accessible (g:object accessible-text))
  (offset :uint)
  (granularity accessible-text-granularity)
  (start (:pointer :int))
  (end (:pointer :int)))

(defun accessible-text-contents-at (accessible offset granularity)
 #+liber-documentation
 "@version{#2025-07-19}
  @syntax{(gtk:accessible-text-contents-at accessible offset granularity)
     => start, end}
  @argument[accessible]{a @class{gtk:accessible-text} object}
  @argument[offset]{an unsigned integer for the offset, in characters}
  @argument[granularity]{a @sym{gtk:accessible-text-granularity} value}
  @argument[start]{an unsigned integer for the start of the range, in
    characters}
  @argument[end]{an unsigned integer for the end of the range, in characters}
  @begin{short}
    Retrieve the current contents of the accessible object starting from the
    given offset, and using the given granularity.
  @end{short}
  The @arg{start} and @arg{end} values contain the boundaries of the text.

  Since 4.14
  @see-class{gtk:accessible-text}
  @see-symbol{gtk:accessible-text-granularity}"
  (cffi:with-foreign-objects ((start :int) (end :int))
    (let ((bytes (%accessible-text-contents-at accessible
                                               offset
                                               granularity
                                               start
                                               end)))
    (values bytes
            (cffi:mem-ref start :int)
            (cffi:mem-ref end :int)))))

(export 'accessible-text-contents-at)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_text_get_default_attributes
;;;
;;; Retrieves the default text attributes inside the accessible object.
;;;
;;; Since: 4.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_text_get_extents
;;; ----------------------------------------------------------------------------

#+gtk-4-16
(cffi:defcfun ("gtk_accessible_text_get_extents" %accessible-text-extents)
    :boolean
  (accessible (g:object accessible-text))
  (start :uint)
  (end :uint)
  (extents (:pointer (:struct graphene:rect-t))))

#+gtk-4-16
(defun accessible-text-extents (accessible start end extents)
 #+liber-documentation
 "@version{#2025-07-25}
  @argument[accessible]{a @class{gtk:accessible-text} object}
  @argument[start]{an unsigned integer for the start offset, in characters}
  @argument[end]{an unsigned integer for the end offset, in characters,}
  @argument[extents]{a @sym{graphene:rect-t} instance}
  @begin{return}
    The @sym{graphene:rect-t} instance if the extents were filled, @code{nil}
    otherwise.
  @end{return}
  @begin{short}
    Obtains the extents of a range of text, in widget coordinates.
  @end{short}

  Since 4.16
  @see-class{gtk:accessible-text}
  @see-symbol{graphene:rect-t}"
  (when (%accessible-text-extents accessible start end extents)
    extents))

#+gtk-4-16
(export 'accessible-text-extents)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_text_get_offset
;;; ----------------------------------------------------------------------------

#+gtk-4-16
(cffi:defcfun ("gtk_accessible_text_get_offset" %accessible-text-offset)
    :boolean
  (accessible (g:object accessible-text))
  (point (:pointer (:struct graphene:point-t)))
  (offset (:pointer :uint)))

#+gtk-4-16
(defun accessible-text-offset (accessible point)
 #+liber-documentation
 "@version{#2025-07-25}
  @argument[accessible]{a @class{gtk:accessible-text} object}
  @argument[point]{a @sym{graphene:point-t} instance}
  @return{The unsigned integer for the offset, if set, otherwise @code{nil}.}
  @begin{short}
    Gets the text offset at a given point.
  @end{short}
  Since 4.16
  @see-class{gtk:accessible-text}
  @see-symbol{graphene:point-t}"
  (cffi:with-foreign-object (offset :uint)
    (when (%accessible-text-offset accessible point offset)
      (cffi:mem-ref offset :uint))))

#+gtk-4-16
(export 'accessible-text-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_text_get_selection
;;;
;;; Retrieves the selection ranges in the accessible object.
;;;
;;; Since 4.14
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_text_update_caret_position
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accessible_text_update_caret_position"
               accessible-text-update-caret-position) :void
 #+liber-documentation
 "@version{#2024-11-05}
  @argument[accessible]{a @class{gtk:accessible-text} object}
  @begin{short}
    Updates the position of the caret.
  @end{short}
  Implementations of the @class{gtk:accessible-text} interface should call this
  function every time the caret has moved, in order to notify assistive
  technologies.

  Since 4.14
  @see-class{gtk:accessible-text}"
  (accessible (g:object accessible-text)))

(export 'accessible-text-update-caret-position)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_text_update_contents
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accessible_text_update_contents"
               accessible-text-update-contents) :void
 #+liber-documentation
 "@version{#2025-07-27}
  @argument[accessible]{a @class{gtk:accessible-text} object}
  @argument[change]{a @sym{gtk:accessible-text-content-change} value for the
    type of change in the contents}
  @argument[start]{an unsigned integer for the start offset, in characters}
  @argument[end]{an unsigned integer for the end offset, in characters}
  @begin{short}
    Notifies assistive technologies of a change in contents.
  @end{short}
  Implementations of the @class{gtk:accessible-text} interface should call this
  function every time their contents change as the result of an operation, like
  an insertion or a removal.
  @begin[Notes]{dictionary}
    If the change is a deletion, this function must be called before removing
    the contents, if it is an insertion, it must be called after inserting the
    new contents.
  @end{dictionary}
  Since 4.14
  @see-class{gtk:accessible-text}"
  (accessible (g:object accessible-text))
  (change accessible-text-content-change)
  (start :uint)
  (end :uint))

(export 'accessible-text-update-contents)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_text_update_selection_bound
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accessible_text_update_selection_bound"
               accessible-text-update-selection-bound) :void
 #+liber-documentation
 "@version{#2024-11-05}
  @argument[accessible]{a @class{gtk:accessible-text} object}
  @begin{short}
    Updates the boundary of the selection.
  @end{short}
  Implementations of the @class{gtk:accessible-text} interface should call this
  function every time the selection has moved, in order to notify assistive
  technologies.

  Since 4.14
  @see-class{gtk:accessible-text}"
  (accessible (g:object accessible-text)))

(export 'accessible-text-update-selection-bound)

;;; --- End of file gtk4.accessible-text.lisp ----------------------------------
