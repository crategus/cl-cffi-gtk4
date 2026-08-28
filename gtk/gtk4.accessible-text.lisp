;;; ----------------------------------------------------------------------------
;;; gtk4.accessible-text.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.22 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2024 - 2026 Dieter Kaiser
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
;;;     gtk_accessible_text_set_caret_position              Since 4.22
;;;     gtk_accessible_text_get_contents
;;;     gtk_accessible_text_get_contents_at
;;;     gtk_accessible_text_get_default_attributes
;;;     gtk_accessible_text_get_extents                     Since 4.16
;;;     gtk_accessible_text_get_offset                      Since 4.16
;;;     gtk_accessible_text_get_selection
;;;     gtk_accessible_text_set_selection                   Since 4.22
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
 "@version{2026-08-26}
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
      @entry[:remove]{Contents change as the result of a remove operation.}
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
 "@version{2026-08-26}
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
    @begin[code]{simple-table}
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
    @end{simple-table}
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
;;; GtkAccessibleText Inferface vtable
;;; ----------------------------------------------------------------------------

(gobject:define-vtable ("GtkAccessibleText" accessible-text)
  (:skip parent-instance (:struct g:type-interface))
  ;; Methods of the GtkAccessibleText interface
  (get-contents ((g:boxed g:bytes)
                 (accessible (g:object accessible-text))
                 (start :uint)
                 (end :uint)))
  (get-contents-at ((g:boxed g:bytes)
                    (accessible (g:object accessible-text))
                    (offset :uint)
                    (granularity accessible-text-granularity)
                    (start (:pointer :uint))
                    (end (:pointer :uint))))
  (get-caret-position (:uint
                       (accessible (g:object accessible-text))))
  (get-selection (:boolean
                  (accessible (g:object accessible-text))
                  (nranges (:pointer :size))
                  (ranges :pointer))) ; array of GtkAccessibleTextRange values
  (get-attributes (:boolean
                   (accessible (g:object accessible-text))
                   (offset :uint)
                   (nranges (:pointer :size))
                   (ranges :pointer) ; array of GtkAccessibleTextRange values
                   (names :pointer) ; array of strings
                   (values :pointer))) ; array of strings
  (get-default-attributes (:void
                           (accessible (g:object accessible-text))
                           (names :pointer) ; array of strings
                           (values :pointer))) ; array of strings
  #+gtk-4-16
  (get-extents (:boolean
                (accessible (g:object accessible-text))
                (start :uint)
                (end :uint)
                (extents (:pointer (:struct graphene:rect-t)))))
  #+gtk-4-16
  (get-offset (:boolean
               (accessible (g:object accessible-text))
               (point (:pointer (:struct graphene:point-t)))
               (offset (:pointer :uint))))
  #+gtk-4-22
  (set-caret-position (:boolean
                       (accessible (g:object accessible-text))
                       (offset :uint)))
  #+gtk-4-22
  (set-selection (:boolean
                  (accessible (g:object accessible-text))
                  (i :size)
                  (range :pointer)))) ; for GtkAccessibleTextRange value

;;; ----------------------------------------------------------------------------
;;; Default virtual methods for the GtkAccessbleText interface
;;; ----------------------------------------------------------------------------

;;; --- gtk:accessible-text-get-contents-impl ----------------------------------

(defmethod accessible-text-get-contents-impl
           ((accessible accessible-text) start end)
  (declare (ignorable accessible start end))
  (warn "gtk:accessible-range-get-contents-impl not implemented for ~a."
        (g:type-name (g:type-from-instance accessible)))
  nil)

;;; --- gtk:accessible-text-get-contents-at-impl -------------------------------

(defmethod accessible-text-get-contents-at-impl
           ((accessible accessible-text) offset granularity start end)
  (declare (ignorable offset granularity start end))
  (warn "gtk:accessible-text-get-contents-at-impl not implemented for ~a."
        (g:type-name (g:type-from-instance accessible)))
  nil)

;;; --- gtk:accessible-text-get-caret-postion-impl -----------------------------

(defmethod accessible-text-get-caret-position-impl
           ((accessible accessible-text))
  (declare (ignorable accessible))
  0)

;;; --- gtk:accessible-text-get-selection-impl ---------------------------------

(defmethod accessible-text-get-selection-impl
           ((accessible accessible-text) nranges ranges)
  (declare (ignorable accessible nranges ranges))
  nil)

;;; --- gtk:accessible-text-get-attributes-impl --------------------------------

(defmethod accessible-text-get-attributes-impl
           ((accessible accessible-text) offset nranges ranges names values)
  (declare (ignorable accessible offset nranges names values))
  nil)

;;; --- gtk:accessible-text-get-default-attributes-impl ------------------------

(defmethod accessible-text-get-default-attributes-impl
           ((accessible accessible-text) names values)
  (declare (ignorable accessible names values))
  nil)

;;; --- gtk:accessible-text-get-extents-impl -----------------------------------

(defmethod accessible-text-get-extents-impl
           ((accessible accessible-text) start end extents)
  (declare (ignorable accessible start end extents))
  nil)

;;; --- gtk:accessible-text-get-offset-impl ------------------------------------

(defmethod accessible-text-get-offset-impl
           ((accessible accessible-text) point offset)
  (declare (ignorable accessible point offset))
  nil)

;;; --- gtk:accessible-text-set-caret-position-impl ----------------------------

(defmethod accessible-text-set-caret-position-impl
           ((accessible accessible-text) offset)
  (declare (ignorable accessible offset))
  nil)

;;; --- gtk:accessible-text-set-selection-impl ---------------------------------

(defmethod accessible-text-set-selection-impl
           ((accessible accessible-text) i range)
  (declare (ignorable accessible i range))
  nil)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_text_get_caret_position
;;; gtk_accessible_text_set_caret_position                  Since 4.22
;;; ----------------------------------------------------------------------------

(defun (setf accessible-text-caret-position) (offset accessible)
  (when (accessible-text-set-caret-position-impl accessible offset)
    offset))

(defun accessible-text-caret-position (accessible)
 #+liber-documentation
 "@version{2026-08-27}
  @syntax{(gtk:accessible-text-caret-position accessible) => offset}
  @syntax{(setf (gtk:accessible-text-caret-position accessible) offset)}
  @argument[accessible]{a @class{gtk:accessible-text} object}
  @argument[offset]{The unsigned integer for the position of the caret, in
    characters.}
  @begin{short}
    Gets or sets the position of the caret inside the accessible object.
  @end{short}
  Since 4.14
  @see-class{gtk:accessible-text}"
  (accessible-text-get-caret-position-impl accessible))

(export 'accessible-text-caret-position)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_text_get_contents
;;; ----------------------------------------------------------------------------

(defun accessible-text-contents (accessible start end)
 #+liber-documentation
 "@version{2026-08-27}
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
  If @arg{end} is @code{G_MAXUINT}, which is the @code{#xffffffff} value, the
  end of the range is the full content of the accessible object.

  Since 4.14
  @see-class{gtk:accessible-text}"
  (accessible-range-get-contents-impl accessible start end))

(export 'accessible-text-contents)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_text_get_contents_at
;;; ----------------------------------------------------------------------------

(defun accessible-text-contents-at (accessible offset granularity)
 #+liber-documentation
 "@version{2026-08-27}
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
    (let ((bytes (accessible-text-get-contents-at-impl accessible
                                                       offset
                                                       granularity
                                                       start
                                                       end)))
      (when bytes
        (values bytes
                (cffi:mem-ref start :int)
                (cffi:mem-ref end :int))))))

(export 'accessible-text-contents-at)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_text_get_attributes
;;; ----------------------------------------------------------------------------

;; TODO: This is a sloppy implementation. Improve it and return the values.

(defun accessible-text-attributes (accessible offset nranges ranges names values)
 #+liber-documentation
 "@version{2026-08-27}
  @argument[accessible]{a @class{gtk:accessible-text} object}
  @argument[offset]{an unsigned integer for the offset, in characters}
  @argument[nranges]{an integer pointer for the number off attributes}
  @argument[ranges]{a pointer for an array with the @code{GtkAccessibleRange}
    instances}
  @argument[names]{a pointer for an array with the names of the attributes
    inside the accessible object}
  @argument[values]{a pointer for a string array for the value of the attributes
    inside the accessible object}
  @return{@em{True} if the accessible object has at least one attribute.}
  @begin{short}
    Retrieves the text attributes inside the accessible object.
  @end{short}
  Each attribute is composed by: a range, a name and a value.

  It is left to the implementation to determine the serialization format of the
  value to a string. GTK provides support for various text attribute names and
  values, but implementations of this interface are free to add their own
  attributes.

  If this function returns @em{true}, @arg{nranges} will be set to a value
  greater than or equal to one, @arg{ranges} will be set to a newly allocated
  array of @code{GtkAccessibleTextRange} instances.

  Since 4.14
  @see-class{gtk:accessbile-text}"
  (accessible-text-get-attributes-impl accessible
                                       offset nranges ranges names values))

(export 'accessible-text-attributes)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_text_get_default_attributes
;;; ----------------------------------------------------------------------------

;; TODO: This is a sloppy implementation. Improve it and return the values.

(defun accessible-text-default-attributes (accessible names values)
 #+liber-documentation
 "@version{2026-08-27}
  @argument[accessible]{a @class{gtk:accessible-text} object}
  @argument[names]{a pointer for an array with the names of the default
    attributes inside the accessible object}
  @argument[values]{a pointer for an array with strings for the values of
    the default attributes inside the accessible object}
  @begin{short}
    Retrieves the default text attributes inside the accessible object.
  @end{short}
  Each attribute is composed by: a name and a a value.

  It is left to the implementation to determine the serialization format of the
  value to a string. GTK provides support for various text attribute names and
  values, but implementations of this interface are free to add their own
  attributes.

  Since 4.14
  @see-class{gtk:accessible-text}"
  (accessible-text-get-default-attributes-impl accessible names values))

(export 'accessible-text-default-attributes)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_text_get_extents                         Since 4.16
;;; ----------------------------------------------------------------------------

#+gtk-4-16
(defun accessible-text-extents (accessible start end extents)
 #+liber-documentation
 "@version{2026-08-27}
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
  (when (accessible-text-get-extents-impl accessible start end extents)
    extents))

#+gtk-4-16
(export 'accessible-text-extents)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_text_get_offset                          Since 4.16
;;; ----------------------------------------------------------------------------

#+gtk-4-16
(defun accessible-text-offset (accessible point)
 #+liber-documentation
 "@version{2026-08-27}
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
    (when (accessible-text-get-offset-impl accessible point offset)
      (cffi:mem-ref offset :uint))))

#+gtk-4-16
(export 'accessible-text-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_text_get_selection
;;; gtk_accessible_text_set_selection                       Since 4.22
;;; ----------------------------------------------------------------------------

;; TODO: This is a sloppy implementation. Improve it and return the values.

#+gtk-4-22
(defun (setf accessible-text-selection) (range accessible nrange)
  (when (accessible-text-set-selection-impl accessible nrange range)
    range))

#+gtk-4-22
(defun accessible-text-selection (accessible nranges ranges)
 #+liber-documentation
 "@version{2026-08-27}
  @argument[accessible]{a @class{gtk:accessible-text} object}
  @argument[nranges]{a pointer for an integer with the number of selection
    ranges}
  @argument[ranges]{an array of @code{GtkAccessibleTextRange} instances}
  @begin{short}
    Gets or sets the selection ranges in the accessible object.
  @end{short}
  If this function returns true, @arg{nranges} will be set to a value greater
  than or equal to one, and @arg{ranges} will be set to a newly allocated array
  of @code{GtkAccessibleTextRange} instances.

  Since 4.14
  @see-class{gtk:accessible-text}"
  (accessible-text-get-selection-impl accessible nranges ranges))

#+gtk-4-22
(export 'accessible-text-selection)

;;; ----------------------------------------------------------------------------
;;; gtk_accessible_text_update_caret_position
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_accessible_text_update_caret_position"
               accessible-text-update-caret-position) :void
 #+liber-documentation
 "@version{2026-08-27}
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
 "@version{2026-08-27}
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
 "@version{2026-08-27}
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
