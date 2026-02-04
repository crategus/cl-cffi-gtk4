;;; ----------------------------------------------------------------------------
;;; gtk4.editable.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2026 Dieter Kaiser
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
;;; GtkEditable
;;;
;;;     Interface for text-editing widgets
;;;
;;; Types and Values
;;;
;;;     GtkEditableProperties                               not implemented
;;;
;;;     GtkEditable
;;;
;;; Accessors
;;;
;;;     gtk_editable_get_editable
;;;     gtk_editable_set_editable
;;;     gtk_editable_get_enable_undo
;;;     gtk_editable_set_enable_undo
;;;     gtk_editable_get_max_width_chars
;;;     gtk_editable_set_max_width_chars
;;;     gtk_editable_get_text
;;;     gtk_editable_set_text
;;;     gtk_editable_get_width_chars
;;;     gtk_editable_set_width_chars
;;;
;;; Functions
;;;
;;;     gtk_editable_get_chars
;;;     gtk_editable_insert_text
;;;     gtk_editable_delete_text
;;;     gtk_editable_get_selection_bounds
;;;     gtk_editable_select_region
;;;     gtk_editable_delete_selection
;;;     gtk_editable_set_position
;;;     gtk_editable_get_position
;;;     gtk_editable_set_alignment
;;;     gtk_editable_get_alignment
;;;
;;;     gtk_editable_install_properties                     not implemented
;;;     gtk_editable_get_delegate                           not implemented
;;;     gtk_editable_init_delegate                          not implemented
;;;     gtk_editable_finish_delegate                        not implemented
;;;
;;;     gtk_editable_delegate_set_property                  not implemented
;;;     gtk_editable_delegate_get_property                  not implemented
;;;
;;;     gtk_editable_delegate_get_accessible_platform_state not implemented
;;;
;;; Properties
;;;
;;;     cursor-position
;;;     editable
;;;     enable-undo
;;;     max-width-chars
;;;     selection-bound
;;;     text
;;;     width-chars
;;;     xalign
;;;
;;; Signals
;;;
;;;     changed
;;;     delete-text
;;;     insert-text
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkEditable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkEditableProperties
;;;
;;; The identifiers for GtkEditable properties.
;;;
;;; See gtk_editable_install_properties() for details on how to implement the
;;; GtkEditable interface.
;;;
;;; GTK_EDITABLE_PROP_TEXT :
;;;     The property id for GtkEditable:text
;;;
;;; GTK_EDITABLE_PROP_CURSOR_POSITION :
;;;     The property id for GtkEditable:cursor-position
;;;
;;; GTK_EDITABLE_PROP_SELECTION_BOUND :
;;;     The property id for GtkEditable:selection-bound
;;;
;;; GTK_EDITABLE_PROP_EDITABLE :
;;;     The property id for GtkEditable:editable
;;;
;;; GTK_EDITABLE_PROP_WIDTH_CHARS :
;;;     The property id for GtkEditable:width-chars
;;;
;;; GTK_EDITABLE_PROP_MAX_WIDTH_CHARS :
;;;     The property id for GtkEditable:max-width-chars
;;;
;;; GTK_EDITABLE_PROP_XALIGN :
;;;     The property id for GtkEditable:xalign
;;;
;;; GTK_EDITABLE_PROP_ENABLE_UNDO :
;;;     The property id for GtkEditable:enable-undo
;;;
;;; GTK_EDITABLE_NUM_PROPERTIES :
;;;     The number of properties.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GtkEditable
;;; ----------------------------------------------------------------------------

(gobject:define-ginterface "GtkEditable" editable
  (:export t
   :type-initializer "gtk_editable_get_type")
  ((cursor-position
    editable-cursor-position
    "cursor-position" "gint" t nil)
   (editable
    editable-editable
    "editable" "gboolean" t t)
   (enable-undo
    editable-enable-undo
    "enable-undo" "gboolean" t t)
   (max-width-chars
    editable-max-width-chars
    "max-width-chars" "gint" t t)
   (selection-bound
    editable-selection-bound
    "selection-bound" "gint" t nil)
   (text
    editable-text
    "text" "gchararray" t t)
   (width-chars
    editable-width-chars
    "width-chars" "gint" t t)
   (xalign
    editable-xalign
    "xalign" "gfloat" t t)))

#+liber-documentation
(setf (liber:alias-for-class 'editable)
      "Interface"
      (documentation 'editable 'type)
 "@version{2025-07-16}
  @begin{short}
    The @class{gtk:editable} interface is an interface which should be
    implemented by text editing widgets, such as the @class{gtk:entry} and
    @class{gtk:spin-button} widgets.
  @end{short}
  It contains functions for generically manipulating an editable widget, a large
  number of action signals used for key bindings, and several signals that an
  application can connect to to modify the behavior of an editable widget.
  @begin[Examples]{dictionary}
    As an example of the latter usage, by connecting the following handler to
    the @sig[gtk:editable]{insert-text} signal, an application can convert all
    entry into an editable widget into uppercase.
    @begin{pre}
;; Handler for the \"insert-text\" signal
(setf handlerid
      (g:signal-connect entry \"insert-text\"
          (lambda (editable text length position)
            (g:signal-handler-block editable handlerid)
            (gtk:editable-insert-text editable
                                      (string-upcase text)
                                      (cffi:mem-ref position :intptr))
            (g:signal-stop-emission editable \"insert-text\")
            (g:signal-handler-unblock editable handlerid))))
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[editable::changed]{signal}
      @begin{pre}
lambda (editable)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[editable]{The @class{gtk:editable} widget that received the
          signal.}
      @end{simple-table}
      The signal is emitted at the end of a single user visible operation on
      the contents of the editable widget. For example, a paste operation that
      replaces the contents of the selection will cause only one signal
      emission, even though it is implemented by first deleting the selection,
      then inserting the new content, and may cause multiple
      @sig[g:object]{notify::text} signals to be emitted.
    @end{signal}
    @begin[editable::delete-text]{signal}
      @begin{pre}
lambda (editable start end)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[editable]{The @class{gtk:editable} widget that received the
          signal.}
        @entry[start]{The integer for the start position.}
        @entry[end]{The integer for the end position.}
      @end{simple-table}
      The signal is emitted when text is deleted from the editable widget by
      the user. The default handler for this signal will normally be responsible
      for deleting the text, so by connecting to this signal and then stopping
      the signal with the @fun{g:signal-stop-emission} function, it is possible
      to modify the range of deleted text, or prevent it from being deleted
      entirely. The @arg{start} and @arg{end} parameters are interpreted as for
      the @fun{gtk:editable-delete-text} function.
    @end{signal}
    @begin[editable::insert-text]{signal}
      @begin{pre}
lambda (editable text length position)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[editable]{The @class{gtk:editable} widget that received the
          signal.}
        @entry[text]{The string for the new text to insert.}
        @entry[length]{The integer for the length of the new text, in bytes,
          or -1 if @arg{text} is null-terminated.}
        @entry[position]{The pointer to an integer for the position, in
          characters, at which to insert the new text. This is an in-out
          parameter. After the signal emission is finished, it should point
          after the newly inserted text. The Lisp value of @arg{position} is
          returned by the @code{(cffi:mem-ref position :intptr)} call.}
      @end{simple-table}
      The signal is emitted when text is inserted into the editable widget by
      the user. The default handler for this signal will normally be responsible
      for inserting the text, so by connecting to this signal and then stopping
      the signal with the @fun{g:signal-stop-emission} function, it is possible
      to modify the inserted text, or prevent it from being inserted entirely.
    @end{signal}
  @end{dictionary}
  @see-class{gtk:entry}
  @see-class{gtk:spin-button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:editable-cursor-position -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "cursor-position" 'editable) t)
 "The @code{cursor-position} property of type @code{:int} (Read) @br{}
  The current position of the insertion cursor in chars. @br{}
  Allowed values: [0, 65535] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'editable-cursor-position)
      "Accessor"
      (documentation 'editable-cursor-position 'function)
 "@version{2025-09-13}
  @syntax{(gtk:editable-cursor-position object) => position}
  @argument[object]{a @class{gtk:editable} widget}
  @argument[position]{an integer for the current cursor position}
  @begin{short}
    The accessor for the @slot[gtk:editable]{cursor-position} slot of the
    @class{gtk:editable} class returns the current position of the insertion
    cursor in chars.
  @end{short}
  @begin[Notes]{dictionary}
    The @slot[gtk:editable]{cursor-position} slot is not writable. Use the
    @fun{gtk:editable-position} function to set the cursor position.
  @end{dictionary}
  @see-class{gtk:editable}
  @see-function{gtk:editable-position}")

;;; --- gtk:editable-editable --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "editable" 'editable) t)
 "The @code{editable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the contents of the editable widget can be edited. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'editable-editable)
      "Accessor"
      (documentation 'editable-editable 'function)
 "@version{2025-08-06}
  @syntax{(gtk:editable-editable object) => setting}
  @syntax{(setf (gtk:editable-editable object) setting)}
  @argument[object]{a @class{gtk:editable} widget}
  @argument[setting]{a boolean whether the user is allowed to edit the text in
    the widget}
  @begin{short}
    The accessor for the @slot[gtk:editable]{editable} slot of the
    @class{gtk:editable} class gets or sets whether the contents of the editable
    widget can be edited.
  @end{short}
  @see-class{gtk:editable}")

;;; --- gtk:editable-enable-undo -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "enable-undo" 'editable) t)
 "The @code{enable-undo} property of type @code{:boolean} (Read / Write) @br{}
  Whether undo/redo should be enabled for the editable widget. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'editable-enable-undo)
      "Accessor"
      (documentation 'editable-enable-undo 'function)
 "@version{2025-08-06}
  @syntax{(gtk:editable-enable-undo object) => setting}
  @syntax{(setf (gtk:editable-enable-undo object) setting)}
  @argument[object]{a @class{gtk:editable} widget}
  @argument[setting]{a boolean whether undo is enabled}
  @begin{short}
    The accessor for the @slot[gtk:editable]{enable-undo} slot of the
    @class{gtk:editable} class gets or sets whether undo/redo actions are
    enabled for the editable widget.
  @end{short}

  If enabled, changes to the editable widget will be saved for undo/redo
  actions. This results in an additional copy of text changes and are not
  stored in secure memory. As such, undo is forcefully disabled when the
  @slot[gtk:text]{visibility} property of the @class{gtk:text} widget is set
  to @em{false}.
  @see-class{gtk:editable}
  @see-class{gtk:text}
  @see-function{gtk:text-visibility}")

;;; --- gtk:editable-max-width-chars -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "max-width-chars" 'editable) t)
 "The @code{max-width-chars} property of type @code{:int} (Read / Write) @br{}
  The desired maximum width of the editable widget, in characters. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'editable-max-width-chars)
      "Accessor"
      (documentation 'editable-max-width-chars 'function)
 "@version{2025-08-06}
  @syntax{(gtk:editable-max-width-chars object) => max}
  @syntax{(setf (gtk:editable-max-width-chars object) max)}
  @argument[object]{a @class{gtk:editable} widget}
  @argument[max]{an integer for the maximum width of the editable widget, in
    characters}
  @begin{short}
    The accessor for the @slot[gtk:editable]{max-width-chars} slot of the
    @class{gtk:editable} class gets or sets the desired maximum width of the
    editable widget, in characters.
  @end{short}
  @see-class{gtk:editable}")

;;; --- gtk:editable-selection-bound -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "selection-bound" 'editable) t)
 "The @code{selection-bound} property of type @code{:int} (Read) @br{}
  The position of the opposite end of the selection from the cursor in chars.
  @br{}
  Allowed values: [0, 65535] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'editable-selection-bound)
      "Accessor"
      (documentation 'editable-selection-bound 'function)
 "@version{2025-08-06}
  @syntax{(gtk:editable-selection-bound object) => end}
  @syntax{(setf (gtk:editable-selection-bound object) end)}
  @argument[object]{a @class{gtk:editable} widget}
  @argument[end]{an integer for the position of the opposite end of the
    selection from the cursor in chars}
  @begin{short}
    The accessor for the @slot[gtk:editable]{selection-bound} slot of the
    @class{gtk:editable} class gets the position of the opposite end of the
    selection from the cursor in chars.
  @end{short}
  @see-class{gtk:editable}")

;;; --- gtk:editable-text ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "text" 'editable) t)
 "The @code{text} property of type @code{:string} (Read / Write) @br{}
  The contents of the editable widget. @br{}
  Default value: \"\"")

#+liber-documentation
(setf (liber:alias-for-function 'editable-text)
      "Accessor"
      (documentation 'editable-text 'function)
 "@version{2025-08-06}
  @syntax{(gtk:editable-text object) => text}
  @syntax{(setf (gtk:editable-text object) text)}
  @argument[object]{a @class{gtk:editable} widget}
  @argument[text]{a string for the contents of the editable widget}
  @begin{short}
    The accessor for the @slot[gtk:editable]{text} slot of the
    @class{gtk:editable} class gets or sets the contents of the editable widget.
  @end{short}
  @see-class{gtk:editable}")

;;; --- gtk:editable-width-chars -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "width-chars" 'editable) t)
 "The @code{width-chars} property of type @code{:int} (Read / Write) @br{}
  The number of characters to leave space for in the editable widget. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'editable-width-chars)
      "Accessor"
      (documentation 'editable-width-chars 'function)
 "@version{2025-08-06}
  @syntax{(gtk:editable-width-chars object) => width}
  @syntax{(setf (gtk:editable-width-chars object) width)}
  @argument[object]{a @class{gtk:editable} widget}
  @argument[width]{an integer for the width in chars}
  @begin{short}
    The accessor for the @slot[gtk:editable]{width-chars} slot of the
    @class{gtk:editable} class gets or sets the value of the width in chars.
  @end{short}

  Note that it changes the size request, the size can still be affected by how
  you pack the widget into containers. If the @arg{width} argument is -1, the
  size reverts to the default size.
  @see-class{gtk:editable}")

;;; --- gtk:editable-xalign ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "xalign" 'editable) t)
 "The @code{xalign} property of type @code{:float} (Read / Write) @br{}
  The horizontal alignment, from 0.0 (left) to 1.0 (right). Reversed for RTL
  layouts. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 0.0")

#+liber-documentation
(setf (liber:alias-for-function 'editable-xalign)
      "Accessor"
      (documentation 'editable-xalign 'function)
 "@version{2025-08-06}
  @syntax{(gtk:editable-xalign object) => align}
  @syntax{(setf (gtk:editable-xalign object) align)}
  @argument[object]{a @class{gtk:editable} widget}
  @argument[align]{a number coerced to a single float for the horizontal
    alignmemnt}
  @begin{short}
    The accessor for the @slot[gtk:editable]{xalign} slot of the
    @class{gtk:editable} class gets or sets the horizontal alignment, from 0.0
    (left) to 1.0 (right).
  @end{short}
  Reversed for RTL layouts.
  @see-class{gtk:editable}")

;;; ----------------------------------------------------------------------------
;;; gtk_editable_get_chars
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_editable_get_chars" %editable-chars) :string
  (editable (g:object editable))
  (start :int)
  (end :int))

(defun editable-chars (editable &key (start 0) (end -1))
 #+liber-documentation
 "@version{2025-07-17}
  @argument[editable]{a @class{gtk:editable} object}
  @argument[start]{an integer for the start position, the default value is 0}
  @argument[end]{an integer for the end position, the default value is -1}
  @return{The string for a sequence of characters from the editable widget.}
  @begin{short}
    Retrieves a sequence of characters.
  @end{short}
  The characters that are retrieved are those characters at positions from
  the @arg{start} position up to, but not including the @arg{end} position. If
  the @arg{end} position is negative, then the characters retrieved are those
  characters from the @arg{start} position to the @arg{end} position of the
  text. Note that positions are specified in characters, not bytes.
  @see-class{gtk:editable}"
  (%editable-chars editable start end))

(export 'editable-chars)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_insert_text
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_editable_insert_text" %editable-insert-text) :void
  (editable (g:object editable))
  (text :string)
  (len :int)
  (pos (:pointer :int)))

(defun editable-insert-text (editable text pos)
 #+liber-documentation
 "@version{2025-07-15}
  @argument[editable]{a @class{gtk:editable} widget}
  @argument[text]{a string for the text to insert}
  @argument[pos]{an integer for the position where @arg{text} is inserted}
  @return{The integer for the position after the newly inserted text.}
  @begin{short}
    Inserts @arg{text} into the contents of the editable widget, at position
    @arg{pos}.
  @end{short}
  Note that the @arg{pos} value is in characters, not in bytes. The function
  returns the position after the newly inserted text.
  @see-class{gtk:editable}"
  (cffi:with-foreign-object (pos1 :int)
    (setf (cffi:mem-ref pos1 :int) pos)
    (%editable-insert-text editable text -1 pos1)
    (cffi:mem-ref pos1 :int)))

(export 'editable-insert-text)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_delete_text
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_editable_delete_text" %editable-delete-text) :void
  (editable (g:object editable))
  (start :int)
  (end :int))

(defun editable-delete-text (editable &key (start 0) (end -1))
 #+liber-documentation
 "@version{2025-05-31}
  @argument[editable]{a @class{gtk:editable} widget}
  @argument[start]{an integer for the start position, the default value is 0}
  @argument[end]{an integer for the end position, the default value is -1}
  @begin{short}
    Deletes a sequence of characters.
  @end{short}
  The characters that are deleted are those characters at positions from the
  @arg{start} position up to, but not including the @arg{end} position. If the
  @arg{end} position is negative, then the characters deleted are those from
  the @arg{start} position to the end of the text. Note that the positions are
  specified in characters, not bytes.
  @see-class{gtk:editable}"
  (%editable-delete-text editable start end))

(export 'editable-delete-text)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_get_selection_bounds
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_editable_get_selection_bounds" %editable-selection-bounds)
    :boolean
  (editable (g:object editable))
  (start (:pointer :int))
  (end (:pointer :int)))

(defun editable-selection-bounds (editable)
 "@version{2025-07-17}
  @syntax{(gtk:editable-selection-bounds editable) => start, end}
  @argument[editable]{a @class{gtk:editable} widget}
  @argument[start]{an integer for the start position}
  @argument[end]{an integer for the end position}
  @begin{short}
    Retrieves the selection bounds of the editable widget.
  @end{short}
  The @arg{start} value will be filled with the start position of the selection
  and the @arg{end} value with the end position. If no text was selected
  @code{nil} will be returned. Note that positions are specified in characters,
  not bytes.
  @see-class{gtk:editable}"
  (cffi:with-foreign-objects ((start :int) (end :int))
    (when (%editable-selection-bounds editable start end)
      (values (cffi:mem-ref start :int)
              (cffi:mem-ref end :int)))))

(export 'editable-selection-bounds)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_select_region
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_editable_select_region" %editable-select-region) :void
  (editable (g:object editable))
  (start :int)
  (end :int))

(defun editable-select-region (editable &key (start 0) (end -1))
 #+liber-documentation
 "@version{2025-05-31}
  @argument[editable]{a @class{gtk:editable} widget}
  @argument[start]{an integer for the start position of the region, the
    default value is 0}
  @argument[end]{an integer for the end position of the region, the default
    value is -1}
  @begin{short}
    Selects a region of text.
  @end{short}
  The characters that are selected are those characters at positions from the
  @arg{start} position up to, but not including the @arg{end} position. If the
  @arg{end} position is negative, then the the characters selected are those
  characters from the @arg{start} position to the end position of the text.
  Note that positions are specified in characters, not bytes.
  @see-class{gtk:editable}"
  (%editable-select-region editable start end))

(export 'editable-select-region)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_delete_selection
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_editable_delete_selection" editable-delete-selection) :void
 #+liber-documentation
 "@version{2025-05-31}
  @argument[editable]{a @class{gtk:editable} widget}
  @begin{short}
    Deletes the currently selected text of the editable widget.
  @end{short}
  This call does not do anything if there is no selected text.
  @see-class{gtk:editable}"
  (editable (g:object editable)))

(export 'editable-delete-selection)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_set_position
;;; gtk_editable_get_position
;;; ----------------------------------------------------------------------------

(defun (setf editable-position) (pos editable)
  (cffi:foreign-funcall "gtk_editable_set_position"
                        (g:object editable) editable
                        :int pos
                        :void)
  pos)

(cffi:defcfun ("gtk_editable_get_position" editable-position) :int
 #+liber-documentation
 "@version{2025-09-13}
  @syntax{(gtk:editable-position editable) => pos}
  @syntax{(setf (gtk:editable-position editable) pos)}
  @argument[editable]{a @class{gtk:editable} widget}
  @argument[pos]{an integer for the position of the cursor, in chars}
  @begin{short}
    Gets or sets the position of the cursor relative to the start of the content
    of the editable widget.
  @end{short}
  The cursor is displayed before the character with the given (base 0) index
  in the contents of the editable widget. The value must be less than or equal
  to the number of characters in the editable widget. A value of -1 indicates
  that the position should be set after the last character of the editable
  widget. Note that @arg{pos} is in characters, not in bytes.
  @begin[Notes]{dictionary}
    The @slot[gtk:editable]{cursor-position} slot is not writable. Use this
    function to set the cursor position of the editable widget.
  @end{dictionary}
  @see-class{gtk:editable}
  @see-function{gtk:editable-cursor-position}"
  (editable (g:object editable)))

(export 'editable-position)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_get_alignment
;;; gtk_editable_set_alignment
;;; ----------------------------------------------------------------------------

(defun (setf editable-alignment) (align editable)
  (setf (editable-xalign editable) align))

(defun editable-alignment (editable)
 #+liber-documentation
 "@version{2025-09-13}
  @syntax{(gtk:editable-alignment editable) => align}
  @syntax{(setf (gtk:editable-alignment editable) align)}
  @argument[editable]{a @class{gtk:editable} widget}
  @argument[align]{a number coerced to a single float for the horizontal
    alignment, from 0.0 (left) to 1.0 (right), reversed for RTL layouts}
  @begin{short}
    Gets or sets the value of the horizontal alignment of the editable widget.
  @end{short}
  This controls the horizontal positioning of the contents when the displayed
  text is shorter than the width of the editable widget.
  @begin[Notes]{dictionary}
    This function is equivalent to the @fun{gtk:editable-xalign} function.
  @end{dictionary}
  @see-class{gtk:editable}
  @see-function{gtk:editable-xalign}"
  (editable-xalign editable))

(export 'editable-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_editable_install_properties ()
;;;
;;; guint
;;; gtk_editable_install_properties (GObjectClass *object_class,
;;;                                  guint first_prop);
;;;
;;; Installs the GtkEditable properties for class .
;;;
;;; This is a helper function that should be called in class_init, after
;;; installing your own properties.
;;;
;;; To handle the properties in your set_property and get_property functions,
;;; you can either use gtk_editable_delegate_set_property() and
;;; gtk_editable_delegate_get_property() (if you are using a delegate), or
;;; remember the first_prop offset and add it to the values in the
;;; GtkEditableProperties enumeration to get the property IDs for these
;;; properties.
;;;
;;; object_class :
;;;     a GObjectClass
;;;
;;; first_prop :
;;;     property ID to use for the first property
;;;
;;; Returns :
;;;     the number of properties that were installed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_editable_get_delegate ()
;;;
;;; GtkEditable *
;;; gtk_editable_get_delegate (GtkEditable *editable);
;;;
;;; Gets the GtkEditable that editable is delegating its implementation to.
;;; Typically, the delegate is a GtkText widget.
;;;
;;; editable :
;;;     a GtkEditable
;;;
;;; Returns :
;;;     the delegate GtkEditable.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_editable_init_delegate ()
;;;
;;; void
;;; gtk_editable_init_delegate (GtkEditable *editable);
;;;
;;; Sets up a delegate for GtkEditable, assuming that the get_delegate vfunc in
;;; the GtkEditable interface has been set up for the editable 's type.
;;;
;;; This is a helper function that should be called in instance init, after
;;; creating the delegate object.
;;;
;;; editable :
;;;     a GtkEditable
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_editable_finish_delegate ()
;;;
;;; void
;;; gtk_editable_finish_delegate (GtkEditable *editable);
;;;
;;; Undoes the setup done by gtk_editable_init_delegate().
;;;
;;; This is a helper function that should be called from dispose, before
;;; removing the delegate object.
;;;
;;; editable :
;;;     a GtkEditable
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_editable_delegate_set_property ()
;;;
;;; gboolean
;;; gtk_editable_delegate_set_property (GObject *object,
;;;                                     guint prop_id,
;;;                                     const GValue *value,
;;;                                     GParamSpec *pspec);
;;;
;;; Sets a property on the GtkEditable delegate for object .
;;;
;;; This is a helper function that should be called in set_property, before
;;; handling your own properties.
;;;
;;; object :
;;;     a GObject
;;;
;;; prop_id :
;;;     a property ID
;;;
;;; value :
;;;     value to set
;;;
;;; pspec :
;;;     the GParamSpec for the property
;;;
;;; Returns :
;;;     TRUE if the property was found
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_editable_delegate_get_property ()
;;;
;;; gboolean
;;; gtk_editable_delegate_get_property (GObject *object,
;;;                                     guint prop_id,
;;;                                     GValue *value,
;;;                                     GParamSpec *pspec);
;;;
;;; Gets a property of the GtkEditable delegate for object .
;;;
;;; This is helper function that should be called in get_property, before
;;; handling your own properties.
;;;
;;; object :
;;;     a GObject
;;;
;;; prop_id :
;;;     a property ID
;;;
;;; value :
;;;     value to set
;;;
;;; pspec :
;;;     the GParamSpec for the property
;;;
;;; Returns :
;;;     TRUE if the property was found
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_editable_delegate_get_accessible_platform_state
;;;
;;; gboolean
;;; gtk_editable_delegate_get_accessible_platform_state (
;;;   GtkEditable* editable,
;;;   GtkAccessiblePlatformState state)
;;;
;;; Retrieves the accessible platform state from the editable delegate.
;;;
;;; This is an helper function to retrieve the accessible state for GtkEditable
;;; interface implementations using a delegate pattern.
;;;
;;; You should call this function in your editable widget implementation of the
;;; Gtk.AccessibleInterface.get_platform_state virtual function, for instance:
;;;
;;; static void
;;; accessible_interface_init (GtkAccessibleInterface *iface)
;;; {
;;;   iface->get_platform_state = your_editable_get_accessible_platform_state;
;;; }
;;;
;;; static gboolean
;;; your_editable_get_accessible_platform_state (
;;;         GtkAccessible *accessible,
;;;         GtkAccessiblePlatformState state)
;;; {
;;;   return gtk_editable_delegate_get_accessible_platform_state (
;;;           GTK_EDITABLE (accessible), state);
;;; }
;;;
;;; state :
;;;     What kind of accessible state to retrieve.
;;;
;;; Return :
;;;     No description available
;;;
;;; Since 4.10
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk4.editable.lisp -----------------------------------------
