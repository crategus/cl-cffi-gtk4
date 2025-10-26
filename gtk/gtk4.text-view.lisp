;;; ----------------------------------------------------------------------------
;;; gtk4.text-view.lisp
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
;;; GtkTextView
;;;
;;;     Widget that displays a GtkTextBuffer
;;;
;;; Types and Values
;;;
;;;     GtkTextView
;;;     GtkTextViewClass
;;;
;;;     GtkTextViewLayer
;;;     GtkTextWindowType
;;;     GtkTextExtendSelection
;;;     GtkWrapMode                                        gtk.enumerations.lisp
;;;
;;;     GtkTextChildAnchor
;;;
;;; Accessors
;;;
;;;     gtk_text_view_set_accepts_tab
;;;     gtk_text_view_get_accepts_tab
;;;     gtk_text_view_set_bottom_margin
;;;     gtk_text_view_get_bottom_margin
;;;     gtk_text_view_set_buffer
;;;     gtk_text_view_get_buffer
;;;     gtk_text_view_set_cursor_visible
;;;     gtk_text_view_get_cursor_visible
;;;     gtk_text_view_set_editable
;;;     gtk_text_view_get_editable
;;;     gtk_text_view_set_extra_menu
;;;     gtk_text_view_get_extra_menu
;;;     gtk_text_view_set_indent
;;;     gtk_text_view_get_indent
;;;     gtk_text_view_set_input_hints
;;;     gtk_text_view_get_input_hints
;;;     gtk_text_view_set_input_purpose
;;;     gtk_text_view_get_input_purpose
;;;     gtk_text_view_set_justification
;;;     gtk_text_view_get_justification
;;;     gtk_text_view_set_left_margin
;;;     gtk_text_view_get_left_margin
;;;     gtk_text_view_set_monospace
;;;     gtk_text_view_get_monospace
;;;     gtk_text_view_set_overwrite
;;;     gtk_text_view_get_overwrite
;;;     gtk_text_view_set_pixels_above_lines
;;;     gtk_text_view_get_pixels_above_lines
;;;     gtk_text_view_set_pixels_below_lines
;;;     gtk_text_view_get_pixels_below_lines
;;;     gtk_text_view_set_pixels_inside_wrap
;;;     gtk_text_view_get_pixels_inside_wrap
;;;     gtk_text_view_set_right_margin
;;;     gtk_text_view_get_right_margin
;;;     gtk_text_view_set_tabs
;;;     gtk_text_view_get_tabs
;;;     gtk_text_view_set_top_margin
;;;     gtk_text_view_get_top_margin
;;;     gtk_text_view_set_wrap_mode
;;;     gtk_text_view_get_wrap_mode
;;;
;;; Functions
;;;
;;;     gtk_text_child_anchor_new
;;;     gtk_text_child_anchor_new_with_replacement         Since 4.6
;;;     gtk_text_child_anchor_get_widgets
;;;     gtk_text_child_anchor_get_deleted
;;;
;;;     gtk_text_view_new
;;;     gtk_text_view_new_with_buffer
;;;     gtk_text_view_scroll_to_mark
;;;     gtk_text_view_scroll_to_iter
;;;     gtk_text_view_scroll_mark_onscreen
;;;     gtk_text_view_move_mark_onscreen
;;;     gtk_text_view_place_cursor_onscreen
;;;     gtk_text_view_get_visible_rect
;;;     gtk_text_view_get_visible_offset                    Since 4.18
;;;     gtk_text_view_get_iter_location
;;;     gtk_text_view_get_cursor_locations
;;;     gtk_text_view_get_line_at_y
;;;     gtk_text_view_get_line_yrange
;;;     gtk_text_view_get_iter_at_location
;;;     gtk_text_view_get_iter_at_position
;;;     gtk_text_view_buffer_to_window_coords
;;;     gtk_text_view_window_to_buffer_coords
;;;     gtk_text_view_forward_display_line
;;;     gtk_text_view_backward_display_line
;;;     gtk_text_view_forward_display_line_end
;;;     gtk_text_view_backward_display_line_start
;;;     gtk_text_view_starts_display_line
;;;     gtk_text_view_move_visually
;;;     gtk_text_view_add_child_at_anchor
;;;     gtk_text_view_remove
;;;     gtk_text_view_get_gutter
;;;     gtk_text_view_set_gutter
;;;     gtk_text_view_add_overlay
;;;     gtk_text_view_move_overlay
;;;     gtk_text_view_reset_cursor_blink
;;;     gtk_text_view_im_context_filter_keypress
;;;     gtk_text_view_reset_im_context
;;;     gtk_text_view_get_ltr_context                      Since 4.4
;;;     gtk_text_view_get_rtl_context                      Since 4.4
;;;
;;; Properties
;;;
;;;     accepts-tab
;;;     bottom-margin
;;;     buffer
;;;     cursor-visible
;;;     editable
;;;     extra-menu
;;;     im-module
;;;     indent
;;;     input-hints
;;;     input-purpose
;;;     justification
;;;     left-margin
;;;     monospace
;;;     overwrite
;;;     pixels-above-lines
;;;     pixels-below-lines
;;;     pixels-inside-wrap
;;;     right-margin
;;;     tabs
;;;     top-margin
;;;     wrap-mode
;;;
;;; Signals
;;;
;;;     backspace
;;;     copy-clipboard
;;;     cut-clipboard
;;;     delete-from-cursor
;;;     extend-selection
;;;     insert-at-cursor
;;;     insert-emoji
;;;     move-cursor
;;;     move-viewport
;;;     paste-clipboard
;;;     preedit-changed
;;;     select-all
;;;     set-anchor
;;;     toggle-cursor-visible
;;;     toggle-overwrite
;;;
;;; Actions
;;;
;;;     menu.popup
;;;     text.redo
;;;     text.undo
;;;     misc.insert-emoji
;;;     selection.select-all
;;;     selection.delete
;;;     clipboard.paste
;;;     clipboard.copy
;;;     clipboard.cut
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ├── GInitiallyUnowned
;;;     │   ╰── GtkWidget
;;;     │       ╰── GtkTextView
;;;     ╰── GtkTextChildAnchor
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkAccessibleText
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkScrollable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTextViewLayer
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkTextViewLayer" text-view-layer
  (:export t
   :type-initializer "gtk_text_view_layer_get_type")
  (:below-text 0)
  (:abvove-text 1))

#+liber-documentation
(setf (liber:alias-for-symbol 'text-view-layer)
      "GEnum"
      (liber:symbol-documentation 'text-view-layer)
 "@version{2025-10-22}
  @begin{declaration}
(gobject:define-genum \"GtkTextViewLayer\" text-view-layer
  (:export t
   :type-initializer \"gtk_text_view_layer_get_type\")
  (:below-text 0)
  (:abvove-text 1))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:below-text]{The layer rendered below the text, but above the
        background.}
      @entry[:above-text]{The layer rendered above the text.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Used to reference the layers of the @class{gtk:text-view} widget for the
    purpose of customized drawing with the @code{::snapshot_layer} virtual
    function.
  @end{short}")

(export 'text-view-layer)

;;; ----------------------------------------------------------------------------
;;; GtkTextWindowType
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkTextWindowType" text-window-type
  (:export t
   :type-initializer "gtk_text_window_type_get_type")
  (:widget 1)
  (:text 2)
  (:left 3)
  (:right 4)
  (:top 5)
  (:bottom 6))

#+liber-documentation
(setf (liber:alias-for-symbol 'text-window-type)
      "GEnum"
      (liber:symbol-documentation 'text-window-type)
 "@version{2025-07-22}
  @begin{declaration}
(gobject:define-genum \"GtkTextWindowType\" text-window-type
  (:export t
   :type-initializer \"gtk_text_window_type_get_type\")
  (:widget 1)
  (:text 2)
  (:left 3)
  (:right 4)
  (:top 5)
  (:bottom 6))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:widget]{Window that floats over scrolling areas.}
      @entry[:text]{Scrollable text window.}
      @entry[:left]{Left side border window.}
      @entry[:right]{Right side border window.}
      @entry[:top]{Top border window.}
      @entry[:bottom]{Bottom border window.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Used to reference the parts of the @class{gtk:text-view} widget.
  @end{short}
  @see-class{gtk:text-view}")

;;; ----------------------------------------------------------------------------
;;; GtkTextExtendSelection
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkTextExtendSelection" text-extend-selection
  (:export t
   :type-initializer "gtk_text_extend_selection_get_type")
  (:word 0)
  (:line 1))

#+liber-documentation
(setf (liber:alias-for-symbol 'text-extend-selection)
      "GEnum"
      (liber:symbol-documentation 'text-extend-selection)
 "@version{2025-07-23}
  @begin{declaration}
(gobject:define-genum \"GtkTextExtendSelection\" text-extend-selection
  (:export t
   :type-initializer \"gtk_text_extend_selection_get_type\")
  (:word 0)
  (:line 1))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:word]{Selects the current word. It is triggered by a double click
        for example.}
      @entry[:line]{Selects the current line. It is triggered by a triple click
        for example.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Granularity types that extend the text selection.
  @end{short}
  Use the @sig[gtk:text-view]{extend-selection} signal to customize the
  selection.
  @see-class{gtk:text-view}")

;;; ----------------------------------------------------------------------------
;;; GtkTextChildAnchor
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkTextChildAnchor" text-child-anchor
  (:superclass g:object
    :export t
    :interfaces nil
    :type-initializer "gtk_text_child_anchor_get_type")
  nil)

#+liber-documentation
(setf (documentation 'text-child-anchor 'type)
 "@version{2024-07-04}
  @begin{short}
    The @class{gtk:text-child-anchor} object is a spot in the text buffer where
    child widgets can be \"anchored\", inserted inline, as if they were
    characters.
  @end{short}
  The anchor can have multiple widgets anchored, to allow for multiple views.
  @see-class{gtk:text-view}
  @see-class{gtk:text-buffer}")

;;; ----------------------------------------------------------------------------
;;; gtk_text_child_anchor_new
;;; ----------------------------------------------------------------------------

;; Note: It is wrong to use MAKE-INSTANCE to create and return a
;; GTK:TEXT-CHILD-ANCHOR object. The replacement character will not be
;; initialized correctly and the code crashes that uses this child anchor.

(cffi:defcfun ("gtk_text_child_anchor_new" text-child-anchor-new)
    (g:object text-child-anchor :return)
 #+liber-documentation
 "@version{2024-10-26}
  @return{The new @class{gtk:text-child-anchor} object.}
  @begin{short}
    Creates a new @class{gtk:text-child-anchor} object.
  @end{short}
  Usually you would then insert it into a text buffer with the
  @fun{gtk:text-buffer-insert-child-anchor} function. To create and insert in
  one step, use the @fun{gtk:text-buffer-create-child-anchor} function.
  @see-class{gtk:text-child-anchor}
  @see-function{gtk:text-buffer-insert-child-anchor}
  @see-function{gtk:text-buffer-create-child-anchor}")

(export 'text-child-anchor-new)

;;; ----------------------------------------------------------------------------
;;; gtk_text_child_anchor_new_with_replacement
;;; ----------------------------------------------------------------------------

#+gtk-4-6
(cffi:defcfun ("gtk_text_child_anchor_new_with_replacement"
               text-child-anchor-new-with-replacement)
    (g:object text-child-anchor :return)
 #+liber-documentation
 "@version{2025-07-27}
  @argument[character]{a string for a replacement character}
  @return{The new @class{gtk:text-child-anchor} object.}
  @begin{short}
    Creates a new @class{gtk:text-child-anchor} object with the given
    replacement character.
  @end{short}
  Usually you would then insert it into a @class{gtk:text-buffer} object with
  the @fun{gtk:text-buffer-insert-child-anchor} function.

  Since 4.6
  @see-class{gtk:text-child-anchor}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-buffer-insert-child-anchor}"
  (char :string))

#+gtk-4-6
(export 'text-child-anchor-new-with-replacement)

;;; ----------------------------------------------------------------------------
;;; gtk_text_child_anchor_get_widgets
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_child_anchor_get_widgets" %text-child-anchor-widgets)
    :pointer
  (anchor (g:object text-child-anchor))
  (len (:pointer :uint)))

(defun text-child-anchor-widgets (anchor)
 #+liber-documentation
 "@version{2024-07-04}
  @argument[anchor]{a @class{gtk:text-child-anchor} object}
  @return{List of @class{gtk:widget} objects anchored at @arg{anchor}.}
  @begin{short}
    Gets a list of all widgets anchored at the anchor.
  @end{short}
  @see-class{gtk:text-child-anchor}
  @see-class{gtk:widget}"
  (cffi:with-foreign-objects ((len :uint))
    (let ((ptr (%text-child-anchor-widgets anchor len)))
      (unwind-protect
        (iter (for n from 0 below (cffi:mem-ref len :uint))
              (for widget = (cffi:mem-aref ptr '(g:object widget) n))
              (collect widget))
        (glib:free ptr)))))

(export 'text-child-anchor-widgets)

;;; ----------------------------------------------------------------------------
;;; gtk_text_child_anchor_get_deleted
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_child_anchor_get_deleted" text-child-anchor-deleted)
    :boolean
 #+liber-documentation
 "@version{2024-07-04}
  @argument[anchor]{a @class{gtk:text-child-anchor} object}
  @return{@em{True} if the anchor has been deleted from its text buffer.}
  @begin{short}
    Determines whether a anchor has been deleted from the text buffer.
  @end{short}
  Keep in mind that the anchor will be unreferenced when removed from the text
  buffer, so you need to hold your own reference if you plan to use this
  function - otherwise all deleted anchors will also be finalized.
  @see-class{gtk:text-child-anchor}"
  (anchor (g:object text-child-anchor)))

(export 'text-child-anchor-deleted)

;;; ----------------------------------------------------------------------------
;;; GtkTextViewClass
;;; ----------------------------------------------------------------------------

(cffi:defcstruct text-view-class
  ;; Parent class
  (:parent-class (:struct gtk:widget-class))
  ;; Virtual functions
  (move-cursor :pointer)
  (set-anchor :pointer)
  (insert-at-cursor :pointer)
  (delete-from-cursor :pointer)
  (backspace :pointer)
  (cut-clipboard :pointer)
  (copy-clipboard :pointer)
  (paste-clipboard :pointer)
  (toggle-overwrite :pointer)
  (create-buffer :pointer)
  (snapshot-layer :pointer)
  (extend-selection :pointer)
  (insert-emoji :pointer)
  ;; Private
  (padding :pointer :count 8))

#+liber-documentation
(setf (liber:alias-for-symbol 'text-view-class)
      "CStruct"
      (liber:symbol-documentation 'text-view-class)
 "@version{2025-10-26}
  @begin{declaration}
(cffi:defcstruct text-view-class
  ;; Parent class
  (:parent-class (:struct gtk:widget-class))
  ;; Virtual functions
  (move-cursor :pointer)
  (set-anchor :pointer)
  (insert-at-cursor :pointer)
  (delete-from-cursor :pointer)
  (backspace :pointer)
  (cut-clipboard :pointer)
  (copy-clipboard :pointer)
  (paste-clipboard :pointer)
  (toggle-overwrite :pointer)
  (create-buffer :pointer)
  (snapshot-layer :pointer)
  (extend-selection :pointer)
  (insert-emoji :pointer)
  ;; Private
  (padding :pointer :count 8))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[parent-class]{The object class structure needs to be the first.}
      @entry[move-cursor]{The class handler for the
        @sig[gtk:text-view]{move-cursor} keybinding signal.}
      @entry[set-anchor]{The class handler for the
        @sig[gtk:text-view]{set-anchor} keybinding signal.}
      @entry[insert-at-cursor]{The class handler for the
        @sig[gtk:text-view]{insert-at-cursor} keybinding signal.}
      @entry[delete-from-cursor]{The class handler for the
        @sig[gtk:text-view]{delete-from-cursor} keybinding signal.}
      @entry[backspace]{The class handler for the
        @sig[gtk:text-view]{backspace} keybinding signal.}
      @entry[cut-clipboard]{The class handler for the
        @sig[gtk:text-view]{cut-clipboard} keybinding signal.}
      @entry[copy-clipboard]{The class handler for the
        @sig[gtk:text-view]{copy-clipboard} keybinding signal.}
      @entry[past-clipboard]{The class handler for the
        @sig[gtk:text-view]{paste-clipboard} keybinding signal.}
      @entry[toggle-overwrite]{The class handler for the
        @sig[gtk:text-view]{toggle-overwrite} keybinding signal.}
      @entry[create-buffer]{This virtual function is called to create the
        @class{gtk:text-buffer} object for the text view. The default
        implementation is to just call the @fun{gtk:text-buffer-new} function.}
      @entry[snapshot-layer]{This virtual function is called before and after
        the text view is drawing its own text. Applications can override this
        virtual function in a subclass to draw customized content underneath or
        above the text. In the @val[gtk:text-view-layer]{:below-text} and
        @val[gtk:text-view-layer]{:above-text} layers the drawing is done in the
        buffer coordinate space.}
      @entry[extend-selection]{The class handler for the
        @sig[gtk:text-view]{extend-selection} signal.}
      @entry[insert-emoji]{The class handler for the
        @sig[gtk:text-view]{insert-emoji} signal.}
    @end{simple-table}
  @end{values}
  @begin{short}
    The class structure for the @class{gtk:text-view} class.
  @end{short}
  @see-class{gtk:text-view}
  @see-symbol{gtk:text-view-layer}")

(export 'text-view-class)

;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (liber:alias-for-symbol 'text-view-vtable)
      "VTable"
      (liber:symbol-documentation 'text-view-vtable)
 "@version{2025-10-26}
  @begin{declaration}
(gobject:define-vtable (\"GtkTextView\" text-view)
  ;; Parent class
  (:skip parent-instance (:struct gtk:widget-class))
  ;; Virtual functions
  (move-cursor        (:void
                       (textview (g:object gtk:text-view))
                       (step gtk:movement-step)
                       (count :int)
                       (extend-selection :boolean)))
  (set-anchor         (:void (textview (g:object gtk:text-view))))
  (insert-at-cursor   (:void
                       (textview (g:object gtk:text-view))
                       (str :string)))
  (delete-from-cursor (:void
                       (textview (g:object gtk:text-view))
                       (type gtk:delete-type)
                       (count :int)))
  (backspace          (:void (textview (g:object gtk:text-view))))
  (cut-clipboard      (:void (textview (g:object gtk:text-view))))
  (copy-clipboard     (:void (textview (g:object gtk:text-view))))
  (paste-clipboard    (:void (textview (g:object gtk:text-view))))
  (toggle-overwrite   (:void (textview (g:object gtk:text-view))))
  (create-buffer     ((g:object gtk:text-buffer)
                      (textview (g:object gtk:text-view))))
  (snapshot-layer    (:void
                      (textview (g:object gtk:text-view))
                      (layer gtk:text-view-layer)
                      (snapshot (g:object gtk:snapshot))))
  (extend-selection  (:boolean
                      (textview (g:object gtk:text-view))
                      (granularity gtk:text-extend-selection)
                      (location (g:boxed gtk:iter))
                      (start (g:boxed gtk:iter))
                      (end (g:boxed gtk:iter))))
  (insert-emoji      (:void (text-view (g:object gtk:text-view)))))
  @end{declaration}
  @begin{short}
    Virtual function table for a widget that is subclassed from the
    @class{gtk:text-view} class.
  @end{short}
  @see-class{gtk:text-view}
  @see-symbol{gtk:text-view-class}
  @see-symbol{gtk:widget-class}")

(export 'text-view-vtable)

;;; ----------------------------------------------------------------------------
;;; GtkTextView
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkTextView" text-view
  (:superclass widget
   :export t
   :interfaces ("GtkAccessibleText"
                "GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkScrollable")
   :type-initializer "gtk_text_view_get_type")
  ((accepts-tab
    text-view-accepts-tab
    "accepts-tab" "gboolean" t t)
   (bottom-margin
    text-view-bottom-margin
    "bottom-margin" "gint" t t)
   (buffer
    text-view-buffer
    "buffer" "GtkTextBuffer" t t)
   (cursor-visible
    text-view-cursor-visible
    "cursor-visible" "gboolean" t t)
   (editable
    text-view-editable
    "editable" "gboolean" t t)
   (extra-menu
    text-view-extra-menu
    "extra-menu" "GMenuModel" t t)
   (im-module
    text-view-im-module
    "im-module" "gchararray" t t)
   (indent
    text-view-indent
    "indent" "gint" t t)
   (input-hints
    text-view-input-hints
    "input-hints" "GtkInputHints" t t)
   (input-purpose
    text-view-input-purpose
    "input-purpose" "GtkInputPurpose" t t)
   (justification
    text-view-justification
    "justification" "GtkJustification" t t)
   (left-margin
    text-view-left-margin
    "left-margin" "gint" t t)
   (monospace
    text-view-monospace
    "monospace" "gboolean" t t)
   (overwrite
    text-view-overwrite
    "overwrite" "gboolean" t t)
   (pixels-above-lines
    text-view-pixels-above-lines
    "pixels-above-lines" "gint" t t)
   (pixels-below-lines
    text-view-pixels-below-lines
    "pixels-below-lines" "gint" t t)
   (pixels-inside-wrap
    text-view-pixels-inside-wrap
    "pixels-inside-wrap" "gint" t t)
   (right-margin
    text-view-right-margin
    "right-margin" "gint" t t)
   (tabs
    text-view-tabs
    "tabs" "PangoTabArray" t t)
   (top-margin
    text-view-top-margin
    "top-margin" "gint" t t)
   (wrap-mode
    text-view-wrap-mode
    "wrap-mode" "GtkWrapMode" t t)))

#+liber-documentation
(setf (documentation 'text-view 'type)
 "@version{2025-07-31}
  @begin{short}
    GTK has a powerful framework for multiline text editing.
  @end{short}

  @image[multiline-text]{Figure: GtkTextView}

  The primary objects involved in the process are the @class{gtk:text-buffer}
  object, which represents the text being edited, and the @class{gtk:text-view}
  widget, a widget that can display a @class{gtk:text-buffer} object. Each text
  buffer can be displayed by any number of views.
  @begin[CSS nodes]{dictionary}
    @begin{pre}
textview.view
├── border.top
├── border.left
├── text
│   ╰── [selection]
├── border.right
├── border.bottom
╰── [window.popup]
    @end{pre}
    The @class{gtk:text-view} implementation has a main CSS node with name
    @code{textview} and @code{.view} style class, and subnodes for each of the
    border windows, and the main text area, with names @code{border} and
    @code{text}, respectively. The border nodes each get one of the
    @code{.left}, @code{.right}, @code{.top} or @code{.bottom} style classes.

    A node representing the selection will appear below the text node. If a
    context menu is opened, the window node will appear as a subnode of the
    main node.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:text-view} implmementation uses the
    @val[gtk:accessible-role]{:text-box} role of the @sym{gtk:accessible-role}
    enumeration.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[text-view::backspace]{signal}
      @begin{pre}
lambda (view)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[view]{The @class{gtk:text-view} widget that received the signal.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted when the user asks
      for it. The default bindings for this signal are the @kbd{Backspace} and
      @kbd{Shift-Backspace} keys.
    @end{signal}
    @begin[text-view::copy-clipboard]{signal}
      @begin{pre}
lambda (view)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[view]{The @class{gtk:text-view} widget that received the signal.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted to copy the
      selection to the clipboard. The default bindings for this signal are the
      @kbd{Ctrl-c} and @kbd{Ctrl-Insert} keys.
    @end{signal}
    @begin[text-view::cut-clipboard]{signal}
      @begin{pre}
lambda (view)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[view]{The @class{gtk:text-view} widget that received the signal.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted to cut the selection
      to the clipboard. The default bindings for this signal are the
      @kbd{Ctrl-x} and @kbd{Shift-Delete} keys.
    @end{signal}
    @begin[text-view::delete-from-cursor]{signal}
      @begin{pre}
lambda (view granularity count)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[view]{The @class{gtk:text-view} widget that received the signal.}
        @entry[granularity]{The granularity of the deletion as a value of the
          @sym{gtk:delete-type} enumeration.}
        @entry[count]{The integer for the number of type units to delete.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted when the user
      initiates a text deletion. If the granularity is @code{:chars}, GTK
      deletes the selection if there is one, otherwise it deletes the requested
      number of characters. The default bindings for this signal are the
      @kbd{Delete} key for deleting a character, the @kbd{Ctrl-Delete} key for
      deleting a word and the @kbd{Ctrl-Backspace} key for deleting a word
      backwords.
    @end{signal}
    @begin[text-view::extend-selection]{signal}
      @begin{pre}
lambda (view granularity location start end)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[view]{The @class{gtk:text-view} widget that received the signal.}
        @entry[granularity]{The granularity as a value of the
          @sym{gtk:text-extend-selection} enumeration.}
        @entry[location]{The @class{gtk:text-iter} iterator where to extend the
          selection.}
        @entry[start]{The @class{gtk:text-iter} iterator where the selection
          should start.}
        @entry[end]{The @class{gtk:text-iter} iterator where the selection
          should end.}
        @entry[Returns]{@em{True} to stop other handlers from being invoked for
          the event, @em{false} to propagate the event further.}
      @end{simple-table}
      The signal is emitted when the selection needs to be extended at the
      @arg{location} iterator.
    @end{signal}
    @begin[text-view::insert-at-cursor]{signal}
      @begin{pre}
lambda (view text)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[view]{The @class{gtk:text-view} widget that received the signal.}
        @entry[text]{The string for the text to insert.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted when the user
      initiates the insertion of text at the cursor. The signal has no default
      bindings.
    @end{signal}
    @begin[text-view::insert-emoji]{signal}
      @begin{pre}
lambda (view)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[view]{The @class{gtk:text-view} widget that received the signal.}
      @end{simple-table}
      This signal is a keybinding signal which gets emitted to present the Emoji
      chooser for the text view. The default bindings for this signal are
      @code{Ctrl-.} and @code{Ctrl-;}.
    @end{signal}
    @begin[text-view::move-cursor]{signal}
      @begin{pre}
lambda (view step count extend)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[view]{The @class{gtk:text-view} widget that received the signal.}
        @entry[step]{The granularity of the move as a value of the
          @sym{gtk:movement-step} enumeration.}
        @entry[count]{The integer for the number of step units to move.}
        @entry[extend]{@em{True} if the move should extend the selection.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted when the user
      initiates a cursor movement. If the cursor is not visible in the text
      view, this signal causes the viewport to be moved instead. Applications
      should not connect to it, but may emit it with the @fun{g:signal-emit}
      function if they need to control the cursor programmatically. The default
      bindings for this signal come in two variants, the variant with the
      @kbd{Shift} modifier extends the selection, the variant without the
      @kbd{Shift} modifer does not. There are too many key combinations to list
      them all here. Arrow keys move by individual characters/lines. The
      @kbd{Ctrl}-arrow key combinations move by words/paragraphs. The
      @kbd{Home}/@kbd{End} keys move to the ends of the text buffer. The
      @kbd{PageUp}/@kbd{PageDown} keys move vertically by pages. The
      @kbd{Ctrl-PageUp}/@kbd{PageDown} keys move horizontally by pages.
    @end{signal}
    @begin[text-view::move-viewport]{signal}
      @begin{pre}
lambda (view step count)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[view]{The @class{gtk:text-view} widget that received the signal.}
        @entry[step]{The granularity of the move as a value of the
          @sym{gtk:movement-step} enumeration.}
        @entry[count]{The integer for the number of step units to move.}
      @end{simple-table}
      The signal is a keybinding signal which can be bound to key combinations
      to allow the user to move the viewport, that is, to change what part of
      the text view is visible in a scrolled window. There are no default
      bindings for this signal.
    @end{signal}
    @begin[text-view::paste-clipboard]{signal}
      @begin{pre}
lambda (view)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[view]{The @class{gtk:text-view} widget that received the signal.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted to paste the
      contents of the clipboard into the text view. The default bindings for
      this signal are the @kbd{Ctrl-v} and @code{Shift-Insert} keys.
    @end{signal}
    @begin[text-view::preedit-changed]{signal}
      @begin{pre}
lambda (view preedit)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[view]{The @class{gtk:text-view} object that received the signal.}
        @entry[preedit]{The string for the current preedit text.}
      @end{simple-table}
      If an input method is used, the typed text will not immediately be
      committed to the text buffer. So if you are interested in the text,
      connect to this signal. The signal is only emitted if the text at the
      given position is actually editable.
    @end{signal}
    @begin[text-view::select-all]{signal}
      @begin{pre}
lambda (view select)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[view]{The @class{gtk:text-view} widget that received the signal.}
        @entry[select]{@em{True} to select, @em{false} to unselect.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted to select or unselect
      the complete contents of the text view. The default bindings for the
      signal are the @kbd{Ctrl-a} and @kbd{Ctrl-/} keys for selecting and the
      @kbd{Shift-Ctrl-a} and @kbd{Ctrl-\} keys for unselecting.
    @end{signal}
    @begin[text-view::set-anchor]{signal}
      @begin{pre}
lambda (view)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[view]{The @class{gtk:text-view} widget that received the signal.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted when the user
      initiates setting the \"anchor\" mark. The \"anchor\" mark gets placed at
      the same position as the \"insert\" mark. The signal has no default
      bindings.
    @end{signal}
    @begin[text-view::toggle-cursor-visible]{signal}
      @begin{pre}
lambda (view)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[view]{The @class{gtk:text-view} widget that received the signal.}
      @end{simple-table}
      This  signal is a keybinding signal which gets emitted to toggle the
      visibility of the cursor. The default binding for this signal is @kbd{F7}.
    @end{signal}
    @begin[text-view::toggle-overwrite]{signal}
      @begin{pre}
lambda (view)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[view]{The @class{gtk:text-view} widget that received the signal.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted to toggle the
      overwrite mode of the text view. The default bindings for this signal is
      the @kbd{Insert} key.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:text-view-new}
  @see-constructor{gtk:text-view-new-with-buffer}
  @see-slot{gtk:text-view-accepts-tab}
  @see-slot{gtk:text-view-bottom-margin}
  @see-slot{gtk:text-view-buffer}
  @see-slot{gtk:text-view-cursor-visible}
  @see-slot{gtk:text-view-editable}
  @see-slot{gtk:text-view-extra-menu}
  @see-slot{gtk:text-view-im-module}
  @see-slot{gtk:text-view-indent}
  @see-slot{gtk:text-view-input-hints}
  @see-slot{gtk:text-view-input-purpose}
  @see-slot{gtk:text-view-justification}
  @see-slot{gtk:text-view-left-margin}
  @see-slot{gtk:text-view-monospace}
  @see-slot{gtk:text-view-overwrite}
  @see-slot{gtk:text-view-pixels-above-lines}
  @see-slot{gtk:text-view-pixels-below-lines}
  @see-slot{gtk:text-view-pixels-inside-wrap}
  @see-slot{gtk:text-view-right-margin}
  @see-slot{gtk:text-view-tabs}
  @see-slot{gtk:text-view-top-margin}
  @see-slot{gtk:text-view-wrap-mode}
  @see-class{gtk:text-buffer}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:text-view-accepts-tab ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accepts-tab" 'text-view) t)
 "The @code{accepts-tab} property of type @code{:boolean} (Read / Write) @br{}
  Whether the @kbd{Tab} key will result in a tab character being entered. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'text-view-accepts-tab)
      "Accessor"
      (documentation 'text-view-accepts-tab 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-view-accepts-tab object) => accepts}
  @syntax{(setf (gtk:text-view-accepts-tab object) accepts)}
  @argument[object]{a @class{gtk:text-view} widget}
  @argument[accepts]{@em{true} if pressing the @kbd{Tab} key should insert a
    tab character, @em{false}, if pressing the @kbd{Tab} key should move the
    keyboard focus}
  @begin{short}
    The accessor for the @slot[gtk:text-view]{accepts-tab} slot of the
    @class{gtk:text-view} class gets or sets the behavior of the text view when
    the @kbd{Tab} key is pressed.
  @end{short}

  If the @arg{accepts} argument is @em{true}, a tab character is inserted. If
  the @arg{accepts} argument is @em{false} the keyboard focus is moved to the
  next widget in the focus chain.
  @see-class{gtk:text-view}")

;;; --- gtk:text-view-bottom-margin --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "bottom-margin" 'text-view) t)
 "The @code{bottom-margin} property of type @code{:int} (Read / Write) @br{}
  The bottom margin for text in the text view. Note that this property is
  confusingly named. In CSS terms, the value set here is padding, and it is
  applied in addition to the padding from the theme. Do not confuse this
  property with the @slot[gtk:widget]{margin-bottom} property. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'text-view-bottom-margin)
      "Accessor"
      (documentation 'text-view-bottom-margin 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-view-bottom-margin object) => margin}
  @syntax{(setf (gtk:text-view-bottom-margin object) margin)}
  @argument[object]{a @class{gtk:text-view} widget}
  @argument[margin]{an integer for the bottom margin in pixels}
  @begin{short}
    The accessor for the @slot[gtk:text-view]{bottom-margin} slot of the
    @class{gtk:text-view} class gets or sets the bottom margin for text in the
    text view.
  @end{short}

  Note that this function is confusingly named. In CSS terms, the value set
  here is padding.
  @see-class{gtk:text-view}")

;;; --- gtk:text-view-buffer ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "buffer" 'text-view) t)
 "The @code{buffer} property of type  @class{gtk:text-buffer} (Read / Write)
  @br{}
  The text buffer which is displayed.")

#+liber-documentation
(setf (liber:alias-for-function 'text-view-buffer)
      "Accessor"
      (documentation 'text-view-buffer 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-view-buffer object) => buffer}
  @syntax{(setf (gtk:text-view-buffer object) buffer)}
  @argument[object]{a @class{gtk:text-view} widget}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @begin{short}
    The accessor for the @slot[gtk:text-view]{buffer} slot of the
    @class{gtk:text-view} class gets or sets the text buffer being displayed by
    the text view.
  @end{short}
  @see-class{gtk:text-view}
  @see-class{gtk:text-buffer}")

;;; --- gtk:text-view-cursor-visible -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "cursor-visible" 'text-view) t)
 "The @code{cursor-visible} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the insertion cursor is shown. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'text-view-cursor-visible)
      "Accessor"
      (documentation 'text-view-cursor-visible 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-view-cursor-visible object) => setting}
  @syntax{(setf (gtk:text-view-cursor-visible object) setting)}
  @argument[object]{a @class{gtk:text-view} widget}
  @argument[setting]{a boolean whether to show the insertion cursor}
  @begin{short}
    The accessor for the @slot[gtk:text-view]{cursor-visible} slot of the
    @class{gtk:text-view} class gets or sets whether the insertion mark is
    visible.
  @end{short}

  A text buffer with no editable text probably should not have a visible cursor,
  so you may want to turn the cursor off.
  @see-class{gtk:text-view}")

;;; --- gtk:text-view-editable -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "editable" 'text-view) t)
 "The @code{editable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the text can be modified by the user. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'text-view-editable)
      "Accessor"
      (documentation 'text-view-editable 'function)
 "@version{2024-09-28}
  @syntax{(gtk:text-view-editable object) => setting}
  @syntax{(setf (gtk:text-view-editable object) setting)}
  @argument[object]{a @class{gtk:text-view} widget}
  @argument[setting]{a boolean whether the text view is editable}
  @begin{short}
    The accessor for the @slot[gtk:text-view]{editable} slot of the
    @class{gtk:text-view} class gets or sets the default editability of the text
    view.
  @end{short}

  You can override this default setting with tags in the text buffer, using the
  @slot[gtk:text-tag]{editable} attribute of tags.
  @see-class{gtk:text-view}
  @see-function{gtk:text-tag-editable}")

;;; --- gtk:text-view-extra-menu -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "extra-menu" 'text-view) t)
 "The @code{extra-menu} property of type @class{g:menu-model} (Read / Write)
  @br{}
  The menu model to append to the context menu.")

#+liber-documentation
(setf (liber:alias-for-function 'text-view-extra-menu)
      "Accessor"
      (documentation 'text-view-extra-menu 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-view-extra-menu object) => menu}
  @syntax{(setf (gtk:text-view-extra-menu object) menu)}
  @argument[object]{a @class{gtk:text-view} widget}
  @argument[menu]{a @class{g:menu-model} object}
  @begin{short}
    The accessor for the @slot[gtk:text-view]{extra-menu} slot of the
     @class{gtk:text-view} class gets or sets a menu model to add when
     constructing the context menu for the text view.
  @end{short}
  You can pass @code{nil} to remove a previously set extra menu.
  @see-class{gtk:text-view}
  @see-class{g:menu-model}")

;;; --- gtk:text-view-im-module ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "im-module" 'text-view) t)
 "The @code{im-module} property of type @code{:string} (Read / Write) @br{}
  The IM (input method) module that should be used for the text view. See the
  @class{gtk:im-context} documentation. Setting this to a non-@code{nil} value
  overrides the system-wide IM module setting. See the
  @slot[gtk:settings]{gtk-im-module} setting. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'text-view-im-module)
      "Accessor"
      (documentation 'text-view-im-module 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-view-im-module object) => module}
  @syntax{(setf (gtk:text-view-im-module object) module)}
  @argument[object]{a @class{gtk:text-view} widget}
  @argument[module]{a string for the IM module to use for the text view}
  @begin{short}
    The accessor for the @slot[gtk:text-view]{im-module} slot of the
    @class{gtk:text-view} class gets or sets the IM (input method) module that
    should be used for the text view.
  @end{short}
  See the @class{gtk:im-context} class. Setting this to a non-@code{nil} value
  overrides the system-wide IM module setting. See the
  @slot[gtk:settings]{gtk-im-module} setting.
  @see-class{gtk:text-view}
  @see-class{gtk:im-context}
  @see-function{gtk:settings-gtk-im-module}")

;;; --- gtk:text-view-indent ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "indent" 'text-view) t)
 "The @code{indent} property of type @code{:int} (Read / Write) @br{}
  The amount to indent the paragraph, in pixels. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'text-view-indent)
      "Accessor"
      (documentation 'text-view-indent 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-view-indent object) => indent}
  @syntax{(setf (gtk:text-view-indent object) indent)}
  @argument[object]{a @class{gtk:text-view} widget}
  @argument[indent]{an integer for the indentation in pixels}
  @begin{short}
    The accessor for the @slot[gtk:text-view]{indent} slot of the
    @class{gtk:text-view} class gets or sets the default indentation of
    paragraphs in the text view.
  @end{short}
  Tags in the text buffer of the text view may override the default. The
  indentation may be negative.
  @see-class{gtk:text-view}
  @see-class{gtk:text-tag}")

;;; --- gtk:text-view-input-hints ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "input-hints" 'text-view) t)
 "The @code{input-hints} property of type @sym{gtk:input-hints} (Read / Write)
  @br{}
  The additional hints, beyond the @slot[gtk:text-view]{input-purpose} property,
  that allow input methods to fine-tune their behaviour.")

#+liber-documentation
(setf (liber:alias-for-function 'text-view-input-hints)
      "Accessor"
      (documentation 'text-view-input-hints 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-view-input-hints object) => hints}
  @syntax{(setf (gtk:text-view-input-hints object) hints)}
  @argument[object]{a @class{gtk:text-view} widget}
  @argument[hints]{a @sym{gtk:input-hints} value for the hints}
  @begin{short}
    The accessor for the @slot[gtk:text-view]{input-hints} slot of the
    @class{gtk:text-view} class gets or sets the additional hints, beyond the
    @slot[gtk:text-view]{input-purpose} property, that allow input methods to
    fine-tune their behaviour.
  @end{short}
  @see-class{gtk:text-view}
  @see-symbol{gtk:input-hints}")

;;; --- gtk:text-view-input-purpose --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "input-purpose" 'text-view) t)
 "The @code{input-purpose} property of type @sym{gtk:input-purpose}
  (Read / Write) @br{}
  The purpose of the text view. This property can be used by on-screen
  keyboards and other input methods to adjust their behaviour. @br{}
  Default value: @val[gtk:input-purpose]{:free-form}")

#+liber-documentation
(setf (liber:alias-for-function 'text-view-input-purpose)
      "Accessor"
      (documentation 'text-view-input-purpose 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-view-input-purpose object) => purpose}
  @syntax{(setf (gtk:text-view-input-purpose object) purpose)}
  @argument[object]{a @class{gtk:text-view} widget}
  @argument[purpose]{a @sym{gtk:input-purpose} value for the purpose}
  @begin{short}
    The accessor for the @slot[gtk:text-view]{input-purpose} slot of the
    @class{gtk:text-view} class gets or sets the purpose of the text view,
    which can be used by on-screen keyboards and other input methods to adjust
    their behaviour.
  @end{short}
  @see-class{gtk:text-view}
  @see-symbol{gtk:input-purpose}")

;;; --- gtk:text-view-justification --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "justification" 'text-view) t)
 "The @code{justification} property of type @sym{gtk:justification}
  (Read / Write) @br{}
  The left, right, or center justification. @br{}
  Default value: @val[gtk:justification]{:left}")

#+liber-documentation
(setf (liber:alias-for-function 'text-view-justification)
      "Accessor"
      (documentation 'text-view-justification 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-view-justification object) => justification}
  @syntax{(setf (gtk:text-view-justification object) justification)}
  @argument[object]{a @class{gtk:text-view} widget}
  @argument[justification]{a value of the @sym{gtk:justification} enumeration}
  @begin{short}
    The accessor for the @slot[gtk:text-view]{justification} slot of the
    @class{gtk:text-view} class gets or sets the default justification of
    paragraphs in the text view.
  @end{short}
  Tags in the text buffer may override the default.
  @see-class{gtk:text-view}
  @see-class{gtk:text-tag}
  @see-symbol{gtk:justification}")

;;; --- gtk:text-view-left-margin ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "left-margin" 'text-view) t)
 "The @code{left-margin} property of type @code{:int} (Read / Write) @br{}
  The width of the left margin in pixels. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'text-view-left-margin)
      "Accessor"
      (documentation 'text-view-left-margin 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-view-left-margin object) => margin}
  @syntax{(setf (gtk:text-view-left-margin object) margin)}
  @argument[object]{a @class{gtk:text-view} widget}
  @argument[margin]{an integer for the left margin in pixels}
  @begin{short}
    The accessor for the @slot[gtk:text-view]{left-margin} slot of the
    @class{gtk:text-view} class gets or sets the default left margin size of
    paragraphs in the text view.
  @end{short}
  Tags in the text buffer may override the default.
  @see-class{gtk:text-view}
  @see-class{gtk:text-tag}")

;;; --- gtk:text-view-monospace ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "monospace" 'text-view) t)
 "The @code{monospace} property of type @code{:boolean} (Read / Write) @br{}
  Whether to use a monospace font. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-view-monospace)
      "Accessor"
      (documentation 'text-view-monospace 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-view-monospace object) => monospace}
  @syntax{(setf (gtk:text-view-monospace object) monospace)}
  @argument[object]{a @class{gtk:text-view} widget}
  @argument[monospace]{@em{true} to request monospace styling}
  @begin{short}
    The accessor for the @slot[gtk:text-view]{monospace} slot of the
    @class{gtk:text-view} class gets or sets whether to use a monospace font.
  @end{short}
  @see-class{gtk:text-view}")

;;; --- gtk:text-view-overwrite ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "overwrite" 'text-view) t)
 "The @code{overwrite} property of type @code{:boolean} (Read / Write) @br{}
  Whether entered text overwrites existing contents. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-view-overwrite)
      "Accessor"
      (documentation 'text-view-overwrite 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-view-overwrite object) => overwrite}
  @syntax{(setf (gtk:text-view-overwrite object) overwrite)}
  @argument[object]{a @class{gtk:text-view} widget}
  @argument[overwrite]{@em{true} to turn on overwrite mode, @em{false} to turn
    it off}
  @begin{short}
    The accessor for the @slot[gtk:text-view]{overwrite} slot of the
    @class{gtk:text-view} class gets or sets whether the text view is in
    overwrite mode or not.
  @end{short}
  @see-class{gtk:text-view}")

;;; --- gtk:text-view-pixels-above-lines ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pixels-above-lines"
                                               'text-view) t)
 "The @code{pixels-above-lines} property of type @code{:int} (Read / Write)
  @br{}
  The pixels of blank space above paragraphs. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'text-view-pixels-above-lines)
      "Accessor"
      (documentation 'text-view-pixels-above-lines 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-view-pixels-above-lines object) => pixels}
  @syntax{(setf (gtk:text-view-pixels-above-lines object) pixels)}
  @argument[object]{a @class{gtk:text-view} widget}
  @argument[pixels]{an integer for the pixels above paragraphs}
  @begin{short}
    The accessor for the @slot[gtk:text-view]{pixels-above-lines} slot of the
    @class{gtk:text-view} class gets or sets the default number of pixels to put
    above paragraphs in the text view.
  @end{short}
  Tags in the text buffer for the text view may override the defaults.
  @see-class{gtk:text-view}
  @see-class{gtk:text-tag}")

;;; --- gtk:text-view-pixels-below-lines ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pixels-below-lines"
                                               'text-view) t)
 "The @code{pixels-below-lines} property of type @code{:int} (Read / Write)
  @br{}
  The pixels of blank space below paragraphs. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'text-view-pixels-below-lines)
      "Accessor"
      (documentation 'text-view-pixels-below-lines 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-view-pixels-below-lines object) => pixels}
  @syntax{(setf (gtk:text-view-pixels-below-lines object) pixels)}
  @argument[object]{a @class{gtk:text-view} widget}
  @argument[pixels]{an integer for the pixels below paragraphs}
  @begin{short}
    The accessor for the @slot[gtk:text-view]{pixels-below-lines} slot of the
    @class{gtk:text-view} class gets or sets the default number of pixels to put
    below paragraphs in the text view.
  @end{short}
  May be overridden by tags applied to the text buffer of the text view.
  @see-class{gtk:text-view}
  @see-class{gtk:text-tag}")

;;; --- gtk:text-view-pixels-inside-wrap ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pixels-inside-wrap"
                                               'text-view) t)
 "The @code{pixels-inside-wrap} property of type @code{:int} (Read / Write)
  @br{}
  The pixels of blank space between wrapped lines in a paragraph. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'text-view-pixels-inside-wrap)
      "Accessor"
      (documentation 'text-view-pixels-inside-wrap 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-view-pixels-inside-wrap object) => pixels}
  @syntax{(setf (gtk:text-view-pixels-inside-wrap object) pixels)}
  @argument[object]{a @class{gtk:text-view} widget}
  @argument[pixels]{an integer for the default number of pixels between wrapped
    lines}
  @begin{short}
    The accessor for the @slot[gtk:text-view]{pixels-inside-wrap} slot of the
    @class{gtk:text-view} class gets or sets the default number of pixels of
    blank space to leave between display/wrapped lines within a paragraph.
  @end{short}
  May be overridden by tags in the text buffer of the text view.
  @see-class{gtk:text-view}
  @see-class{gtk:text-tag}")

;;; --- gtk:text-view-right-margin ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "right-margin" 'text-view) t)
 "The @code{right-margin} property of type @code{:int} (Read / Write) @br{}
  The width of the right margin in pixels. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'text-view-right-margin)
      "Accessor"
      (documentation 'text-view-right-margin 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-view-right-margin object) => margin}
  @syntax{(setf (gtk:text-view-right-margin object) margin)}
  @argument[object]{a @class{gtk:text-view} widget}
  @argument[margin]{an integer for the right margin in pixels}
  @begin{short}
    The accessor for the @slot[gtk:text-view]{right-margin} slot of the
    @class{gtk:text-view} class gets or sets the default right margin for text
    in the text view.
  @end{short}
  Tags in the text buffer may override the default.
  @see-class{gtk:text-view}
  @see-class{gtk:text-tag}")

;;; --- gtk:text-view-tabs -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tabs" 'text-view) t)
 "The @code{tabs} property of type @class{pango:tab-array} (Read / Write) @br{}
  The custom tabs for this text.")

#+liber-documentation
(setf (liber:alias-for-function 'text-view-tabs)
      "Accessor"
      (documentation 'text-view-tabs 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-view-tabs object) => tabs}
  @syntax{(setf (gtk:text-view-tabs object) tabs)}
  @argument[object]{a @class{gtk:text-view} widget}
  @argument[tabs]{a @class{pango:tab-array} instance for the tabs}
  @begin{short}
    The accessor for the @slot[gtk:text-view]{tabs} slot of the
    @class{gtk:text-view} class gets or sets a copy of the default Pango tab
    array, or @code{nil} if \"standard\" tabs are used.
  @end{short}
  Tags in the text buffer may override the defaults.
  @see-class{gtk:text-view}
  @see-class{gtk:text-tag}
  @see-class{pango:tab-array}")

;;; --- gtk:text-view-top-margin -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "top-margin" 'text-view) t)
 "The @code{top-margin} property of type @code{:int} (Read / Write) @br{}
  The top margin for text in the text view. Note that this property is
  confusingly named. In CSS terms, the value set here is padding, and it is
  applied in addition to the padding from the theme. Do not confuse this
  property with the @slot[gtk:widget]{margin-top} property. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'text-view-top-margin)
      "Accessor"
      (documentation 'text-view-top-margin 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-view-top-margin object) => margin}
  @syntax{(setf (gtk:text-view-top-margin object) margin)}
  @argument[object]{a @class{gtk:text-view} widget}
  @argument[margin]{an integer for the top margin in pixels}
  @begin{short}
    The accessor for the @slot[gtk:text-view]{top-margin} slot of the
    @class{gtk:text-view} class gets or sets the top margin for text in the text
    view.
  @end{short}
  Note that this function is confusingly named. In CSS terms, the value set
  here is padding.
  @see-class{gtk:text-view}")

;;; --- gtk:text-view-wrap-mode ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "wrap-mode" 'text-view) t)
 "The @code{wrap-mode} property of type @sym{gtk:wrap-mode} (Read / Write) @br{}
  Whether to wrap lines never, at word boundaries, or at character boundaries.
  @br{}
  Default value: @val[gtk:wrap-mode]{:none}")

#+liber-documentation
(setf (liber:alias-for-function 'text-view-wrap-mode)
      "Accessor"
      (documentation 'text-view-wrap-mode 'function)
 "@version{2025-09-28}
  @syntax{(gtk:text-view-wrap-mode object) => mode}
  @syntax{(setf (gtk:text-view-wrap-mode object) mode)}
  @argument[object]{a @class{gtk:text-view} widget}
  @argument[mode]{a value of the @sym{gtk:wrap-mode} enumeration}
  @begin{short}
    The accessor for the @slot[gtk:text-view]{wrap-mode} slot of the
    @class{gtk:text-view} class gets or sets the line wrapping for the text
    view.
  @end{short}
  @see-class{gtk:text-view}
  @see-symbol{gtk:wrap-mode}")

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_new
;;; ----------------------------------------------------------------------------

(declaim (inline text-view-new))

(defun text-view-new ()
 #+liber-documentation
 "@version{2024-07-04}
  @return{The new @class{gtk:text-view} widget.}
  @begin{short}
    Creates a new text view.
  @end{short}
  If you do not call the @fun{gtk:text-view-buffer} function before using the
  text view, an empty default text buffer will be created for you. Get the text
  buffer with the @fun{gtk:text-view-buffer} function. If you want to specify
  your own text buffer, consider the @fun{gtk:text-view-new-with-buffer}
  function.
  @see-class{gtk:text-view}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-view-buffer}
  @see-function{gtk:text-view-new-with-buffer}"
  (make-instance 'text-view))

(export 'text-view-new)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_new_with_buffer
;;; ----------------------------------------------------------------------------

(declaim (inline text-view-new-with-buffer))

(defun text-view-new-with-buffer (buffer)
 #+liber-documentation
 "@version{2024-07-04}
  @argument[buffer]{a @class{gtk:text-buffer} object}
  @return{The new @class{gtk:text-view} widget.}
  @begin{short}
    Creates a new text view displaying the text buffer.
  @end{short}
  One text buffer can be shared among many widgets. The @arg{buffer} argument
  may be @code{nil} to create a default text buffer, in which case this
  function is equivalent to the @fun{gtk:text-view-new} function.
  @see-class{gtk:text-view}
  @see-class{gtk:text-buffer}
  @see-function{gtk:text-view-new}"
  (make-instance 'text-view
                 :buffer buffer))

(export 'text-view-new-with-buffer)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_scroll_to_mark
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_view_scroll_to_mark" %text-view-scroll-to-mark) :void
  (view (g:object text-view))
  (mark (g:object text-mark))
  (margin :double)
  (use-align :boolean)
  (xalign :double)
  (yalign :double))

(defun text-view-scroll-to-mark (view mark &key (margin 0.4)
                                                (xalign 0.0 xalign-p)
                                                (yalign 0.0 yalign-p))
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[view]{a @class{gtk:text-view} widget}
  @argument[mark]{a @class{gtk:text-mark} object}
  @argument[margin]{a double float margin as a [0.0, 0.5) fraction of screen
    size}
  @argument[xalign]{a double float for the horizontal alignment of the mark
    within visible area}
  @argument[yalign]{a double float for the vertical alignment of the mark
    within visible area}
  @begin{short}
    Scrolls the text view so that the mark is on the screen in the position
    indicated by the @arg{xalign} and @arg{yalign} arguments.
  @end{short}
  An alignment of 0.0 indicates left or top, 1.0 indicates right or bottom, 0.5
  means center. If you do not pass a value for the @arg{xalign} and @arg{yalign}
  arguments, the text scrolls the minimal distance to get the mark onscreen,
  possibly not scrolling at all. The effective screen for purposes of this
  function is reduced by a margin of size @arg{margin}.
  @begin[Notes]{dictionary}
    The double float arguments take any number. The numbers are coerced to
    double floats.
  @end{dictionary}
  @see-class{gtk:text-view}
  @see-class{gtk:text-mark}"
  (%text-view-scroll-to-mark view
                             mark
                             (coerce margin 'double-float)
                             (or xalign-p yalign-p)
                             (coerce xalign 'double-float)
                             (coerce yalign 'double-float)))

(export 'text-view-scroll-to-mark)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_scroll_to_iter
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_view_scroll_to_iter" %text-view-scroll-to-iter) :void
  (view (g:object text-view))
  (iter (g:boxed text-iter))
  (margin :double)
  (use-align :boolean)
  (xalign :double)
  (yalign :double))

(defun text-view-scroll-to-iter (view iter &key (margin 0.4)
                                                (xalign 0.0 xalign-p)
                                                (yalign 0.0 yalign-p))
 #+liber-documentation
 "@version{#2025-07-26}
  @argument[view]{a @class{gtk:text-view} widget}
  @argument[iter]{a @class{gtk:text-iter} iterator}
  @argument[margin]{a double float margin as a [0.0, 0.5) fraction of screen
    size}
  @argument[xalign]{a double float for the horizontal alignment of the mark
    within visible area}
  @argument[yalign]{a double float for the vertical alignment of the mark
    within visible area}
  @return{@em{True} if scrolling occurred.}
  @begin{short}
    Scrolls the text view so that the iterator is on the screen in the position
    indicated by the @arg{xalign} and @arg{yalign} arguments.
  @end{short}
  An alignment of 0.0 indicates left or top, 1.0 indicates right or bottom, 0.5
  means center. If you do not pass a value for the @arg{xalign} and @arg{yalign}
  arguments, the text scrolls the minimal distance to get the iterator onscreen,
  possibly not scrolling at all. The effective screen for purposes of this
  function is reduced by a margin of size @arg{margin}.

  Note that this function uses the currently computed height of the lines in
  the text buffer. Line heights are computed in an idle handler. So this
  function may not have the desired effect if it is called before the height
  computations. To avoid oddness, consider using the
  @fun{gtk:text-view-scroll-to-mark} function which saves a point to be scrolled
  to after line validation.
  @begin[Notes]{dictionary}
    The double float arguments take any number. The numbers are coerced to
    double floats.
  @end{dictionary}
  @see-class{gtk:text-view}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-view-scroll-to-mark}"
  (%text-view-scroll-to-iter view
                             iter
                             (coerce margin 'double-float)
                             (or xalign-p yalign-p)
                             (coerce xalign 'double-float)
                             (coerce yalign 'double-float)))

(export 'text-view-scroll-to-iter)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_scroll_mark_onscreen
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_view_scroll_mark_onscreen"
               text-view-scroll-mark-onscreen) :void
 #+liber-documentation
 "@version{2024-07-04}
  @argument[view]{a @class{gtk:text-view} widget}
  @argument[mark]{a @class{gtk:text-mark} object in the text buffer for the
    text view}
  @begin{short}
    Scrolls the text view the minimum distance such that the mark is contained
    within the visible area of the widget.
  @end{short}
  @see-class{gtk:text-view}
  @see-class{gtk:text-mark}"
  (view (g:object text-view))
  (mark (g:object text-mark)))

(export 'text-view-scroll-mark-onscreen)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_move_mark_onscreen
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_view_move_mark_onscreen"
               text-view-move-mark-onscreen) :boolean
 #+liber-documentation
 "@version{#2024-07-04}
  @argument[view]{a @class{gtk:text-view} widget}
  @argument[mark]{a @class{gtk:text-mark} object}
  @return{@em{True} if the mark moved, was not already onscreen.}
  @begin{short}
    Moves a the mark within the text buffer so that it is located within the
    currently visible text area.
  @end{short}
  @see-class{gtk:text-view}
  @see-class{gtk:text-mark}"
  (view (g:object text-view))
  (mark (g:object text-mark)))

(export 'text-view-move-mark-onscreen)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_place_cursor_onscreen
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_view_place_cursor_onscreen"
                text-view-place-cursor-onscreen) :boolean
 #+liber-documentation
 "@version{#2024-07-04}
  @argument[view]{a @class{gtk:text-view} widget}
  @return{@em{True} if the cursor had to be moved.}
  @begin{short}
    Moves the cursor to the currently visible region of the text buffer, if it
    is not there already.
  @end{short}
  @see-class{gtk:text-view}"
  (view (g:object text-view)))

(export 'text-view-place-cursor-onscreen)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_visible_rect
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_view_get_visible_rect" %text-view-visible-rect) :void
  (view (g:object text-view))
  (rect (g:boxed gdk:rectangle)))

(defun text-view-visible-rect (view)
 #+liber-documentation
 "@version{#2024-07-25}
  @argument[view]{a @class{gtk:text-view} widget}
  @return{The @class{gdk:rectangle} instance for the current visible region.}
  @begin{short}
    The currently visible region of the text buffer, in text buffer coordinates.
  @end{short}
  Convert to window coordinates with the
  @fun{gtk:text-view-buffer-to-window-coords} function.
  @see-class{gtk:text-view}
  @see-class{gdk:rectangle}
  @see-function{gtk:text-view-buffer-to-window-coords}"
  (let ((rect (gdk:rectangle-new)))
    (%text-view-visible-rect view rect)
    rect))

(export 'text-view-visible-rect)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_visible_offset
;;; ----------------------------------------------------------------------------

#+gtk-4-18
(cffi:defcfun ("gtk_text_view_get_visible_offset" %text-view-visible-offset)
    :void
  (view (g:object text-view))
  (xoffset (:pointer :double))
  (yoffset (:pointer :double)))

#+gtk-4-18
(defun text-view-visible-offset (view)
 "@version{#2025-10-23}
  @syntax{(gtk:text-view-visible-offset view) => xoffset, yoffset}
  @argument[view]{a @class{gtk:text-view} widget}
  @argument[xoffset]{a double float for the x offset}
  @argument[yoffset]{a double float for the y offset}
  @begin{short}
    Gets the x, y offset in buffer coordinates of the top-left corner of the
    the text view contents.
  @end{short}
  This allows for more precise positioning than what is provided by the
  @fun{gtk:text-view-visible-rect} function as you can discover what device
  pixel is being quantized for text positioning.

  You might want this when making ulterior widgets align with quantized device
  pixels of the text view contents such as line numbers.

  Since 4.18
  @see-class{gtk:text-view}
  @see-function{gtk:text-view-visible-rect}"
  (cffi:with-foreign-objects ((xoffset :double) (yoffset :double))
    (%text-view-visible-offset view xoffset yoffset)
    (values (cffi:mem-ref xoffset :double)
            (cffi:mem-ref yoffset :double))))

#+gtk-4-18
(export 'text-view-visible-offset)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_iter_location
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_view_get_iter_location" %text-view-iter-location) :void
  (view (g:object text-view))
  (iter (g:boxed text-iter))
  (location (g:boxed gdk:rectangle)))

(defun text-view-iter-location (view iter)
 #+liber-documentation
 "@version{#2024-07-04}
  @argument[view]{a @class{gtk:text-view} widget}
  @argument[iter]{a @class{gtk:text-iter} iterator}
  @begin{return}
    The @class{gdk:rectangle} instance for the bounds of the character at
    @arg{iter}.
  @end{return}
  @begin{short}
    Gets a rectangle which roughly contains the coordinates of the character at
    the position of the iterator.
  @end{short}
  The rectangle position is in text buffer coordinates. Use the
  @fun{gtk:text-view-buffer-to-window-coords} function to convert these
  coordinates to coordinates for one of the windows in the text view.
  @see-class{gtk:text-view}
  @see-class{gtk:text-iter}
  @see-class{gdk:rectangle}
  @see-function{gtk:text-view-buffer-to-window-coords}"
  (let ((rect (gdk:rectangle-new)))
    (%text-view-iter-location view iter rect)
    rect))

(export 'text-view-iter-location)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_cursor_locations
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_view_get_cursor_locations" %text-view-cursor-locations)
    :void
  (view (g:object text-view))
  (iter (g:boxed text-iter))
  (strong (g:boxed gdk:rectangle))
  (weak (g:boxed gdk:rectangle)))

(defun text-view-cursor-locations (view iter)
 #+liber-documentation
 "@version{#2025-07-25}
  @syntax{(gtk:text-view-cursor-locations view iter) => strong, weak}
  @argument[view]{a @class{gtk:text-view} widget}
  @argument[iter]{a @class{gtk:text-iter} iterator}
  @argument[strong]{a @class{gdk:rectangle} instance for the strong cursor
    position}
  @argument[weak]{a @class{gdk:rectangle} instance for the weak cursor position}
  @begin{short}
    Given an iterator within a text layout, determine the positions of the
    strong and weak cursors if the insertion point is at that iterator.
  @end{short}
  The position of each cursor is stored as a zero-width rectangle. The strong
  cursor location is the location where characters of the directionality equal
  to the base direction of the paragraph are inserted. The weak cursor location
  is the location where characters of the directionality opposite to the base
  direction of the paragraph are inserted.

  If the @arg{iter} argument is @code{nil}, the actual cursor position is used.

  Note that if the @arg{iter} argument happens to be the actual cursor position,
  and there is currently an IM preedit sequence being entered, the returned
  locations will be adjusted to account for the offset of the preedit cursor
  within the preedit sequence.

  The rectangle position is in text buffer coordinates. Use the
  @fun{gtk:text-view-buffer-to-window-coords} function to convert these
  coordinates to coordinates for one of the windows in the text view.
  @see-class{gtk:text-view}
  @see-class{gtk:text-iter}
  @see-class{gdk:rectangle}
  @see-function{gtk:text-view-buffer-to-window-coords}"
  (let ((strong (gdk:rectangle-new))
        (weak (gdk:rectangle-new)))
    (%text-view-cursor-locations view iter strong weak)
    (values strong weak)))

(export 'text-view-cursor-locations)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_line_at_y
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_view_get_line_at_y" %text-view-line-at-y) :void
  (view (g:object text-view))
  (iter (g:boxed text-iter))
  (y :int)
  (line (:pointer :int)))

(defun text-view-line-at-y (view y)
 #+liber-documentation
 "@version{#2025-07-25}
  @syntax{(gtk:text-view-line-at-y view y) => iter, line}
  @argument[view]{a @class{gtk:text-view} widget}
  @argument[y]{an integer for the y coordinate}
  @argument[iter]{a @class{gtk:text-iter} iterator}
  @argument[line]{an integer for the top coordinate of the line}
  @begin{short}
    Gets the @class{gtk:text-iter} iterator at the start of the line containing
    the coordinate @arg{y}.
  @end{short}
  The @arg{y} argument is in text buffer coordinates, convert from window
  coordinates with the @fun{gtk:text-view-window-to-buffer-coords} function. If
  non-@code{nil}, @arg{line} will be filled with the coordinate of the top edge
  of the line.
  @see-class{gtk:text-view}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-view-window-to-buffer-coords}"
  (let ((iter (make-instance 'text-iter)))
    (cffi:with-foreign-object (line :int)
      (%text-view-line-at-y view iter y line)
      (values iter (cffi:mem-ref line :int)))))

(export 'text-view-line-at-y)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_line_yrange
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_view_get_line_yrange" %text-view-line-yrange) :void
  (view (g:object text-view))
  (iter (g:boxed text-iter))
  (y (:pointer :int))
  (height (:pointer :int)))

(defun text-view-line-yrange (view iter)
 #+liber-documentation
 "@version{#2024-07-25}
  @syntax{(gtk:text-view-line-yrange view iter) => y, height}
  @argument[view]{a @class{gtk:text-view} widget}
  @argument[iter]{a @class{gtk:text-iter} iterator}
  @argument[y}{an integer for the y coordinate}
  @argument[height]{an integer for the height}
  @begin{short}
    Gets the @arg{y} coordinate of the top of the line containing @arg{iter},
    and the @arg{height} of the line.
  @end{short}
  The coordinate is a text buffer coordinate. Convert to window coordinates with
  the @fun{gtk:text-view-buffer-to-window-coords} function.
  @see-class{gtk:text-view}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-view-buffer-to-window-coords}"
  (cffi:with-foreign-objects ((y :int) (height :int))
    (%text-view-line-yrange view iter y height)
    (values (cffi:mem-ref y :int)
            (cffi:mem-ref height :int))))

(export 'text-view-line-yrange)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_iter_at_location
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_view_get_iter_at_location" %text-view-iter-at-location)
    :void
  (view (g:object text-view))
  (iter (g:boxed text-iter))
  (x :int)
  (y :int))

(defun text-view-iter-at-location (view x y)
 #+liber-documentation
 "@version{2025-07-25}
  @argument[view]{a @class{gtk:text-view} widget}
  @argument[x]{an integer for the x position, in text buffer coordinates}
  @argument[y]{an integer for the y position, in text buffer coordinates}
  @begin{return}
    The @class{gtk:text-iter} iterator at text buffer coordinates.
  @end{return}
  @begin{short}
    Retrieves the iterator at text buffer coordinates @arg{x} and @arg{y}.
  @end{short}
  Text buffer coordinates are coordinates for the entire text buffer, not just
  the currently displayed portion. If you have coordinates from an event, you
  have to convert those to text buffer coordinates with the
  @fun{gtk:text-view-window-to-buffer-coords} function.
  @see-class{gtk:text-view}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-view-window-to-buffer-coords}"
  (let ((iter (make-instance 'text-iter)))
    (%text-view-iter-at-location view iter x y)
    iter))

(export 'text-view-iter-at-location)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_iter_at_position
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_view_get_iter_at_position" %text-view-iter-at-position)
    :void
  (view (g:object text-view))
  (iter (g:boxed text-iter))
  (trailing (:pointer :int))
  (x :int)
  (y :int))

(defun text-view-iter-at-position (view x y)
 #+liber-documentation
 "@version{#2024-07-25}
  @syntax{(gtk:text-view-iter-at-position view x y) => iter, trailing}
  @argument[view]{a @class{gtk:text-view} widget}
  @argument[x]{an integer for the x position, in text buffer coordinates}
  @argument[y]{an integer for the y position, in text buffer coordinates}
  @argument[iter]{a @class{gtk:text-iter} iterator}
  @argument[trailing]{if non-@code{nil}, an integer indicating where in the
    grapheme the user clicked, it will either be zero, or the number of
    characters in the grapheme, 0 represents the trailing edge of the grapheme}
  @begin{short}
    Retrieves the iterator pointing to the character at text buffer coordinates
    @arg{x} and @arg{y}.
  @end{short}
  Text buffer coordinates are coordinates for the entire text buffer, not just
  the currently displayed portion. If you have coordinates from an event, you
  have to convert those to text buffer coordinates with the
  @fun{gtk:text-view-window-to-buffer-coords} function.

  Note that this is different from the @fun{gtk:text-view-iter-at-location}
  function, which returns cursor locations, that is, positions between
  characters.
  @see-class{gtk:text-view}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-view-window-to-buffer-coords}
  @see-function{gtk:text-view-iter-at-location}"
  (cffi:with-foreign-object (trailing :int)
    (let ((iter (make-instance 'text-iter)))
      (%text-view-iter-at-position view iter trailing x y)
      (values iter (cffi:mem-ref trailing :int)))))

(export 'text-view-iter-at-position)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_buffer_to_window_coords
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_view_buffer_to_window_coords"
               %text-view-buffer-to-window-coords) :void
  (view (g:object text-view))
  (wtype text-window-type)
  (xbuffer :int)
  (ybuffer :int)
  (xwindow (:pointer :int))
  (ywindow (:pointer :int)))

(defun text-view-buffer-to-window-coords (view wtype xbuffer ybuffer)
 #+liber-documentation
 "@version{#2025-07-25}
  @syntax{(gtk:text-view-buffer-to-window-coords view wtype xbuffer ybuffer)
    => xwindow, ywindow}
  @argument[view]{a @class{gtk:text-view} widget}
  @argument[wtype]{a @sym{gtk:text-window-type} value}
  @argument[xbuffer]{an integer for the text buffer x coordinate}
  @argument[ybuffer]{an integer for the text buffer y coordinate}
  @argument[xwindow]{an integer for the window x coordinate}
  @argument[ywindow]{an integer for the window y coordinate}
  @begin{short}
    Converts the text buffer coordinates to window coordinates.
  @end{short}
  @see-class{gtk:text-view}
  @see-symbol{gtk:text-window-type}"
  (cffi:with-foreign-objects ((xwindow :int) (ywindow :int))
    (%text-view-buffer-to-window-coords view
                                        wtype
                                        xbuffer ybuffer
                                        xwindow ywindow)
    (values (cffi:mem-ref xwindow :int)
            (cffi:mem-ref ywindow :int))))

(export 'text-view-buffer-to-window-coords)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_window_to_buffer_coords
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_view_window_to_buffer_coords"
               %text-view-window-to-buffer-coords) :void
  (view (g:object text-view))
  (wtype text-window-type)
  (xwindow :int)
  (ywindow :int)
  (xbuffer (:pointer :int))
  (ybuffer (:pointer :int)))

(defun text-view-window-to-buffer-coords (view wtype xwindow ywindow)
 #+liber-documentation
 "@version{2025-07-25}
  @syntax{(gtk:text-view-window-to-buffer-coords view wtype xwindow ywindow)
    => xbuffer, ybuffer}
  @argument[view]{a @class{gtk:text-view} widget}
  @argument[wtype]{a @sym{gtk:text-window-type} value}
  @argument[xwindow]{an integer for the window x coordinate}
  @argument[ywindow]{an integer for the window y coordinate}
  @argument[xbuffer]{an integer for the text buffer x coordinate}
  @argument[ybuffer]{an integer for the text buffer y coordinate}
  @begin{short}
    Converts coordinates on the window identified by @arg{wtype} to
    text buffer coordinates.
  @end{short}
  @see-class{gtk:text-view}
  @see-symbol{gtk:text-window-type}"
  (cffi:with-foreign-objects ((xbuffer :int) (ybuffer :int))
    (%text-view-window-to-buffer-coords view
                                        wtype
                                        xwindow ywindow
                                        xbuffer ybuffer)
    (values (cffi:mem-ref xbuffer :int)
            (cffi:mem-ref ybuffer :int))))

(export 'text-view-window-to-buffer-coords)

;;; ----------------------------------------------------------------------------
;;; gtk:text-view-move-display-line
;;;
;;; gtk_text_view_forward_display_line                      not exported
;;; gtk_text_view_backward_display_line                     not exported
;;; gtk_text_view_forward_display_line_end                  not exported
;;; gtk_text_view_backward_display_line_start               not exported
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_view_forward_display_line"
               text-view-forward-display-line) :boolean
  (view (g:object text-view))
  (iter (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_view_backward_display_line"
               text-view-backward-display-line) :boolean
  (view (g:object text-view))
  (iter (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_view_forward_display_line_end"
               text-view-forward-display-line-end) :boolean
  (view (g:object text-view))
  (iter (g:boxed text-iter)))

(cffi:defcfun ("gtk_text_view_backward_display_line_start"
               text-view-backward-display-line-start) :boolean
  (view (g:object text-view))
  (iter (g:boxed text-iter)))

;;; ----------------------------------------------------------------------------

(defun text-view-move-display-line (view iter &key (direction :forward)
                                                   (start-or-end nil))
 #+liber-documentation
 "@version{#2024-07-04}
  @argument[view]{a @class{gtk:text-view} widget}
  @argument[iter]{a @class{gtk:text-iter} iterator}
  @argument[direction]{a @code{:forward} or @code{:backward} keyword value, the
    default value is @code{:forward}}
  @argument[start-or-end]{@em{true} to go the end of the next or the start of
    the previous display line, the default value is @em{false}}
  @return{@em{True} if @arg{iter} was moved and is not on the end iterator.}
  @begin{short}
    Moves the given @arg{iter} by one display (wrapped) line.
  @end{short}
  If the @arg{direction} argument is @code{:forward} moves forward otherwise
  backward. If the @arg{start-or-end} argument is @em{true} moves to next
  display line end for a forward move and to the display line start for a
  backward move.

  A display line is different from a paragraph. Paragraphs are separated by
  newlines or other paragraph separator characters. Display lines are created
  by line-wrapping a paragraph. If wrapping is turned off, display lines and
  paragraphs will be the same. Display lines are divided differently for each
  view, since they depend on the width of the text view. Paragraphs are the
  same in all text views, since they depend on the contents of the text buffer.
  @begin[Notes]{dictionary}
    This function combines the @code{gtk_text_view_forward_display_line ()},
    @code{gtk_text_view_forward_display_line_end()},
    @code{gtk_text_view_backward_display_line()}, and
    @code{gtk_text_view_backward_display_line_start()} functions into one
    Lisp function.
  @end{dictionary}
  @see-class{gtk:text-view}
  @see-class{gtk:text-iter}"
  (assert (typep direction '(member :forward :backward)))
  (ecase direction
    (:forward
     (if start-or-end
         (text-view-forward-display-line-end view iter)
         (text-view-forward-display-line view iter)))
     (:backward
      (if start-or-end
          (text-view-backward-display-line-start view iter)
          (text-view-backward-display-line view iter)))))

(export 'text-view-move-display-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_starts_display_line
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_view_starts_display_line"
               text-view-starts-display-line) :boolean
 #+liber-documentation
 "@version{#2024-07-04}
  @argument[view]{a @class{gtk:text-view} widget}
  @argument[iter]{a @class{gtk:text-iter} iterator}
  @return{@em{True} if @arg{iter} begins a wrapped line.}
  @begin{short}
    Determines whether @arg{iter} is at the start of a display line.
  @end{short}
  See the @fun{gtk:text-view-forward-display-line} function for an explanation
  of display lines versus paragraphs.
  @see-class{gtk:text-view}
  @see-class{gtk:text-iter}
  @see-function{gtk:text-view-forward-display-line}"
  (view (g:object text-view))
  (iter (g:boxed text-iter)))

(export 'text-view-starts-display-line)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_move_visually
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_view_move_visually" text-view-move-visually) :boolean
 #+liber-documentation
 "@version{#2025-07-27}
  @argument[view]{a @class{gtk:text-view} widget}
  @argument[iter]{a @class{gtk:text-iter} iterator}
  @argument[count]{an integer for the number of characters to move, negative
    moves left, positive moves right}
  @return{@em{True} if @arg{iter} moved and is not on the end iterator.}
  @begin{short}
    Move the iterator a given number of characters visually, treating it as the
    strong cursor position.
  @end{short}
  If the @arg{count} argument is positive, then the new strong cursor position
  will be @arg{count} positions to the right of the old cursor position. If the
  @arg{count} argument is negative then the new strong cursor position will be
  @arg{count} positions to the left of the old cursor position.

  In the presence of bi-directional text, the correspondence between logical
  and visual order will depend on the direction of the current run, and there
  may be jumps when the cursor is moved off of the end of a run.
  @see-class{gtk:text-view}
  @see-class{gtk:text-iter}"
  (view (g:object text-view))
  (iter (g:boxed text-iter))
  (count :int))

(export 'text-view-move-visually)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_add_child_at_anchor
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_view_add_child_at_anchor"
               text-view-add-child-at-anchor) :void
 #+liber-documentation
 "@version{2024-07-04}
  @argument[view]{a @class{gtk:text-view} widget}
  @argument[child]{a @class{gtk:widget} object}
  @argument[anchor]{a @class{gtk:text-child-anchor} object in the text buffer
    for @arg{view}}
  @begin{short}
    Adds a child widget in the text buffer, at the given anchor.
  @end{short}
  @see-class{gtk:text-view}
  @see-class{gtk:widget}
  @see-class{gtk:text-child-anchor}"
  (view (g:object text-view))
  (child (g:object widget))
  (anchor (g:object text-child-anchor)))

(export 'text-view-add-child-at-anchor)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_remove
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_view_remove" text-view-remove) :void
 #+liber-documentation
 "@version{#2024-07-04}
  @argument[view]{a @class{gtk:text-view} object}
  @argument[child]{a @class{gtk:widget} object to remove}
  @short{Removes a child widget from the text view.}
  @see-class{gtk:text-view}
  @see-class{gtk:widget}"
  (view (g:object text-view))
  (child (g:object widget)))

(export 'text-view-remove)

;; -----------------------------------------------------------------------------
;;; gtk_text_view_get_gutter
;;; gtk_text_view_set_gutter
;;; ----------------------------------------------------------------------------

(defun (setf text-view-gutter) (widget view win)
  (cffi:foreign-funcall "gtk_text_view_set_gutter"
                        (g:object text-view) view
                        text-window-type win
                        (g:object widget) widget
                        :void)
  widget)

(cffi:defcfun ("gtk_text_view_get_gutter" text-view-gutter) (g:object widget)
 #+liber-documentation
 "@version{#2025-07-25}
  @syntax{(gtk:text-view-gutter view win) => widget}
  @syntax{(setf (gtk:text-view-gutter view win) widget)}
  @argument[view]{a @class{gtk:text-view} widget}
  @argument[wtype]{a @sym{gtk:text-window-type} value}
  @argument[widget]{a @class{gtk:widget} object or @code{nil}}
  @begin{short}
    The @fun{gtk:text-view-gutter} function gets @arg{widget} that has
    previously been set.
  @end{short}
  The @setf{gtk:text-view-gutter} function places @arg{widget} into the gutter
  specified by @arg{win}.

  The @arg{wtype} argument must be one of the values
  @val[gtk:text-window-type]{:left}, @val[gtk:text-window-type]{:right},
  @val[gtk:text-window-type]{:top}, or @val[gtk:text-window-type]{:bottom} of
  the @sym{gtk:text-window-type} enumeration.
  @see-class{gtk:text-view}
  @see-class{gtk:widget}
  @see-symbol{gtk:text-window-type}"
  (view (g:object text-view))
  (wtype text-window-type))

(export 'text-view-gutter)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_add_overlay
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_view_add_overlay" text-view-add-overlay) :void
 #+liber-documentation
 "@version{#2025-07-27}
  @argument[view]{a @class{gtk:text-view} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @argument[xpos]{an integer for the x position of @arg{child} in window
    coordinates}
  @argument[ypos]{an integer for the y position of @arg{child} in window
    coordinates}
  @begin{short}
    Adds @arg{child} at a fixed coordinate in the text window of the
    @class{gtk:text-view} widget.
  @end{short}
  The @arg{xpos} and @arg{ypos} arguments must be in buffer coordinates. See
  the @fun{gtk:text-view-iter-location} function to convert to buffer
  coordinates.

  The child widget will scroll with the text view. If instead you want a widget
  that will not move with the text view contents see the @class{gtk:overlay}
  widget.
  @see-class{gtk:text-view}
  @see-class{gtk:widget}
  @see-class{gtk:overlay}
  @see-function{gtk:text-view-iter-location}"
  (view (g:object view))
  (child (g:object widget))
  (xpos :int)
  (ypos :int))

(export 'text-view-add-overlay)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_move_overlay
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_view_move_overlay" text-view-move-overlay) :void
 #+liber-documentation
 "@version{#2025-07-27}
  @argument[view]{a @class{gtk:text-view} widget}
  @argument[child]{a @class{gtk:widget} child widget already added}
  @argument[xpos]{an integer for the new x position of @arg{child} in buffer
    coordinates}
  @argument[ypos]{an integer for the new y position of @arg{child} in buffer
    coordinates}
  @begin{short}
    Updates the position of the child widget added with the
    @fun{gtk:text-view-add-overlay} function.
  @end{short}
  @see-class{gtk:text-view}
  @see-class{gtk:widget}
  @see-function{gtk:text-view-add-overlay}"
  (view (g:object view))
  (child (g:object widget))
  (xpos :int)
  (ypos :int))

(export 'text-view-move-overlay)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_reset_cursor_blink
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_view_reset_cursor_blink" text-view-reset-cursor-blink)
    :void
 #+liber-documentation
 "@version{#2024-07-04}
  @argument[view]{a @class{gtk:text-view} widget}
  @begin{short}
    Ensures that the cursor is shown, that is, not in an 'off' blink interval,
    and resets the time that it will stay blinking, or visible, in case blinking
    is disabled.
  @end{short}
  This function should be called in response to user input, for example, from
  derived classes that override the @code{\"key-press-event\"} handler of the
  text view.
  @see-class{gtk:text-view}"
  (view (g:object text-view)))

(export 'text-view-reset-cursor-blink)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_im_context_filter_keypress
;;; ----------------------------------------------------------------------------

;; TODO: Show a Lisp example, or remove the example

(cffi:defcfun ("gtk_text_view_im_context_filter_keypress"
               text-view-im-context-filter-keypress) :boolean
 #+liber-documentation
 "@version{#2024-07-04}
  @argument[view]{a @class{gtk:text-view} widget}
  @argument[event]{a @class{gdk:event} key event}
  @return{@em{True} if the input method handled the key event.}
  @begin{short}
    Allow the text view input method to internally handle key press and
    release events.
  @end{short}
  If this function returns @em{true}, then no further processing should be done
  for this key event. See the @fun{gtk:im-context-filter-keypress} function.

  Note that you are expected to call this function from your handler when
  overriding key event handling. This is needed in the case when you need to
  insert your own key handling between the input method and the default key
  event handling of the text view.
  @begin{pre}
static gboolean
gtk_foo_bar_key_press_event (GtkWidget   *widget,
                             GdkEventKey *event)
{
  if ((key->keyval == GDK_KEY_Return || key->keyval == GDK_KEY_KP_Enter))
    {
      if (gtk_text_view_im_context_filter_keypress (GTK_TEXT_VIEW (view),
                                                    event))
        return TRUE;
    @}

    /* Do some stuff */

  return GTK_WIDGET_CLASS (gtk_foo_bar_parent_class)
                          ->key_press_event (widget, event);
@}
  @end{pre}
  @see-class{gtk:text-view}
  @see-class{gdk:event}
  @see-function{gtk:im-context-filter-keypress}"
  (view (g:object text-view))
  (event gdk:event))

(export 'text-view-im-context-filter-keypress)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_reset_im_context
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_view_reset_im_context" text-view-reset-im-context)
    :void
 #+liber-documentation
 "@version{#2024-07-04}
  @argument[view]{a @class{gtk:text-view} widget}
  @begin{short}
    Reset the input method context of the text view if needed.
  @end{short}
  This can be necessary in the case where modifying the text buffer would
  confuse on-going input method behavior.
  @see-class{gtk:text-view}"
  (view (g:object text-view)))

(export 'text-view-reset-im-context)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_ltr_context                           Since 4.4
;;; ----------------------------------------------------------------------------

#+gtk-4-4
(cffi:defcfun ("gtk_text_view_get_ltr_context" text-view-ltr-context)
    (g:object pango:context)
 #+liber-documentation
 "@version{#2024-07-04}
  @argument[view]{a @class{gtk:text-view} widget}
  @return{The @class{pango:context} object.}
  @begin{short}
    Gets the Pango context that is used for rendering LTR directed text layouts.
  @end{short}
  The context may be replaced when CSS changes occur.

  Since 4.4
  @see-class{gtk:text-view}
  @see-class{pango:context}"
  (view (g:object text-view)))

#+gtk-4-4
(export 'text-view-ltr-context)

;;; ----------------------------------------------------------------------------
;;; gtk_text_view_get_rtl_context                          Since 4.4
;;; ----------------------------------------------------------------------------

#+gtk-4-4
(cffi:defcfun ("gtk_text_view_get_rtl_context" text-view-rtl-context)
    (g:object pango:context)
 #+liber-documentation
 "@version{#2024-07-04}
  @argument[view]{a @class{gtk:text-view} widget}
  @return{The @class{pango:context} object.}
  @begin{short}
    Gets the Pango context that is used for rendering RTL directed text layouts.
  @end{short}
  The context may be replaced when CSS changes occur.

  Since 4.4
  @see-class{gtk:text-view}
  @see-class{pango:context}"
  (view (g:object text-view)))

#+gtk-4-4
(export 'text-view-rtl-context)

;;; --- End of file gtk4.text-view.lisp ----------------------------------------
