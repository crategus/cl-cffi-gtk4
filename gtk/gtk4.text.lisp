;;; ----------------------------------------------------------------------------
;;; gtk4.text.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2023 Dieter Kaiser
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
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;; ----------------------------------------------------------------------------
;;;
;;; GtkText
;;;
;;;     A simple single-line text entry field
;;;
;;; Types and Values
;;;
;;;     GtkText
;;;
;;; Accessors
;;;
;;;     gtk_text_set_activates_default
;;;     gtk_text_get_activates_default
;;;     gtk_text_set_attributes
;;;     gtk_text_get_attributes
;;;     gtk_text_set_buffer
;;;     gtk_text_get_buffer
;;;     gtk_text_set_enable_emoji_completion
;;;     gtk_text_get_enable_emoji_completion
;;;     gtk_text_set_extra_menu
;;;     gtk_text_get_extra_menu
;;;     gtk_text_set_input_hints
;;;     gtk_text_get_input_hints
;;;     gtk_text_set_input_purpose
;;;     gtk_text_get_input_purpose
;;;     gtk_text_set_invisible_char
;;;     gtk_text_get_invisible_char
;;;     gtk_text_set_max_length
;;;     gtk_text_get_max_length
;;;     gtk_text_set_overwrite_mode
;;;     gtk_text_get_overwrite_mode
;;;     gtk_text_set_placeholder_text
;;;     gtk_text_get_placeholder_text
;;;     gtk_text_set_propagate_text_width
;;;     gtk_text_get_propagate_text_width
;;;     gtk_text_set_tabs
;;;     gtk_text_get_tabs
;;;     gtk_text_set_truncate_multiline
;;;     gtk_text_get_truncate_multiline
;;;     gtk_text_set_visibility
;;;     gtk_text_get_visibility
;;;
;;; Functions
;;;
;;;     gtk_text_new
;;;     gtk_text_new_with_buffer
;;;     gtk_text_unset_invisible_char
;;;     gtk_text_get_text_length
;;;     gtk_text_grab_focus_without_selecting
;;;
;;; Properties
;;;
;;;     activates-default
;;;     attributes
;;;     buffer
;;;     enable-emoji-completion
;;;     extra-menu
;;;     im-module
;;;     input-hints
;;;     input-purpose
;;;     invisible-char
;;;     invisible-char-set
;;;     max-length
;;;     overwrite-mode
;;;     placeholder-text
;;;     propagate-text-width
;;;     scroll-offset
;;;     tabs
;;;     truncate-multiline
;;;     visibility
;;;
;;; Signals
;;;
;;;     activate
;;;     backspace
;;;     copy-clipboard
;;;     cut-clipboard
;;;     delete-from-cursor
;;;     insert-at-cursor
;;;     insert-emoji
;;;     move-cursor
;;;     paste-clipboard
;;;     preedit-changed
;;;     toggle-overwrite
;;;
;;; Actions
;;;
;;;     menu.popup
;;;     text.redo
;;;     text.undo
;;;     misc.toggle-visibility
;;;     misc.insert-emoji
;;;     selection.select-all
;;;     selection.delete
;;;     clipboard.paste
;;;     clipboard.copy
;;;     clipboard.cut
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkText
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkEditable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkText
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkText" text
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkEditable")
   :type-initializer "gtk_text_get_type")
  ((activates-default
    text-activates-default
    "activates-default" "gboolean" t t)
   (attributes
    text-attributes
    "attributes" "PangoAttrList" t t)
   (buffer
    text-buffer
    "buffer" "GtkEntryBuffer" t t)
   (enable-emoji-completion
    text-enable-emoji-completion
    "enable-emoji-completion" "gboolean" t t)
   (extra-menu
    text-extra-menu
    "extra-menu" "GMenuModel" t t)
   (im-module
    text-im-module
    "im-module" "gchararray" t t)
   (input-hints
    text-input-hints
    "input-hints" "GtkInputHints" t t)
   (input-purpose
    text-input-purpose
    "input-purpose" "GtkInputPurpose" t t)
   (invisible-char
    text-invisible-char
    "invisible-char" "guint" t t)
   (invisible-char-set
    text-invisible-char-set
    "invisible-char-set" "gboolean" t t)
   (max-length
    text-max-length
    "max-length" "gint" t t)
   (overwrite-mode
    text-overwrite-mode
    "overwrite-mode" "gboolean" t t)
   (placeholder-text
    text-placeholder-text
    "placeholder-text" "gchararray" t t)
   (propagate-text-width
    text-propagate-text-width
    "propagate-text-width" "gboolean" t t)
   (scroll-offset
    text-scroll-offset
    "scroll-offset" "gint" t nil)
   (tabs
    text-tabs
    "tabs" "PangoTabArray" t t)
   (truncate-multiline
    text-truncate-multiline
    "truncate-multiline" "gboolean" t t)
   (visibility
    text-visibility
    "visibility" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'text 'type)
 "@version{#2023-3-12}
  @begin{short}
    The @sym{gtk:text} widget is a single line text entry widget.
  @end{short}
  A fairly large set of key bindings are supported by default. If the entered
  text is longer than the allocation of the widget, the widget will scroll so
  that the cursor position is visible.

  When using an entry for passwords and other sensitive information, it can be
  put into \"password mode\" using the @fun{gtk:text-visibility} function. In
  this mode, entered text is displayed using a \"invisible\" character. By
  default, GTK picks the best invisible character that is available in the
  current font, but it can be changed with the @fun{gtk:text-invisible-char}
  function.

  If you are looking to add icons or progress display in an entry, look at
  the @class{gtk:entry} widget. There other alternatives for more specialized
  use cases, such as the @class{gtk:search-entry} widget.

  If you need multi-line editable text, look at the @class{gtk:text-view}
  widget.
  @begin[CSS nodes]{dictionary}
    @begin{pre}
text[.read-only]
├── placeholder
├── undershoot.left
├── undershoot.right
├── [selection]
├── [block-cursor]
╰── [window.popup]
    @end{pre}
    The @sym{gtk:text} implementation has a main node with the name text.
    Depending on the properties of the widget, the @code{.read-only} style class
    may appear.

    When the entry has a selection, it adds a subnode with the name
    @code{selection}.

    When the entry is in overwrite mode, it adds a subnode with the name
    @code{block-cursor} that determines how the block cursor is drawn.

    The CSS node for a context menu is added as a subnode below @code{text} as
    well.

    The undershoot nodes are used to draw the underflow indication when content
    is scrolled out of view. These nodes get the @code{.left} and @code{.right}
    style classes added depending on where the indication is drawn.

    When touch is used and touch selection handles are shown, they are using CSS
    nodes with name @code{cursor-handle}. They get the @code{.top} or
    @code{.bottom} style class depending on where they are shown in relation to
    the selection. If there is just a single handle for the text cursor, it gets
    the @code{.insertion-cursor} style class.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @sym{gtk:text} implementation uses the @code{:none} role of the
    @symbol{gtk:accessible-role}, which causes it to be skipped for
    accessibility. This is because the @sym{gtk:text} implementation is expected
    to be used as a delegate for a @class{gtk:editable} implementation that will
    be represented to accessibility.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
lambda (entry)    :action
      @end{pre}
      The signal is emitted when the user hits the @kbd{Enter} key. The default
      bindings for this signal are all forms of the @kbd{Enter} key.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk:text} widget on which the signal is emitted.}
      @end{table}
    @subheading{The \"backspace\" signal}
      @begin{pre}
lambda (entry)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted when the user asks
      for it. The default bindings for this signal are the @kbd{Backspace} and
      @kbd{Shift-Backspace} keys.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk:text} widget which received the signal.}
      @end{table}
    @subheading{The \"copy-clipboard\" signal}
      @begin{pre}
lambda (entry)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted to copy the selection
      to the clipboard. The default bindings for this signal are the
      @kbd{Ctrl-c} and @kbd{Ctrl-Insert} keys.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk:text} widget which received the signal.}
      @end{table}
    @subheading{The \"cut-clipboard\" signal}
      @begin{pre}
lambda (entry)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted to cut the selection
      to the clipboard. The default bindings for this signal are the
      @kbd{Ctrl-x} and @kbd{Shift-Delete} keys.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk:text} widget which received the signal.}
      @end{table}
    @subheading{The \"delete-from-cursor\" signal}
      @begin{pre}
lambda (entry type count)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted when the user
      initiates a text deletion. If the type is @code{:chars}, @code{:delete}
      GTK deletes the selection if there is one, otherwise it deletes the
      requested number of characters. The default bindings for this signal are
      the @kbd{Delete} key for deleting a character and the @kbd{Ctrl-Delete}
      key for deleting a word.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk:text} widget which received the signal.}
        @entry[type]{The granularity of the deletion, as a value of the
          @symbol{gtk:delete-type} enumeration.}
        @entry[count]{An integer with the number of type units to delete.}
      @end{table}
    @subheading{The \"insert-at-cursor\" signal}
      @begin{pre}
lambda (entry string)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted when the user
      initiates the insertion of a fixed string at the cursor. This signal has
      no default bindings.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk:text} widget which received the signal.}
        @entry[string]{The string to insert.}
      @end{table}
    @subheading{The \"insert-emoji\" signal}
      @begin{pre}
lambda (entry)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted to present the Emoji
      chooser for the text entry field. The default bindings for this signal are
      the @kbd{Ctrl-.} and @kbd{Ctrl-;} keys.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk:text} widget which received the signal.}
      @end{table}
    @subheading{The \"move-cursor\" signal}
      @begin{pre}
lambda (entry step count extend)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted when the user
      initiates a cursor movement. If the cursor is not visible in the text
      entry field, this signal causes the viewport to be moved instead.
      Applications should not connect to it, but may emit it with the
      @fun{g:signal-emit} function if they need to control the cursor
      programmatically. The default bindings for this signal come in two
      variants, the variant with the @kbd{Shift} modifier extends the selection,
      the variant without the @kbd{Shift} modifier does not. There are too many
      key combinations to list them all here. Arrow keys move by individual
      characters/lines. @kbd{Ctrl}-arrow key combinations move by
      words/paragraphs. @kbd{Home}/@kbd{End} keys move to the ends of the
      buffer.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk:text} widget which received the signal.}
        @entry[step]{The granularity of the move, as a value of the
        @symbol{gtk:movement-step} enumeration.}
        @entry[count]{An integer with the number of step units to move.}
        @entry[extend]{@em{True} if the move should extend the selection.}
      @end{table}
    @subheading{The \"paste-clipboard\" signal}
      @begin{pre}
lambda (entry)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted to paste the contents
      of the clipboard into the text view. The default bindings for this signal
      are the @kbd{Ctrl-v} and @kbd{Shift-Insert} keys.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk:text} widget which received the signal.}
      @end{table}
    @subheading{The \"preedit-changed\" signal}
      @begin{pre}
lambda (entry preedit)    :action
      @end{pre}
      If an input method is used, the typed text will not immediately be
      committed to the buffer. So if you are interested in the text, connect to
      this signal.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk:text} widget which received the signal.}
        @entry[preedit]{The current preedit string.}
      @end{table}
    @subheading{The \"toggle-overwrite\" signal}
      @begin{pre}
lambda (entry)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted to toggle the
      overwrite mode of the text entry field. The default bindings for this
      signal is the @kbd{Insert} key.
      @begin[code]{table}
        @entry[entry]{The @sym{gtk:text} widget which received the signal.}
      @end{table}
  @end{dictionary}
  @begin[Action Details]{dictionary}
    @subheading{The \"menu.popup\" action}
      Opens the context menu.

    @subheading{The \"text.redo\" action}
      Redoes the last change to the contents.

    @subheading{The \"text.undo\" action}
      Undoes the last change to the contents.

    @subheading{The \"misc.toggle-visibility\" action}
      Toggles the “visibility” property.

    @subheading{The \"misc.insert-emoji\" action}
      Opens the Emoji chooser.

    @subheading{The \"selection.select-all\" action}
      Selects all of the widgets content.

    @subheading{The \"selection.delete\" action}
      Deletes the current selection.

    @subheading{The \"clipboard.paste\" action}
      Inserts the contents of the clipboard into the widget.

    @subheading{The \"clipboard.copy\" action}
      Copies the contents to the clipboard.

    @subheading{The \"clipboard.cut\" action}
      Copies the contents to the clipboard and deletes it from the widget.
  @end{dictionary}
  @see-constructor{gtk:text-new}
  @see-constructor{gtk:text-new-with-buffer}
  @see-slot{gtk:text-activates-default}
  @see-slot{gtk:text-attributes}
  @see-slot{gtk:text-buffer}
  @see-slot{gtk:text-enable-emoji-completion}
  @see-slot{gtk:text-extra-menu}
  @see-slot{gtk:text-im-module}
  @see-slot{gtk:text-input-hints}
  @see-slot{gtk:text-input-purpose}
  @see-slot{gtk:text-invisible-char}
  @see-slot{gtk:text-invisible-char-set}
  @see-slot{gtk:text-max-length}
  @see-slot{gtk:text-overwrite-mode}
  @see-slot{gtk:text-placeholder-text}
  @see-slot{gtk:text-propagate-text-width}
  @see-slot{gtk:text-scroll-offset}
  @see-slot{gtk:text-tabs}
  @see-slot{gtk:text-truncate-multiline}
  @see-slot{gtk:text-visibility}
  @see-class{gtk:entry}
  @see-class{gtk:text-view}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- text-activates-default -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "activates-default" 'text) t)
 "The @code{activates-default} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to activate the default widget, such as the default button in a
  dialog, when @kbd{Enter} is pressed. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-activates-default)
      "Accessor"
      (documentation 'text-activates-default 'function)
 "@version{#2023-3-12}
  @syntax[]{(gtk:text-activates-default object) => setting}
  @syntax[]{(setf (gtk:text-activates-default object) setting)}
  @argument[object]{a @class{gtk:text} widget}
  @argument[setting]{a boolean whether to activate the default widget, when
    @kbd{Enter} is pressed}
  @begin{short}
    Accessor of the @slot[gtk:text]{activates-default} slot of the
    @class{gtk:text} class.
  @end{short}
  If the @slot[gtk:text]{activates-default} property is @em{true}, pressing
  @kbd{Enter} in the text entry field will activate the default widget for the
  window containing the text entry field. This usually means that the dialog box
  containing the text entry field will be closed, since the default widget is
  usually one of the dialog buttons.
  @see-class{gtk:text}")

;;; --- text-attributes --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "attributes" 'text) t)
 "The @code{attributes} property of type @class{pango:attr-list} (Read / Write)
  @br{}
  A list of Pango attributes to apply to the text of the text entry field. This
  is mainly useful to change the size or weight of the text. The
  @code{start_index} and @code{end_index} of the @class{pango:attribute}
  structure must refer to the text of the @class{gtk:entry-buffer} object, i.e.
  without the preedit string.")

#+liber-documentation
(setf (liber:alias-for-function 'text-attributes)
      "Accessor"
      (documentation 'text-attributes 'function)
 "@version{#2023-3-12}
  @syntax[]{(gtk:text-attributes object) => attributes}
  @syntax[]{(setf (gtk:text-attributes object) attributes)}
  @argument[object]{a @class{gtk:text} widget}
  @argument[attributes]{a @class{pango:attr-list} instance}
  @begin{short}
    Accessor of the @slot[gtk:text]{attributes} slot of the @class{gtk:text}
    class.
  @end{short}
  The @sym{gtk:text-attributes} function gets the attribute list that was set on
  the text entry field, if any. The @sym{(setf gtk:text-attributes)} function
  sets a attribute list. The attributes in the list are applied to the text.
  @see-class{gtk:text}
  @see-class{pango:attr-list}")

;;; --- text-buffer ------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "buffer" 'text) t)
 "The @code{buffer} property of type @class{gtk:entry-buffer} (Read / Write)
  @br{}
  Text buffer object which actually stores the text of the text entry filed.")

#+liber-documentation
(setf (liber:alias-for-function 'text-buffer)
      "Accessor"
      (documentation 'text-buffer 'function)
 "@version{#2023-3-12}
  @syntax[]{(gtk:text-buffer object) => buffer}
  @syntax[]{(setf (gtk:text-buffer object) buffer)}
  @argument[object]{a @class{gtk:text} widget}
  @argument[buffer]{a @class{gtk:entry-buffer} object}
  @begin{short}
    Accessor of the @slot[gtk:text]{buffer} slot of the @class{gtk:text} class.
  @end{short}
  The @sym{gtk:text-buffer} function gets the @class{gtk:entry-buffer} object
  which holds the text for the text entry field. The
  @sym{(setf gtk:text-buffer)} function sets the @class{gtk:entry-buffer}
  object.
  @see-class{gtk:text}
  @see-class{gtk:entry-buffer}")

;;; --- text-enable-emoji-completion -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "enable-emoji-completion"
                                               'text) t)
 "The @code{enable-emoji-completion} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to suggest Emoji replacements. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-enable-emoji-completion)
      "Accessor"
      (documentation 'text-enable-emoji-completion 'function)
 "@version{#2023-3-12}
  @syntax[]{(gtk:text-enable-emoji-completion object) => setting}
  @syntax[]{(setf (gtk:text-enable-emoji-completion object) setting)}
  @argument[object]{a @class{gtk:text} widget}
  @argument[setting]{a boolean whether to enable Emoji completion}
  @begin{short}
    Accessor of the @slot[gtk:text]{enable-emoji-completion} slot of the
    @class{gtk:text} class.
  @end{short}
  The @sym{gtk:text-enable-emoji-completion} function returns whether Emoji
  completion is enabled for text entry field. The
  @sym{(setf gtk:text-enable-emoji-completion)} function sets whether Emoji
  completion is enabled. If it is, typing ':', followed by a recognized keyword,
  will pop up a window with suggested Emojis matching the keyword.
  @see-class{gtk:text}")

;;; --- text-extra-menu --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "extra-menu" 'text) t)
 "The @code{extra-menu} property of type @class{g:menu-model} (Read / Write)
  @br{}
  A menu model whose contents will be appended to the context menu.")

#+liber-documentation
(setf (liber:alias-for-function 'text-extra-menu)
      "Accessor"
      (documentation 'text-extra-menu 'function)
 "@version{#2023-3-12}
  @syntax[]{(gtk:text-extra-menu object) => menu}
  @syntax[]{(setf (gtk:text-extra-menu object) menu)}
  @argument[object]{a @class{gtk:text} widget}
  @argument[menu]{a @class{g:menu-model} object}
  @begin{short}
    Accessor of the @slot[gtk:text]{extra-menu} slot of the @class{gtk:text}
    class.
  @end{short}
  The @sym{gtk:text-extra-menu} function gets the menu model. The
  @sym{(setf gtk:text-extra-menu)} function sets a menu model to add when
  constructing the context menu for the text entry field.
  @see-class{gtk:text}
  @see-class{g:menu-model}")

;;; --- text-im-module ---------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "im-module" 'text) t)
 "The @code{im-module} property of type @code{:string} (Read / Write) @br{}
  Which IM (input method) module should be used for the text entry field. See
  the @class{gtk:im-context} documentation. Setting this to a non-@code{nil}
  value overrides the system-wide IM module setting. See the
  @slot[gtk:settings]{gtk-im-module} property. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'text-im-module)
      "Accessor"
      (documentation 'text-im-module 'function)
 "@version{#2023-3-12}
  @syntax[]{(gtk:text-im-module object) => module}
  @syntax[]{(setf (gtk:text-im-module object) module)}
  @argument[object]{a @class{gtk:text} widget}
  @argument[module]{a string with the IM (input method) module}
  @begin{short}
    Accessor of the @slot[gtk:text]{im-module} slot of the @class{gtk:text}
    class.
  @end{short}
  Which IM (input method) module should be used for the text entry field. See
  the @class{gtk:im-context} documentation. Setting this to a non-@code{nil}
  value overrides the system-wide IM module setting. See the
  @slot[gtk:settings]{gtk-im-module} property.
  @see-class{gtk:text}
  @see-class{gtk:im-context}
  @see-function{gtk:settings-gtk-im-module}")

;;; --- text-input-hints -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "input-hints" 'text) t)
 "The @code{input-hints} property of type @symbol{gtk:input-hints}
  (Read / Write) @br{}
  Additional hints, beyond the @code{input-purpose} property, that allow input
  methods to fine-tune their behaviour.")

#+liber-documentation
(setf (liber:alias-for-function 'text-input-hints)
      "Accessor"
      (documentation 'text-input-hints 'function)
 "@version{#2023-3-12}
  @syntax[]{(gtk:text-input-hints object) => hints}
  @syntax[]{(setf (gtk:text-input-hints object) hints)}
  @argument[object]{a @class{gtk:text} widget}
  @argument[module]{a @symbol{gtk:input-hints} value}
  @begin{short}
    Accessor of the @slot[gtk:text]{input-hints} slot of the @class{gtk:text}
    class.
  @end{short}
  The @sym{gtk:text-input-hints} function gets the value of the
  @slot[gtk:text]{input-hints} property, which allows input methods to fine-tune
  their behaviour. The @sym{(setf gtk:text-input-hints)} function sets the
  property.
  @see-class{gtk:text}
  @see-symbol{gtk:input-hints}")

;;; --- text-input-purpose -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "input-purpose" 'text) t)
 "The @code{input-purpose} property of type @symbol{gtk:input-purpose}
  (Read / Write) @br{}
  The purpose of the text entry field. The property can be used by on-screen
  keyboards and other input methods to adjust their behaviour. Note that setting
  the purpose to @code{:password} or @code{:pin} is independent from setting
  the @code{visibility} property. @br{}
  Default value: @code{:free-from}")

#+liber-documentation
(setf (liber:alias-for-function 'text-input-purpose)
      "Accessor"
      (documentation 'text-input-purpose 'function)
 "@version{#2023-3-12}
  @syntax[]{(gtk:text-input-purpose object) => purpose}
  @syntax[]{(setf (gtk:text-input-purpose object) purpose)}
  @argument[object]{a @class{gtk:text} widget}
  @argument[purpose]{a @symbol{gtk:input-purpose} value}
  @begin{short}
    Accessor of the @slot[gtk:text]{input-purpose} slot of the @class{gtk:text}
    class.
  @end{short}
  The @sym{gtk:text-input-purpose} function gets the value of the
  @slot[gtk:text]{input-purpose} property, which can be used by on-screen
  keyboards and other input methods to adjust their behaviour. The
  @sym{(setf gtk:text-input-purpose)} function sets the property.
  @see-class{gtk:text}
  @see-symbol{gtk:input-purpose}")

;;; --- text-invisible-char ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "invisible-char" 'text) t)
 "The @code{invisible-char} property of type @code{:uint} (Read / Write) @br{}
  The character to use when masking the content of the text entry field in
  \"password mode\". @br{}
  Default value: @code{#\\*}")

#+liber-documentation
(setf (liber:alias-for-function 'text-invisible-char)
      "Accessor"
      (documentation 'text-invisible-char 'function)
 "@version{#2023-3-12}
  @syntax[]{(gtk:text-invisible-char object) => char}
  @syntax[]{(setf (gtk:text-invisible-char object) char)}
  @argument[object]{a @class{gtk:text} widget}
  @argument[char]{an unsigned integer with an Unicode character}
  @begin{short}
    Accessor of the @slot[gtk:text]{invisible-char} slot of the @class{gtk:text}
    class.
  @end{short}
  The @sym{gtk:text-invisible-char} function retrieves the character displayed
  in place of the real characters for entries with visibility set to false. Note
  that GTK does not compute this value unless it needs it, so the value returned
  by this function is not very useful unless it has been explicitly set. The
  @sym{(setf gtk:text-invisible-char)} function sets the character.

  By default, GTK picks the best invisible char available in the current font.
  If you set the invisible char to 0, then the user will get no feedback at all.
  There will be no text on the screen as they type.
  @see-class{gtk:text}")

;;; --- text-invisible-char-set ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "invisible-char-set" 'text) t)
 "The @code{invisible-char-set} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the invisible char has been set for the text entry field. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-invisible-char-set)
      "Accessor"
      (documentation 'text-invisible-char-set 'function)
 "@version{#2023-3-12}
  @syntax[]{(gtk:text-invisible-char-set object) => setting}
  @syntax[]{(setf (gtk:text-invisible-char-set object) setting)}
  @argument[object]{a @class{gtk:text} widget}
  @argument[setting]{a boolean whether the invisible char has been set}
  @begin{short}
    Accessor of the @slot[gtk:text]{invisible-char-set} slot of the
    @class{gtk:text} class.
  @end{short}
  Whether the invisible char has been set for the text entry field.
  @see-class{gtk:text}
  @see-function{gtk:text-invisible-char}")

;;; --- text-max-length ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "max-length" 'text) t)
 "The @code{max-length} property of type @code{:int} (Read / Write) @br{}
  Maximum number of characters for the text entry field. Zero if no maximum.
  @br{}
  Allowed values: [0, 65535] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'text-max-length)
      "Accessor"
      (documentation 'text-max-length 'function)
 "@version{#2023-3-12}
  @syntax[]{(gtk:text-max-length object) => length}
  @syntax[]{(setf (gtk:text-max-length object) length)}
  @argument[object]{a @class{gtk:text} widget}
  @argument[length]{an integer with the maximum length of the text entry field
    or 0 if there is no maximum}
  @begin{short}
    Accessor of the @slot[gtk:text]{max-length} slot of the @class{gtk:text}
    class.
  @end{short}
  The @sym{gtk:text-max-length} function retrieves the maximum allowed length
  of the text in the text entry field. This is equivalent to getting the
  @class{gtk:entry-buffer} object of the text entry field and calling the
  @fun{gtk:entry-buffer-max-length} function on it.

  The @sym{(setf gtk:text-max-length)} function sets the maximum allowed length.
  If the current contents are longer than the given length, then they will be
  truncated to fit. This is equivalent to getting the @class{gtk:entry-buffer}
  object of the text entry field and calling the
  @sym{(setf gtk:entry-buffer-max-length)} function on it.
  @see-class{gtk:text}
  @see-class{gtk:entry-buffer}
  @see-function{gtk:entry-buffer-max-length}")

;;; --- text-overwrite-mode ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "overwrite-mode" 'text) t)
 "The @code{overwrite-mode} property of type @code{:boolean} (Read / Write)
  @br{}
  If text is overwritten when typing in the text entry field. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-overwrite-mode)
      "Accessor"
      (documentation 'text-overwrite-mode 'function)
 "@version{#2023-3-12}
  @syntax[]{(gtk:text-overwrite-mode object) => setting}
  @syntax[]{(setf (gtk:text-overwrite-mode object) setting)}
  @argument[object]{a @class{gtk:text} widget}
  @argument[setting]{a boolean whether the text is overwritten when typing}
  @begin{short}
    Accessor of the @slot[gtk:text]{overwrite-mode} slot of the @class{gtk:text}
    class.
  @end{short}
  The @sym{gtk:text-overwrite-mode} function gets the value of the property. The
  @sym{(setf gtk:text-max-length)} function sets whether the text is overwritten
  when typing in the text entry field.
  @see-class{gtk:text}")

;;; --- text-placeholder-text --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "placeholder-text" 'text) t)
 "The @code{placeholder-text} property of type @code{:string} (Read / Write)
  @br{}
  The text that will be displayed in the text entry field when it is empty and
  unfocused. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'text-placeholder-text)
      "Accessor"
      (documentation 'text-placeholder-text 'function)
 "@version{#2023-3-12}
  @syntax[]{(gtk:text-placeholder-text object) => text}
  @syntax[]{(setf (gtk:text-placeholder-text object) text)}
  @argument[object]{a @class{gtk:text} widget}
  @argument[text]{a string to be displayed when the text entry field is empty
    and unfocused, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:text]{placeholder-text} slot of the
    @class{gtk:text} class.
  @end{short}
  The @sym{gtk:text-placeholder-text} function retrieves the text that will be
  displayed when the text entry field is empty and unfocused. The
  @sym{(setf gtk:text-placeholder-text)} function sets text to be displayed.
  This can be used to give a visual hint of the expected contents of the text
  entry field.
  @see-class{gtk:text}")

;;; --- text-propagate-text-width ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "propagate-text-width" 'text) t)
 "The @code{propagate-text-width} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the text entry field should grow and shrink with the content. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-propagate-text-width)
      "Accessor"
      (documentation 'text-propagate-text-width 'function)
 "@version{#2023-3-12}
  @syntax[]{(gtk:text-propagate-text-width object) => setting}
  @syntax[]{(setf (gtk:text-propagate-text-width object) setting)}
  @argument[object]{a @class{gtk:text} widget}
  @argument[setting]{a boolean whether the text entry field should grow and
    shrink with the content}
  @begin{short}
    Accessor of the @slot[gtk:text]{propagate-text-width} slot of the
    @class{gtk:text} class.
  @end{short}

  The @sym{gtk:text-propagate-text-width} function returns whether the text
  entry field will grow and shrink with the content. The
  @sym{(setf gtk:text-propagate-text-width)} function sets whether the text
  entry field will grow and shrink with the content.
  @see-class{gtk:text}")

;;; --- text-scroll-offset -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "scroll-offset" 'text) t)
 "The @code{scroll-offset} property of type @code{:int} (Read) @br{}
  Number of pixels the text entry field scrolled off the screen to the left.
  @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'text-scroll-offset)
      "Accessor"
      (documentation 'text-scroll-offset 'function)
 "@version{#2023-3-12}
  @syntax[]{(gtk:text-scroll-offset object) => offset}
  @syntax[]{(setf (gtk:text-scroll-offset object) offset)}
  @argument[object]{a @class{gtk:text} widget}
  @argument[offset]{an integer with the number of pixels the text entry field
    scrolled off the screen}
  @begin{short}
    Accessor of the @slot[gtk:text]{scroll-offset} slot of the @class{gtk:text}
    class.
  @end{short}
  Number of pixels the text entry field scrolled off the screen to the left.
  @see-class{gtk:text}")

;;; --- text-tabs --------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tabs" 'text) t)
 "The @code{tabs} property of type @class{pango:tab-array} (Read / Write) @br{}
  A list of tabstops to apply to the text of the text entry field.")

#+liber-documentation
(setf (liber:alias-for-function 'text-tabs)
      "Accessor"
      (documentation 'text-tabs 'function)
 "@version{#2023-3-12}
  @syntax[]{(gtk:text-tabs object) => tabs}
  @syntax[]{(setf (gtk:text-tabs object) tabs)}
  @argument[object]{a @class{gtk:text} widget}
  @argument[tabs]{a @class{pango:tab-array} instance with the tabstops}
  @begin{short}
    Accessor of the @slot[gtk:text]{tabs} slot of the @class{gtk:text} class.
  @end{short}
  The @sym{gtk:text-tabs} function gets the tabstops that were set on the text
  entry field, if any. The @sym{(setf gtk:text-tabs)} function sets a
  @class{pango:tab-array} instance. The tabstops in the array are applied to
  the text in the text entry field.
  @see-class{gtk:text}
  @see-class{pango:tab-array}")

;;;  --- text-truncate-multiline -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "truncate-multiline" 'text) t)
 "The @code{multiline-truncate} property of type @code{:boolean} (Read / Write)
  @br{}
  When @em{true}, pasted multi-line text is truncated to the first line. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'text-truncate-multiline)
      "Accessor"
      (documentation 'text-truncate-multiline 'function)
 "@version{#2023-3-12}
  @syntax[]{(gtk:text-truncate-multiline object) => setting}
  @syntax[]{(setf (gtk:text-truncate-multiline object) setting)}
  @argument[object]{a @class{gtk:text} widget}
  @argument[setting]{a boolean whether the text entry field will truncate
    multi-line text}
  @begin{short}
    Accessor of the @slot[gtk:text]{truncate-multiline} slot of the
    @class{gtk:text} class.
  @end{short}
  The @sym{gtk:text-truncate-multiline} function returns whether the text
  entry field will truncate multi-line text that is pasted into the widget. The
  @sym{(setf gtk:text-truncate-multiline)} function sets whether the text entry
  field should truncate multi-line text.
  @see-class{gtk:text}")

;;; --- text-visibility --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "visibility" 'text) t)
 "The @code{visibility} property of type @code{:boolean} (Read / Write) @br{}
  When @em{false} displays the \"invisible char\" instead of the actual text
  (password mode). @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'text-visibility)
      "Accessor"
      (documentation 'text-visibility 'function)
 "@version{#2023-3-12}
  @syntax[]{(gtk:text-visibility object) => visible}
  @syntax[]{(setf (gtk:text-visibility object) visible)}
  @argument[object]{a @class{gtk:text} widget}
  @argument[visible]{a boolean whether the text is visible}
  @begin{short}
    Accessor of the @slot[gtk:text]{visibility} slot of the @class{gtk:text}
    class.
  @end{short}
  The @sym{gtk:text-visibility} function retrieves whether the text in the
  text entry field is visible. The @sym{(setf gtk:text-visibility)} function
  sets whether the contents of the text entry field are visible or not. When
  visibility is set to @em{false}, characters are displayed as the invisible
  char, and will also appear that way when the text in the text entry field is
  copied to the clipboard.

  By default, GTK picks the best invisible character available in the current
  font, but it can be changed with the @fun{gtk:text-invisible-char} function.

  Note that you probably want to set @slot[gtk:text]{input-purpose} property to
  the @code{:password} or @code{:pin} value to inform input methods about the
  purpose of the text entry field, in addition to setting the @code{visibility}
  property to @em{false}.
  @see-class{gtk:text}
  @see-function{gtk:text-invisible-char}
  @see-function{gtk:text-input-purpose}")

;;; ----------------------------------------------------------------------------
;;; gtk_text_new ()
;;; ----------------------------------------------------------------------------

(defun text-new ()
 #+liber-documentation
 "@version{#2023-3-12}
  @return{A new @class{gtk:text} widget.}
  @short{Creates a new text widget.}
  @see-class{gtk:text}
  @see-function{gtk:text-new-with-buffer}"
  (make-instance 'text))

(export 'text-new)

;;; ----------------------------------------------------------------------------
;;; gtk_text_new_with_buffer ()
;;; ----------------------------------------------------------------------------

(defun text-new-with-buffer (buffer)
 #+liber-documentation
 "@version{#2023-3-12}
  @argument[buffer]{a @class{gtk:entry-buffer} object}
  @return{A new @class{gtk:text} widget.}
  @short{Creates a new text widget with the specified entry buffer.}
  @see-class{gtk:text}
  @see-class{gtk:entry-buffer}
  @see-function{gtk:text-new}"
  (make-instance 'text
                 :buffer buffer))

(export 'text-new-with-buffer)

;;; ----------------------------------------------------------------------------
;;; gtk_text_unset_invisible_char ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_unset_invisible_char" text-unset-invisible-char) :void
 #+liber-documentation
 "@version{#2023-3-12}
  @argument[entry]{a @class{gtk:text} widget}
  @begin{short}
    Unsets the invisible char previously set with the
    @fun{gtk:text-invisible-char} function, so that the default invisible char
    is used again.
  @end{short}
  @see-class{gtk:text}
  @see-function{gtk:text-invisible-char}"
  (entry (g:object text)))

(export 'text-unset-invisible-char)

;;; ----------------------------------------------------------------------------
;;; gtk_text_get_text_length ()
;;; ----------------------------------------------------------------------------

(defun text-text-length (entry)
 #+liber-documentation
 "@version{#2023-3-12}
  @argument[entry]{a @class{gtk:text} widget}
  @return{An unsigned integer with the current number of characters in the
    @class{gtk:text} widget.}
  @begin{short}
    Retrieves the current length of the text in @arg{entry}.
  @end{short}
  This is equivalent to getting the @class{gtk:entry-buffer} object of the
  text widget and calling the @fun{gtk:entry-buffer-length} function on it.
  @see-class{gtk:text}
  @see-class{gtk:entry-buffer}
  @see-function{gtk:entry-buffer-length}"
  (entry-buffer-length (text-buffer entry)))

(export 'text-text-length)

;;; ----------------------------------------------------------------------------
;;; gtk_text_grab_focus_without_selecting ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_text_grab_focus_without_selecting"
               text-grab-focus-without-selecting) :boolean
 #+liber-documentation
 "@version{#2023-3-12}
  @argument[entry]{a @class{gtk:text} widget}
  @return{@em{True} if focus is inside @arg{entry}.}
  @begin{short}
    Causes the text widget to have keyboard focus.
  @end{short}
  It behaves likes the @fun{gtk:widget-grab-focus} function, except that it
  does not select the contents of @arg{entry}. You only want to call this on
  some special entries which the user usually does not want to replace all text
  in, such as search-as-you-type entries.
  @see-class{gtk:text}
  @see-function{gtk:widget-grab-focus}"
  (entry (g:object text)))

(export 'text-grab-focus-without-selecting)

;;; --- End of file gtk4.text.lisp ---------------------------------------------
