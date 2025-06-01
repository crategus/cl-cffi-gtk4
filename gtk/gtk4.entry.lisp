;;; ----------------------------------------------------------------------------
;;; gtk4.entry.lisp
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
;;; GtkEntry
;;;
;;;     A single line text entry
;;;
;;; Types and Values
;;;
;;;     GtkEntryIconPosition
;;;     GtkInputPurpose                                 -> gtk.enumerations.lisp
;;;     GtkInputHints                                   -> gtk.enumerations.lisp
;;;
;;;     GtkEntry
;;;
;;; Accessors
;;;
;;;     gtk_entry_set_activates_default
;;;     gtk_entry_get_activates_default
;;;     gtk_entry_set_attributes
;;;     gtk_entry_get_attributes
;;;     gtk_entry_get_buffer
;;;     gtk_entry_set_buffer
;;;     gtk_entry_set_completion                            Deprecated 4.10
;;;     gtk_entry_get_completion                            Deprecated 4.10
;;;     gtk_entry_set_extra_menu
;;;     gtk_entry_get_extra_menu
;;;     gtk_entry_set_has_frame
;;;     gtk_entry_get_has_frame
;;;     gtk_entry_set_input_hints
;;;     gtk_entry_get_input_hints
;;;     gtk_entry_set_input_purpose
;;;     gtk_entry_get_input_purpose
;;;     gtk_entry_set_invisible_char
;;;     gtk_entry_get_invisible_char
;;;     gtk_entry_set_max_length
;;;     gtk_entry_get_max_length
;;;     gtk_entry_set_overwrite_mode
;;;     gtk_entry_get_overwrite_mode
;;;     gtk_entry_set_placeholder_text
;;;     gtk_entry_get_placeholder_text
;;;     gtk_entry_set_progress_fraction
;;;     gtk_entry_get_progress_fraction
;;;     gtk_entry_set_progress_pulse_step
;;;     gtk_entry_get_progress_pulse_step
;;;     gtk_entry_set_tabs
;;;     gtk_entry_get_tabs
;;;     gtk_entry_get_text_length
;;;     gtk_entry_set_visibility
;;;     gtk_entry_get_visibility
;;;
;;; Functions
;;;
;;;     gtk_entry_new
;;;     gtk_entry_new_with_buffer
;;;     gtk_entry_unset_invisible_char
;;;     gtk_entry_set_alignment
;;;     gtk_entry_get_alignment
;;;     gtk_entry_progress_pulse
;;;     gtk_entry_reset_im_context
;;;     gtk_entry_set_icon_from_paintable
;;;     gtk_entry_set_icon_from_icon_name
;;;     gtk_entry_set_icon_from_gicon
;;;     gtk_entry_get_icon_storage_type
;;;     gtk_entry_get_icon_paintable
;;;     gtk_entry_get_icon_name
;;;     gtk_entry_get_icon_gicon
;;;     gtk_entry_set_icon_activatable
;;;     gtk_entry_get_icon_activatable
;;;     gtk_entry_set_icon_sensitive
;;;     gtk_entry_get_icon_sensitive
;;;     gtk_entry_get_icon_at_pos
;;;     gtk_entry_set_icon_tooltip_text
;;;     gtk_entry_get_icon_tooltip_text
;;;     gtk_entry_set_icon_tooltip_markup
;;;     gtk_entry_get_icon_tooltip_markup
;;;     gtk_entry_set_icon_drag_source
;;;     gtk_entry_get_current_icon_drag_source
;;;     gtk_entry_get_icon_area
;;;     gtk_entry_grab_focus_without_selecting
;;;
;;; Properties
;;;
;;;     activates-default
;;;     attributes
;;;     buffer
;;;     completion                                          Deprecated 4.10
;;;     enable-emoji-completion
;;;     extra-menu
;;;     has-frame
;;;     im-module
;;;     input-hints
;;;     input-purpose
;;;     invisible-char
;;;     invisible-char-set
;;;     max-length
;;;     overwrite-mode
;;;     placeholder-text
;;;     primary-icon-activatable
;;;     primary-icon-gicon
;;;     primary-icon-name
;;;     primary-icon-paintable
;;;     primary-icon-sensitive
;;;     primary-icon-storage-type
;;;     primary-icon-tooltip-markup
;;;     primary-icon-tooltip-text
;;;     progress-fraction
;;;     progress-pulse-step
;;;     scroll-offset
;;;     secondary-icon-activatable
;;;     secondary-icon-gicon
;;;     secondary-icon-name
;;;     secondary-icon-paintable
;;;     secondary-icon-sensitive
;;;     secondary-icon-storage-type
;;;     secondary-icon-tooltip-markup
;;;     secondary-icon-tooltip-text
;;;     show-emoji-icon
;;;     tabs
;;;     text-length
;;;     truncate-multiline
;;;     visibility
;;;
;;; Signals
;;;
;;;     activate
;;;     icon-press
;;;     icon-release
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkEntry
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkEditable
;;;     GtkCellEditable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkEntryIconPosition
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkEntryIconPosition" entry-icon-position
  (:export t
   :type-initializer "gtk_entry_icon_position_get_type")
  (:primary 0)
  (:secondary 1))

#+liber-documentation
(setf (liber:alias-for-symbol 'entry-icon-position)
      "GEnum"
      (liber:symbol-documentation 'entry-icon-position)
 "@version{2025-05-31}
  @begin{declaration}
(gobject:define-genum \"GtkEntryIconPosition\" entry-icon-position
  (:export t
   :type-initializer \"gtk_entry_icon_position_get_type\")
  (:primary 0)
  (:secondary 1))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:primary]{At the beginning of the text entry, depending on the
        text direction.}
      @entry[:secondary]{At the end of the text entry, depending on the text
        direction.}
    @end{table}
  @end{values}
  @begin{short}
    Specifies the side of the text entry at which an icon is placed.
  @end{short}
  @see-class{gtk:entry}")

;;; ----------------------------------------------------------------------------
;;; GtkEntry
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkEntry" entry
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkEditable"
                "GtkCellEditable")
   :type-initializer "gtk_entry_get_type")
  ((activates-default
    entry-activates-default
    "activates-default" "gboolean" t t)
   (attributes
    entry-attributes
    "attributes" "PangoAttrlist" t t)
   (buffer
    entry-buffer
    "buffer" "GtkEntryBuffer" t t)
   (completion
    entry-completion
    "completion" "GtkEntryCompletion" t t)
   (enable-emoji-completion
    entry-enable-emoji-completion
    "enable-emoji-completion" "gboolean" t t)
   (extra-menu
    entry-extra-menu
    "extra-menu" "GMenuModel" t t)
   (has-frame
    entry-has-frame
    "has-frame" "gboolean" t t)
   (im-module
    entry-im-module
    "im-module" "gchararray" t t)
   (input-hints
    entry-input-hints
    "input-hints" "GtkInputHints" t t)
   (input-purpose
    entry-input-purpose
    "input-purpose" "GtkInputPurpose" t t)
   (invisible-char
    entry-invisible-char
    "invisible-char" "guint" t t)
   (invisible-char-set
    entry-invisible-char-set
    "invisible-char-set" "gboolean" t t)
   (max-length
    entry-max-length
    "max-length" "gint" t t)
   (overwrite-mode
    entry-overwrite-mode
    "overwrite-mode" "gboolean" t t)
   (placeholder-text
    entry-placeholder-text
    "placeholder-text" "gchararray" t t)
   (primary-icon-activatable
    entry-primary-icon-activatable
    "primary-icon-activatable" "gboolean" t t)
   (primary-icon-gicon
    entry-primary-icon-gicon
    "primary-icon-gicon" "GIcon" t t)
   (primary-icon-name
    entry-primary-icon-name
    "primary-icon-name" "gchararray" t t)
   (primary-icon-paintable
    entry-primary-icon-paintable
    "primary-icon-paintable" "GdkPaintable" t t)
   (primary-icon-sensitive
    entry-primary-icon-sensitive
    "primary-icon-sensitive" "gboolean" t t)
   (primary-icon-storage-type
    entry-primary-icon-storage-type
    "primary-icon-storage-type" "GtkImageType" t nil)
   (primary-icon-tooltip-markup
    entry-primary-icon-tooltip-markup
    "primary-icon-tooltip-markup" "gchararray" t t)
   (primary-icon-tooltip-text
    entry-primary-icon-tooltip-text
    "primary-icon-tooltip-text" "gchararray" t t)
   (progress-fraction
    entry-progress-fraction
    "progress-fraction" "gdouble" t t)
   (progress-pulse-step
    entry-progress-pulse-step
    "progress-pulse-step" "gdouble" t t)
   (scroll-offset
    entry-scroll-offset
    "scroll-offset" "gint" t nil)
   (secondary-icon-activatable
    entry-secondary-icon-activatable
    "secondary-icon-activatable" "gboolean" t t)
   (secondary-icon-gicon
    entry-secondary-icon-gicon
    "secondary-icon-gicon" "GIcon" t t)
   (secondary-icon-name
    entry-secondary-icon-name
    "secondary-icon-name" "gchararray" t t)
   (secondary-icon-paintable
    entry-secondary-icon-paintable
    "secondary-icon-paintable" "GdkPaintable" t t)
   (secondary-icon-sensitive
    entry-secondary-icon-sensitive
    "secondary-icon-sensitive" "gboolean" t t)
   (secondary-icon-storage-type
    entry-secondary-icon-storage-type
    "secondary-icon-storage-type" "GtkImageType" t nil)
   (secondary-icon-tooltip-markup
    entry-secondary-icon-tooltip-markup
    "secondary-icon-tooltip-markup" "gchararray" t t)
   (secondary-icon-tooltip-text
    entry-secondary-icon-tooltip-text
    "secondary-icon-tooltip-text" "gchararray" t t)
   (show-emoji-icon
    entry-show-emoji-icon
    "show-emoji-icon" "gboolean" t t)
   (tabs
    entry-tabs
    "tabs" "PangoTabArray" t t)
   (text-length
    entry-text-length
    "text-length" "guint" t nil)
   (truncate-multiline
    entry-truncate-multiline
    "truncate-multiline" "gboolean" t t)
   (visibility
    entry-visibility
    "visibility" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'entry 'type)
 "@version{2025-05-31}
  @begin{short}
    The @class{gtk:entry} widget is a single line text entry.
  @end{short}
  A fairly large set of key bindings are supported by default. If the entered
  text is longer than the allocation of the widget, the widget will scroll so
  that the cursor position is visible.

  @image[entry]{Figure: GtkEntry}

  When using a text entry for passwords and other sensitive information, it can
  be put into \"password mode\" using the @fun{gtk:entry-visibility} function.
  In this mode, entered text is displayed using an 'invisible' character. By
  default, GTK picks the best invisible character that is available in the
  current font, but it can be changed with the @fun{gtk:entry-invisible-char}
  function.

  The @class{gtk:entry} widget has the ability to display progress or activity
  information behind the text. To make a text entry display such information,
  use the @fun{gtk:entry-progress-fraction} or
  @fun{gtk:entry-progress-pulse-step} functions.

  Additionally, the @class{gtk:entry} widget can show icons at either side of
  the text entry. These icons can be activatable by clicking, can be set up as
  drag source and can have tooltips. To add an icon, use the
  @fun{gtk:entry-set-icon-from-gicon} function or one of the various other
  functions that set an icon from an icon name or a paintable. To trigger an
  action when the user clicks an icon, connect to the @code{\"icon-press\"}
  signal. To allow DND operations from an icon, use the
  @fun{gtk:entry-set-icon-drag-source} function. To set a tooltip on an icon,
  use the @fun{gtk:entry-icon-tooltip-text} function or the corresponding
  function for markup.

  Note that functionality or information that is only available by clicking on
  an icon in an text entry may not be accessible at all to users which are not
  able to use a mouse or other pointing device. It is therefore recommended that
  such functionality is also available by other means, such as the context menu
  of the text entry.
  @begin[CSS nodes]{dictionary}
    @begin{pre}
entry[.flat][.warning][.error]
├── text[.readonly]
├── image.left
├── image.right
╰── [progress[.pulse]]
    @end{pre}
    The @class{gtk:entry} implementation has a main node with the name
    @code{entry}. Depending on the properties of the text entry, the
    @code{.read-only} and @code{.flat} style classes may appear. The
    @code{.warning} and @code{.error} style classes may also be used with
    entries.

    When the text entry shows icons, it adds subnodes with the name @code{image}
    and the @code{.left} or @code{.right} style class, depending on where the
    icon appears.

    When the text entry shows progress, it adds a subnode with the name
    progress. The node has the @code{.pulse} style class when the shown
    progress is pulsing.

    For all the subnodes added to the text node in various situations, see the
    @class{gtk:text} widget.
  @end{dictionary}
  @begin[GtkEntry as GtkBuildable]{dictionary}
    The @class{gtk:entry} implementation of the @class{gtk:buildable} interface
    supports a custom @code{<attributes>} element, which supports any number of
    @code{<attribute>} elements. The @code{<attribute>} element has attributes
    named @code{\"name\"}, @code{\"value\"}, @code{\"start\"} and @code{\"end\"}
    and allows you to specify @class{pango:attribute} values for this label.

    An example of a UI definition fragment specifying Pango attributes:
    @begin{pre}
<object class=\"GtkEntry\">
  <attributes>
    <attribute name=\"weight\" value=\"PANGO_WEIGHT_BOLD\"/>
    <attribute name=\"background\" value=\"red\" start=\"5\" end=\"10\"/>
  </attributes>
</object>
    @end{pre}
    The @code{start} and @code{end} attributes specify the range of characters
    to which the Pango attribute applies. If @code{start} and @code{end} are not
    specified, the attribute is applied to the whole text. Note that specifying
    ranges does not make much sense with translatable attributes. Use markup
    embedded in the translatable content instead.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:entry} implementation uses the @code{:text-box} role of the
    @symbol{gtk:accessible-role} enumeration.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
lambda (entry)    :action
      @end{pre}
      @begin[code]{table}
        @entry[entry]{The @class{gtk:entry} widget on which the signal is
          emitted.}
      @end{table}
      A keybinding signal which gets emitted when the user activates the text
      entry. Applications should not connect to it, but may emit it with the
      @fun{g:signal-emit} function if they need to control activation
      programmatically. The default bindings for this signal are all forms of
      the @kbd{Enter} key.
    @subheading{The \"icon-press\" signal}
      @begin{pre}
lambda (entry pos)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[entry]{The @class{gtk:entry} widget on which the signal is
          emitted.}
        @entry[pos]{The position of the clicked icon as a
          @symbol{gtk:entry-icon-position} value.}
      @end{table}
      The signal is emitted when an activatable icon is clicked.
    @subheading{The \"icon-release\" signal}
      @begin{pre}
lambda (entry pos)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[entry]{The @class{gtk:entry} widget on which the signal is
          emitted.}
        @entry[pos]{The position of the clicked icon as a
          @symbol{gtk:entry-icon-position} value.}
      @end{table}
      The signal is emitted on the button release from a mouse click over an
      activatable icon.
  @end{dictionary}
  @see-constructor{gtk:entry-new}
  @see-constructor{gtk:entry-new-with-buffer}
  @see-slot{gtk:entry-activates-default}
  @see-slot{gtk:entry-attributes}
  @see-slot{gtk:entry-buffer}
  @see-slot{gtk:entry-completion}
  @see-slot{gtk:entry-enable-emoji-completion}
  @see-slot{gtk:entry-extra-menu}
  @see-slot{gtk:entry-has-frame}
  @see-slot{gtk:entry-im-module}
  @see-slot{gtk:entry-input-hints}
  @see-slot{gtk:entry-input-purpose}
  @see-slot{gtk:entry-invisible-char}
  @see-slot{gtk:entry-invisible-char-set}
  @see-slot{gtk:entry-max-length}
  @see-slot{gtk:entry-overwrite-mode}
  @see-slot{gtk:entry-placeholder-text}
  @see-slot{gtk:entry-primary-icon-activatable}
  @see-slot{gtk:entry-primary-icon-gicon}
  @see-slot{gtk:entry-primary-icon-name}
  @see-slot{gtk:entry-primary-icon-paintable}
  @see-slot{gtk:entry-primary-icon-sensitive}
  @see-slot{gtk:entry-primary-icon-storage-type}
  @see-slot{gtk:entry-primary-icon-tooltip-markup}
  @see-slot{gtk:entry-primary-icon-tooltip-text}
  @see-slot{gtk:entry-progress-fraction}
  @see-slot{gtk:entry-progress-pulse-step}
  @see-slot{gtk:entry-scroll-offset}
  @see-slot{gtk:entry-secondary-icon-activatable}
  @see-slot{gtk:entry-secondary-icon-gicon}
  @see-slot{gtk:entry-secondary-icon-name}
  @see-slot{gtk:entry-secondary-icon-paintable}
  @see-slot{gtk:entry-secondary-icon-sensitive}
  @see-slot{gtk:entry-secondary-icon-storage-type}
  @see-slot{gtk:entry-secondary-icon-tooltip-markup}
  @see-slot{gtk:entry-secondary-icon-tooltip-text}
  @see-slot{gtk:entry-show-emoji-icon}
  @see-slot{gtk:entry-tabs}
  @see-slot{gtk:entry-text-length}
  @see-slot{gtk:entry-truncate-multiline}
  @see-slot{gtk:entry-visibility}
  @see-class{gtk:text-view}
  @see-class{gtk:entry-completion}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:entry-activates-default --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "activates-default" 'entry) t)
 "The @code{activates-default} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to activate the default widget, such as the default button in a
  dialog, when the @kbd{Enter} key is pressed. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-activates-default)
      "Accessor"
      (documentation 'entry-activates-default 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-activates-default object) => setting}
  @syntax{(setf (gtk:entry-activates-default object) setting)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[setting]{@em{true} to activate the default widget of the window on
    @kbd{Enter} keypress}
  @begin{short}
    Accessor of the @slot[gtk:entry]{activates-default} slot of the
    @class{gtk:entry} class.
  @end{short}
  The @fun{gtk:entry-activates-default} function retrieves whether to activate
  the default widget, when the @kbd{Enter} key is pressed.

  If the @arg{setting} argument is @em{true}, pressing the @kbd{Enter} key in
  the text entry will activate the default widget for the window containing the
  text entry. This usually means that the dialog containing the text entry will
  be closed, since the default widget is usually one of the dialog buttons.
  @see-class{gtk:entry}")

;;; --- gtk:entry-attributes ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "attributes" 'entry) t)
 "The @code{attributes} property of type @class{pango:attr-list} (Read / Write)
  @br{}
  The list of Pango attributes to apply to the text of the text entry. This is
  mainly useful to change the size or weight of the text. The @code{start-index}
  and @code{end-index} fields of the @class{pango:attribute} structure must
  refer to the text of the @class{gtk:entry-buffer} object, for example, without
  the preedit string.")

#+liber-documentation
(setf (liber:alias-for-function 'entry-attributes)
      "Accessor"
      (documentation 'entry-attributes 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-attributes object) => attrs}
  @syntax{(setf (gtk:entry-attributes object) attrs)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[attrs]{a @class{pango:attr-list} instance}
  @begin{short}
    Accessor of the @slot[gtk:entry]{attributes} slot of the @class{gtk:entry}
    class.
  @end{short}
  The @fun{gtk:entry-attributes} function gets the attribute list, if any. The
  @setf{gtk:entry-attributes} function sets a attributes list. The attributes
  in the list are applied to the text of the text entry.
  @see-class{gtk:entry}
  @see-class{pango:attr-list}")

;;; --- gtk:entry-buffer -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "buffer" 'entry) t)
 "The @code{buffer} property of type @class{gtk:entry-buffer}
  (Read / Write / Construct) @br{}
  The entry buffer which actually stores the text for the text entry.")

#+liber-documentation
(setf (liber:alias-for-function 'entry-buffer)
      "Accessor"
      (documentation 'entry-buffer 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-buffer object) => buffer}
  @syntax{(setf (gtk:entry-buffer object) buffer)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[buffer]{a @class{gtk:entry-buffer} object}
  @begin{short}
    Accessor of the @slot[gtk:entry]{buffer} slot of the @class{gtk:entry}
    class.
  @end{short}
  The @fun{gtk:entry-buffer} function gets the entry buffer which holds the text
  for the text entry. The @setf{gtk:entry-buffer} function sets the entry
  buffer.
  @see-class{gtk:entry}
  @see-class{gtk:entry-buffer}")

;;; --- gtk:entry-completion ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "completion" 'entry) t)
 "The @code{completion} property of type @class{gtk:entry-completion}
  (Read / Write) @br{}
  The auxiliary completion object to use with the entry. Deprecated 4.10")

#+liber-documentation
(setf (liber:alias-for-function 'entry-completion)
      "Accessor"
      (documentation 'entry-completion 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-completion object) => completion}
  @syntax{(setf (gtk:entry-completion object) completion)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[completion]{a @class{gtk:entry-completion} object or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:entry]{completion} slot of the @class{gtk:entry}
    class.
  @end{short}
  The @fun{gtk:entry-completion} function returns the auxiliary completion
  object currently in use by the text entry. The @setf{gtk:entry-completion}
  function sets the auxiliary completion object.

  All further configuration of the completion mechanism is done on
  @arg{completion} using the @class{gtk:entry-completion} API. Completion is
  disabled if the @slot[gtk:entry]{completion} property is set to @code{nil}.
  @begin[Warning]{dictionary}
    This function is deprectated since 4.10. The @class{gtk:entry-completion}
    implementation will be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:entry}
  @see-class{gtk:entry-completion}")

;;; --- gtk:entry-enable-emoji-completion --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "enable-emoji-completion"
                                               'entry) t)
 "The @code{enable-emoji-completion} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether to suggest Emoji replacements. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-enable-emoji-completion)
      "Accessor"
      (documentation 'entry-enable-emoji-completion 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-enable-emoji-completion object) => enable}
  @syntax{(setf (gtk:entry-enable-emoji-completion object) enable)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[enable]{a boolean whether to suggest Emoji replacements}
  @begin{short}
    Accessor of the @slot[gtk:entry]{enable-emoji-completion} slot of the
    @class{gtk:entry} class.
  @end{short}
  Whether to suggest Emoji replacements.
  @see-class{gtk:entry}")

;;; --- gtk:entry-extra-menu ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "extra-menu" 'entry) t)
 "The @code{extra-menu} property of type @class{g:menu-model} (Read / Write)
  @br{}
  The menu model whose contents will be appended to the context menu.")

#+liber-documentation
(setf (liber:alias-for-function 'entry-extra-menu)
      "Accessor"
      (documentation 'entry-extra-menu 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-extra-menu object) => menu}
  @syntax{(setf (gtk:entry-extra-menu object) menu)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[menu]{a @class{g:menu-model} object}
  @begin{short}
    Accessor of the @slot[gtk:entry]{extra-menu} slot of the @class{gtk:entry}
    class.
  @end{short}
  The @fun{gtk:entry-extra-menu} function gets the menu model. The
  @setf{gtk:entry-extra-menu} function sets a menu model to add when
  constructing the context menu for the text entry.
  @see-class{gtk:entry}
  @see-class{g:menu-model}")

;;; --- gtk:entry-has-frame ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-frame" 'entry) t)
 "The @code{has-frame} property of type @code{:boolean} (Read / Write) @br{}
  @em{False} removes outside bevel from the text entry. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-has-frame)
      "Accessor"
      (documentation 'entry-has-frame 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-has-frame object) => setting}
  @syntax{(setf (gtk:entry-has-frame object) setting)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[setting]{a boolean whether to remove bevel from the text entry
    field}
  @begin{short}
    Accessor of the @slot[gtk:entry]{has-frame} slot of the @class{gtk:entry}
    class.
  @end{short}
  The @fun{gtk:entry-has-frame} function returns whether the text entry has a
  beveled frame. The @setf{gtk:entry-has-frame} function sets whether the the
  entry has a beveled frame around it.
  @see-class{gtk:entry}")

;;; --- gtk:entry-im-module ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "im-module" 'entry) t)
 "The @code{im-module} property of type @code{:string} (Read / Write) @br{}
  The IM (Input Method) module that should be used for the text entry. See the
  @class{gtk:im-context} documentation. Setting this to a non-@code{nil} value
  overrides the system-wide IM module setting. See the
  @slot[gtk:settings]{gtk-im-module} setting. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-im-module)
      "Accessor"
      (documentation 'entry-im-module 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-im-module object) => setting}
  @syntax{(setf (gtk:entry-im-module object) setting)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[setting]{a string for the IM (Input Method] module that should be
    used for the text entry}
  @begin{short}
    Accessor of the @slot[gtk:entry]{im-module} slot of the @class{gtk:entry}
    class.
  @end{short}
  The IM (Input Method) module that should be used for the text entry. See the
  @class{gtk:im-context} documentation. Setting this to a non-@code{nil} value
  overrides the system-wide IM module setting. See the
  @slot[gtk:settings]{im-module} documentation.
  @see-class{gtk:entry}
  @see-class{gtk:im-context}
  @see-function{gtk:settings-gtk-im-module}")

;;; --- gtk:entry-input-hints --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "input-hints" 'entry) t)
 "The @code{input-hints} property of type @symbol{gtk:input-hints}
  (Read / Write) @br{}
  The additional hints, beyond the @slot[gtk:entry]{input-purpose} property,
  that allow input methods to fine-tune their behaviour.")

#+liber-documentation
(setf (liber:alias-for-function 'entry-input-hints)
      "Accessor"
      (documentation 'entry-input-hints 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-input-hints object) => hints}
  @syntax{(setf (gtk:entry-input-hints object) hints)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[hints]{a value of the @symbol{gtk:input-hints} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:entry]{input-hints} slot of the @class{gtk:entry}
    class.
  @end{short}
  The @fun{gtk:entry-input-hints} function gets the property. The
  @setf{gtk:entry-input-hints} function sets the property, which allows input
  methods to fine-tune their behaviour.
  @see-class{gtk:entry}
  @see-symbol{gtk:input-hints}")

;;; --- gtk:entry-input-purpose ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "input-purpose" 'entry) t)
 "The @code{input-purpose} property of type @symbol{gtk:input-purpose}
  (Read / Write) @br{}
  The purpose of the text entry. This property can be used by on-screen
  keyboards and other input methods to adjust their behaviour. Note that setting
  the purpose to the @code{:password} or @code{:pin} values is independent from
  setting the @slot[gtk:entry]{visibility} property. @br{}
  Default value: @code{:free-form}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-input-purpose)
      "Accessor"
      (documentation 'entry-input-purpose 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-input-purpose object) => purpose}
  @syntax{(setf (gtk:entry-input-purpose object) purpose)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[purpose]{a value of the @symbol{gtk:input-purpose} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:entry]{input-purpose} slot of the
    @class{gtk:entry} class.
  @end{short}
  The @fun{gtk:entry-input-purpose} function gets the value of the property. The
  @setf{gtk:entry-input-purpose} function sets the property which can be used by
  on-screen keyboards and other input methods to adjust their behaviour.
  @see-class{gtk:entry}
  @see-symbol{gtk:input-purpose}")

;;; --- gtk:entry-invisible-char -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "invisible-char" 'entry) t)
 "The @code{invisible-char} property of type @code{:uint} (Read / Write) @br{}
  The invisible character is used when masking the text entry contents in
  \"password mode\". @br{}
  Default value: @code{(char-code #\*)}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-invisible-char)
      "Accessor"
      (documentation 'entry-invisible-char 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-invisible-char object) => char}
  @syntax{(setf (gtk:entry-invisble-char object) char)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[char]{an unsigned integer for a Unicode char code}
  @begin{short}
    Accessor of the @slot[gtk:entry]{invisible-char} slot of the
    @class{gtk:entry} class.
  @end{short}
  The @fun{gtk:entry-invisible-char} function retrieves the character displayed
  in place of the real characters for entries with visibility set to @em{false}.
  The @setf{gtk:entry-invisible-char} function sets the character to use in
  place of the actual text.

  For example, this is the character used in \"password mode\" to show the user
  how many characters have been typed. By default, GTK picks the best invisible
  char available in the current font. If you set the invisible char to 0, then
  the user will get no feedback at all. There will be no text on the screen as
  they type.
  @see-class{gtk:entry}
  @see-function{gtk:entry-visibility}
  @see-function{gtk:entry-unset-invisible-char}")

;;; --- gtk:entry-invisible-char-set -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "invisible-char-set" 'entry) t)
 "The @code{invisible-char-set} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether the invisible char has been set for the text entry. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-invisible-char-set)
      "Accessor"
      (documentation 'entry-invisible-char-set 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-invisible-char-set object) => setting}
  @syntax{(setf (gtk:entry-invisible-char-set object) setting)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[setting]{a boolean whether the invisible char has been set for the
    text entry}
  @begin{short}
    Accessor of the @slot[gtk:entry]{invisible-char-set} slot of the
    @class{gtk:entry} class.
  @end{short}
  Whether the invisible char has been set for the text entry.
  @see-class{gtk:entry}
  @see-function{gtk:entry-invisible-char}")

;;; --- gtk:entry-max-length ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "max-length" 'entry) t)
 "The @code{max-length} property of type @code{:int} (Read / Write) @br{}
  The maximum number of characters for the text entry. Zero if no maximum. @br{}
  Allowed values: [0,65535] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'entry-max-length)
      "Accessor"
      (documentation 'entry-max-length 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-max-length object) => max}
  @syntax{(setf (gtk:entry-max-length object) max)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[max]{an integer for the maximum length of the text entry, or 0 for
    no maximum, the value passed in will be clamped to the range [0, 65536]}
  @begin{short}
    Accessor of the @slot[gtk:entry]{max-length} slot of the @class{gtk:entry}
    class.
  @end{short}
  The @fun{gtk:entry-max-length} function retrieves the maximum allowed length
  of the text in the text entry, or 0 if there is no maximum. The
  @setf{gtk:entry-max-length} function sets the maximum allowed length of the
  contents of the widget. If the current contents are longer than the given
  length, then they will be truncated to fit.
  @begin[Notes]{dictionary}
    This function is equivalent to
    @begin{pre}
(gtk:entry-buffer-max-length (gtk:entry-buffer object))
    @end{pre}
    and
    @begin{pre}
(setf (gtk:entry-buffer-max-length (gtk:entry-buffer object)) max)
    @end{pre}
  @end{dictionary}
  @see-class{gtk:entry}
  @see-class{gtk:entry-buffer}
  @see-function{gtk:entry-buffer}
  @see-function{gtk:entry-buffer-max-length}")

;;; --- gtk:entry-overwrite-mode -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "overwrite-mode" 'entry) t)
 "The @code{overwrite-mode} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether text is overwritten when typing in the text entry. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-overwrite-mode)
      "Accessor"
      (documentation 'entry-overwrite-mode 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-overwrite-mode object) => overwrite}
  @syntax{(setf (gtk:entry-overwrite-mode object) overwrite)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[overwrite]{a boolean whether the text is overwritten when typing}
  @begin{short}
    Accessor of the @slot[gtk:entry]{overwrite-mode} slot of the
    @class{gtk:entry} class.
  @end{short}
  The @fun{gtk:entry-overwrite-mode} function returns whether the text is
  overwritten when typing in the text entry. The
  @setf{gtk:entry-overwrite-mode} function sets whether the text is overwritten
  when typing.
  @see-class{gtk:entry}")

;;; --- gtk:entry-placeholder-text ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "placeholder-text" 'entry) t)
 "The @code{placeholder-text} property of type @code{:string} (Read / Write)
  @br{}
  The text that will be displayed in the text entry when it is empty and
  unfocused. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-placeholder-text)
      "Accessor"
      (documentation 'entry-placeholder-text 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-placeholder-text object) => text}
  @syntax{(setf (gtk:entry-placeholder-text object) text)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[text]{a string to be displayed when the text entry is empty and
    unfocused, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:entry]{placeholder-text} slot of the
    @class{gtk:entry} class.
  @end{short}
  The @fun{gtk:entry-placeholder-text} function retrieves the text that will be
  displayed when the text entry is empty and unfocused. The
  @setf{gtk:entry-placeholder-text} function sets the text. This can be used to
  give a visual hint of the expected contents of the text entry.

  Note that since the placeholder text gets removed when the text entry received
  focus, using this feature is a bit problematic if the text entry is given the
  initial focus in a window. Sometimes this can be worked around by delaying
  the initial focus setting until the first key event arrives.
  @see-class{gtk:entry}")

;;; --- gtk:entry-primary-icon-activatable -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "primary-icon-activatable"
                                               'entry) t)
 "The @code{primary-icon-activatable} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the primary icon is activatable. GTK emits the @code{\"icon-press\"}
  and @code{\"icon-release\"} signals only on sensitive, activatable icons.
  Sensitive, but non-activatable icons can be used for purely informational
  purposes. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-primary-icon-activatable)
      "Accessor"
      (documentation 'entry-primary-icon-activatable 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-primary-icon-activatable object) => activatable}
  @syntax{(setf (gtk:entry-primary-icon-activatable object) activatable)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[activatable]{a boolean whether the primary icon is activatable}
  @begin{short}
    Accessor of the @slot[gtk:entry]{primary-icon-activatable} slot of the
    @class{gtk:entry} class.
  @end{short}
  Whether the primary icon is activatable. GTK emits the @code{\"icon-press\"}
  and @code{\"icon-release\"} signals only on sensitive, activatable icons.
  Sensitive, but non-activatable icons can be used for purely informational
  purposes.
  @see-class{gtk:entry}
  @see-function{gtk:entry-icon-activatable}")

;;; --- gtk:entry-primary-icon-gicon -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "primary-icon-gicon" 'entry) t)
 "The @code{primary-icon-gicon} property of type @class{g:icon} (Read / Write)
  @br{}
  The icon to use as the primary icon for the text entry.")

#+liber-documentation
(setf (liber:alias-for-function 'entry-primary-icon-gicon)
      "Accessor"
      (documentation 'entry-primary-icon-gicon 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-primary-icon-gicon object) => icon}
  @syntax{(setf (gtk:entry-primary-icon-gicon object) icon)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[icon]{a @class{g:icon} object}
  @begin{short}
    Accessor of the @slot[gtk:entry]{primary-icon-gicon} slot of the
    @class{gtk:entry} class.
  @end{short}
  The icon to use as the primary icon for the text entry.
  @see-class{gtk:entry}
  @see-class{g:icon}")

;;; --- gtk:entry-primary-icon-name --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "primary-icon-name" 'entry) t)
 "The @code{primary-icon-name} property of type @code{:string} (Read / Write)
  @br{}
  The icon name from the current icon theme to use as the primary icon for the
  text entry. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-primary-icon-name)
      "Accessor"
      (documentation 'entry-primary-icon-name 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-primary-icon-name object) => name}
  @syntax{(setf (gtk:entry-primary-icon-name object) name)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[name]{a string for the icon name}
  @begin{short}
    Accessor of the @slot[gtk:entry]{primary-icon-name} slot of the
    @class{gtk:entry} class.
  @end{short}
  The icon name from the current icon theme to use as the primary icon for the
  text entry.
  @see-class{gtk:entry}
  @see-function{gtk:entry-icon-name}
  @see-function{gtk:entry-secondary-icon-name}")

;;; --- gtk:entry-primary-icon-paintable ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "primary-icon-paintable"
                                               'entry) t)
 "The @code{primary-icon-paintable} property of type @class{gdk:paintable}
  (Read / Write) @br{}
  The paintable to use as the primary icon for the text entry.")

#+liber-documentation
(setf (liber:alias-for-function 'entry-primary-icon-paintable)
      "Accessor"
      (documentation 'entry-primary-icon-paintable 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-primary-icon-paintable object) => paintable}
  @syntax{(setf (gtk:entry-primary-icon-paintable object) paintable)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[paintable]{a @class{gdk:paintable} object}
  @begin{short}
    Accessor of the @slot[gtk:entry]{primary-icon-paintable} slot of the
    @class{gtk:entry} class.
  @end{short}
  A paintable to use as the primary icon for the text entry.
  @see-class{gtk:entry}
  @see-class{gdk:paintable}")

;;; --- gtk:entry-primary-icon-sensitive ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "primary-icon-sensitive"
                                               'entry) t)
 "The @code{primary-icon-sensitive} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the primary icon is sensitive. An insensitive icon appears grayed out.
  GTK does not emit the @code{\"icon-press\"} and @code{\"icon-release\"}
  signals and does not allow drag and drop from insensitive icons. An icon
  should be set insensitive if the action that would trigger when clicked is
  currently not available. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-primary-icon-sensitive)
      "Accessor"
      (documentation 'entry-primary-icon-sensitive 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-primary-icon-sensitive object) => sensitive}
  @syntax{(setf (gtk:entry-primary-icon-sensitive object) sensitive)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[sensitive]{a boolean whether the primary icon is sensitive}
  @begin{short}
    Accessor of the @slot[gtk:entry]{primary-icon-sensitive} slot of the
    @class{gtk:entry} class.
  @end{short}
  Whether the primary icon is sensitive. An insensitive icon appears grayed out.
  GTK does not emit the @code{\"icon-press\"} and @code{\"icon-release\"}
  signals and does not allow drag and drop from insensitive icons. An icon
  should be set insensitive if the action that would trigger when clicked is
  currently not available.
  @see-class{gtk:entry}
  @see-function{gtk:entry-icon-sensitive}
  @see-function{gtk:entry-secondary-icon-sensitive}")

;;; --- gtk:entry-primary-icon-storage-type ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "primary-icon-storage-type"
                                               'entry) t)
 "The @code{primary-icon-storage-type} property of type @symbol{gtk:image-type}
  (Read) @br{}
  The representation which is used for the primary icon of the text entry. @br{}
  Default value: @code{:empty}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-primary-icon-storage-type)
      "Accessor"
      (documentation 'entry-primary-icon-storage-type 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-primary-icon-storage-type object) => type}
  @syntax{(setf (gtk:entry-primary-icon-storage-type object) type)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[type]{a value of the @symbol{gtk:image-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:entry]{primary-icon-storage-type} slot of the
    @class{gtk:entry} class.
  @end{short}
  The representation which is used for the primary icon of the text entry.
  @see-class{gtk:entry}
  @see-symbol{gtk:image-type}")

;;; --- gtk:entry-primary-icon-tooltip-markup ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "primary-icon-tooltip-markup"
                                               'entry) t)
 "The @code{primary-icon-tooltip-markup} property of type @code{:string}
  (Read / Write) @br{}
  The contents of the tooltip on the primary icon, which is marked up with
  the Pango text markup language. Also see the
  @fun{gtk:entry-icon-tooltip-markup} function. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-primary-icon-tooltip-markup)
      "Accessor"
      (documentation 'entry-primary-icon-tooltip-markup 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-primary-icon-tooltip-markup object) => markup}
  @syntax{(setf (gtk:entry-primary-icon-tooltip-markup object) markup)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[markup]{a string for the contents of the tooltip}
  @begin{short}
    Accessor of the @slot[gtk:entry]{primary-icon-tooltip-markup} slot of the
    @class{gtk:entry} class.
  @end{short}
  The contents of the tooltip on the primary icon, which is marked up with
  the Pango text markup language. Also see the
  @fun{gtk:entry-icon-tooltip-markup} function.
  @see-class{gtk:entry}
  @see-function{gtk:entry-icon-tooltip-markup}")

;;; --- gtk:entry-primary-icon-tooltip-text ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "primary-icon-tooltip-text"
                                               'entry) t)
 "The @code{primary-icon-tooltip-text} property of type @code{:string}
  (Read / Write) @br{}
  The contents of the tooltip on the primary icon. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-primary-icon-tooltip-text)
      "Accessor"
      (documentation 'entry-primary-icon-tooltip-text 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-primary-icon-tooltip-text object) => text}
  @syntax{(setf (gtk:entry-primary-icon-tooltip-text object) text)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[text]{a string for the contents of the tooltip}
  @begin{short}
    Accessor of the @slot[gtk:entry]{primary-icon-tooltip-text} slot of the
    @class{gtk:entry} class.
  @end{short}
  The contents of the tooltip on the primary icon. Also see the
  @fun{gtk:entry-icon-tooltip-text} function.
  @see-class{gtk:entry}
  @see-function{gtk:entry-icon-tooltip-text}
  @see-function{gtk:entry-secondary-icon-tooltip-text}")

;;; --- gtk:entry-progress-fraction --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "progress-fraction" 'entry) t)
 "The @code{progress-fraction} property of type @code{:double} (Read / Write)
  @br{}
  The current fraction of the task that is been completed. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'entry-progress-fraction)
      "Accessor"
      (documentation 'entry-progress-fraction 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-progress-fraction object) => fraction}
  @syntax{(setf (gtk:entry-progress-fraction object) fraction)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[fraction]{a number coerced to a double float for the fraction of
    the task that is been completed}
  @begin{short}
    Accessor of the @slot[gtk:entry]{progress-fraction} slot of the
    @class{gtk:entry} class.
  @end{short}
  The @fun{gtk:entry-progress-fraction} function returns the current fraction
  that is been completed. The @setf{gtk:entry-progress-fraction} function causes
  the text entry progress indicator to \"fill in\" the given fraction of the
  progress bar. The fraction should be between 0.0 and 1.0, inclusive.
  @see-class{gtk:entry}")

;;; --- gtk:entry-progress-pulse-step ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "progress-pulse-step" 'entry) t)
 "The @code{progress-pulse-step} property of type @code{:double} (Read / Write)
  @br{}
  The fraction of total text entry width to move the progress bouncing block
  for each call to the @fun{gtk:entry-progress-pulse} function. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0.0")

#+liber-documentation
(setf (liber:alias-for-function 'entry-progress-pulse-step)
      "Accessor"
      (documentation 'entry-progress-pulse-step 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-progress-pulse-step object) => step}
  @syntax{(setf (gtk:entry-progress-pulse-step object) step)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[step]{a number coerced to a double float for the fraction between
    0.0 and 1.0}
  @begin{short}
    Accessor of the @slot[gtk:entry]{progress-pulse-step} slot of the
    @class{gtk:entry} class.
  @end{short}
  The @fun{gtk:entry-progress-pulse-step} function retrieves the pulse step as
  a fraction from 0.0 to 1.0. The @setf{gtk:entry-progress-pulse-step} function
  sets the fraction of total text entry width to move the progress bouncing
  block for each call to the @fun{gtk:entry-progress-pulse} function.
  @see-class{gtk:entry}
  @see-function{gtk:entry-progress-pulse}")

;;; --- gtk:entry-scroll-offset ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "scroll-offset" 'entry) t)
 "The @code{scroll-offset} property of type @code{:int} (Read) @br{}
  The number of pixels of the text entry scrolled off the screen to the left.
  @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'entry-scroll-offset)
      "Accessor"
      (documentation 'entry-scroll-offset 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-scroll-offset object) => offset}
  @syntax{(setf (gtk:entry-scroll-offset object) offset)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[offset]{an integer for the number of pixels the text entry is
    scrolled off}
  @begin{short}
    Accessor of the @slot[gtk:entry]{scroll-offset} slot of the
    @class{gtk:entry} class.
  @end{short}
  The number of pixels the text entry is scrolled off the screen to the left.
  @see-class{gtk:entry}")

;;; --- gtk:entry-secondary-icon-activatable -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "secondary-icon-activatable"
                                               'entry) t)
 "The @code{secondary-icon-activatable} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the secondary icon is activatable. GTK emits the @code{\"icon-press\"}
  and @code{\"icon-release\"} signals only on sensitive, activatable icons.
  Sensitive, but non-activatable icons can be used for purely informational
  purposes. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-secondary-icon-activatable)
      "Accessor"
      (documentation 'entry-secondary-icon-activatable 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-secondary-icon-activatable object) => activatable}
  @syntax{(setf (gtk:entry-secondary-icon-activatable object) activatable)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[activatable]{a boolean whether the secondary icon is activatable}
  @begin{short}
    Accessor of the @slot[gtk:entry]{secondary-icon-activatable} slot of the
    @class{gtk:entry} class.
  @end{short}
  Whether the secondary icon is activatable. GTK emits the @code{\"icon-press\"}
  and @code{\"icon-release\"} signals only on sensitive, activatable icons.
  Sensitive, but non-activatable icons can be used for purely informational
  purposes.
  @see-class{gtk:entry}
  @see-function{gtk:entry-icon-activatable}")

;;; --- gtk:entry-secondary-icon-gicon -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "secondary-icon-gicon" 'entry) t)
 "The @code{secondary-icon-gicon} property of type @class{g:icon} (Read / Write)
  @br{}
  The icon to use as the secondary icon for the text entry.")

#+liber-documentation
(setf (liber:alias-for-function 'entry-secondary-icon-gicon)
      "Accessor"
      (documentation 'entry-secondary-icon-gicon 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-secondary-icon-gicon object) => icon}
  @syntax{(setf (gtk:entry-secondary-icon-gicon object) icon)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[icon]{a @class{g:icon} object}
  @begin{short}
    Accessor of the @slot[gtk:entry]{secondary-icon-gicon} slot of the
    @class{gtk:entry} class.
  @end{short}
  The icon to use as the secondary icon for the text entry.
  @see-class{gtk:entry}
  @see-class{g:icon}
  @see-function{gtk:entry-icon-gicon}")

;;; --- gtk:entry-secondary-icon-name ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "secondary-icon-name" 'entry) t)
 "The @code{secondary-icon-name} property of type @code{:string} (Read / Write)
  @br{}
  The icon name from the current icon theme to use as the secondary icon for
  the text entry. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-secondary-icon-name)
      "Accessor"
      (documentation 'entry-secondary-icon-name 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-secondary-icon-name object) => name}
  @syntax{(setf (gtk:entry-secondary-icon-name object) name)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[name]{a string for the icon name}
  @begin{short}
    Accessor of the @slot[gtk:entry]{secondary-icon-name} slot of the
    @class{gtk:entry} class.
  @end{short}
  The icon name from the current icon theme to use as the secondary icon for
  the text entry.
  @see-class{gtk:entry}
  @see-function{gtk:entry-icon-name}
  @see-function{gtk:entry-primary-icon-name}")

;;; --- gtk:entry-secondary-icon-paintable -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "secondary-icon-paintable"
                                               'entry) t)
 "The @code{secondary-icon-paintable} property of type @class{gdk:paintable}
  (Read / Write) @br{}
  The paintable to use as the secondary icon for the text entry.")

#+liber-documentation
(setf (liber:alias-for-function 'entry-secondary-icon-paintable)
      "Accessor"
      (documentation 'entry-secondary-icon-paintable 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-secondary-icon-paintable object) => paintable}
  @syntax{(setf (gtk:entry-secondary-icon-paintable object) paintable)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[paintable]{a @class{gdk:paintable} object}
  @begin{short}
    Accessor of the @slot[gtk:entry]{secondary-icon-paintable} slot of the
    @class{gtk:entry} class.
  @end{short}
  The paintable to use as the secondary icon for the text entry.
  @see-class{gtk:entry}
  @see-class{gdk:paintable}
  @see-function{gtk:entry-icon-paintable}")

;;; --- gtk:entry-secondary-icon-sensitive -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "secondary-icon-sensitive"
                                               'entry) t)
 "The @code{secondary-icon-sensitive} property of type @code{:boolean}
  (Read / Write) @br{}
  Whether the secondary icon is sensitive. An insensitive icon appears grayed
  out. GTK does not emit the @code{\"icon-press\"} and @code{\"icon-release\"}
  signals and does not allow drag and drop from insensitive icons. An icon
  should be set insensitive if the action that would trigger when clicked is
  currently not available.@br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-secondary-icon-sensitive)
      "Accessor"
      (documentation 'entry-secondary-icon-sensitive 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-secondary-icon-sensitive object) => sensitive}
  @syntax{(setf (gtk:entry-secondary-icon-sensitive object) sensitive)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[sensitive]{a boolean whether the icon is sensitive}
  @begin{short}
    Accessor of the @slot[gtk:entry]{secondary-icon-sensitive} slot of the
    @class{gtk:entry} class.
  @end{short}
  Whether the secondary icon is sensitive. An insensitive icon appears grayed
  out. GTK does not emit the @code{\"icon-press\"} and @code{\"icon-release\"}
  signals and does not allow drag and drop from insensitive icons. An icon
  should be set insensitive if the action that would trigger when clicked is
  currently not available.
  @see-class{gtk:entry}
  @see-function{gtk:entry-icon-sensitive}
  @see-function{gtk:entry-secondary-icon-sensitive}")

;;; --- gtk:entry-secondary-icon-storage-type ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "secondary-icon-storage-type"
                                               'entry) t)
 "The @code{secondary-icon-storage-type} property of type
  @symbol{gtk:image-type} (Read) @br{}
  The representation which is used for the secondary icon of the text entry.
  @br{}
  Default value: @code{:empty}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-secondary-icon-storage-type)
      "Accessor"
      (documentation 'entry-secondary-icon-storage-type 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-secondary-icon-storage-type object) => type}
  @syntax{(setf (gtk:entry-secondary-icon-storage-type object) type)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[type]{a value of the @symbol{gtk:image-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:entry]{secondary-icon-storage-type} slot of the
    @class{gtk:entry} class.
  @end{short}
  The representation which is used for the secondary icon of the text entry.
  @see-class{gtk:entry}
  @see-symbol{gtk:image-type}
  @see-function{gtk:entry-icon-storage-type}")

;;; --- gtk:entry-secondary-icon-tooltip-markup --------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "secondary-icon-tooltip-markup"
                                               'entry) t)
 "The @code{secondary-icon-tooltip-markup} property of type @code{:string}
  (Read / Write) @br{}
  The contents of the tooltip on the secondary icon, which is marked up with
  the Pango text markup language. Also see the
  @fun{gtk:entry-icon-tooltip-markup} function. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-secondary-icon-tooltip-markup)
      "Accessor"
      (documentation 'entry-secondary-icon-tooltip-markup 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-secondary-icon-tooltip-markup object) => markup}
  @syntax{(setf (gtk:entry-secondary-icon-tooltip-markup object) markup)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[markup]{a string for the contents of the tooltip}
  @begin{short}
    Accessor of the @slot[gtk:entry]{secondary-icon-tooltip-markup} slot of the
    @class{gtk:entry} class.
  @end{short}
  The contents of the tooltip on the secondary icon, which is marked up with
  the Pango text markup language. Also see the
  @fun{gtk:entry-icon-tooltip-markup} function.
  @see-class{gtk:entry}
  @see-function{gtk:entry-icon-tooltip-markup}")

;;; --- gtk:entry-secondary-icon-tooltip-text ----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "secondary-icon-tooltip-text"
                                               'entry) t)
 "The @code{secondary-icon-tooltip-text} property of type @code{:string}
  (Read / Write) @br{}
  The contents of the tooltip on the secondary icon. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-secondary-icon-tooltip-text)
      "Accessor"
      (documentation 'entry-secondary-icon-tooltip-text 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-secondary-icon-tooltip-text object) => text}
  @syntax{(setf (gtk:entry-secondary-icon-tooltip-text object) text)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[text]{a string for the contents of the tooltip}
  @begin{short}
    Accessor of the @slot[gtk:entry]{secondary-icon-tooltip-text} slot of the
    @class{gtk:entry} class.
  @end{short}
  The contents of the tooltip on the secondary icon. Also see the
  @fun{gtk:entry-icon-tooltip-text} function.
  @see-class{gtk:entry}
  @see-function{gtk:entry-icon-tooltip-text}
  @see-function{gtk:entry-primary-icon-tooltip-text}")

;;; --- gtk:entry-show-emoji-icon ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-emoji-icon" 'entry) t)
 "The @code{show-emoji-icon} property of type @code{:boolean} (Read / Write)
  @br{}
  Whether to show an icon for Emoji. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-show-emoji-icon)
      "Accessor"
      (documentation 'entry-show-emoji-icon 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-show-emoji-icon object) => setting}
  @syntax{(setf (gtk:entry-show-emoji-icon object) setting)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[setting]{a boolean whether to show an icon for Emoji}
  @begin{short}
    Accessor of the @slot[gtk:entry]{show-emoji-icon} slot of the
    @class{gtk:entry} class.
  @end{short}
  Whether to show an icon for Emoji.
  @see-class{gtk:entry}")

;;; --- gtk:entry-tabs ---------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tabs" 'entry) t)
 "The @code{tabs} property of type @class{pango:tab-array} (Read / Write) @br{}
  The list of tabstop locations to apply to the text of the text entry.")

#+liber-documentation
(setf (liber:alias-for-function 'entry-tabs)
      "Accessor"
      (documentation 'entry-tabs 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-tabs object) => tabs}
  @syntax{(setf (gtk:entry-tabs object) tabs)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[tabs]{a @class{pango:tab-array} instance}
  @begin{short}
    Accessor of the @slot[gtk:entry]{tabs} slot of the @class{gtk:entry} class.
  @end{short}
  The @fun{gtk:entry-tabs} function gets the tabstops that were set on the text
  entry using the @setf{gtk:entry-tabs} function, if any. The tabstops are
  applied to the text of the text entry.
  @see-class{gtk:entry}
  @see-class{pango:tab-array}")

;;; --- gtk:entry-text-length --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "text-length" 'entry) t)
 "The @code{text-length} property of type @code{:uint} (Read) @br{}
  The length of the text in the text entry. @br{}
  Allowed values: <= 65535 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'entry-text-length)
      "Accessor"
      (documentation 'entry-text-length 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-text-length object) => length}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[length]{an unsigned integer for the length of the text}
  @begin{short}
    Accessor of the @slot[gtk:entry]{text-length} slot of the @class{gtk:entry}
    class.
  @end{short}
  The @fun{gtk:entry-text-length} function retrieves the current length of the
  text in the text entry, or 0 if there are none.
  @begin[Notes]{dictionary}
    This function is equivalent to:
    @begin{pre}
(gtk:entry-buffer-length (gtk:entry-buffer object))
    @end{pre}
  @end{dictionary}
  @see-class{gtk:entry}
  @see-class{gtk:entry-buffer}
  @see-function{gtk:entry-buffer-length}")

;;; --- gtk:entry-truncate-multiline -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "truncate-multiline" 'entry) t)
 "The @code{truncate-multiline} property of type @code{:boolean} (Read / Write)
  @br{}
  When @em{true}, pasted multi-line text is truncated to the first line. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-truncate-multiline)
      "Accessor"
      (documentation 'entry-truncate-multiline 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-truncate-multiline object) => truncate}
  @syntax{(setf (gtk:entry-truncate-multiline object) truncate)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[truncate]{a boolean whether multi-line text is truncated}
  @begin{short}
    Accessor of the @slot[gtk:entry]{truncate-multiline} slot of the
    @class{gtk:entry} class.
  @end{short}
  If @em{true}, pasted multi-line text is truncated to the first line.
  @see-class{gtk:entry}")

;;; --- gtk:entry-visibility ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "visibility" 'entry) t)
 "The @code{visibility} property of type @code{:boolean} (Read / Write) @br{}
  @em{False} displays the \"invisible char\" instead of the actual text
  (password mode). @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'entry-visibility)
      "Accessor"
      (documentation 'entry-visibility 'function)
 "@version{2025-05-31}
  @syntax{(gtk:entry-visibility object) => visible}
  @syntax{(setf (gtk:entry-visibility object) visible)}
  @argument[object]{a @class{gtk:entry} widget}
  @argument[visible]{@em{true} if the contents of the text entry are displayed
    as plaintext}
  @begin{short}
    Accessor of the @slot[gtk:entry]{visibility} slot of the @class{gtk:entry}
    class.
  @end{short}
  The @fun{gtk:entry-visibility} function retrieves whether the text in the
  text entry is visible. The @setf{gtk:entry-visibility} function sets whether
  the contents of the text entry are visible or not.

  When visibility is set to @em{false}, characters are displayed as the
  invisible char, and will also appear that way when the text in the text entry
  is copied elsewhere. By default, GTK picks the best invisible character
  available in the current font, but it can be changed with the
  @fun{gtk:entry-invisible-char} function.
  @see-class{gtk:entry}
  @see-function{gtk:entry-invisible-char}")

;;; ----------------------------------------------------------------------------
;;; gtk_entry_new
;;; ----------------------------------------------------------------------------

(declaim (inline entry-new))

(defun entry-new ()
 #+liber-documentation
 "@version{2025-05-31}
  @return{The new @class{gtk:entry} widget.}
  @short{Creates a new text entry.}
  @see-class{gtk:entry}"
  (make-instance 'entry))

(export 'entry-new)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_new_with_buffer
;;; ----------------------------------------------------------------------------

(declaim (inline entry-new-with-buffer))

(defun entry-new-with-buffer (buffer)
 #+liber-documentation
 "@version{2025-05-31}
  @argument[buffer]{a @class{gtk:entry-buffer} object to use for the text entry}
  @return{The new @class{gtk:entry} widget.}
  @begin{short}
    Creates a new text entry with the specified entry buffer.
  @end{short}
  @see-class{gtk:entry}
  @see-class{gtk:entry-buffer}"
  (make-instance 'entry
                 :buffer buffer))

(export 'entry-new-with-buffer)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_unset_invisible_char
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_unset_invisible_char" entry-unset-invisible-char)
    :void
 #+liber-documentation
 "@version{2025-05-31}
  @argument[entry]{a @class{gtk:entry} widget}
  @begin{short}
    Unsets the invisible char previously set with the
    @fun{gtk:entry-invisible-char} function.
  @end{short}
  So that the default invisible char is used again.
  @see-class{gtk:entry}
  @see-function{gtk:entry-invisible-char}"
  (entry (g:object entry)))

(export 'entry-unset-invisible-char)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_alignment
;;; gtk_entry_get_alignment
;;; ----------------------------------------------------------------------------

(defun (setf entry-alignment) (align entry)
  (cffi:foreign-funcall "gtk_entry_set_alignment"
                        (g:object entry) entry
                        :float (coerce align 'single-float)
                        :void)
  align)

(cffi:defcfun ("gtk_entry_get_alignment" entry-alignment) :float
 #+liber-documentation
 "@version{2025-05-31}
  @syntax{(gtk:entry-alignment entry) => align}
  @syntax{(setf (gtk:entry-alignment entry) align)}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[align]{a number coerced to a single float with the horizontal
    alignment, from 0.0 (left) to 1.0 (right), reversed for RTL layouts}
  @begin{short}
    This controls the horizontal positioning of the contents when the displayed
    text is shorter than the width of the text entry.
  @end{short}
  @see-class{gtk:entry}"
  (entry (g:object entry)))

(export 'entry-alignment)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_progress_pulse
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_progress_pulse" entry-progress-pulse) :void
 #+liber-documentation
 "@version{#2025-05-31}
  @argument[entry]{a @class{gtk:entry} widget}
  @begin{short}
    Indicates that some progress is made, but you do not know how much.
  @end{short}
  Causes the progress indicator of the text entry to enter \"activity mode\",
  where a block bounces back and forth. Each call to the
  @fun{gtk:entry-progress-pulse} function causes the block to move by a little
  bit. The amount of movement per pulse is determined by the
  @fun{gtk:entry-progress-pulse-step} function.
  @see-class{gtk:entry}
  @see-function{gtk:entry-progress-pulse-step}"
  (entry (g:object entry)))

(export 'entry-progress-pulse)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_reset_im_context
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_reset_im_context" entry-reset-im-context) :void
 #+liber-documentation
 "@version{#2025-05-31}
  @argument[entry]{a @class{gtk:entry} widget}
  @begin{short}
    Reset the input method context of the text entry if needed.
  @end{short}
  This can be necessary in the case where modifying the entry buffer would
  confuse on-going input method behavior.
  @see-class{gtk:entry}"
  (entry (g:object entry)))

(export 'entry-reset-im-context)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_from_paintable
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_set_icon_from_paintable"
               entry-set-icon-from-paintable) :void
 #+liber-documentation
 "@version{#2025-05-31}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value for the icon position}
  @argument[paintable]{a @class{gdk:paintable} object, or @code{nil}}
  @begin{short}
    Sets the icon shown in the specified position using a paintable.
  @end{short}
  If the @arg{paintable} argument is @code{nil}, no icon will be shown in the
  specified position.
  @see-class{gtk:entry}
  @see-class{gdk:paintable}
  @see-symbol{gtk:entry-icon-position}"
  (entry (g:object entry))
  (pos entry-icon-position)
  (paintable (g:object gdk:paintable)))

(export 'entry-set-icon-from-paintable)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_from_icon_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_set_icon_from_icon_name"
               entry-set-icon-from-icon-name) :void
 #+liber-documentation
 "@version{#2025-05-31}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value for the icon position}
  @argument[name]{a string for the icon name, or @code{nil}}
  @begin{short}
    Sets the icon shown in the text entry at the specified position from the
    current icon theme.
  @end{short}
  If the icon name is not known, a \"broken image\" icon will be displayed
  instead. If the @arg{name} argument is @code{nil}, no icon will be shown in
  the specified position.
  @see-class{gtk:entry}
  @see-symbol{gtk:entry-icon-position}"
  (entry (g:object entry))
  (pos entry-icon-position)
  (name :string))

(export 'entry-set-icon-from-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_from_gicon
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_set_icon_from_gicon" entry-set-icon-from-gicon) :void
 #+liber-documentation
 "@version{#2025-05-31}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value for the icon position}
  @argument[icon]{a @class{g:icon} object with the icon to set, or @code{nil}}
  @begin{short}
    Sets the icon shown in the text entry at the specified position from the
    current icon theme.
  @end{short}
  If the icon is not known, a \"broken image\" icon will be displayed instead.

  If the @arg{icon} argument is @code{nil}, no icon will be shown in the
  specified position.
  @see-class{gtk:entry}
  @see-class{g:icon}
  @see-symbol{gtk:entry-icon-position}"
  (entry (g:object entry))
  (pos entry-icon-position)
  (icon (g:object g:icon)))

(export 'entry-set-icon-from-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_storage_type
;;; ----------------------------------------------------------------------------

(defun entry-icon-storage-type (entry pos)
 #+liber-documentation
 "@version{#2025-05-31}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value for the icon position}
  @begin{return}
    The @symbol{gtk:image-type} value for the image representation being used.
  @end{return}
  @begin{short}
    Gets the type of representation being used by the icon to store image data.
  @end{short}
  If the icon has no image data, the return value will be @code{:empty}.
  @see-class{gtk:entry}
  @see-symbol{gtk:entry-icon-position}
  @see-symbol{gtk:image-type}"
  (cond ((eq pos :primary)
         (entry-primary-icon-storage-type entry))
        ((eq pos :secondary)
         (entry-secondary-icon-storage-type entry))
        (t
         (error "GTK:ENTRY-ICON-STORAGE-TYPE: Unexpected icon position"))))

(export 'entry-icon-storage-type)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_paintable
;;; ----------------------------------------------------------------------------

(defun entry-icon-paintable (entry pos)
 #+liber-documentation
 "@version{#2025-05-31}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value for the icon position}
  @begin{return}
    The @class{gdk:paintable} object, or @code{nil} if no icon is set for this
    position.
  @end{return}
  @begin{short}
    Retrieves the paintable used for the icon.
  @end{short}
  If no paintable was used for the icon, @code{nil} is returned.
  @see-class{gtk:entry}
  @see-class{gdk:paintable}
  @see-symbol{gtk:entry-icon-position}"
  (cond ((eq pos :primary)
         (entry-primary-icon-paintable entry))
        ((eq pos :secondary)
         (entry-secondary-icon-paintable entry))
        (t
         (error "GTK:ENTRY-ICON-PAINTABLE: Unexpected icon position"))))

(export 'entry-icon-paintable)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_name
;;; ----------------------------------------------------------------------------

(defun entry-icon-name (entry pos)
 #+liber-documentation
 "@version{#2025-05-31}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value for the icon position}
  @begin{return}
    The icon name, or @code{nil} if no icon is set or if the icon was not set
    from an icon name.
  @end{return}
  @begin{short}
    Retrieves the icon name used for the icon, or @code{nil} if there is no icon
    or if the icon was set by some other method, for example, by paintable, or
    gicon.
  @end{short}
  @see-class{gtk:entry}
  @see-symbol{gtk:entry-icon-position}"
  (cond ((eq pos :primary)
         (entry-primary-icon-name entry))
        ((eq pos :secondary)
         (entry-secondary-icon-name entry))
        (t
         (error "GTK:ENTRY-ICON-NAME: Unexpected icon position"))))

(export 'entry-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_gicon
;;; ----------------------------------------------------------------------------

(defun entry-icon-gicon (entry pos)
 #+liber-documentation
 "@version{#2025-05-31}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value for the icon position}
  @begin{return}
    The @class{g:icon} object, or @code{nil} if no icon is set or if the icon
    is not a @class{g:icon} object.
  @end{return}
  @begin{short}
    Retrieves the @class{g:icon} object used for the icon, or @code{nil} if
    there is no icon or if the icon was set by some other method, for example,
    by paintable, or icon name.
  @end{short}
  @see-class{gtk:entry}
  @see-symbol{gtk:entry-icon-position}"
  (cond ((eq pos :primary)
         (entry-primary-icon-gicon entry))
        ((eq pos :secondary)
         (entry-secondary-icon-gicon entry))
        (t
         (error "GTK:ENTRY-ICON-GICON: Unexpected icon position"))))

(export 'entry-icon-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_activatable
;;; gtk_entry_get_icon_activatable
;;; ----------------------------------------------------------------------------

(defun (setf entry-icon-activatable) (activatable entry pos)
  (cond ((eq pos :primary)
         (setf (entry-primary-icon-activatable entry) activatable))
        ((eq pos :secondary)
         (setf (entry-secondary-icon-activatable entry) activatable))
        (t
         (error "GTK:ENTRY-ICON-ACTIVATABLE: Unexpected icon position"))))

(defun entry-icon-activatable (entry pos)
 #+liber-documentation
 "@version{#2025-05-31}
  @syntax{(gtk:entry-icon-activatable entry pos) => activatable}
  @syntax{(setf (gtk:entry-icon-activatable entry pos) activatable)}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value for the icon position}
  @argument[activatable]{@em{true} if the icon is activatable}
  @begin{short}
    The @fun{gtk:entry-icon-activatable} function returns whether the icon is
    activatable.
  @end{short}
  The @setf{gtk:entry-icon-activatable} function sets whether the icon is
  activatable.
  @see-class{gtk:entry}
  @see-symbol{gtk:entry-icon-position}"
  (cond ((eq pos :primary)
         (entry-primary-icon-activatable entry))
        ((eq pos :secondary)
         (entry-secondary-icon-activatable entry))
        (t
         (error "GTK:ENTRY-ICON-ACTIVATABLE: Unexpected icon position"))))

(export 'entry-icon-activatable)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_sensitive
;;; gtk_entry_get_icon_sensitive
;;; ----------------------------------------------------------------------------

(defun (setf entry-icon-sensitive) (sensitive entry pos)
  (cond ((eq pos :primary)
         (setf (entry-primary-icon-sensitive entry) sensitive))
        ((eq pos :secondary)
         (setf (entry-secondary-icon-sensitive entry) sensitive))
        (t
         (error "GTK:ENTRY-ICON-SENSITIVE: Unexpected icon position"))))

(defun entry-icon-sensitive (entry pos)
 #+liber-documentation
 "@version{#2025-05-31}
  @syntax{(gtk:entry-icon-sensitive entry pos) => sensitive}
  @syntax{(setf (gtk:entry-icon-sensitive entry pos) sensitive)}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value for the icon position}
  @argument[sensitive]{specifies whether the icon should appear sensitive or
    insensitive}
  @begin{short}
    The @fun{gtk:entry-icon-sensitive} function returns whether the icon
    appears sensitive or insensitive.
  @end{short}
  The @setf{gtk:entry-icon-sensitive} function sets
  the sensitivity for the specified icon.
  @see-class{gtk:entry}
  @see-symbol{gtk:entry-icon-position}"
  (cond ((eq pos :primary)
         (entry-primary-icon-sensitive entry))
        ((eq pos :secondary)
         (entry-secondary-icon-sensitive entry))
        (t
         (error "GTK:ENTRY-ICON-SENSITIVE: Unexpected icon position"))))

(export 'entry-icon-sensitive)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_at_pos
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_get_icon_at_pos" entry-icon-at-pos) :int
 #+liber-documentation
 "@version{#2025-05-31}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[x]{an integer for the x coordinate of the position to find}
  @argument[y]{an integer for the y coordinate of the position to find}
  @return{The integer for the index of the icon at the given position, or -1.}
  @begin{short}
    Finds the icon at the given position and return its index.
  @end{short}
  The coordinates of the posistion are relative to the top left corner of the
  text entry. If x, y does not lie inside an icon, -1 is returned. This function
  is intended for use in a @code{\"query-tooltip\"} signal handler.
  @see-class{gtk:entry}"
  (entry (g:object entry))
  (x :int)
  (y :int))

(export 'entry-icon-at-pos)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_tooltip_text
;;; gtk_entry_get_icon_tooltip_text
;;; ----------------------------------------------------------------------------

(defun (setf entry-icon-tooltip-text) (tooltip entry pos)
  (cond ((eq pos :primary)
         (setf (entry-primary-icon-tooltip-text entry) tooltip))
        ((eq pos :secondary)
         (setf (entry-secondary-icon-tooltip-text entry) tooltip))
        (t
         (error "GTK:ENTRY-ICON-TOOLTIP-TEXT: Unexpected icon position"))))

(defun entry-icon-tooltip-text (entry pos)
 #+liber-documentation
 "@version{#2025-05-31}
  @syntax{(gtk:entry-icon-tooltip-text entry pos) => tooltip}
  @syntax{(setf (gtk:entry-icon-tooltip-text entry pos) tooltip)}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value for the icon position}
  @argument[tooltip]{a string for the contents of the tooltip for the icon,
    or @code{nil}}
  @begin{short}
    The @fun{gtk:entry-icon-tooltip-text} function gets the contents of the
    tooltip on the icon at the specified position in the text entry.
  @end{short}
  The @setf{gtk:entry-icon-tooltip-text} function sets a tooltip for the icon at
  the specified position. Use @code{nil} for @arg{tooltip} to remove an existing
  tooltip.

  See also the @fun{gtk:widget-tooltip-text} and
  @fun{gtk:entry-icon-tooltip-markup} functions.
  @see-class{gtk:entry}
  @see-symbol{gtk:entry-icon-position}
  @see-function{gtk:widget-tooltip-text}
  @see-function{gtk:entry-icon-tooltip-markup}"
  (cond ((eq pos :primary)
         (entry-primary-icon-tooltip-text entry))
        ((eq pos :secondary)
         (entry-secondary-icon-tooltip-text entry))
        (t
         (error "GTK:ENTRY-ICON-TOOLTIP-TEXT: Unexpected icon position"))))

(export 'entry-icon-tooltip-text)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_tooltip_markup
;;; gtk_entry_get_icon_tooltip_markup
;;; ----------------------------------------------------------------------------

(defun (setf entry-icon-tooltip-markup) (tooltip entry pos)
  (cond ((eq pos :primary)
         (setf (entry-primary-icon-tooltip-markup entry) tooltip))
        ((eq pos :secondary)
         (setf (entry-secondary-icon-tooltip-markup entry) tooltip))
        (t
         (error "GTK:ENTRY-ICON-TOOLTIP-MARKUP: Unexpected icon position"))))

(defun entry-icon-tooltip-markup (entry pos)
 #+liber-documentation
 "@version{#2025-05-31}
  @syntax{(gtk:entry-icon-tooltip-markup entry pos) => tooltip}
  @syntax{(setf (gtk:entry-icon-tooltip-markup entry pos) tooltip)}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value for the icon position}
  @argument[tooltip]{a string for the contents of the tooltip for the icon,
    or @code{nil}}
  @begin{short}
    The @fun{gtk:entry-icon-tooltip-markup} function gets the contents of the
    tooltip on the icon at the specified position in the text entry.
  @end{short}
  The @setf{gtk:entry-icon-tooltip-markup} function sets the tooltip for the
  icon at the specified position. The @arg{tooltip} argument is assumed to be
  marked up with the Pango text markup language. Use @code{nil} for
  @arg{tooltip} to remove an existing tooltip.

  See also the @fun{gtk:widget-tooltip-markup} and
  @fun{gtk:entry-icon-tooltip-text} functions.
  @see-class{gtk:entry}
  @see-symbol{gtk:entry-icon-position}
  @see-function{gtk:widget-tooltip-markup}
  @see-function{gtk:entry-icon-tooltip-text}"
  (cond ((eq pos :primary)
         (entry-primary-icon-tooltip-markup entry))
        ((eq pos :secondary)
         (entry-secondary-icon-tooltip-markup entry))
        (t
         (error "GTK:ENTRY-ICON-TOOLTIP-MARKUP: Unexpected icon position"))))

(export 'entry-icon-tooltip-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_set_icon_drag_source
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_set_icon_drag_source" entry-set-icon-drag-source)
    :void
 #+liber-documentation
 "@version{#2025-05-31}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value for the icon position}
  @argument[provider]{a @class{gdk:content-provider} object}
  @argument[actions]{a @symbol{gdk:drag-action} bitmask for the allowed drag
    actions}
  @begin{short}
    Sets up the icon at the given position so that GTK will start a drag
    operation when the user clicks and drags the icon.
  @end{short}
  @see-class{gtk:entry}
  @see-class{gdk:content-provider}
  @see-symbol{gdk:drag-action}
  @see-symbol{gtk:entry-icon-position}"
  (entry (g:object entry))
  (pos entry-icon-position)
  (provider (g:object gdk:content-provider))
  (actions gdk:drag-action))

(export 'entry-set-icon-drag-source)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_current_icon_drag_source
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_get_current_icon_drag_source"
               entry-current-icon-drag-source) :int
 #+liber-documentation
 "@version{#2025-05-31}
  @argument[entry]{a @class{gtk:entry} widget}
  @begin{return}
    The integer with the index of the icon which is the source of the current
    DND operation, or -1.
  @end{return}
  @begin{short}
    Returns the index of the icon which is the source of the current DND
    operation, or -1.
  @end{short}
  @see-class{gtk:entry}"
  (entry (g:object entry)))

(export 'entry-current-icon-drag-source)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_get_icon_area
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_get_icon_area" %entry-get-icon-area) :void
  (entry (g:object entry))
  (pos entry-icon-position)
  (area (g:boxed gdk:rectangle)))

(defun entry-icon-area (entry pos)
 #+liber-documentation
 "@version{#2025-05-31}
  @argument[entry]{a @class{gtk:entry} widget}
  @argument[pos]{a @symbol{gtk:entry-icon-position} value for the icon position}
  @return{The @class{gdk:rectangle} instance with the area of the icon.}
  @begin{short}
    Gets the area where the icon of the text entry at @arg{pos} is drawn.
  @end{short}
  This function is useful when drawing something to the text entry in a draw
  callback function.

  If the text entry is not realized or has no icon at the given position,
  @arg{area} is filled with zeros. Otherwise, @arg{area} will be filled with
  the allocation of the icon, relative to the allocation of the text entry.
  @see-class{gtk:entry}
  @see-class{gdk:rectangle}
  @see-symbol{gtk:entry-icon-position}"
  (let ((area (gdk:rectangle-new)))
    (%entry-get-icon-area entry pos area)
    area))

(export 'entry-icon-area)

;;; ----------------------------------------------------------------------------
;;; gtk_entry_grab_focus_without_selecting
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_entry_grab_focus_without_selecting"
               entry-grab-focus-without-selecting) :void
 #+liber-documentation
 "@version{#2025-05-31}
  @argument[entry]{a @class{gtk:entry} widget}
  @begin{short}
    Causes the text entry to have keyboard focus.
  @end{short}
  It behaves like the @fun{gtk:widget-grab-focus} function, except that it
  does not select the contents of the text entry. You only want to call this on
  some special entries which the user usually does not want to replace all text
  in, such as search-as-you-type entries.
  @see-class{gtk:entry}
  @see-function{gtk:widget-grab-focus}"
  (entry (g:object entry)))

(export 'entry-grab-focus-without-selecting)

;;; --- End of file gtk4.entry.lisp --------------------------------------------
