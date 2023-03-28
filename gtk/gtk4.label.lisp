;;; ----------------------------------------------------------------------------
;;; gtk4.label.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;; GtkLabel
;;;
;;;     A widget that displays a small to medium amount of text
;;;
;;; Types and Values
;;;
;;;     GtkLabel
;;;
;;; Accessors
;;;
;;;     gtk_label_get_attributes
;;;     gtk_label_set_attributes
;;;     gtk_label_get_ellipsize
;;;     gtk_label_set_ellipsize
;;;     gtk_label_get_extra_menu
;;;     gtk_label_set_extra_menu
;;;     gtk_label_get_justify
;;;     gtk_label_set_justify
;;;     gtk_label_get_label
;;;     gtk_label_set_label
;;;     gtk_label_get_lines
;;;     gtk_label_set_lines
;;;     gtk_label_get_max_width_chars
;;;     gtk_label_set_max_width_chars
;;;     gtk_label_get_mnemonic_keyval
;;;     gtk_label_get_mnemonic_widget
;;;     gtk_label_set_mnemonic_widget
;;;     gtk_label_get_selectable
;;;     gtk_label_set_selectable
;;;     gtk_label_get_single_line_mode
;;;     gtk_label_set_single_line_mode
;;;     gtk_label_get_use_markup
;;;     gtk_label_set_use_markup
;;;     gtk_label_get_use_underline
;;;     gtk_label_set_use_underline
;;;     gtk_label_get_width_chars
;;;     gtk_label_set_width_chars
;;;
;;; Functions
;;;
;;;     gtk_label_new
;;;     gtk_label_set_text
;;;     gtk_label_set_markup
;;;     gtk_label_set_markup_with_mnemonic
;;;     gtk_label_get_layout_offsets
;;;     gtk_label_get_text
;;;     gtk_label_new_with_mnemonic
;;;     gtk_label_select_region
;;;     gtk_label_set_text_with_mnemonic
;;;     gtk_label_get_layout
;;;     gtk_label_get_selection_bounds
;;;     gtk_label_get_current_uri
;;;
;;; Properties
;;;
;;;     attributes
;;;     ellipsize
;;;     extra-menu
;;;     justify
;;;     label
;;;     lines
;;;     max-width-chars
;;;     mnemonic-keyval
;;;     mnemonic-widget
;;;     selectable
;;;     single-line-mode
;;;     use-markup
;;;     use-underline
;;;     width-chars
;;;     wrap
;;;     wrap-mode
;;;     xalign
;;;     yalign
;;;
;;; Signals
;;;
;;;     activate-current-link
;;;     activate-link
;;;     copy-clipboard
;;;     move-cursor
;;;
;;; Actions
;;;
;;;     link.copy
;;;     link.open
;;;     selection.select-all
;;;     selection.delete
;;;     clipboard.paste
;;;     clipboard.copy
;;;     clipboard.cut
;;;     menu.popup
;;;
;;; Hierarchy
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkLabel
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkLabel
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkLabel" label
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_label_get_type")
  ((attributes
    label-attributes
    "attributes" "PangoAttrList" t t)
   (ellipsize
    label-ellipsize
    "ellipsize" "PangoEllipsizeMode" t t)
   (extra-menu
    label-extra-menu
    "extra-menu" "GMenuModel" t t)
   (justify
    label-justify
    "justify" "GtkJustification" t t)
   (label
    label-label
    "label" "gchararray" t t)
   (lines
    label-lines
    "lines" "gint" t t)
   (max-width-chars
    label-max-width-chars
    "max-width-chars" "gint" t t)
   (mnemonic-keyval
    label-mnemonic-keyval
    "mnemonic-keyval" "guint" t nil)
   (mnemonic-widget
    label-mnemonic-widget
    "mnemonic-widget" "GtkWidget" t t)
   #+gtk-4-6
   (natural-wrap-mode
    label-natural-wrap-mode
    "natural-wrap-mode" "GtkNaturalWrapMode" t t)
   (selectable
    label-selectable
    "selectable" "gboolean" t t)
   (single-line-mode
    label-single-line-mode
    "single-line-mode" "gboolean" t t)
   (use-markup
    label-use-markup
    "use-markup" "gboolean" t t)
   (use-underline
    label-use-underline
    "use-underline" "gboolean" t t)
   (width-chars
    label-width-chars
    "width-chars" "gint" t t)
   (wrap
    label-wrap
    "wrap" "gboolean" t t)
   (wrap-mode
    label-wrap-mode
    "wrap-mode" "PangoWrapMode" t t)
   (xalign
    label-xalign
    "xalign" "gfloat" t t)
   (yalign
    label-yalign
    "yalign" "gfloat" t t)))

#+liber-documentation
(setf (documentation 'label 'type)
 "@version{2023-3-19}
  @begin{short}
    The @sym{gtk:label} widget displays a small amount of text.
  @end{short}
  As the name implies, most labels are used to label another widget such as
  a @class{gtk:button} widget.

  @image[label]{Figure: GtkLabel}

  @subheading{Mnemonics}
  Labels may contain mnemonics. Mnemonics are underlined characters in the
  label, used for keyboard navigation. Mnemonics are created by providing a
  string with an underscore before the mnemonic character, such as \"_File\",
  to the @fun{gtk:label-new-with-mnemonic} or
  @fun{gtk:label-set-text-with-mnemonic} functions.

  Mnemonics automatically activate any activatable widget the label is inside,
  such as a @class{gtk:button} widget. If the label is not inside the
  target widget of the mnemonic, you have to tell the label about the target
  using the @fun{gtk:label-mnemonic-widget} function. Here is a simple example
  where the label is inside a button:
  @begin{pre}
;; Pressing Alt+H will activate this button
(let* ((label (gtk:label-new-with-mnemonic \"_Hello\"))
       (button (make-instance 'gtk:button
                              :child label)))
   ... )
  @end{pre}
  There is a convenience function to create buttons with a mnemonic label
  already inside:
  @begin{pre}
;; Pressing Alt+H will activate this button
(gtk:button-new-with-mnemonic \"_Hello\")
  @end{pre}
  To create a mnemonic for a widget alongside the label, such as a
  @class{gtk:entry} widget, you have to point the label at the entry with the
  @fun{gtk:label-mnemonic-widget} function:
  @begin{pre}
;; Pressing Alt+H will focus the entry
(let ((entry (make-instance 'gtk:entry))
      (label (gtk:label-new-with-mnemonic \"_Hello\")))
   (setf (gtk:label-mnemonic-widget label) entry)
   ... )
  @end{pre}
  @subheading{Markup (styled text)}
  To make it easy to format text in a label, changing colors, fonts, etc.,
  label text can be provided in a simple markup format. Here is how to create
  a label with a small font:
  @begin{pre}
(let ((label (make-instance 'gtk:label)))
  (gtk:label-set-markup label
                        \"<span style=\"color: red\">
                         <small>Small text</small></span>\")
  ... )
  @end{pre}
  See complete documentation of available tags in the Pango manual.

  The markup passed to the @fun{gtk:label-set-markup} function must be valid.
  For example, literal <, > and & characters must be escaped as \<, \gt;, and
  \&. If you pass text obtained from the user, file, or a network to the
  @fun{gtk:label-set-markup} function, you will want to escape it with the
  @code{g_markup_escape_text()} or @code{g_markup_printf_escaped()} functions.

  Markup strings are just a convenient way to set the @class{pango:attr-list}
  instance on a label. The @fun{gtk:label-attributes} function may be a simpler
  way to set attributes in some cases. Be careful though, a
  @class{pango:attr-list} instance tends to cause internationalization problems,
  unless you are applying attributes to the entire string, i.e. unless you set
  the range of each attribute to @code{[0, G_MAXINT]}). The reason is that
  specifying the @code{start_index} and @code{end_index} for a
  @class{pango:attribute} structure requires knowledge of the exact string being
  displayed, so translations will cause problems.

  @subheading{Selectable labels}
  Labels can be made selectable with the @fun{gtk:label-selectable} function.
  Selectable labels allow the user to copy the label contents to the clipboard.
  Only labels that contain useful-to-copy information - such as error messages -
  should be made selectable.

  @subheading{Text layout}
  A label can contain any number of paragraphs, but will have performance
  problems if it contains more than a small number. Paragraphs are separated
  by newlines or other paragraph separators understood by Pango.

  Labels can automatically wrap text if you call the @fun{gtk:label-wrap}
  function.

  The @fun{gtk:label-justify} function sets how the lines in a label align with
  one another. If you want to set how the label as a whole aligns in its
  available space, see the @slot[widget]{halign} and
  @slot[widget]{valign} properties.

  The @slot[gtk:label]{width-chars} and @slot[gtk:label]{max-width-chars}
  properties can be used to control the size allocation of ellipsized or
  wrapped labels. For ellipsizing labels, if either is specified and less than
  the actual text size, it is used as the minimum width, and the actual text
  size is used as the natural width of the label. For wrapping labels, the
  @slot[gtk:label]{width-chars} property is used as the minimum width, if
  specified, and the @slot[gtk:label]{max-width-chars} property is used as the
  natural width. Even if the @slot[gtk:label]{max-width-chars} property
  specified, wrapping labels will be rewrapped to use all of the available
  width.

  Note that the interpretation of the @slot[gtk:label]{width-chars} and
  @slot[gtk:label]{max-width-chars} properties has changed a bit with the
  introduction of \"width-for-height\" geometry management.

  @subheading{Links}
  GTK supports markup for clickable hyperlinks in addition to regular Pango
  markup. The markup for links is borrowed from HTML, using the @code{a} with
  href and title attributes. GTK renders links similar to the way they appear
  in web browsers, with colored, underlined text. The title attribute is
  displayed as a tooltip on the link. An example looks like this:
  @begin{pre}
(gtk:label-set-markup label
                      \"Go to the <span style=\"color: red\">
                                   <a>GTK website</a></span> for more...\")
  @end{pre}
  It is possible to implement custom handling for links and their tooltips
  with the \"activate-link\" signal and the @fun{gtk:label-current-uri}
  function.
  @begin[GtkLabel as GtkBuildable]{dictionary}
    The @sym{gtk:label} implementation of the @class{gtk:buildable} interface
    supports a custom @code{<attributes>} element, which supports any number of
    @code{<attribute>} elements. The @code{<attribute>} element has attributes
    named @code{name}, @code{value}, @code{start} and @code{end} and allows you
    to specify @code{PangoAttribute} values for the label.

    @b{Example:} A UI definition fragment specifying Pango attributes
  @begin{pre}
 <object class=\"GtkLabel\">
  <attributes>
     <attribute name=\"weight\" value=\"PANGO_WEIGHT_BOLD\"/>
     <attribute name=\"background\" value=\"red\" start=\"5\" end=\"10\"/>\"
   </attributes>
 </object>
    @end{pre}
    The @code{start} and @code{end} attributes specify the range of characters
    to which the Pango attribute applies. If @code{start} and @code{end} are not
    specified, the attribute is applied to the whole text. Note that specifying
    ranges does not make much sense with translatable attributes. Use markup
    embedded in the translatable content instead.
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
label
├── [selection]
├── [link]
┊
╰── [link]
    @end{pre}
    The @sym{gtk:label} implementation has a single CSS node with the name
    @code{label}. A wide variety of style classes may be applied to labels, such
    as the @code{.title}, @code{.subtitle}, @code{.dim-label} style classes. In
    the @class{gtk:shortcuts-window} widget, labels are used wth the
    @code{.keycap} style class.

    If the label has a selection, it gets a subnode with name @code{selection}.

    If the label has links, there is one subnode per link. These subnodes carry
    the link or visited state depending on whether they have been visited. In
    this case, the label node also gets a @code{.link} style class.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @sym{gtk:label} implementation uses the @code{:label} role of the
    @symbol{gtk:accessible-role} enumeration.
  @end{dictionary}
  @begin[Action Details]{dictionary}
    @subheading{The \"link.copy\" action}
      Copies the link to the clipboard, when activated on a link inside the
      label.
    @subheading{The \"link.open\" action}
      Opens the link, when activated on a link inside the label.
    @subheading{The \"selection.select-all\" action}
      Selects all of the text, if the label allows selection.
    @subheading{The \"selection.delete\" action}
      Does not do anything, since text in labels can not be deleted.
    @subheading{The \"clipboard.paste\" action}
      Doe not do anything, since text in labels can not be edited.
    @subheading{The \"clipboard.copy\" action}
      Copies the text to the clipboard.
    @subheading{The \"clipboard.cut\" action}
      Does not do anything, since text in labels can not be deleted.
    @subheading{The \"menu.popup\" action}
      Opens the context menu.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate-current-link\" signal}
      @begin{pre}
lambda (label)    :action
      @end{pre}
      A keybinding signal which gets emitted when the user activates a link in
      the label. Applications may also emit the signal with the
      @fun{g:signal-emit} function if they need to control activation of URIs
      programmatically. The default bindings for this signal are all forms of
      the @kbd{Enter} key.
      @begin[code]{table}
        @entry[label]{The @sym{gtk:label} widget on which the signal was
          emitted.}
      @end{table}
    @subheading{The \"activate-link\" signal}
      @begin{pre}
lambda (label uri)    :run-last
      @end{pre}
      The signal which gets emitted to activate a URI. Applications may connect
      to it to override the default behaviour, which is to call the
      @fun{gtk:show-uri} function.
      @begin[code]{table}
        @entry[label]{The @sym{gtk:label} widget on which the signal was
          emitted.}
        @entry[uri]{A string with the URI that is activated.}
        @entry[Returns]{@em{True} if the link has been activated.}
      @end{table}
    @subheading{The \"copy-clipboard\" signal}
      @begin{pre}
lambda (label)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted to copy the selection
      to the clipboard. The default binding for this signal is the @kbd{Ctrl-c}
      key.
      @begin[code]{table}
        @entry[label]{The @sym{gtk:label} widget which received the signal.}
      @end{table}
    @subheading{The \"move-cursor\" signal}
      @begin{pre}
lambda (label step count extend)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted when the user
      initiates a cursor movement. If the cursor is not visible in the label,
      this signal causes the viewport to be moved instead. Applications should
      not connect to it, but may emit it with the @fun{g:signal-emit} function
      if they need to control the cursor programmatically. The default bindings
      for this signal come in two variants, the variant with the @kbd{Shift}
      modifier extends the selection, the variant without the @kbd{Shift}
      modifer does not. There are too many key combinations to list them all
      here.
      @begin{itemize}
        @item{Arrow keys move by individual characters/lines.}
        @item{@kbd{Ctrl}-arrow key combinations move by words/paragraphs.}
        @item{@kbd{Home}/@kbd{End} keys move to the ends of the buffer.}
      @end{itemize}
      @begin[code]{table}
        @entry[label]{The @sym{gtk:label} widget which received the signal.}
        @entry[step]{The granularity of the move, as a value of the
          @symbol{gtk:movement-step} enumeration.}
        @entry[count]{An integer with the number of step units to move.}
        @entry[extend]{@em{True} if the move should extend the selection.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:label-new}
  @see-constructor{gtk:label-new-with-mnemonic}
  @see-slot{gtk:label-attributes}
  @see-slot{gtk:label-ellipsize}
  @see-slot{gtk:label-extra-menu}
  @see-slot{gtk:label-justify}
  @see-slot{gtk:label-label}
  @see-slot{gtk:label-lines}
  @see-slot{gtk:label-max-width-chars}
  @see-slot{gtk:label-mnemonic-keyval}
  @see-slot{gtk:label-mnemonic-widget}
  @see-slot{gtk:label-natural-wrap-mode}
  @see-slot{gtk:label-selectable}
  @see-slot{gtk:label-single-line-mode}
  @see-slot{gtk:label-use-markup}
  @see-slot{gtk:label-use-underline}
  @see-slot{gtk:label-width-chars}
  @see-slot{gtk:label-wrap}
  @see-slot{gtk:label-wrap-mode}
  @see-slot{gtk:label-xalign}
  @see-slot{gtk:label-yalign}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- label-attributes ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "attributes" 'label) t)
 "The @code{attributes} property of type @class{pango:attr-list}
  (Read / Write) @br{}
  A list of style attributes to apply to the text of the label.")

#+liber-documentation
(setf (liber:alias-for-function 'label-attributes)
      "Accessor"
      (documentation 'label-attributes 'function)
 "@version{#2021-10-31}
  @syntax[]{(gtk:label-attributes object) => attrs}
  @syntax[]{(setf (gtk:label-attributes object) attrs)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[attrs]{a @class{pango:attr-list} instance}
  @begin{short}
    Accessor of the @slot[gtk:label]{attributes} slot of the @class{gtk:label}
    class.
  @end{short}
  The @sym{gtk:label-attributes} function gets the attribute list that was set
  on the label, if any. The @sym{(setf gtk:label-attributes)} function sets a
  attribute list. The attributes in the list are applied to the label text.

  This function does not reflect attributes that come from the labels markup,
  see the @fun{gtk:label-set-markup} function. If you want to get the
  effective attributes for the label, use
  @begin{pre}
(pango:layout-attributes (gtk:label-layout label))
  @end{pre}
  @begin[Note]{dictionary}
    The attributes set with this function will be applied and merged with any
    other attributes previously effected by way of the
    @slot[gtk:label]{use-underline} or @slot[gtk:label]{use-markup} properties.
    While it is not recommended to mix markup strings with manually set
    attributes, if you must, know that the attributes will be applied to the
    label after the markup string is parsed.
  @end{dictionary}
  @see-class{gtk:label}
  @see-class{pango:attr-list}
  @see-function{gtk:label-set-markup}
  @see-function{gtk:label-layout}
  @see-function{gtk:label-use-markup}
  @see-function{gtk:label-use-underline}")

;;; --- label-ellipsize ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "ellipsize" 'label) t)
 "The @code{ellipsize} property of type @symbol{pango:ellipsize-mode}
  (Read / Write) @br{}
  The preferred place to ellipsize the string, if the label does not have
  enough room to display the entire string, specified as a value of the
  @symbol{pango:ellipsize-mode} enumeration. Note that setting this property to
  a value other than @code{:none} has the side-effect that the label requests
  only enough space to display the ellipsis \"...\". In particular, this means
  that ellipsizing labels do not work well in notebook tabs, unless the
  @code{tab-expand} child property of the tab is set to @em{true}. Other ways
  to set a width of the label are the @fun{gtk:widget-size-request} and
  @fun{gtk:label-width-chars} functions. @br{}
  Default value: @code{:none}")

#+liber-documentation
(setf (liber:alias-for-function 'label-ellipsize)
      "Accessor"
      (documentation 'label-ellipsize 'function)
 "@version{#2021-10-31}
  @syntax[]{(gtk:label-ellipsize object) => mode}
  @syntax[]{(setf (gtk:label-ellipsize object) mode)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[mode]{a value of the @symbol{pango:ellipsize-mode} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:label]{ellipsize} slot of the @class{gtk:label}
    class.
  @end{short}
  The @sym{gtk:label-ellipsize} function returns the ellipsizing position of
  the label. The @sym{(setf gtk:label-ellipsize)} function sets the mode used
  to ellipsize, add an ellipsis: \"...\", to the text if there is not enough
  space to render the entire string.
  @see-class{gtk:label}
  @see-symbol{pango:ellipsize-mode}")

;;; --- label-extra-menu -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "extra-menu" 'label) t)
 "The @code{extra-menu} property of type @class{g:menu-model} (Read / Write)
  @br{}
  A menu model whose contents will be appended to the context menu.")

#+liber-documentation
(setf (liber:alias-for-function 'label-extra-menu)
      "Accessor"
      (documentation 'label-extra-menu 'function)
 "@version{#2022-1-21}
  @syntax[]{(gtk:label-extra-menu object) => menu}
  @syntax[]{(setf (gtk:label-extra-menu object) menu)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[menu]{a @class{g:menu-model} object}
  @begin{short}
    Accessor of the @slot[gtk:label]{extra-menu} slot of the @class{gtk:label}
    class.
  @end{short}
  The @sym{gtk:label-extra-menu} function gets the menu model. The
  @sym{(setf gtk:label-extra-menu)} function sets a menu model to add when
  constructing the context menu for the label.
  @see-class{gtk:label}
  @see-class{g:menu-model}")

;;; --- label-justify ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "justify" 'label) t)
 "The @code{justify} property of type @symbol{gtk:justification} (Read / Write)
  @br{}
  The alignment of the lines in the text of the label relative to each other.
  This does not affect the alignment of the label within its allocation. See
  the @slot[gtk:label]{xalign} property for that. @br{}
  Default value: @code{:left}")

#+liber-documentation
(setf (liber:alias-for-function 'label-justify)
      "Accessor"
      (documentation 'label-justify 'function)
 "@version{#2021-10-31}
  @syntax[]{(gtk:label-justify object) => justify}
  @syntax[]{(setf (gtk:label-justify object) justify)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[justify]{a value of the @symbol{gtk:justification} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:label]{justify} slot of the @class{gtk:label}
    class.
  @end{short}
  The @sym{gtk:label-justify} function returns the justification of the label.
  The @sym{(setf gtk:label-justify)} function sets the alignment of the lines
  in the text of the label relative to each other.

  The @code{:left} value is the default value when the widget is first created
  with the @fun{gtk:label-new} function. If you instead want to set the
  alignment of the label as a whole, use the @fun{gtk:widget-halign} function
  instead. The @sym{(setf gtk:label-justify)} function has no effect on labels
  containing only a single line.
  @see-class{gtk:label}
  @see-symbol{gtk:justification}
  @see-function{gtk:label-new}
  @see-function{gtk:widget-halign}")

;;; --- label-label ------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label" 'label) t)
 "The @code{label} property of type @code{:string} (Read / Write) @br{}
  The text of the label. @br{}
  Default value: \"\"")

#+liber-documentation
(setf (liber:alias-for-function 'label-label)
      "Accessor"
      (documentation 'label-label 'function)
 "@version{2023-3-19}
  @syntax[]{(gtk:label-label object) => text}
  @syntax[]{(setf (gtk:label-label object) text)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[text]{a string with the text for the label}
  @begin{short}
    Accessor of the @slot[gtk:label]{label} slot of the @class{gtk:label}
    class.
  @end{short}
  The @sym{gtk:label-label} function returns the text of the label widget
  including any embedded underlines indicating mnemonics and Pango markup.
  The @sym{(setf gtk:label-label)} function sets the text of the label. The
  label is interpreted as including embedded underlines and/or Pango markup
  depending on the values of the @slot[gtk:label]{use-underline} and
  @slot[gtk:label]{use-markup} properties.

  See also the @fun{gtk:label-text} function, which fetches the text from a
  label, as displayed on the screen.
  @see-class{gtk:label}
  @see-function{gtk:label-text}
  @see-function{gtk:label-use-markup}
  @see-function{gtk:label-use-underline}")

;;; --- label-lines --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "lines" 'label) t)
 "The @code{lines} property of type @code{:int} (Read / Write) @br{}
  The number of lines to which an ellipsized, wrapping label should be limited.
  This property has no effect if the label is not wrapping or ellipsized. Set
  this property to -1 if you do not want to limit the number of lines. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'label-lines)
      "Accessor"
      (documentation 'label-lines 'function)
 "@version{#2021-10-31}
  @syntax[]{(gtk:label-lines object) => lines}
  @syntax[]{(setf (gtk:label-lines object) lines)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[lines]{an integer with the desired number of lines, or -1}
  @begin{short}
    Accessor of the @slot[gtk:label]{lines} slot of the @class{gtk:label}
    class.
  @end{short}
  The @sym{gtk:label-lines} function gets the number of lines to which an
  ellipsized, wrapping label should be limited. The @sym{(setf gtk:label-lines)}
  function sets the number of lines. This has no effect if the label is not
  wrapping or ellipsized. Set this to -1 if you do not want to limit the number
  of lines.
  @see-class{gtk:label}")

;;; --- label-max-width-chars ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "max-width-chars" 'label) t)
 "The @code{max-width-chars} property of type @code{:int} (Read / Write) @br{}
  The desired maximum width of the label, in characters. If this property is
  set to -1, the width will be calculated automatically. See the section on text
  layout for details of how the @code{width-chars} and @code{max-width-chars}
  properties determine the width of ellipsized and wrapped labels. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'label-max-width-chars)
      "Accessor"
      (documentation 'label-max-width-chars 'function)
 "@version{#2021-10-31}
  @syntax[]{(gtk:label-max-width-chars object) => n-chars}
  @syntax[]{(setf (gtk:label-max-width-chars object) n-chars)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[n-chars]{an integer with the desired maximum width, in characters}
  @begin{short}
    Accessor of the @slot[gtk:label]{max-width-chars} slot of the
    @class{gtk:label} class.
  @end{short}
  The @sym{gtk:label-max-width-chars} function returns the maximum width of the
  label in characters. The @sym{(setf gtk:label-max-width-chars)} function sets
  the desired maximum width.
  @see-class{gtk:label}")

;;; --- label-mnemonic-keyval ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "mnemonic-keyval" 'label) t)
 "The @code{mnemonic-keyval} property of @code{:uint} (Read) @br{}
  The mnemonic accelerator key for this label. @br{}
  Default value: @code{#xffffff}")

#+liber-documentation
(setf (liber:alias-for-function 'label-mnemonic-keyval)
      "Accessor"
      (documentation 'label-mnemonic-keyval 'function)
 "@version{#2021-10-31}
  @syntax[]{(gtk:label-mnemonic-keyval object) => keyval}
  @argument[object]{a @class{gtk:label} widget}
  @argument[keyval]{an unsigned integer with the keyval}
  @begin{short}
    Accessor of the @slot[gtk:label]{mnemonic-keyval} slot of the
    @class{gtk:label} class.
  @end{short}
  If the label has been set so that it has a mnemonic key the
  @sym{gtk:label-mnemonic-keyval} function returns the keyval used for the
  mnemonic accelerator. If there is no mnemonic set up it returns
  @code{#xffffff}.
  @begin[Example]{dictionary}
    @begin{pre}
(setq label (gtk:label-new-with-mnemonic \"_Print\"))
=> #<gtk:label {1001A051E3@}>
(gtk:label-mnemonic-keyval label) => 112
    @end{pre}
  @end{dictionary}
  @see-class{gtk:label}")

;;; --- label-mnemonic-widget ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "mnemonic-widget" 'label) t)
 "The @code{mnemonic-widget} property of type @class{gtk:widget}
  (Read / Write) @br{}
  The widget to be activated when the mnemonic key of the label is pressed.")

#+liber-documentation
(setf (liber:alias-for-function 'label-mnemonic-widget)
      "Accessor"
      (documentation 'label-mnemonic-widget 'function)
 "@version{#2021-10-31}
  @syntax[]{(gtk:label-mnemonic-widget object) => widget}
  @syntax[]{(setf (gtk:label-mnemonic-widget object) widget)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[widget]{a @class{gtk:widget} target}
  @begin{short}
    Accessor of the @slot[gtk:label]{mnemonic-widget} slot of the
    @class{gtk:label} class.
  @end{short}
  The @sym{gtk:label-mnemonic-widget} function returns the target of the
  mnemonic of the label, or @code{nil} if none has been set and the default
  algorithm will be used.

  If the label has been set so that it has an mnemonic key, using i.e.
  the @fun{gtk:label-set-markup-with-mnemonic},
  @fun{gtk:label-set-text-with-mnemonic}, @fun{gtk:label-new-with-mnemonic}
  functions or the @slot[gtk:label]{use-underline} property, the label can be
  associated with a widget that is the target of the mnemonic.

  When the label is inside a widget, like a @class{gtk:button} widget or a
  @class{gtk:notebook} tab, it is automatically associated with the correct
  widget, but sometimes, i.e. when the target is a @class{gtk:entry} widget
  next to the label, you need to set it explicitly using this function.

  The target widget will be accelerated by emitting the \"mnemonic-activate\"
  signal on it. The default handler for this signal will activate the widget if
  there are no mnemonic collisions and toggle focus between the colliding
  widgets otherwise.
  @see-class{gtk:label}
  @see-class{gtk:widget}
  @see-function{gtk:label-set-markup-with-mnemonic}
  @see-function{gtk:label-set-text-with-mnemonic}
  @see-function{gtk:label-new-with-mnemonic}
  @see-function{gtk:label-use-underline}")

;;; --- label-natural-wrap-mode --------------------------------------------

#+(and gtk-4-6 liber-documentation)
(setf (documentation (liber:slot-documentation "natural-wrap-mode"
                                               'label) t)
 "The @code{natural-wrap-mode} property of type @symbol{gtk:natural-wrap-mode}
  (Read / Write) @br{}
  Select the line wrapping for the natural size request. This only affects the
  natural size requested. For the actual wrapping used, see the @code{wrap-mode}
  property. The default is the @code{:inherit} value, which inherits
  the behavior of the @code{wrap-mode} property. Since 4.6 @br{}
  Default value: @code{:inherit}")

#+(and gtk-4-6 liber-documentation)
(setf (liber:alias-for-function 'label-natural-wrap-mode)
      "Accessor"
      (documentation 'label-natural-wrap-mode 'function)
 "@version{#2022-7-21}
  @syntax[]{(gtk:label-natural-wrap-mode object) => mode}
  @syntax[]{(setf (gtk:label-natural-wrap-mode object) mode)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[mode]{a @symbol{gtk:natural-wrap-mode} value}
  @begin{short}
    Accessor of the @slot[gtk:label]{natural-wrap-mode} slot of the
    @class{gtk:label} class.
  @end{short}
  The @sym{gtk:label-natural-wrap-mode} function returns the line wrap mode used
  by the label. The @sym{(setf gtk:label-natural-wrap-mode)} function sets the
  line wrap mode.

  This only affects the natural size requested, for the actual wrapping used,
  see the @slot[gtk:label]{wrap-mode} property.

  Since 4.6
  @see-class{gtk:label}
  @see-function{gtk:label-wrap-mode}")

;;; --- label-selectable ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "selectable" 'label) t)
 "The @code{selectable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the label text can be selected with the mouse. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'label-selectable)
      "Accessor"
      (documentation 'label-selectable 'function)
 "@version{#2021-10-31}
  @syntax[]{(gtk:label-selectable object) => selectable}
  @syntax[]{(setf (gtk:label-selectable object) selectable)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[selectable]{@em{true} to allow selecting text in the label}
  @begin{short}
    Accessor of the @slot[gtk:label]{selectable} slot of the @class{gtk:label}
    class.
  @end{short}
  Selectable labels allow the user to select text from the label, for copy and
  paste.
  @see-class{gtk:label}")

;;; --- label-single-line-mode ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "single-line-mode"
                                               'label) t)
 "The @code{single-line-mode} property of type  @code{:boolean} (Read / Write)
  @br{}
  Whether the label is in single line mode. In single line mode, the height
  of the label does not depend on the actual text, it is always set to the
  @code{(ascent + descent)} value of the font. This can be an advantage in
  situations where resizing the label because of text changes would be
  distracting, e.g. in a statusbar. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'label-single-line-mode)
      "Accessor"
      (documentation 'label-single-line-mode 'function)
 "@version{#2021-10-31}
  @syntax[]{(gtk:label-single-line-mode object) => mode}
  @syntax[]{(setf (gtk:label-single-line-mode object) mode)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[mode]{@em{true} if the label should be in single line mode}
  @begin{short}
    Accessor of the @slot[gtk:label]{single-line-mode} slot of the
    @class{gtk:label} class.
  @end{short}
  The @sym{gtk:label-single-line-mode} function returns whether the label is in
  single line mode. The @sym{(setf gtk:label-single-line-mode)} function sets
  whether the label is in single line mode.
  @see-class{gtk:label}")

;;; --- label-use-markup ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-markup" 'label) t)
 "The @code{use-markup} property of type @code{:boolean} (Read / Write) @br{}
  The text of the label includes XML Pango markup. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'label-use-markup)
      "Accessor"
      (documentation 'label-use-markup 'function)
 "@version{#2021-12-3}
  @syntax[]{(gtk:label-use-markup object) => setting}
  @syntax[]{(setf (gtk:label-use-markup object) setting)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[setting]{@em{true} if the text of the label should be parsed for
    markup}
  @begin{short}
    Accessor of the @slot[gtk:label]{use-markup} slot of the @class{gtk:label}
    class.
  @end{short}
  The @sym{gtk:label-use-markup} function returns whether the text of the label
  is interpreted as marked up with the Pango text markup language. The
  @sym{(setf gtk:label-use-markup)} function sets whether the text of the label
  contains markup. See the @fun{gtk:label-set-markup} function.
  @see-class{gtk:label}
  @see-function{gtk:label-set-markup}")

;;; --- label-use-underline ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-underline" 'label) t)
 "The @code{use-underline} property of type @code{:boolean} (Read / Write) @br{}
  If set, an underline in the text indicates the next character should be used
  for the mnemonic accelerator key. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'label-use-underline)
      "Accessor"
      (documentation 'label-use-underline 'function)
 "@version{#2021-10-31}
  @syntax[]{(gtk:label-use-underline object) => setting}
  @syntax[]{(setf (gtk:label-use-underline object) setting)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[setting]{@em{true} if underlines in the text indicate mnemonics}
  @begin{short}
    Accessor of the @slot[gtk:label]{use-underline} slot of the
    @class{gtk:label} class.
  @end{short}
  The @sym{gtk:label-use-underline} function returns whether an embedded
  underline in the label indicates a mnemonic. If @em{true}, an underline in the
  text indicates the next character should be used for the mnemonic accelerator
  key.
  @see-class{gtk:label}")

;;; --- label-width-chars --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "width-chars" 'label) t)
 "The @code{width-chars} property of @code{:int} (Read / Write) @br{}
  The desired width of the label, in characters. If this property is set to
  -1, the width will be calculated automatically. See the section on text
  layout for details of how the @code{width-chars} and @code{max-width-chars}
  properties determine the width of ellipsized and wrapped labels. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'label-width-chars)
      "Accessor"
      (documentation 'label-width-chars 'function)
 "@version{#2021-10-31}
  @syntax[]{(gtk:label-width-chars object) => n-chars}
  @syntax[]{(setf (gtk:label-width-chars object) n-chars)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[n-chars]{an integer with the new desired width, in characters}
  @begin{short}
    Accessor of the @slot[gtk:label]{width-chars} slot of the @class{gtk:label}
    class.
  @end{short}
  The @sym{gtk:label-width-chars} function retrieves the desired width of the
  label, in characters. The @sym{(setf gtk:label-width-chars)} function sets the
  desired width.
  @see-class{gtk:label}")

;;; --- label-wrap ---------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "wrap" 'label) t)
 "The @code{wrap} property of type @code{:boolean} (Read / Write) @br{}
  If set, wrap lines if the text becomes too wide. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'label-wrap)
      "Accessor"
      (documentation 'label-wrap 'function)
 "@version{#2021-10-31}
  @syntax[]{(gtk:label-wrap object) => wrap}
  @syntax[]{(setf (gtk:label-wrap object) wrap)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[wrap]{a boolean whether lines are wrapped}
  @begin{short}
    Accessor of the @slot[gtk:label]{wrap} slot of the @class{gtk:label} class.
  @end{short}
  If set, wrap lines if the text becomes too wide.
  @see-class{gtk:label}")

;;; --- label-wrap-mode ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "wrap-mode" 'label) t)
 "The @code{wrap-mode} property of type @symbol{pango:wrap-mode}
  (Read / Write) @br{}
  If line wrapping is on, see the @code{wrap} property, this controls how the
  line wrapping is done. The default is @code{:word}, which means wrap on word
  boundaries. @br{}
  Default value: @code{:word}")

#+liber-documentation
(setf (liber:alias-for-function 'label-wrap-mode)
      "Accessor"
      (documentation 'label-wrap-mode 'function)
 "@version{#2021-10-31}
  @syntax[]{(gtk:label-wrap-mode object) => setting}
  @syntax[]{(setf (gtk:label-wrap-mode object) setting)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[setting]{a value of the @symbol{pango:wrap-mode} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:label]{wrap-mode} slot of the @class{gtk:label}
    class.
  @end{short}
  If line wrapping is on, see the @slot[gtk:label]{wrap} property, this controls
  how the line wrapping is done. The default is @code{:word}, which means wrap
  on word boundaries.
  @see-class{gtk:label}
  @see-symbol{pango:wrap-mode}
  @see-function{gtk:label-wrap}")

;;; --- label-xalign -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "xalign" 'label) t)
 "The @code{xalign} property of type @code{:float} (Read / Write) @br{}
  Determines the horizontal aligment of the label text inside the size
  allocation of the label. Compare this to the @slot[widget]{halign}
  property, which determines how the size allocation is positioned in the space
  available for the label. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0.5")

#+liber-documentation
(setf (liber:alias-for-function 'label-xalign)
      "Accessor"
      (documentation 'label-xalign 'function)
 "@version{#2021-10-31}
  @syntax[]{(gtk:label-xalign object) => xalign}
  @syntax[]{(setf (gtk:label-xalign object) xalign)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[xalign]{a float with the horzizontal alignment, between 0 and 1}
  @begin{short}
    Accessor of the @slot[gtk:label]{xalign} slot of the @class{gtk:label}
    class.
  @end{short}
  The @sym{gtk:label-xalign} function sets the @slot[gtk:label]{xalign} property
  for the label. The @sym{(setf gtk:label-xalign)} function sets the property.
  @see-class{gtk:label}
  @see-function{gtk:label-yalign}
  @see-function{gtk:widget-halign}")

;;; --- label-yalign -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "yalign" 'label) t)
 "The @code{yalign} property of type @code{:float} (Read / Write) @br{}
  Determines the vertical aligment of the label text inside the size
  allocation of the label. Compare this to @slot[widget]{valign}
  property, which determines how the size allocation is positioned in the space
  available for the label.
  @br{}
  Allowed values: [0,1] @br{}
  Default value: 0.5")

#+liber-documentation
(setf (liber:alias-for-function 'label-yalign)
      "Accessor"
      (documentation 'label-yalign 'function)
 "@version{#2021-10-31}
  @syntax[]{(gtk:label-yalign object) => yalign}
  @syntax[]{(setf (gtk:label-yalign object) yalign)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[yalign]{a float with the vertical alignment, between 0 and 1}
  @begin{short}
    Accessor of the @slot[gtk:label]{yalign} slot of the @class{gtk:label}
    class.
  @end{short}
  The @sym{gtk:label-yalign} function sets the @slot[gtk:label]{yalign} property
  for the label. The @sym{(setf gtk:label-yalign)} function sets property.
  @see-class{gtk:label}
  @see-function{gtk:label-yalign}
  @see-function{gtk:widget-valign}")

;;; ----------------------------------------------------------------------------
;;; gtk_label_new
;;; ----------------------------------------------------------------------------

(defun label-new (text)
 #+liber-documentation
 "@version{#2021-12-22}
  @argument[text]{a string with the text of the label}
  @return{The new @class{gtk:label} widget.}
  @begin{short}
    Creates a new label with the given text inside it.
  @end{short}
  You can pass @code{nil} to get an empty label widget.
  @see-class{gtk:label}"
  (let ((label (make-instance 'label)))
    (when text
      (setf (label-label label) text))
    label))

(export 'label-new)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_text
;;; gtk_label_set_text -> label-text
;;; ----------------------------------------------------------------------------

(defun (setf label-text) (text label)
  (cffi:foreign-funcall "gtk_label_set_text"
                        (g:object label) label
                        :string text
                        :void)
  text)

(defcfun ("gtk_label_get_text" label-text) :string
 #+liber-documentation
 "@version{#2021-10-31}
  @syntax[]{(gtk:label-text label) => text}
  @syntax[]{(setf (gtk:label-text-label) text)}
  @argument[label]{a @class{gtk:label} widget}
  @argument[text]{a string with the text}
  @begin{short}
    Accessor of the text in the label.
  @end{short}

  The @sym{gtk:label-text} function fetches the text from a label, as displayed
  on the screen. The @sym{(setf gtk:label-text)} function sets the text.

  It overwrites any text that was there before. This will also clear any
  previously set mnemonic accelerators. This does not include any embedded
  underlines indicating mnemonics or Pango markup. See the @fun{gtk:label-label}
  function.
  @see-class{gtk:label}
  @see-function{gtk:label-label}"
  (label (g:object label)))

(export 'label-text)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_markup
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_set_markup" label-set-markup) :void
 #+liber-documentation
 "@version{#2021-10-31}
  @argument[label]{a @class{gtk:label} widget}
  @argument[text]{a markup string}
  @begin{short}
    Parses @arg{text} which is marked up with the Pango text markup language,
    setting the text of the label and attribute list based on the parse results.
  @end{short}
  @see-class{gtk:label}
  @see-function{gtk:label-text}
  @see-function{gtk:label-set-markup-with-mnemonic}"
  (label (g:object label))
  (text :string))

(export 'label-set-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_markup_with_mnemonic
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_set_markup_with_mnemonic"
           label-set-markup-with-mnemonic) :void
 #+liber-documentation
 "@version{#2021-10-31}
  @argument[label]{a @class{gtk:label} widget}
  @argument[text]{a Pango markup string}
  @begin{short}
    Parses @arg{text} which is marked up with the Pango text markup language.
  @end{short}
  This sets the text and attribute list of the label based on the parse results.

  If characters in @arg{text} are preceded by an underscore, they are underlined
  indicating that they represent a keyboard accelerator called a mnemonic.
  The mnemonic key can be used to activate another widget, chosen automatically,
  or explicitly using the @fun{gtk:label-mnemonic-widget} function.
  @see-class{gtk:label}
  @see-function{gtk:label-mnemonic-widget}"
  (label (g:object label))
  (text :string))

(export 'label-set-markup-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_layout_offsets -> label-layout-offsets
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_get_layout_offsets" %label-get-layout-offsets) :void
  (label (g:object label))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun label-layout-offsets (label)
 #+liber-documentation
 "@version{#2022-6-19}
  @argument[label]{a @class{gtk:label} widget}
  @begin{return}
    @arg{x} -- an integer with the x offset @br{}
    @arg{y} -- an integer with the y offset
  @end{return}
  @begin{short}
    Obtains the coordinates where the label will draw the @class{pango:layout}
    object representing the text in the label.
  @end{short}
  This is useful to convert mouse events into coordinates inside the
  @class{pango:layout} object, e.g. to take some action if some part of the
  label is clicked. Remember when using the @class{pango:layout} functions you
  need to convert to and from pixels using the @fun{pango:pixels} function or
  the @var{+pango-scale+} constant.
  @begin[Example]{dictionary}
    @begin{pre}
(gtk:label-layout-offsets (make-instance 'gtk:label))
=> 0
=> -9
(gtk:label-layout-offsets (make-instance 'gtk:label :label \"text\"))
=> -14
=> -9
    @end{pre}
  @end{dictionary}
  @see-class{gtk:label}
  @see-class{pango:layout}
  @see-variable{+pango-scale+}
  @see-function{pango:pixels}"
  (with-foreign-objects ((x :int) (y :int))
    (%label-get-layout-offsets label x y)
    (values (cffi:mem-ref x :int) (cffi:mem-ref y :int))))

(export 'label-layout-offsets)

;;; ----------------------------------------------------------------------------
;;; gtk_label_new_with_mnemonic
;;; ----------------------------------------------------------------------------

;; TODO: Check implementation with make-instance

(defcfun ("gtk_label_new_with_mnemonic" label-new-with-mnemonic)
    (g:object widget)
 #+liber-documentation
 "@version{#2021-12-22}
  @argument[text]{a string with the text of the label, with an underscore in
    front of the mnemonic character}
  @return{The new @class{gtk:label} widget.}
  @begin{short}
    Creates a new @class{gtk:label} widget, containing the given.
  @end{short}

  If characters in @arg{text} are preceded by an underscore, they are
  underlined. If you need a literal underscore character in a label, use '__'
  (two underscores). The first underlined character represents a keyboard
  accelerator called a mnemonic. The mnemonic key can be used to activate
  another widget, chosen automatically, or explicitly using the
  @fun{gtk:label-mnemonic-widget} function.

  If the @fun{gtk:label-mnemonic-widget} function is not called, then the first
  activatable ancestor of the @class{gtk:label} widget will be chosen as the
  mnemonic widget. For instance, if the label is inside a button or menu item,
  the button or menu item will automatically become the mnemonic widget and be
  activated by the mnemonic.
  @see-class{gtk:label}
  @see-function{gtk:label-mnemonic-widget}"
  (text :string))

(export 'label-new-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_label_select_region
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_select_region" label-select-region) :void
 #+liber-documentation
 "@version{#2021-10-31}
  @argument[label]{a @class{gtk:label} widget}
  @argument[start]{an integer with the start offset, in characters not bytes}
  @argument[end]{an integer with the end offset, in characters not bytes}
  @begin{short}
    Selects a range of characters in the label, if the label is selectable.
  @end{short}
  See the @fun{gtk:label-selectable} function. If the label is not
  selectable, this function has no effect. If @arg{start} or @arg{end} are -1,
  then the end of the label will be substituted.
  @see-class{gtk:label}
  @see-function{gtk:label-selectable}"
  (label (g:object label))
  (start :int)
  (end :int))

(export 'label-select-region)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_text_with_mnemonic
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_set_text_with_mnemonic" label-set-text-with-mnemonic)
    :void
 #+liber-documentation
 "@version{#2021-10-31}
  @argument[label]{a @class{gtk:label} widget}
  @argument[text]{a string for the label}
  @begin{short}
    Sets the text of the label from the string @arg{text}.
  @end{short}
  If characters in @arg{text} are preceded by an underscore, they are underlined
  indicating that they represent a keyboard accelerator called a mnemonic. The
  mnemonic key can be used to activate another widget, chosen automatically, or
  explicitly using the @fun{gtk:label-mnemonic-widget} function.
  @see-class{gtk:label}
  @see-function{gtk:label-mnemonic-widget}"
  (label (g:object label))
  (text :string))

(export 'label-set-text-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_layout -> label-layout
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_get_layout" label-layout) (g:object pango:layout)
 #+liber-documentation
 "@version{#2021-10-31}
  @argument[label]{a @class{gtk:label} widget}
  @return{The @class{pango:layout} object for this label.}
  @begin{short}
    Gets the Pango layout used to display the label.
  @end{short}
  The layout is useful to e.g. convert text positions to pixel positions,
  in combination with the @fun{gtk:label-layout-offsets} function. The label
  is free to recreate its layout at any time, so it should be considered
  read-only.
  @see-class{gtk:label}
  @see-class{pango:layout}
  @see-function{gtk:label-layout-offsets}"
  (label (g:object label)))

(export 'label-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_selection_bounds -> label-selection-bounds
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_get_selection_bounds" %label-get-selection-bounds)
    :boolean
  (label (g:object label))
  (start (:pointer :int))
  (end (:pointer :int)))

(defun label-selection-bounds (label)
 #+liber-documentation
 "@version{#2021-10-31}
  @argument[label]{a @class{gtk:label} widget}
  @begin{return}
    @arg{start} -- an integer with the start of selection, as a character
      offset @br{}
    @arg{end} -- an integer with the end of selection, as a character offset
  @end{return}
  @begin{short}
    Gets the selected range of characters in the label.
  @end{short}
  @see-class{gtk:label}"
  (with-foreign-objects ((start :int) (end :int))
    (when (%label-get-selection-bounds label start end)
      (values (cffi:mem-ref start :int)
              (cffi:mem-ref end :int)))))

(export 'label-selection-bounds)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_current_uri -> label-current-uri
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_label_get_current_uri" label-current-uri) :string
 #+liber-documentation
 "@version{#2021-10-31}
  @argument[label]{a @class{gtk:label} widget}
  @begin{return}
    A string with the currently active URI.
  @end{return}
  @begin{short}
    Returns the URI for the currently active link in the label.
  @end{short}
  The active link is the one under the mouse pointer or, in a selectable label,
  the link in which the text cursor is currently positioned.

  This function is intended for use in a \"activate-link\" signal handler or
  for use in a \"query-tooltip\" signal handler.
  @see-class{gtk:label}"
  (label (g:object label)))

(export 'label-current-uri)

;;; --- End of file gtk4.label.lisp --------------------------------------------
