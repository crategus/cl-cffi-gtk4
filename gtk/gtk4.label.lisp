;;; ----------------------------------------------------------------------------
;;; gtk4.label.lisp
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
;;;     gtk_label_get_natural_wrap_mode                     Since 4.6
;;;     gtk_label_set_natural_wrap_mode                     Since 4.6
;;;     gtk_label_get_selectable
;;;     gtk_label_set_selectable
;;;     gtk_label_get_single_line_mode
;;;     gtk_label_set_single_line_mode
;;;     gtk_label_get_tabs                                  Since 4.8
;;;     gtk_label_set_tabs                                  Since 4.8
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
;;;     gtk_label_new_with_mnemonic
;;;     gtk_label_get_text
;;;     gtk_label_set_text
;;;     gtk_label_set_markup
;;;     gtk_label_set_text_with_mnemonic
;;;     gtk_label_set_markup_with_mnemonic
;;;     gtk_label_get_layout
;;;     gtk_label_get_layout_offsets
;;;     gtk_label_select_region
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
;;;     natural-wrap-mode                                   Since 4.6
;;;     selectable
;;;     single-line-mode
;;;     tabs                                                Since 4.8
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
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkLabel
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkAccessibleText
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkLabel
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkLabel" label
  (:superclass widget
   :export t
   :interfaces ("GtkAccessibleText"
                "GtkAccessible"
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
   #+gtk-4-8
   (tabs
    label-tabs
    "tabs" "PangoTabArray" t t)
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
 "@version{2025-08-28}
  @begin{short}
    The @class{gtk:label} widget displays a small amount of text.
  @end{short}
  As the name implies, most labels are used to label another widget such as
  a @class{gtk:button} widget.

  @image[label]{Figure: GtkLabel}

  @subheading{Mnemonics}
  Labels may contain mnemonics. Mnemonics are underlined characters in the
  label, used for keyboard navigation. Mnemonics are created by providing a
  string with an underscore before the mnemonic character, such as
  @code{\"_File\"}, to the @fun{gtk:label-new-with-mnemonic} or
  @fun{gtk:label-set-text-with-mnemonic} functions.

  Mnemonics automatically activate any activatable widget the label is inside,
  such as a @class{gtk:button} widget. If the label is not inside the target
  widget of the mnemonic, you have to tell the label about the target using the
  @slot[gtk:label]{mnemonic-widget} property. Here is a simple example where the
  label is inside a button:
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
  @class{gtk:entry} widget, you have to point the label at the text entry with
  the @slot[gtk:label]{mnemonic-widget} property:
  @begin{pre}
;; Pressing Alt+H will focus the text entry
(let ((entry (make-instance 'gtk:entry))
      (label (make-instance 'gtk:label
                            :use-underline t
                            :label \"_Hello\"
                            :mnemonic-widget entry)))
  ... )
  @end{pre}
  @subheading{Markup (styled text)}
  To make it easy to format text in a label, changing colors, fonts, and so on,
  label text can be provided in a simple markup format. Here is how to create
  a label with a small font:
  @begin{pre}
(let ((label (make-instance 'gtk:label
                            :use-markup t
                            :label \"<small>Small text</small>\")))
  ... )
  @end{pre}
  See complete documentation of available tags in the Pango manual.

  The markup passed to the @fun{gtk:label-set-markup} function must be valid.
  For example, literal @code{<}, @code{>} and @code{&} characters must be
  escaped as @code{&lt;}, @code{&gt;} and @code{&amp;}.

  Markup strings are just a convenient way to set the @class{pango:attr-list}
  instance on a label. The @slot[gtk:label]{attributes} property may be a
  simpler way to set attributes in some cases. Be careful though, a
  @class{pango:attr-list} instance tends to cause internationalization problems,
  unless you are applying attributes to the entire string, that is, unless you
  set the range of each attribute to @code{[0, G_MAXINT]}). The reason is that
  specifying the @code{start_index} and @code{end_index} for a
  @class{pango:attribute} structure requires knowledge of the exact string
  being displayed, so translations will cause problems.

  @subheading{Selectable labels}
  Labels can be made selectable with the @slot[gtk:label]{selectable} property.
  Selectable labels allow the user to copy the label contents to the clipboard.
  Only labels that contain useful-to-copy information - such as error messages -
  should be made selectable.

  @subheading{Text layout}
  A label can contain any number of paragraphs, but will have performance
  problems if it contains more than a small number. Paragraphs are separated
  by newlines or other paragraph separators understood by Pango. Labels can
  automatically wrap text if you set the @slot[gtk:label]{wrap} property.

  The @slot[gtk:label]{justify} property sets how the lines in a label align
  with one another. If you want to set how the label as a whole aligns in its
  available space, see the @slot[gtk:widget]{halign} and
  @slot[gtk:widget]{valign} properties.

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
  markup. The markup for links is borrowed from HTML, using the @code{<a>} tag
  with @code{\"href\"}, @code{\"title\"} and @code{\"class\"} attributes. GTK
  renders links similar to the way they appear in web browsers, with colored,
  underlined text. The @code{\"title\"} attribute is displayed as a tooltip on
  the link. The @code{\"class\"} attribute is used as style class on the CSS
  node for the link. An example looks like this:
  @begin{pre}
(gtk:label-set-markup label
                      \"Go to <a href=\"http://gtk.org/\">GTK Website</a> ...\")
  @end{pre}
  It is possible to implement custom handling for links and their tooltips
  with the @sig[gtk:label]{activate-link} signal and the
  @fun{gtk:label-current-uri} function.
  @begin[GtkLabel as GtkBuildable]{dictionary}
    The @class{gtk:label} implementation of the @class{gtk:buildable} interface
    supports a custom @code{<attributes>} element, which supports any number of
    @code{<attribute>} elements. The @code{<attribute>} element has attributes
    named @code{name}, @code{value}, @code{start} and @code{end} and allows you
    to specify Pango attribute values for the label.

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
    The @class{gtk:label} implementation has a single CSS node with the name
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
    The @class{gtk:label} implementation uses the
    @val[gtk:accessible-role]{:label} role of the @sym{gtk:accessible-role}
    enumeration.
  @end{dictionary}
  @begin[Shortcuts and Gestures]{dictionary}
    The @class{gtk:label} implementation supports the following keyboard
    shortcuts, when the cursor is visible.
    @begin{itemize}
      @item{@kbd{Shift+F10} or @kbd{Menu} opens the context menu.}
      @item{@kbd{Ctrl+A} or @kbd{Ctrl+/} selects all.}
      @item{@kbd{Ctrl+Shift+A} or @kbd{Ctrl+\} unselects all.}
    @end{itemize}
    Additionally, the following signals have default keybindings.
    @begin{itemize}
      @item{@code{GtkLabel::activate-current-link}}
      @item{@code{GtkLabel::copy-clipboard}}
      @item{@code{GtkLabel::move-cursor}}
    @end{itemize}
  @end{dictionary}
  @begin[Action Details]{dictionary}
    The @class{gtk:label} implementation defines a set of built-in actions.
    @begin[code]{simple-table}
      @entry[clipboard.copy]{Copies the text to the clipboard.}
      @entry[clipboard.cut]{Does not do anything, since text in labels cannot
        be deleted.}
      @entry[clipboard.paste]{Does not do anything, since text in labels
        cannot be edited.}
      @entry[link.open]{Opens the link, when activated on a link inside the
        label.}
      @entry[link.copy]{Copies the link to the clipboard, when activated on a
        link inside the label.}
      @entry[menu.popup]{Opens the context menu.}
      @entry[selection.delete]{Does not do anything, since text in labels
        cannot be deleted.}
      @entry[selection.select-all]{Selects all of the text, if the label allows
        selection.}
    @end{simple-table}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[label::activate-current-link]{signal}
      @begin{pre}
lambda (label)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[label]{The @class{gtk:label} widget on which the signal was
          emitted.}
      @end{simple-table}
      A keybinding signal which gets emitted when the user activates a link in
      the label. Applications may also emit the signal with the
      @fun{g:signal-emit} function if they need to control activation of URIs
      programmatically. The default bindings for this signal are all forms of
      the @kbd{Enter} key.
    @end{signal}
    @begin[label::activate-link]{signal}
      @begin{pre}
lambda (label uri)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[label]{The @class{gtk:label} widget on which the signal was
          emitted.}
        @entry[uri]{The string for the URI that is activated.}
        @entry[Returns]{@em{True} if the link has been activated.}
      @end{simple-table}
      The signal which gets emitted to activate a URI. Applications may connect
      to it to override the default behaviour, which is to call the
      @fun{gtk:file-launcher-launch} function.
    @end{signal}
    @begin[label::copy-clipboard]{signal}
      @begin{pre}
lambda (label)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[label]{The @class{gtk:label} widget that received the signal.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted to copy the selection
      to the clipboard. The default binding for this signal is the @kbd{Ctrl-c}
      key.
    @end{signal}
    @begin[label::move-cursor]{signal}
      @begin{pre}
lambda (label step count extend)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[label]{The @class{gtk:label} widget that received the signal.}
        @entry[step]{The granularity for the move, as a value of the
          @sym{gtk:movement-step} enumeration.}
        @entry[count]{The integer for the number of step units to move.}
        @entry[extend]{@em{True} if the move should extend the selection.}
      @end{simple-table}
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
    @end{signal}
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
  @see-slot{gtk:label-tabs}
  @see-slot{gtk:label-use-markup}
  @see-slot{gtk:label-use-underline}
  @see-slot{gtk:label-width-chars}
  @see-slot{gtk:label-wrap}
  @see-slot{gtk:label-wrap-mode}
  @see-slot{gtk:label-xalign}
  @see-slot{gtk:label-yalign}
  @see-class{gtk:inscription}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:label-attributes ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "attributes" 'label) t)
 "The @code{attributes} property of type @class{pango:attr-list}
  (Read / Write) @br{}
  The list of style attributes to apply to the text of the label.")

#+liber-documentation
(setf (liber:alias-for-function 'label-attributes)
      "Accessor"
      (documentation 'label-attributes 'function)
 "@version{2025-08-28}
  @syntax{(gtk:label-attributes object) => attrs}
  @syntax{(setf (gtk:label-attributes object) attrs)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[attrs]{a @class{pango:attr-list} instance}
  @begin{short}
    The accessor for the @slot[gtk:label]{attributes} slot of the
    @class{gtk:label} class gets or sets the attribute list on the label.
  @end{short}
  The attributes in the list are applied to the label text.

  This function does not reflect attributes that come from the labels markup,
  see the @fun{gtk:label-set-markup} function. If you want to get the
  effective attributes for the label, use
  @begin{pre}
(pango:layout-attributes (gtk:label-layout label)) => attrs
  @end{pre}
  @begin[Notes]{dictionary}
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

;;; --- gtk:label-ellipsize ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "ellipsize" 'label) t)
 "The @code{ellipsize} property of type @sym{pango:ellipsize-mode}
  (Read / Write) @br{}
  The preferred place to ellipsize the string, if the label does not have
  enough room to display the entire string, specified as a value of the
  @sym{pango:ellipsize-mode} enumeration. Note that setting this property to a
  value other than @val[pango:ellipsize-mode]{:none} has the side-effect that
  the label requests only enough space to display the ellipsis \"...\". In
  particular, this means that ellipsizing labels do not work well in notebook
  tabs, unless the @slot[gtk:notebook-page]{tab-expand} child property of the
  tab is set to @em{true}. Other ways to set a width of the label are the
  @slot[gtk:widget]{width-request}, @slot[gtk:widget]{height-request} and
  @slot[gtk:label]{width-chars} properties. @br{}
  Default value: @val[pango:ellipsize-mode]{:none}")

#+liber-documentation
(setf (liber:alias-for-function 'label-ellipsize)
      "Accessor"
      (documentation 'label-ellipsize 'function)
 "@version{2025-08-28}
  @syntax{(gtk:label-ellipsize object) => mode}
  @syntax{(setf (gtk:label-ellipsize object) mode)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[mode]{a value of the @sym{pango:ellipsize-mode} enumeration}
  @begin{short}
    The accessor for the @slot[gtk:label]{ellipsize} slot of the
    @class{gtk:label} class gets or sets the mode used to ellipsize the label.
  @end{short}
  Adds an ellipsis: \"...\", to the text if there is not enough space to render
  the entire string.
  @see-class{gtk:label}
  @see-symbol{pango:ellipsize-mode}")

;;; --- gtk:label-extra-menu ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "extra-menu" 'label) t)
 "The @code{extra-menu} property of type @class{g:menu-model} (Read / Write)
  @br{}
  The menu model whose contents will be appended to the context menu.")

#+liber-documentation
(setf (liber:alias-for-function 'label-extra-menu)
      "Accessor"
      (documentation 'label-extra-menu 'function)
 "@version{2025-08-03}
  @syntax{(gtk:label-extra-menu object) => menu}
  @syntax{(setf (gtk:label-extra-menu object) menu)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[menu]{a @class{g:menu-model} object}
  @begin{short}
    The accessor for the @slot[gtk:label]{extra-menu} slot of the
    @class{gtk:label} class gets or sets the menu model to add when constructing
    the context menu for the label.
  @end{short}
  @see-class{gtk:label}
  @see-class{g:menu-model}")

;;; --- gtk:label-justify ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "justify" 'label) t)
 "The @code{justify} property of type @sym{gtk:justification} (Read / Write)
  @br{}
  The alignment of the lines in the text of the label relative to each other.
  This does not affect the alignment of the label within its allocation. See
  the @slot[gtk:label]{xalign} property for that. @br{}
  Default value: @val[gtk:justification]{:left}")

#+liber-documentation
(setf (liber:alias-for-function 'label-justify)
      "Accessor"
      (documentation 'label-justify 'function)
 "@version{2025-08-28}
  @syntax{(gtk:label-justify object) => justify}
  @syntax{(setf (gtk:label-justify object) justify)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[justify]{a value of the @sym{gtk:justification} enumeration}
  @begin{short}
    The accessor for the @slot[gtk:label]{justify} slot of the @class{gtk:label}
    class gets or sets the alignment of the lines in the text of the label
    relative to each other.
  @end{short}

  The @val[gtk:justification]{:left} value is the default value when the widget
  is first created with the @fun{gtk:label-new} function. If you instead want
  to set the alignment of the label as a whole, use the
  @slot[gtk:widget]{halign} property instead. Setting the property has no effect
  on labels containing only a single line.
  @see-class{gtk:label}
  @see-symbol{gtk:justification}
  @see-function{gtk:label-new}
  @see-function{gtk:widget-halign}")

;;; --- gtk:label-label --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label" 'label) t)
 "The @code{label} property of type @code{:string} (Read / Write) @br{}
  The text of the label. @br{}
  Default value: \"\"")

#+liber-documentation
(setf (liber:alias-for-function 'label-label)
      "Accessor"
      (documentation 'label-label 'function)
 "@version{2025-08-03}
  @syntax{(gtk:label-label object) => text}
  @syntax{(setf (gtk:label-label object) text)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[text]{a string for the text of the label}
  @begin{short}
    The accessor for the @slot[gtk:label]{label} slot of the @class{gtk:label}
    class gets or sets the text of the label widget including any embedded
    underlines indicating mnemonics and Pango markup.
  @end{short}
  The label is interpreted as including embedded underlines and/or Pango markup
  depending on the values of the @slot[gtk:label]{use-underline} and
  @slot[gtk:label]{use-markup} properties.

  See also the @fun{gtk:label-text} function, which fetches the text from a
  label, as displayed on the screen.
  @see-class{gtk:label}
  @see-function{gtk:label-text}
  @see-function{gtk:label-use-markup}
  @see-function{gtk:label-use-underline}")

;;; --- gtk:label-lines --------------------------------------------------------

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
 "@version{2025-08-03}
  @syntax{(gtk:label-lines object) => lines}
  @syntax{(setf (gtk:label-lines object) lines)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[lines]{an integer for the desired number of lines, or -1}
  @begin{short}
    The accessor for the @slot[gtk:label]{lines} slot of the @class{gtk:label}
    class gets or sets the number of lines to which an ellipsized, wrapping
    label should be limited.
  @end{short}
  This has no effect if the label is not wrapping or ellipsized. Set this to -1
  if you do not want to limit the number of lines.
  @see-class{gtk:label}")

;;; --- gtk:label-max-width-chars ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "max-width-chars" 'label) t)
 "The @code{max-width-chars} property of type @code{:int} (Read / Write) @br{}
  The desired maximum width of the label, in characters. If this property is
  set to -1, the width will be calculated automatically. See the section on text
  layout for details of how the @slot[gtk:label]{width-chars} and
  @slot[gtk:label]{max-width-chars} properties determine the width of ellipsized
  and wrapped labels. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'label-max-width-chars)
      "Accessor"
      (documentation 'label-max-width-chars 'function)
 "@version{2025-08-28}
  @syntax{(gtk:label-max-width-chars object) => chars}
  @syntax{(setf (gtk:label-max-width-chars object) chars)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[chars]{an integer for the desired maximum width, in characters}
  @begin{short}
    The accessor for the @slot[gtk:label]{max-width-chars} slot of the
    @class{gtk:label} class gets or sets the maximum width of the label in
    characters.
  @end{short}
  @see-class{gtk:label}")

;;; --- gtk:label-mnemonic-keyval ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "mnemonic-keyval" 'label) t)
 "The @code{mnemonic-keyval} property of @code{:uint} (Read) @br{}
  The mnemonic accelerator key for this label. @br{}
  Default value: @code{#xffffff}")

#+liber-documentation
(setf (liber:alias-for-function 'label-mnemonic-keyval)
      "Accessor"
      (documentation 'label-mnemonic-keyval 'function)
 "@version{2025-08-03}
  @syntax{(gtk:label-mnemonic-keyval object) => keyval}
  @argument[object]{a @class{gtk:label} widget}
  @argument[keyval]{an unsigned integer for the keyval}
  @begin{short}
    The accessor for the @slot[gtk:label]{mnemonic-keyval} slot of the
    @class{gtk:label} class gets the mnemonic accelerator key for this label.
  @end{short}
  If there is no mnemonic set up it returns @code{#xffffff}.
  @begin[Examples]{dictionary}
    @begin{pre}
(setq label (gtk:label-new-with-mnemonic \"_Print\"))
=> #<gtk:label {1001A051E3@}>
(gtk:label-mnemonic-keyval label) => 112
    @end{pre}
  @end{dictionary}
  @see-class{gtk:label}")

;;; --- gtk:label-mnemonic-widget ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "mnemonic-widget" 'label) t)
 "The @code{mnemonic-widget} property of type @class{gtk:widget}
  (Read / Write) @br{}
  The widget to be activated when the mnemonic key of the label is pressed.")

#+liber-documentation
(setf (liber:alias-for-function 'label-mnemonic-widget)
      "Accessor"
      (documentation 'label-mnemonic-widget 'function)
 "@version{2025-08-28}
  @syntax{(gtk:label-mnemonic-widget object) => widget}
  @syntax{(setf (gtk:label-mnemonic-widget object) widget)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[widget]{a @class{gtk:widget} target}
  @begin{short}
    The accessor for the @slot[gtk:label]{mnemonic-widget} slot of the
    @class{gtk:label} class gets or sets the target of the mnemonic of the
    label.
  @end{short}
  Returns @code{nil} if none has been set and the default algorithm will be
  used.

  If the label has been set so that it has an mnemonic key, using the
  @fun{gtk:label-set-markup-with-mnemonic},
  @fun{gtk:label-set-text-with-mnemonic}, @fun{gtk:label-new-with-mnemonic}
  functions or the @slot[gtk:label]{use-underline} property, the label can be
  associated with a widget that is the target of the mnemonic.

  When the label is inside a widget, like a @class{gtk:button} widget or a
  @class{gtk:notebook} tab, it is automatically associated with the correct
  widget, but sometimes, for example, when the target is a @class{gtk:entry}
  widget next to the label, you need to set it explicitly using this function.

  The target widget will be accelerated by emitting the
  @sig[gtk:widget]{mnemonic-activate} signal on it. The default handler for this
  signal will activate the widget if there are no mnemonic collisions and toggle
  focus between the colliding widgets otherwise.
  @see-class{gtk:label}
  @see-class{gtk:widget}
  @see-function{gtk:label-set-markup-with-mnemonic}
  @see-function{gtk:label-set-text-with-mnemonic}
  @see-function{gtk:label-new-with-mnemonic}
  @see-function{gtk:label-use-underline}")

;;; --- gtk:label-natural-wrap-mode --------------------------------------------

#+(and gtk-4-6 liber-documentation)
(setf (documentation (liber:slot-documentation "natural-wrap-mode" 'label) t)
 "The @code{natural-wrap-mode} property of type @sym{gtk:natural-wrap-mode}
  (Read / Write) @br{}
  Select the line wrapping for the natural size request. This only affects the
  natural size requested. For the actual wrapping used, see the
  @slot[gtk:label]{wrap-mode} property. The default is the
  @val[gtk:natural-wrap-mode]{:inherit} value, which inherits the behavior of
  the @slot[gtk:label]{wrap-mode} property. Since 4.6 @br{}
  Default value: @val[gtk:natural-wrap-mode]{:inherit}")

#+(and gtk-4-6 liber-documentation)
(setf (liber:alias-for-function 'label-natural-wrap-mode)
      "Accessor"
      (documentation 'label-natural-wrap-mode 'function)
 "@version{2025-08-03}
  @syntax{(gtk:label-natural-wrap-mode object) => mode}
  @syntax{(setf (gtk:label-natural-wrap-mode object) mode)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[mode]{a @sym{gtk:natural-wrap-mode} value}
  @begin{short}
    The accessor for the @slot[gtk:label]{natural-wrap-mode} slot of the
    @class{gtk:label} class gets or sets the line wrap mode used by the label.
  @end{short}

  This only affects the natural size requested, for the actual wrapping used,
  see the @slot[gtk:label]{wrap-mode} property.

  Since 4.6
  @see-class{gtk:label}
  @see-function{gtk:label-wrap-mode}")

;;; --- gtk:label-selectable ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "selectable" 'label) t)
 "The @code{selectable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the label text can be selected with the mouse. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'label-selectable)
      "Accessor"
      (documentation 'label-selectable 'function)
 "@version{2025-08-28}
  @syntax{(gtk:label-selectable object) => selectable}
  @syntax{(setf (gtk:label-selectable object) selectable)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[selectable]{@em{true} to allow selecting text in the label}
  @begin{short}
    The accessor for the @slot[gtk:label]{selectable} slot of the
    @class{gtk:label} class gets or sets wheter the label text can be selected
    with the mouse.
  @end{short}
  Selectable labels allow the user to select text from the label for copy and
  paste.
  @see-class{gtk:label}")

;;; --- gtk:label-single-line-mode ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "single-line-mode" 'label) t)
 "The @code{single-line-mode} property of type  @code{:boolean} (Read / Write)
  @br{}
  Whether the label is in single line mode. In single line mode, the height
  of the label does not depend on the actual text, it is always set to the
  @code{(ascent + descent)} value of the font. This can be an advantage in
  situations where resizing the label because of text changes would be
  distracting, for example in a statusbar. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'label-single-line-mode)
      "Accessor"
      (documentation 'label-single-line-mode 'function)
 "@version{2025-08-03}
  @syntax{(gtk:label-single-line-mode object) => mode}
  @syntax{(setf (gtk:label-single-line-mode object) mode)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[mode]{@em{true} if the label should be in single line mode}
  @begin{short}
    The accessor for the @slot[gtk:label]{single-line-mode} slot of the
    @class{gtk:label} class gets or sets whether the label is in single line
    mode.
  @end{short}
  @see-class{gtk:label}")

;;; --- gtk:label-tabs ---------------------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "tabs" 'label) t)
 "The @code{tabs} property of type @class{pango:tab-array} (Read / Write) @br{}
  The custom tabs for this label.")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'label-tabs)
      "Accessor"
      (documentation 'label-tabs 'function)
 "@version{2025-08-03}
  @syntax{(gtk:label-tabs object) => tabs}
  @syntax{(setf (gtk:label-tabs object) tabs)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[tabs]{a @class{pango:tab-array} instance for the tabs}
  @begin{short}
    The accessor for the @slot[gtk:label]{tabs} slot of the @class{gtk:label}
    class gets or sets the default tab stops for paragraphs in the label.
  @end{short}
  The returned array will be @code{nil} if \"standard\" (8-space) tabs are used.

  Since 4.8
  @see-class{gtk:label}
  @see-class{pango:tab-array}")

;;; --- gtk:label-use-markup ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-markup" 'label) t)
 "The @code{use-markup} property of type @code{:boolean} (Read / Write) @br{}
  The text of the label includes XML Pango markup. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'label-use-markup)
      "Accessor"
      (documentation 'label-use-markup 'function)
 "@version{2025-08-03}
  @syntax{(gtk:label-use-markup object) => setting}
  @syntax{(setf (gtk:label-use-markup object) setting)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[setting]{@em{true} if the text of the label should be parsed for
    markup}
  @begin{short}
    The accessor for the @slot[gtk:label]{use-markup} slot of the
    @class{gtk:label} class gets or sets whether the text of the label is
    interpreted as marked up with the Pango text markup language.
  @end{short}
  See the @fun{gtk:label-set-markup} function.
  @see-class{gtk:label}
  @see-function{gtk:label-set-markup}")

;;; --- gtk:label-use-underline ------------------------------------------------

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
 "@version{2025-08-03}
  @syntax{(gtk:label-use-underline object) => setting}
  @syntax{(setf (gtk:label-use-underline object) setting)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[setting]{@em{true} if underlines in the text indicate mnemonics}
  @begin{short}
    The accessor for the @slot[gtk:label]{use-underline} slot of the
    @class{gtk:label} class gets or sets whether an embedded underline in the
    label indicates a mnemonic.
  @end{short}
  If @em{true}, an underline in the text indicates the next character should be
  used for the mnemonic accelerator key.
  @see-class{gtk:label}")

;;; --- gtk:label-width-chars --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "width-chars" 'label) t)
 "The @code{width-chars} property of type @code{:int} (Read / Write) @br{}
  The desired width of the label, in characters. If this property is set to
  -1, the width will be calculated automatically. See the section on text
  layout for details of how the @slot[gtk:label]{width-chars} and
  @slot[gtk:label]{max-width-chars} properties determine the width of ellipsized
  and wrapped labels. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'label-width-chars)
      "Accessor"
      (documentation 'label-width-chars 'function)
 "@version{2025-08-28}
  @syntax{(gtk:label-width-chars object) => chars}
  @syntax{(setf (gtk:label-width-chars object) chars)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[chars]{an integer for the new desired width, in characters}
  @begin{short}
    The accessor for the @slot[gtk:label]{width-chars} slot of the
    @class{gtk:label} class gets or sets the desired width of the label, in
    characters.
  @end{short}
  @see-class{gtk:label}")

;;; --- gtk:label-wrap ---------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "wrap" 'label) t)
 "The @code{wrap} property of type @code{:boolean} (Read / Write) @br{}
  If set, wrap lines if the text becomes too wide. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'label-wrap)
      "Accessor"
      (documentation 'label-wrap 'function)
 "@version{2025-08-03}
  @syntax{(gtk:label-wrap object) => wrap}
  @syntax{(setf (gtk:label-wrap object) wrap)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[wrap]{a boolean whether lines are wrapped}
  @begin{short}
    The accessor for the @slot[gtk:label]{wrap} slot of the @class{gtk:label}
    class whether lines are wrapped.
  @end{short}
  If set, wrap lines if the text becomes too wide.
  @see-class{gtk:label}")

;;; --- gtk:label-wrap-mode ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "wrap-mode" 'label) t)
 "The @code{wrap-mode} property of type @sym{pango:wrap-mode} (Read / Write)
  @br{}
  If line wrapping is on, see the @slot[gtk:label]{wrap} property, this controls
  how the line wrapping is done. The default is @val[pango:wrap-mode]{:word},
  which means wrap on word boundaries. @br{}
  Default value: @val[pango:wrap-mode]{:word}")

#+liber-documentation
(setf (liber:alias-for-function 'label-wrap-mode)
      "Accessor"
      (documentation 'label-wrap-mode 'function)
 "@version{2025-08-03}
  @syntax{(gtk:label-wrap-mode object) => setting}
  @syntax{(setf (gtk:label-wrap-mode object) setting)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[setting]{a value of the @sym{pango:wrap-mode} enumeration}
  @begin{short}
    The accessor for the @slot[gtk:label]{wrap-mode} slot of the
    @class{gtk:label} class gets or sets whether line wrapping is on.
  @end{short}
  If line wrapping is on, see the @slot[gtk:label]{wrap} property, this controls
  how the line wrapping is done. The default is @val[pango:wrap-mode]{:word},
  which means wrap on word boundaries.
  @see-class{gtk:label}
  @see-symbol{pango:wrap-mode}
  @see-function{gtk:label-wrap}")

;;; --- gtk:label-xalign -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "xalign" 'label) t)
 "The @code{xalign} property of type @code{:float} (Read / Write) @br{}
  Determines the horizontal aligment of the label text inside the size
  allocation of the label. Compare this to the @slot[gtk:widget]{halign}
  property, which determines how the size allocation is positioned in the space
  available for the label. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 0.5")

#+liber-documentation
(setf (liber:alias-for-function 'label-xalign)
      "Accessor"
      (documentation 'label-xalign 'function)
 "@version{2025-08-03}
  @syntax{(gtk:label-xalign object) => xalign}
  @syntax{(setf (gtk:label-xalign object) xalign)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[xalign]{a number coerced to a single float for the horizontal
    alignment, between 0.0 and 1.0}
  @begin{short}
    The accessor for the @slot[gtk:label]{xalign} slot of the @class{gtk:label}
    class gets or sets the horizontal alignment of the label inside the size
    allocation of the label.
  @end{short}
  @see-class{gtk:label}
  @see-function{gtk:label-yalign}
  @see-function{gtk:widget-halign}")

;;; --- gtk:label-yalign -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "yalign" 'label) t)
 "The @code{yalign} property of type @code{:float} (Read / Write) @br{}
  Determines the vertical aligment of the label text inside the size allocation
  of the label. Compare this to the @slot[gtk:widget]{valign} property, which
  determines how the size allocation is positioned in the space available for
  the label. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 0.5")

#+liber-documentation
(setf (liber:alias-for-function 'label-yalign)
      "Accessor"
      (documentation 'label-yalign 'function)
 "@version{2025-08-03}
  @syntax{(gtk:label-yalign object) => yalign}
  @syntax{(setf (gtk:label-yalign object) yalign)}
  @argument[object]{a @class{gtk:label} widget}
  @argument[yalign]{a number coerced to a single float for the vertical
    alignment, between 0.0 and 1.0}
  @begin{short}
    The accessor for the @slot[gtk:label]{yalign} slot of the @class{gtk:label}
    class gets or sets the vertical alignment of the label text inside the size
    allocation of the label.
  @end{short}
  @see-class{gtk:label}
  @see-function{gtk:label-yalign}
  @see-function{gtk:widget-valign}")

;;; ----------------------------------------------------------------------------
;;; gtk_label_new
;;; ----------------------------------------------------------------------------

(defun label-new (&optional text)
 #+liber-documentation
 "@version{2025-04-27}
  @argument[text]{an optional string for the text of the label, or @code{nil}}
  @return{The new @class{gtk:label} widget.}
  @begin{short}
    Creates a new label with the given text inside it.
  @end{short}
  You can pass @code{nil}, the default value, to get an empty label.
  @see-class{gtk:label}"
  (make-instance 'label
                 :label (or text (cffi:null-pointer))))

(export 'label-new)

;;; ----------------------------------------------------------------------------
;;; gtk_label_new_with_mnemonic
;;; ----------------------------------------------------------------------------

(defun label-new-with-mnemonic (text)
 #+liber-documentation
 "@version{2025-08-28}
  @argument[text]{a string for the text of the label, with an underscore in
    front of the mnemonic character}
  @return{The new @class{gtk:label} widget.}
  @begin{short}
    Creates a new @class{gtk:label} widget containing the given text.
  @end{short}

  If characters in @arg{text} are preceded by an underscore, they are
  underlined. If you need a literal underscore character in a label, use '__'
  (two underscores). The first underlined character represents a keyboard
  accelerator called a mnemonic. The mnemonic key can be used to activate
  another widget, chosen automatically, or explicitly using the
  @slot[gtk:label]{mnemonic-widget} property.

  If the @slot[gtk:label]{mnemonic-widget} property is not set, then the first
  activatable ancestor of the @class{gtk:label} widget will be chosen as the
  mnemonic widget. For instance, if the label is inside a button or menu item,
  the button or menu item will automatically become the mnemonic widget and be
  activated by the mnemonic.
  @see-class{gtk:label}
  @see-function{gtk:label-mnemonic-widget}"
  (make-instance 'label
                 :use-underline t
                 :label (or text (cffi:null-pointer))))

(export 'label-new-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_text
;;; gtk_label_set_text
;;; ----------------------------------------------------------------------------

(defun (setf label-text) (text label)
  (cffi:foreign-funcall "gtk_label_set_text"
                        (g:object label) label
                        :string text
                        :void)
  text)

(cffi:defcfun ("gtk_label_get_text" label-text) :string
 #+liber-documentation
 "@version{2025-08-28}
  @syntax{(gtk:label-text label) => text}
  @syntax{(setf (gtk:label-text-label) text)}
  @argument[label]{a @class{gtk:label} widget}
  @argument[text]{a string for the text}
  @begin{short}
    Gets or sets the text of a label, as displayed on the screen.
  @end{short}
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

(cffi:defcfun ("gtk_label_set_markup" label-set-markup) :void
 #+liber-documentation
 "@version{2025-04-27}
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
;;; gtk_label_set_text_with_mnemonic
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_label_set_text_with_mnemonic" label-set-text-with-mnemonic)
    :void
 #+liber-documentation
 "@version{2025-08-28}
  @argument[label]{a @class{gtk:label} widget}
  @argument[text]{a string for the label}
  @begin{short}
    Sets the text of the label from the string @arg{text}.
  @end{short}
  If characters in @arg{text} are preceded by an underscore, they are underlined
  indicating that they represent a keyboard accelerator called a mnemonic. The
  mnemonic key can be used to activate another widget, chosen automatically, or
  explicitly using the @slot[gtk:label]{mnemonic-widget} property.
  @see-class{gtk:label}
  @see-function{gtk:label-mnemonic-widget}"
  (label (g:object label))
  (text :string))

(export 'label-set-text-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_label_set_markup_with_mnemonic
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_label_set_markup_with_mnemonic"
               label-set-markup-with-mnemonic) :void
 #+liber-documentation
 "@version{2025-08-28}
  @argument[label]{a @class{gtk:label} widget}
  @argument[text]{a Pango markup string}
  @begin{short}
    Parses @arg{text} which is marked up with the Pango text markup language.
  @end{short}
  This sets the text and attribute list of the label based on the parse results.

  If characters in @arg{text} are preceded by an underscore, they are underlined
  indicating that they represent a keyboard accelerator called a mnemonic.
  The mnemonic key can be used to activate another widget, chosen automatically,
  or explicitly using the @slot[gtk:label]{mnemonic-widget} property.
  @see-class{gtk:label}
  @see-function{gtk:label-mnemonic-widget}"
  (label (g:object label))
  (text :string))

(export 'label-set-markup-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_layout
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_label_get_layout" label-layout) (g:object pango:layout)
 #+liber-documentation
 "@version{2025-04-27}
  @argument[label]{a @class{gtk:label} widget}
  @return{The @class{pango:layout} object for this label.}
  @begin{short}
    Gets the Pango layout used to display the label.
  @end{short}
  The layout is useful, for example, to convert text positions to pixel
  positions, in combination with the @fun{gtk:label-layout-offsets} function.
  The label is free to recreate its layout at any time, so it should be
  considered read-only.
  @see-class{gtk:label}
  @see-class{pango:layout}
  @see-function{gtk:label-layout-offsets}"
  (label (g:object label)))

(export 'label-layout)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_layout_offsets
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_label_get_layout_offsets" %label-get-layout-offsets) :void
  (label (g:object label))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun label-layout-offsets (label)
 #+liber-documentation
 "@version{2025-04-27}
  @syntax{(gtk:label-layout-offsets label) => x, y}
  @argument[label]{a @class{gtk:label} widget}
  @argument[x]{an integer for the x offset}
  @argument[y]{an integer for the y offset}
  @begin{short}
    Obtains the coordinates where the label will draw the @class{pango:layout}
    object representing the text in the label.
  @end{short}
  This is useful to convert mouse events into coordinates inside the
  @class{pango:layout} object, for example, to take some action if some part of
  the label is clicked. Remember when using the @class{pango:layout} functions
  you need to convert to and from pixels using the @fun{pango:pixels} function
  or the @var{pango:+scale+} constant.
  @begin[Examples]{dictionary}
    @begin{pre}
(gtk:label-layout-offsets (gtk:label-new))
=> 0
=> -9
(gtk:label-layout-offsets (gtk:label-new \"text\"))
=> -14
=> -9
    @end{pre}
  @end{dictionary}
  @see-class{gtk:label}
  @see-class{pango:layout}
  @see-variable{pango:+scale+}
  @see-function{pango:pixels}"
  (cffi:with-foreign-objects ((x :int) (y :int))
    (%label-get-layout-offsets label x y)
    (values (cffi:mem-ref x :int) (cffi:mem-ref y :int))))

(export 'label-layout-offsets)

;;; ----------------------------------------------------------------------------
;;; gtk_label_select_region
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_label_select_region" label-select-region) :void
 #+liber-documentation
 "@version{2025-08-28}
  @argument[label]{a @class{gtk:label} widget}
  @argument[start]{an integer for the start offset, in characters not bytes}
  @argument[end]{an integer for the end offset, in characters not bytes}
  @begin{short}
    Selects a range of characters in the label, if the label is selectable.
  @end{short}
  See the @slot[gtk:label]{selectable} property. If the label is not selectable,
  this function has no effect. If @arg{start} or @arg{end} are -1, then the end
  of the label will be substituted.
  @see-class{gtk:label}
  @see-function{gtk:label-selectable}"
  (label (g:object label))
  (start :int)
  (end :int))

(export 'label-select-region)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_selection_bounds
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_label_get_selection_bounds" %label-get-selection-bounds)
    :boolean
  (label (g:object label))
  (start (:pointer :int))
  (end (:pointer :int)))

(defun label-selection-bounds (label)
 #+liber-documentation
 "@version{2025-04-27}
  @syntax{(gtk:label-selection-bounds label) => start, end}
  @argument[label]{a @class{gtk:label} widget}
  @argument[start]{an integer for the start of the selection, as a character
    offset}
  @argument[end]{an integer for the end of the selection, as a character offset}
  @begin{short}
    Gets the selected range of characters in the label.
  @end{short}
  @see-class{gtk:label}"
  (cffi:with-foreign-objects ((start :int) (end :int))
    (when (%label-get-selection-bounds label start end)
      (values (cffi:mem-ref start :int)
              (cffi:mem-ref end :int)))))

(export 'label-selection-bounds)

;;; ----------------------------------------------------------------------------
;;; gtk_label_get_current_uri
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_label_get_current_uri" label-current-uri) :string
 #+liber-documentation
 "@version{2025-08-28}
  @argument[label]{a @class{gtk:label} widget}
  @return{The string for the currently active URI.}
  @begin{short}
    Returns the URI for the currently active link in the label.
  @end{short}
  The active link is the one under the mouse pointer or, in a selectable label,
  the link in which the text cursor is currently positioned.

  This function is intended for use in a @sig[gtk:label]{activate-link} or in a
  @sig[gtk:widget]{query-tooltip} signal handler.
  @see-class{gtk:label}"
  (label (g:object label)))

(export 'label-current-uri)

;;; --- End of file gtk4.label.lisp --------------------------------------------
