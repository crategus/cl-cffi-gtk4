;;; ----------------------------------------------------------------------------
;;; gtk4.expander.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
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
;;; GtkExpander
;;;
;;;     A container which can hide its child
;;;
;;; Types and Values
;;;
;;;     GtkExpander
;;;
;;; Accessors
;;;
;;;     gtk_expander_set_child
;;;     gtk_expander_get_child
;;;     gtk_expander_set_expanded
;;;     gtk_expander_get_expanded
;;;     gtk_expander_set_label
;;;     gtk_expander_get_label
;;;     gtk_expander_set_label_widget
;;;     gtk_expander_get_label_widget
;;;     gtk_expander_set_resize_toplevel
;;;     gtk_expander_get_resize_toplevel
;;;     gtk_expander_set_use_markup
;;;     gtk_expander_get_use_markup
;;;     gtk_expander_set_use_underline
;;;     gtk_expander_get_use_underline
;;;
;;; Functions
;;;
;;;     gtk_expander_new
;;;     gtk_expander_new_with_mnemonic
;;;
;;; Properties
;;;
;;;     child
;;;     expanded
;;;     label
;;;     label-widget
;;;     resize-toplevel
;;;     use-markup
;;;     use-underline
;;;
;;; Signals
;;;
;;;     activate
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkExpander
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkExpander
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkExpander" expander
  (:superclass widget
    :export t
    :interfaces ("GtkAccessible"
                 "GtkBuildable"
                 "GtkConstraintTarget")
    :type-initializer "gtk_expander_get_type")
  ((child
    expander-child
    "child" "GtkWidget" t t)
   (expanded
    expander-expanded
    "expanded" "gboolean" t t)
   (label
    expander-label
    "label" "gchararray" t t)
   (label-widget
    expander-label-widget
    "label-widget" "GtkWidget" t t)
   (resize-toplevel
    expander-resize-toplevel
    "resize-toplevel" "gboolean" t t)
   (use-markup
    expander-use-markup
    "use-markup" "gboolean" t t)
   (use-underline
    expander-use-underline
    "use-underline" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'expander 'type)
 "@version{2023-8-23}
  @begin{short}
    A @class{gtk:expander} widget allows the user to hide or show its child by
    clicking on an expander triangle similar to the triangles used in a
    @class{gtk:tree-view} widget.
  @end{short}

  @image[expander]{Figure: GtkExpander}

  Normally you use an expander as you would use a frame. You create the child
  widget and use the @fun{gtk:expander-child} function to add it to the
  expander. When the expander is toggled, it will take care of showing and
  hiding the child automatically.

  @subheading{Special Usage}
  There are situations in which you may prefer to show and hide the expanded
  widget yourself, such as when you want to actually create the widget at
  expansion time. In this case, create a @class{gtk:expander} widget but do not
  add a child widget to it. The expander widget has an
  @slot[gtk:expander]{expanded} property which can be used to monitor its
  expansion state. You should watch this property with a signal connection as
  follows:
  @begin{pre}
(let ((expander (gtk:expander-new-with-mnemonic \"_More Options\")))
  (g:signal-connect expander \"notify::expanded\"
                    (lambda (object param)
                      (if (gtk:expander-expanded object)
                          ;; Show or create widgets
                          ...
                          ;; Hide or destroy widgets
                          ... )))
  ... )
  @end{pre}
  @begin[GtkExpander as GtkBuildable]{dictionary}
    The @class{gtk:expander} implementation of the @class{gtk:buildable}
    interface supports placing a child in the label position by specifying
    @code{\"label\"} as the @code{\"type\"} attribute of a @code{<child>}
    element. A normal content child can be specified without specifying a
    @code{<child>} type attribute.

    @b{Example:} A UI definition fragment with a @class{gtk:expander} widget.
    @begin{pre}
<object class=\"GtkExpander\">
  <child type=\"label\">
    <object class=\"GtkLabel\" id=\"expander-label\"/>
  </child>
  <child>
    <object class=\"GtkEntry\" id=\"expander-content\"/>
  </child>
</object>
    @end{pre}
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
expander
├── title
│   ├── arrow
│   ╰── <label widget>
╰── <child>
    @end{pre}
    The @class{gtk:expander} implementation has three CSS nodes, the main node
    with the name @code{expander}, a subnode with name @code{title} and node
    below it with name @code{arrow}. The arrow of an expander that is showing
    its child gets the @code{:checked} pseudoclass added to it.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:expander} implementation uses the @code{:button} role of the
    @symbol{gtk:accessible-role} enumeration.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      Activates the expander.
      @begin{pre}
lambda (expander)   :action
      @end{pre}
      @begin[code]{table}
        @entry[expander]{The @class{gtk:expander} widget which receives the
          signal.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:expander-new}
  @see-constructor{gtk:expander-new-with-mnemonic}
  @see-slot{gtk:expander-child}
  @see-slot{gtk:expander-expanded}
  @see-slot{gtk:expander-label}
  @see-slot{gtk:expander-label-widget}
  @see-slot{gtk:expander-resize-toplevel}
  @see-slot{gtk:expander-use-markup}
  @see-slot{gtk:expander-use-underline}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- expander-child ---------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'expander) t)
 "The @code{child} property of type @class{gtk:widget} (Read / Write) @br{}
  The child widget.")

#+liber-documentation
(setf (liber:alias-for-function 'expander-child)
      "Accessor"
      (documentation 'expander-child 'function)
 "@version{2023-8-23}
  @syntax[]{(gtk:expander-child object) => child}
  @syntax[]{(setf (gtk:expander-child object) child)}
  @argument[object]{a @class{gtk:expander} widget}
  @argument[child]{a @class{gtk:widget} child widget of the expander}
  @begin{short}
    Accessor of the @slot[gtk:expander]{child} slot of the @class{gtk:expander}
    class.
  @end{short}
  @see-class{gtk:expander}
  @see-class{gtk:widget}")

;;; --- expander-expanded ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "expanded" 'expander) t)
 "The @code{expanded} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Whether the expander has been opened to reveal the child widget. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'expander-expanded)
      "Accessor"
      (documentation 'expander-expanded 'function)
 "@version{2023-8-23}
  @syntax[]{(gtk:expander-expanded object) => expanded}
  @syntax[]{(setf (gtk:expander-expanded object) expanded)}
  @argument[object]{a @class{gtk:expander} widget}
  @argument[expanded]{a boolean whether the child widget is revealed}
  @begin{short}
    Accessor of the @slot[gtk:expander]{expanded} slot of the
    @class{gtk:expander} class.
  @end{short}
  The @fun{gtk:expander-expanded} function queries a @class{gtk:expander} widget
  and returns its current state. Set to @em{true}, if you want the child widget
  to be revealed, and @em{false} if you want the child widget to be hidden.
  @see-class{gtk:expander}")

;;; --- expander-label ---------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label" 'expander) t)
 "The @code{label} property of type @code{:string} (Read / Write / Construct)
  @br{}
  Text of the label of the expander. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'expander-label)
      "Accessor"
      (documentation 'expander-label 'function)
 "@version{2023-8-23}
  @syntax[]{(gtk:expander-label object) => label}
  @syntax[]{(setf (gtk:expander-label object) label)}
  @argument[object]{a @class{gtk:expander} widget}
  @argument[label]{a string with the text of the label of the expander}
  @begin{short}
    Accessor of the @slot[gtk:expander]{label} slot of the @class{gtk:expander}
    class.
  @end{short}
  The @fun{gtk:expander-label} function fetches the text from a label widget
  including any embedded underlines indicating mnemonics and Pango markup, as
  set by the @setf{gtk:expander-label} function.

  If the label text has not been set the return value will be @code{nil}. This
  will be the case if you create an empty button with the @fun{gtk:button-new}
  function to use as a container.
  @see-class{gtk:expander}
  @see-function{gtk:button-new}")

;;; --- expander-label-widget --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label-widget" 'expander) t)
 "The @code{label-widget} property of type @class{gtk:widget} (Read / Write)
  @br{}
  A widget to display in place of the usual expander label.")

#+liber-documentation
(setf (liber:alias-for-function 'expander-label-widget)
      "Accessor"
      (documentation 'expander-label-widget 'function)
 "@version{2023-8-23}
  @syntax[]{(gtk:expander-label-widget object) => label-widget}
  @syntax[]{(setf gtk:expander-label-widget object) label-widget)}
  @argument[object]{a @class{gtk:expander} widget}
  @argument[label-widget]{a @class{gtk:widget} label widget}
  @begin{short}
    Accessor of the @slot[gtk:expander]{label-widget} slot of the
    @class{gtk:expander} class.
  @end{short}
  The @fun{gtk:expander-label-widget} function retrieves the label widget for
  the frame. The @setf{gtk:expander-label-widget} function sets the label
  widget for the expander.

  This is the widget that will appear embedded alongside the expander arrow.
  @see-class{gtk:expander}
  @see-class{gtk:widget}")

;;; --- expander-resize-toplevel -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "resize-toplevel" 'expander) t)
 "The @code{resize-toplevel} property of type @code{:boolean}
  (Read / Write) @br{}
  When this property is @em{true}, the expander will resize the toplevel widget
  containing the expander upon expanding and collapsing. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'expander-resize-toplevel)
      "Accessor"
      (documentation 'expander-resize-toplevel 'function)
 "@version{2023-8-23}
  @syntax[]{(gtk:expander-resize-toplevel object) => resize-toplevel}
  @syntax[]{(setf (gtk:expander-resize-toplevel object) resize-toplevel)}
  @argument[object]{a @class{gtk:expander} widget}
  @argument[resize-toplevel]{a boolean whether to resize the toplevel}
  @begin{short}
    Accessor of the @slot[gtk:expander]{resize-toplevel} slot of the
    @class{gtk:expander} class.
  @end{short}
  The @fun{gtk:expander-resize-toplevel} function returns whether the expander
  will resize the toplevel widget containing the expander upon resizing and
  collpasing. The @setf{gtk:expander-resize-toplevel} function sets whether the
  expander will resize.
  @see-class{gtk:expander}")

;;; --- expander-use-markup ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-markup" 'expander) t)
 "The @code{use-markup} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  The text of the label includes XML Pango markup. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'expander-use-markup)
      "Accessor"
      (documentation 'expander-use-markup 'function)
 "@version{2023-8-23}
  @syntax[]{(gtk:expander-use-markup object) => use-markup}
  @syntax[]{(setf (gtk:expander-use-markup object) use-markup)}
  @argument[object]{a @class{gtk:expander} widget}
  @argument[use-markup]{@em{true} if the text of the label should be parsed for
    markup}
  @begin{short}
    Accessor of the @slot[gtk:expander]{use-markup} slot of the
    @class{gtk:expander} class.
  @end{short}
  The @fun{gtk:expander-use-markup} function returns whether the text of the
  label is interpreted as marked up with the Pango text markup language. The
  @setf{gtk:expander-use-markup} function sets whether the text of the label
  contains markup.
  @see-class{gtk:expander}
  @see-function{gtk:label-set-markup}")

;;; --- expander-use-underline -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-underline" 'expander) t)
 "The @code{use-underline} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  If set, an underline in the text indicates the next character should be used
  for the mnemonic accelerator key. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'expander-use-underline)
      "Accessor"
      (documentation 'expander-use-underline 'function)
 "@version{2023-8-23}
  @syntax[]{(gtk:expander-use-underline object) => use-underline}
  @syntax[]{(setf (gtk:expander-use-underline object) use-underline)}
  @argument[object]{a @class{gtk:expander} widget}
  @argument[use-underline]{@em{true} if underlines in the text indicate
    mnemonics}
  @begin{short}
    Accessor of the @slot[gtk:expander]{use-underline} slot of the
    @class{gtk:expander} class.
  @end{short}
  The @fun{gtk:expander-use-underline} function returns whether an embedded
  underline in the expander label indicates a mnemonic.

  If @em{true}, an underline in the text of the expander label indicates the
  next character should be used for the mnemonic accelerator key.
  @see-class{gtk:expander}")

;;; ----------------------------------------------------------------------------
;;; gtk_expander_new
;;; ----------------------------------------------------------------------------

(declaim (inline expander-new))

(defun expander-new (label)
 #+liber-documentation
 "@version{2023-8-23}
  @argument[label]{a string with the text of the label}
  @return{A new @class{gtk:expander} widget.}
  @begin{short}
    Creates a new expander using @arg{label} as the text of the label.
  @end{short}
  @see-class{gtk:expander}
  @see-function{gtk:expander-new-with-mnemonic}"
  (make-instance 'expander
                 :label label))

(export 'expander-new)

;;; ----------------------------------------------------------------------------
;;; gtk_expander_new_with_mnemonic
;;; ----------------------------------------------------------------------------

(declaim (inline expander-new-with-mnemonic))

(defun expander-new-with-mnemonic (label)
 #+liber-documentation
 "@version{2023-8-23}
  @argument[label]{a string with the text of the label with an underscore in
    front of the mnemonic character}
  @return{A new @class{gtk:expander} widget.}
  @begin{short}
    Creates a new expander using @arg{label} as the text of the label.
  @end{short}
  If characters in @arg{label} are preceded by an underscore, they are
  underlined. If you need a literal underscore character in a label, use two
  underscores '__'. The first underlined character represents a keyboard
  accelerator called a mnemonic. Pressing @kbd{Alt} and that key activates the
  button.
  @see-class{gtk:expander}
  @see-function{gtk:expander-new}"
  (make-instance 'expander
                 :label label
                 :use-underline t))

(export 'expander-new-with-mnemonic)

;;; --- End of file gtk4.expander.lisp -----------------------------------------
