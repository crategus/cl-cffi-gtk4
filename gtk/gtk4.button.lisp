;;; ----------------------------------------------------------------------------
;;; gtk4.button.lisp
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
;;; GtkButton
;;;
;;;     A widget that emits a signal when clicked on.
;;;
;;; Types and Values
;;;
;;;     GtkButton
;;;
;;; Accessors
;;;
;;;     gtk_button_get_child
;;;     gtk_button_set_child
;;;     gtk_button_get_has_frame
;;;     gtk_button_set_has_frame
;;;     gtk_button_get_icon_name
;;;     gtk_button_set_icon_name
;;;     gtk_button_get_label
;;;     gtk_button_set_label
;;;     gtk_button_get_use_underline
;;;     gtk_button_set_use_underline
;;;
;;; Functions
;;;
;;;     gtk_button_new
;;;     gtk_button_new_with_label
;;;     gtk_button_new_with_mnemonic
;;;     gtk_button_new_from_icon_name
;;;
;;; Properties
;;;
;;;     child
;;;     has-frame
;;;     icon-name
;;;     label
;;;     use-underline
;;;
;;; Signals
;;;
;;;     activate
;;;     clicked
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkButton
;;;                 ├── GtkLinkButton
;;;                 ├── GtkLockButton
;;;                 ╰── GtkToggleButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkActionable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkButton
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkButton" button
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkActionable")
   :type-initializer "gtk_button_get_type")
  ((child
    button-child
    "child" "GtkWidget" t t)
   (has-frame
    button-has-frame
    "has-frame" "gboolean" t t)
   (icon-name
    button-icon-name
    "icon-name" "gchararray" t t)
   (label
    button-label
    "label" "gchararray" t t)
   (use-underline
    button-use-underline
    "use-underline" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'button 'type)
 "@version{2023-3-18}
  @begin{short}
    The @sym{gtk:button} widget is generally used to trigger a callback
    function that is called when the button is pressed.
  @end{short}

  @image[button]{Figure: GtkButton}

  The @sym{gtk:button} widget can hold any valid child widget. That is, it can
  hold almost any other standard @class{gtk:widget} widget. The most commonly
  used child is the @class{gtk:label} widget.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:button} implementation has a single CSS node with name
    @code{button}. The node will get the @code{.image-button} or
    @code{.text-button} style classes, if the content is just an image or label,
    respectively. It may also receive the @code{.flat} style class. When
    activating a button via the keyboard, the button will temporarily gain
    the @code{.keyboard-activating} style class.

    Other style classes that are commonly used with the @sym{gtk:button}
    implementation include the @code{.suggested-action} and
    @code{.destructive-action} style classes. In special cases, buttons can be
    made round by adding the @code{.circular} style class.

    Button-like widgets like the @class{gtk:toggle-button},
    @class{gtk:menu-button}, @class{gtk:volume-button},
    @class{gtk:loock-button}, @class{gtk:color-button} or
    @class{gtk:font-button} widgets use style classes such as the
    @code{.toggle}, @code{.popup}, @code{.scale}, @code{.lock}, @code{.color}
    style classes on the button node to differentiate themselves from a plain
    @sym{gtk:button} widget.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
lambda (button)    :action
      @end{pre}
      The signal on the @sym{gtk:button} widget is an action signal and emitting
      it causes the button to animate press then release. Applications should
      never connect to this signal, but use the \"clicked\" signal.
      @begin[code]{table}
        @entry[button]{The @sym{gtk:button} widget which received the signal.}
      @end{table}
    @subheading{The \"clicked\" signal}
      @begin{pre}
lambda (button)    :action
      @end{pre}
      Emitted when the button has been activated (pressed and released).
      @begin[code]{table}
        @entry[button]{The @sym{gtk:button} widget which received the signal.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:button-new}
  @see-constructor{gtk:button-new-with-label}
  @see-constructor{gtk:button-new-with-mnemonic}
  @see-constructor{gtk:button-new-from-icon-name}
  @see-slot{gtk:button-child}
  @see-slot{gtk:button-has-frame}
  @see-slot{gtk:button-icon-name}
  @see-slot{gtk:button-label}
  @see-slot{gtk:button-use-underline}
  @see-class{gtk:widget}
  @see-class{gtk:label}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- button-child -----------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'button) t)
 "The @code{child} property of type @class{gtk:widget} (Read / Write)  @br{}
  The child widget. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'button-child)
      "Accessor"
      (documentation 'button-child 'function)
 "@version{2023-3-18}
  @syntax[]{(gtk:button-child object) => child}
  @syntax[]{(setf (gtk:button-child object) child)}
  @argument[object]{a @class{gtk:button} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Accessor of the @slot[gtk:button]{child} slot of the @class{gtk:button}
    class.
  @end{short}
  The @sym{gtk:button-child} function gets the child widget of the button. The
  @sym{(setf gtk:button-child)} function sets the child widget.

  Note that by using this API, you take full responsibility for setting up the
  proper accessibility label and description information for the button. Most
  likely, you will either set the accessibility label or description for the
  button explicitly, or you will set a labelled-by or described-by relations
  from the child widget to the button.
  @see-class{gtk:button}
  @see-class{gtk:widget}")

;;; --- button-has-frame -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-frame" 'button) t)
 "The @code{has-frame} property of type @code{:boolean} (Read / Write)  @br{}
  Whether the button has a frame. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'button-has-frame)
      "Accessor"
      (documentation 'button-has-frame 'function)
 "@version{2023-3-18}
  @syntax[]{(gtk:button-has-frame object) => setting}
  @syntax[]{(setf (gtk:button-has-frame object) setting)}
  @argument[object]{a @class{gtk:button} widget}
  @argument[setting]{a boolean whether the button has a frame}
  @begin{short}
    Accessor of the @slot[gtk:button]{has-frame} slot of the @class{gtk:button}
    class.
  @end{short}
  The @sym{gtk:button-has-frame} function returns whether the button has a
  frame. The @sym{(setf gtk:button-has-frame)} function sets the style of the
  button.

  Buttons can have a flat appearance or a frame drawn around them.
  @see-class{gtk:button}")

;;; --- button-icon-name -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "icon-name" 'button) t)
 "The @code{icon-name} property of type @code{:string} (Read / Write)  @br{}
  The name of the icon used to automatically populate the button. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'button-icon-name)
      "Accessor"
      (documentation 'button-icon-name 'function)
 "@version{2023-3-18}
  @syntax[]{(gtk:button-icon-name object) => name}
  @syntax[]{(setf (gtk:button-icon-name object) name)}
  @argument[object]{a @class{gtk:button} widget}
  @argument[name]{a string with an icon name, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:button]{icon-name} slot of the @class{gtk:button}
    class.
  @end{short}
  The @sym{gtk:button-icon-name} function returns the icon name of the button.
  If the icon name has not been set the return value will be @code{nil}. This
  will be the case if you create an empty button with the @fun{gtk:button-new}
  function to use as a container. The @sym{(setf gtk:button-has-frame)} function
  adds a @class{gtk:image} widget with the given icon name as a child widget. If
  the button already contains a child widget, that child widget will be removed
  and replaced with the image.
  @see-class{gtk:button}
  @see-class{gtk:image}")

;;; --- button-label -----------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label" 'button) t)
 "The @code{label} property of type @code{:string} (Read / Write) @br{}
  Text of the label inside the button, if the button contains a label. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'button-label)
      "Accessor"
      (documentation 'button-label 'function)
 "@version{2023-3-18}
  @syntax[]{(gtk:button-label object) => label}
  @syntax[]{(setf (gtk:button-label object) label)}
  @argument[object]{a @class{gtk:button} widget}
  @argument[label]{a string with the text of the label}
  @begin{short}
    Accessor of the @slot[gtk:button]{label} slot of the @class{gtk:button}
    class.
  @end{short}
  The @sym{gtk:button-label} function fetches the text from the label of the
  button. The @sym{(setf gtk:button-label)} function sets the text. If the label
  text has not been set the return value will be @code{nil}. This will be the
  case if you create an empty button with the @fun{gtk:button-new} function to
  use as a container.
  @see-class{gtk:button}
  @see-function{gtk:button-new}")

;;; --- button-use-underline ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-underline" 'button) t)
 "The @code{use-underline} property of type @code{:boolean} (Read / Write) @br{}
  If set, an underline in the text indicates the next character should be used
  for the mnemonic accelerator key. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'button-use-underline)
      "Accessor"
      (documentation 'button-use-underline 'function)
 "@version{2023-3-18}
  @syntax[]{(gtk:button-use-underline object) => setting}
  @syntax[]{(setf (gtk:button-use-underline object) setting)}
  @argument[object]{a @class{gtk:button} widget}
  @argument[setting]{@em{true} if underlines in the text indicate mnemonics}
  @begin{short}
    Accessor of the @slot[gtk:button]{use-underline} slot of the
    @class{gtk:button} class.
  @end{short}
  The @sym{gtk:button-use-underline} function gets whether underlines are
  interpreted as mnemonics. The @sym{(setf gtk:button-user-underline)} function
  sets whether to use underlines as mnemonics. If @em{true}, an underline in the
  text of the button label indicates the next character should be used for the
  mnemonic accelerator key.
  @see-class{gtk:button}")

;;; ----------------------------------------------------------------------------
;;; gtk_button_new
;;; ----------------------------------------------------------------------------

(declaim (inline button-new))

(defun button-new ()
 #+liber-documentation
 "@version{2023-3-18}
  @return{The newly created @class{gtk:button} widget.}
  @begin{short}
    Creates a new button.
  @end{short}
  To add a child widget to the button, use the @fun{gtk:button-child} function.
  @see-class{gtk:button}
  @see-function{gtk:button-new-with-label}
  @see-function{gtk:button-new-with-mnemonic}
  @see-function{gtk:button-new-from-icon-name}
  @see-function{gtk:button-child}"
  (make-instance 'button))

(export 'button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_button_new_with_label
;;; ----------------------------------------------------------------------------

(declaim (inline button-new-with-label))

(defun button-new-with-label (label)
 #+liber-documentation
 "@version{2023-3-18}
  @argument[label]{a string with the text you want the @class{gtk:label} child
    widget to hold}
  @return{The newly created @class{gtk:button} widget.}
  @begin{short}
    Creates a button with a label containing the given text in @arg{label}.
  @end{short}
  @see-class{gtk:button}
  @see-class{gtk:label}
  @see-function{gtk:button-new}
  @see-function{gtk:button-new-with-mnemonic}
  @see-function{gtk:button-new-from-icon-name}"
  (make-instance 'button
                 :label label))

(export 'button-new-with-label)

;;; ----------------------------------------------------------------------------
;;; gtk_button_new_with_mnemonic
;;; ----------------------------------------------------------------------------

(declaim (inline button-new-with-mnemonic))

(defun button-new-with-mnemonic (label)
 #+liber-documentation
 "@version{2023-3-18}
  @argument[label]{a string with the text of the button, with an underscore in
    front of the mnemonic character}
  @return{A new @class{gtk:button} widget.}
  @begin{short}
    Creates a new button widget containing a label with a mnemonic.
  @end{short}
  If characters in @arg{label} are preceded by an underscore, they are
  underlined. If you need a literal underscore character in a label, use
  '__' (two underscores). The first underlined character represents a keyboard
  accelerator called a mnemonic. Pressing @kbd{Alt} and that key activates the
  button.
  @see-class{gtk:button}
  @see-function{gtk:button-new}
  @see-function{gtk:button-new-with-label}
  @see-function{gtk:button-new-from-icon-name}"
  (make-instance 'button
                 :label label
                 :use-underline t))

(export 'button-new-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_button_new_from_icon_name
;;; ----------------------------------------------------------------------------

(declaim (inline button-new-from-icon-name))

(defun button-new-from-icon-name (name)
 #+liber-documentation
 "@version{2023-3-18}
  @argument[name]{a string with the icon name}
  @return{A new @class{gtk:button} widget displaying the themed icon.}
  @begin{short}
    Creates a new button containing an icon from the current icon theme.
  @end{short}
  If the icon name is not known, a \"broken image\" icon will be displayed
  instead. If the current icon theme is changed, the icon will be updated
  appropriately.
  @see-class{gtk:button}
  @see-function{gtk:button-new}"
  (make-instance 'button
                 :icon-name name))

(export 'button-new-from-icon-name)

;;; --- End of file gtk4.button.lisp -------------------------------------------
