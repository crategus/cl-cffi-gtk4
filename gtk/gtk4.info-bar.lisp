;;; ----------------------------------------------------------------------------
;;; gtk4.info-bar.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.12 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2026 Dieter Kaiser
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
;;; GtkInfoBar
;;;
;;;     Report important messages to the user
;;;
;;; Types and Values
;;;
;;;     GtkInfoBar
;;;
;;; Accessors
;;;
;;;     gtk_info_bar_get_message_type
;;;     gtk_info_bar_set_message_type
;;;     gtk_info_bar_get_revealed
;;;     gtk_info_bar_set_revealed
;;;     gtk_info_bar_get_show_close_button
;;;     gtk_info_bar_set_show_close_button
;;;
;;; Functions
;;;
;;;     gtk_info_bar_new
;;;     gtk_info_bar_new_with_buttons
;;;     gtk_info_bar_add_action_widget
;;;     gtk_info_bar_remove_action_widget
;;;     gtk_info_bar_add_button
;;;     gtk_info_bar_add_buttons
;;;     gtk_info_bar_set_response_sensitive
;;;     gtk_info_bar_set_default_response
;;;     gtk_info_bar_response
;;;     gtk_info_bar_add_child
;;;     gtk_info_bar_remove_child
;;;
;;; Properties
;;;
;;;     message-type
;;;     revealed
;;;     show-close-button
;;;
;;; Signals
;;;
;;;     close
;;;     response
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkInfoBar
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkInfoBar
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkInfoBar" info-bar
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_info_bar_get_type")
  ((message-type
    info-bar-message-type
    "message-type" "GtkMessageType" t t)
   (revealed
    info-bar-revealed
    "revealed" "gboolean" t t)
   (show-close-button
    info-bar-show-close-button
    "show-close-button" "gboolean" t t)))

#+(and gtk-4-10 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj info-bar) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:INFO-BAR is deprecated since 4.10")))

#+liber-documentation
(setf (documentation 'info-bar 'type)
 "@version{2025-07-22}
  @begin{short}
    The @class{gtk:info-bar} widget can be used to show messages to the user
    without showing a dialog.
  @end{short}
  It is often temporarily shown at the top or bottom of a document. In contrast
  to the @class{gtk:dialog} widget, which has a horizontal action area at the
  bottom, the info bar has a vertical action area at the side.

  @image[info-bar]{Figure: GtkInfoBar}

  The API of the @class{gtk:info-bar} widget is very similar to the
  @class{gtk:dialog} widget, allowing you to add buttons to the action area
  with the @fun{gtk:info-bar-add-button} or @fun{gtk:info-bar-new-with-buttons}
  functions. The sensitivity of action widgets can be controlled with the
  @fun{gtk:info-bar-set-response-sensitive} function. To add widgets to the main
  content area of an info bar, use the @fun{gtk:info-bar-add-child} function.

  Similar to the @class{gtk:message-dialog} widget, the contents of an info bar
  can by classified as error message, warning, informational message, and so on,
  by using the @fun{gtk:info-bar-message-type} function. GTK may use the message
  type to determine how the message is displayed.
  @begin[Examples]{dictionary}
    Simple info bar usage.
    @begin{pre}
(defun create-info-bar (msg type)
  (let ((infobar (make-instance 'gtk:info-bar
                                :message-type type
                                :show-close-button t))
        (message (make-instance 'gtk:label :label msg)))
    ;; Add a label with the message to the content of the info bar
    (gtk:info-bar-add-child infobar message)
    ;; Connect a signal handler to the info bar
    (g:signal-connect infobar \"response\"
                      (lambda (widget response)
                        (declare (ignore response))
                        (setf (gtk:widget-visible widget) nil)))
    infobar))
    @end{pre}
  @end{dictionary}
  @begin[GtkInfoBar as GtkBuildable]{dictionary}
    The @class{gtk:info-bar} implementation of the @class{gtk:buildable}
    interface exposes the content area and action area as internal children
    with the names @code{content_area} and @code{action_area}.

    The @class{gtk:info-bar} implementation supports a custom
    @code{<action-widgets>} element, which can contain multiple
    @code{<action-widget>} elements. The @code{response} attribute specifies a
    numeric response, and the content of the element is the ID of the widget,
    which should be a child of the dialogs action area.
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    The @class{gtk:info-bar} implementation has a single CSS node with name
    @code{infobar}. The node may get one of the @code{.info}, @code{.warning},
    @code{.error} or @code{.question} style classes, depending on the message
    type. If the info bar shows a Close button, that button will have the
    @code{.close} style class applied.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:info-bar} implementation is deprecated since 4.10. Do not
    use it in newly written code. There is no replacement in GTK for an
    \"info bar\" widget. You can use the @class{gtk:revealer} widget with a
    @class{gtk:box} widget containing a @class{gtk:label} widget and an
    optional @class{gtk:button} widget, according to your design of the
    application.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[info-bar::close]{signal}
      @begin{pre}
lambda (infobar)    :action
      @end{pre}
      @begin[code]{simple-table}
        @entry[infobar]{The @class{gtk:info-bar} widget on which the signal is
          emitted.}
      @end{simple-table}
      The signal is a keybinding signal which gets emitted when the user uses a
      keybinding to dismiss the info bar. The default binding for this signal
      is the @kbd{Escape} key.
    @end{signal}
    @begin[info-bar::response]{signal}
      @begin{pre}
lambda (infobar response)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[infobar]{The @class{gtk:info-bar} widget on which the signal is
          emitted.}
        @entry[response]{The integer for the response ID.}
      @end{simple-table}
      Emitted when an action widget is clicked or the application programmer
      calls the @fun{gtk:dialog-response} function. The @arg{response} argument
      depends on which action widget was clicked.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:info-bar-new}
  @see-constructor{gtk:info-bar-new-with-buttons}
  @see-slot{gtk:info-bar-message-type}
  @see-slot{gtk:info-bar-revealed}
  @see-slot{gtk:info-bar-show-close-button}
  @see-class{gtk:statusbar}
  @see-class{gtk:message-dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:info-bar-message-type ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "message-type" 'info-bar) t)
 "The @code{message-type} property of type @sym{gtk:message-type}
  (Read / Write / Construct) @br{}
  The type of the message. The type may be used to determine the appearance of
  the info bar. @br{}
  Default value: @val[gtk:message-type]{:info}")

#+liber-documentation
(setf (liber:alias-for-function 'info-bar-message-type)
      "Accessor"
      (documentation 'info-bar-message-type 'function)
 "@version{2025-09-16}
  @syntax{(gtk:info-bar-message-type object) => message-type}
  @syntax{(setf (gtk:info-bar-message-type object) message-type)}
  @argument[object]{a @class{gtk:info-bar} widget}
  @argument[message-type]{a value of the @sym{gtk:message-type} enumeration}
  @begin{short}
    The accessor for the @slot[gtk:info-bar]{message-type} slot of the
    @class{gtk:info-bar} class gets or sets the type of the message.
  @end{short}
  GTK uses the message type to determine how the message is displayed.
  @begin[Warning]{dictionary}
    The @class{gtk:info-bar} implementation is deprecated since 4.10. Do not
    use it in newly written code.
  @end{dictionary}
  @see-class{gtk:info-bar}
  @see-symbol{gtk:message-type}")

;;; --- gtk:info-bar-revealed --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "revealed" 'info-bar) t)
 "The @code{revealed} property of type @code{:boolean} (Read / Write) @br{}
  Controls whether the action bar shows its contents or not. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'info-bar-revealed)
      "Accessor"
      (documentation 'info-bar-revealed 'function)
 "@version{2025-09-16}
  @syntax{(gtk:info-bar-revealed object) => revealed}
  @syntax{(setf (gtk:info-bar-revealed object) revealed)}
  @argument[object]{a @class{gtk:info-bar} widget}
  @argument[revealed]{a boolean whether the action bar shows its contents}
  @begin{short}
    The accessor for the @slot[gtk:info-bar]{revealed} slot of the
    @class{gtk:info-bar} class gets or sets whether the info bar is currently
    revealed.
  @end{short}

  Changing this will make the info bar reveal or conceal itself by means of
  a sliding transition. Note: this does not show or hide the info bar in the
  visible sense, so revealing has no effect if the @slot[gtk:widget]{visible}
  property is @em{false}.
  @begin[Warning]{dictionary}
    The @class{gtk:info-bar} implementation is deprecated since 4.10. Do not
    use it in newly written code.
  @end{dictionary}
  @see-class{gtk:info-bar}
  @see-function{gtk:widget-visible}")

;;; --- gtk:info-bar-show-close-button -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-close-button" 'info-bar) t)
 "The @code{show-close-button} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Whether to include a standard Close button. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'info-bar-show-close-button)
      "Accessor"
      (documentation 'info-bar-show-close-button 'function)
 "@version{2025-09-16}
  @syntax{(gtk:info-bar-show-close-button object) => setting}
  @syntax{(setf (gtk:info-bar-show-close-button object) setting)}
  @argument[object]{a @class{gtk:info-bar} widget}
  @argument[setting]{@em{true} to include a Close button}
  @begin{short}
    The accessor for the @slot[gtk:info-bar]{show-close-button} slot of the
    @class{gtk:info-bar} class gets or sets whether to include a standard Close
    button.
  @end{short}
  If @em{true}, a standard Close button is shown. When clicked it emits the
  @val[gtk:response-type]{:close} response.
  @begin[Warning]{dictionary}
    The @class{gtk:info-bar} implementation is deprecated since 4.10. Do not
    use it in newly written code.
  @end{dictionary}
  @see-class{gtk:info-bar}")

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_new
;;; ----------------------------------------------------------------------------

(declaim (inline info-bar-new))

(defun info-bar-new ()
 #+liber-documentation
 "@version{2025-03-13}
  @return{The new @class{gtk:info-bar} widget.}
  @short{Creates a new info bar.}
  @begin[Warning]{dictionary}
    The @class{gtk:info-bar} implementation is deprecated since 4.10. Do not
    use it in newly written code.
  @end{dictionary}
  @see-class{gtk:info-bar}"
  (make-instance 'info-bar))

(export 'info-bar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_new_with_buttons
;;; ----------------------------------------------------------------------------

(defun info-bar-new-with-buttons (&rest args)
 #+liber-documentation
 "@version{2025-07-22}
  @argument[args]{first a string for the text and second an integer for the
    response ID for each button, then more pairs for each button}
  @return{The new @class{gtk:info-bar} widget.}
  @begin{short}
    Creates a new info bar with buttons.
  @end{short}
  Button text/response ID pairs should be listed. Button text can be some
  arbitrary text. A response ID can be any positive number, or one of the values
  in the @sym{gtk:response-type} enumeration. If the user clicks one of these
  dialog buttons, the info bar will emit the @sig[gtk:info-bar]{response} signal
  with the corresponding response ID.
  @begin[Warning]{dictionary}
    The @class{gtk:info-bar} implementation is deprecated since 4.10. Do not
    use it in newly written code.
  @end{dictionary}
  @see-class{gtk:info-bar}
  @see-symbol{gtk:response-type}"
  (let ((infobar (make-instance 'info-bar)))
     (apply #'info-bar-add-buttons infobar args)
     infobar))

(export 'info-bar-new-with-buttons)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_add_action_widget
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_info_bar_add_action_widget" info-bar-add-action-widget)
    :void
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[infobar]{a @class{gtk:info-bar} widget}
  @argument[child]{an activatable @class{gtk:widget} child widget}
  @argument[response]{an integer for the response ID for @arg{child}}
  @begin{short}
    Add an activatable widget to the action area of an info bar, connecting a
    signal handler that will emit the @sig[gtk:info-bar]{response} signal on the
    message area when the widget is activated.
  @end{short}
  The widget is appended to the end of the action area of the infor bar.
  @begin[Warning]{dictionary}
    The @class{gtk:info-bar} implementation is deprecated since 4.10. Do not
    use it in newly written code.
  @end{dictionary}
  @see-class{gtk:info-bar}
  @see-class{gtk:widget}"
  (infobar (g:object info-bar))
  (child (g:object widget))
  (response :int))

(export 'info-bar-add-action-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_remove_action_widget
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_info_bar_remove_action_widget"
               info-bar-remove-action-widget) :void
 #+liber-documentation
 "@version{#2025-03-13}
  @argument[infobar]{a @class{gtk:info-bar} widget}
  @argument[widget]{a @class{gtk:widget} action widget to remove}
  @begin{short}
    Removes a widget from the action area of the info bar, after it been put
    there by a call to the @fun{gtk:info-bar-add-action-widget} or
    @fun{gtk:info-bar-add-button} function.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:info-bar} implementation is deprecated since 4.10. Do not
    use it in newly written code.
  @end{dictionary}
  @see-class{gtk:info-bar}
  @see-class{gtk:widget}
  @see-function{gtk:info-bar-add-action-widget}
  @see-function{gtk:info-bar-add-button}"
  (infobar (g:object info-bar))
  (widget (g:object widget)))

(export 'info-bar-remove-action-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_add_button
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_info_bar_add_button" info-bar-add-button) (g:object widget)
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[infobar]{a @class{gtk:info-bar} widget}
  @argument[text]{a string for the text of the button}
  @argument[response]{an integer for the response ID of the button}
  @return{The @class{gtk:button} widget that was added.}
  @begin{short}
    Adds a button with the given text, and sets things up so that clicking the
    button will emit the @sig[gtk:info-bar]{response} signal with the given
    response ID.
  @end{short}
  The button is appended to the end of the action area of the info bar. The
  button widget is returned, but usually you do not need it.
  @begin[Warning]{dictionary}
    The @class{gtk:info-bar} implementation is deprecated since 4.10. Do not
    use it in newly written code.
  @end{dictionary}
  @see-class{gtk:info-bar}
  @see-class{gtk:button}
  @see-function{gtk:info-bar-add-buttons}"
  (infobar (g:object info-bar))
  (text :string)
  (response response-type))

(export 'info-bar-add-button)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_add_buttons
;;; ----------------------------------------------------------------------------

(defun info-bar-add-buttons (infobar &rest args)
 #+liber-documentation
 "@version{#2025-03-13}
  @argument[infobar]{a @class{gtk:info-bar} widget}
  @argument[args]{first a string for a button text and second an integer for
    a response ID, then more pairs for each button}
  @begin{short}
    Adds more buttons, same as calling the @fun{gtk:info-bar-add-button}
    function repeatedly.
  @end{short}
  Each button must have both text and a response ID.
  @begin[Warning]{dictionary}
    The @class{gtk:info-bar} implementation is deprecated since 4.10. Do not
    use it in newly written code.
  @end{dictionary}
  @see-class{gtk:info-bar}
  @see-function{gtk:info-bar-add-button}"
  (iter (for (text response) on args by #'cddr)
        (info-bar-add-button infobar text response)))

(export 'info-bar-add-buttons)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_set_response_sensitive
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_info_bar_set_response_sensitive"
               info-bar-set-response-sensitive) :void
 #+liber-documentation
 "@version{#2025-03-13}
  @argument[infobar]{a @class{gtk:info-bar} widget}
  @argument[response]{an integer for a response ID}
  @argument[setting]{@em{true} for sensitive}
  @begin{short}
    Calls the @fun{gtk:widget-sensitive} function for each widget in the action
    area of the info bar with the given response ID.
  @end{short}
  A convenient way to sensitize/desensitize dialog buttons.
  @begin[Warning]{dictionary}
    The @class{gtk:info-bar} implementation is deprecated since 4.10. Do not
    use it in newly written code.
  @end{dictionary}
  @see-class{gtk:info-bar}
  @see-function{gtk:widget-sensitive}"
  (infobar (g:object info-bar))
  (response :int)
  (setting :boolean))

(export 'info-bar-set-response-sensitive)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_set_default_response
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_info_bar_set_default_response"
               info-bar-set-default-response) :void
 #+liber-documentation
 "@version{#2025-03-13}
  @argument[infobar]{a @class{gtk:info-bar} widget}
  @argument[response]{an integer for a response ID}
  @begin{short}
    Sets the last widget in the action area of the info bar with the given
    response ID as the default widget for the info bar.
  @end{short}
  Pressing the @kbd{Enter} key usually activates the default widget.

  Note that this function currently requires the info bar to be added to a
  widget hierarchy.
  @begin[Warning]{dictionary}
    The @class{gtk:info-bar} implementation is deprecated since 4.10. Do not
    use it in newly written code.
  @end{dictionary}
  @see-class{gtk:info-bar}"
  (infobar (g:object info-bar))
  (response :int))

(export 'info-bar-set-default-response)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_response
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_info_bar_response" info-bar-response) :void
 #+liber-documentation
 "@version{#2025-07-22}
  @argument[infobar]{a @class{gtk:info-bar} widget}
  @argument[response]{an integer for a response ID}
  @begin{short}
    Emits the @sig[gtk:info-bar]{response} signal with the given response ID.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:info-bar} implementation is deprecated since 4.10. Do not
    use it in newly written code.
  @end{dictionary}
  @see-class{gtk:info-bar}"
  (infobar (g:object info-bar))
  (response :int))

(export 'info-bar-response)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_add_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_info_bar_add_child" info-bar-add-child) :void
 #+liber-documentation
 "@version{#2025-03-13}
  @argument[infobar]{a @class{gtk:info-bar} widget}
  @argument[widget]{a @class{gtk:widget} child widget to be added}
  @short{Adds a widget to the content area of the info bar.}
  @begin[Warning]{dictionary}
    The @class{gtk:info-bar} implementation is deprecated since 4.10. Do not
    use it in newly written code.
  @end{dictionary}
  @see-class{gtk:info-bar}
  @see-class{gtk:widget}"
  (infobar (g:object info-bar))
  (widget (g:object widget)))

(export 'info-bar-add-child)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_remove_child
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_info_bar_remove_child" info-bar-remove-child) :void
 #+liber-documentation
 "@version{#2025-03-13}
  @argument[infobar]{a @class{gtk:info-bar} widget}
  @argument[widget]{a @class{gtk:widget} child widget to be removed}
  @begin{short}
    Removes a widget from the content area of the info bar, after it has been
    added with the @fun{gtk:info-bar-add-child} function.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:info-bar} implementation is deprecated since 4.10. Do not
    use it in newly written code.
  @end{dictionary}
  @see-class{gtk:info-bar}
  @see-class{gtk:widget}
  @see-function{gtk:info-bar-add-child}"
  (infobar (g:object info-bar))
  (widget (g:object widget)))

(export 'info-bar-remove-child)

;;; --- End of file gtk4.info-bar.lisp -----------------------------------------
