;;; ----------------------------------------------------------------------------
;;; gtk.info-bar.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2022 Dieter Kaiser
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU Lesser General Public License for Lisp
;;; as published by the Free Software Foundation, either version 3 of the
;;; License, or (at your option) any later version and with a preamble to
;;; the GNU Lesser General Public License that clarifies the terms for use
;;; with Lisp programs and is referred as the LLGPL.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU Lesser General Public License for more details.
;;;
;;; You should have received a copy of the GNU Lesser General Public
;;; License along with this program and the preamble to the Gnu Lesser
;;; General Public License.  If not, see <http://www.gnu.org/licenses/>
;;; and <http://opensource.franz.com/preamble.html>.
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
;;; Class GtkInfoBar
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkInfoBar" info-bar
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

#+liber-documentation
(setf (documentation 'info-bar 'type)
 "@version{#2022-5-27}
  @begin{short}
    The @sym{gtk:info-bar} widget can be used to show messages to the user
    without showing a dialog.
  @end{short}
  It is often temporarily shown at the top or bottom of a document. In contrast
  to the @class{gtk:dialog} widget, which has a horizontal action area at the
  bottom, the info bar has a vertical action area at the side.

  @image[info-bar]{Figure: GtkInfoBar}

  The API of the @sym{gtk:info-bar} widget is very similar to the
  @class{gtk:dialog} widget, allowing you to add buttons to the action area
  with the @fun{gtk:info-bar-add-button} or @fun{gtk:info-bar-new-with-buttons}
  functions. The sensitivity of action widgets can be controlled with the
  @fun{gtk:info-bar-set-response-sensitive} function. To add widgets to the main
  content area of an info bar, use the @fun{gtk:info-bar-add-child} function.

  Similar to @class{gtk:message-dialog} widget, the contents of an info bar can
  by classified as error message, warning, informational message, etc, by using
  the @fun{gtk:info-bar-message-type} function. GTK may use the message type to
  determine how the message is displayed.
  @begin[Example]{dictionary}
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
    (g-signal-connect infobar \"response\"
                      (lambda (widget response)
                        (declare (ignore response))
                        (gtk:widget-hide widget)))
    infobar))
    @end{pre}
  @end{dictionary}
  @begin[GtkInfoBar as GtkBuildable]{dictionary}
    The @sym{gtk:info-bar} implementation of the @class{gtk:buildable} interface
    exposes the content area and action area as internal children with the names
    @code{content_area} and @code{action_area}.

    The @sym{gtk:info-bar} implementation supports a custom
    @code{<action-widgets>} element, which can contain multiple
    @code{<action-widget>} elements. The @code{response} attribute specifies a
    numeric response, and the content of the element is the ID of the widget,
    which should be a child of the dialogs action area.
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:info-bar} implementation has a single CSS node with name
    @code{infobar}. The node may get one of the @code{.info}, @code{.warning},
    @code{.error} or @code{.question} style classes, depending on the message
    type. If the info bar shows a close button, that button will have the
    @code{.close} style class applied.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"close\" signal}
      @begin{pre}
lambda (infobar)    :action
      @end{pre}
      The signal is a keybinding signal which gets emitted when the user uses a
      keybinding to dismiss the info bar. The default binding for this signal
      is the @kbd{Escape} key. @br{}
      @begin[code]{table}
        @entry[infobar]{The @sym{gtk:info-bar} widget on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"response\" signal}
      @begin{pre}
lambda (infobar response)    :run-last
      @end{pre}
      Emitted when an action widget is clicked or the application programmer
      calls the @fun{gtk:dialog-response} function. The @arg{response} argument
      depends on which action widget was clicked. @br{}
      @begin[code]{table}
        @entry[infobar]{The @sym{gtk:info-bar} widget on which the signal is
          emitted.}
        @entry[response]{An integer with the response ID.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:info-bar-message-type}
  @see-slot{gtk:info-bar-revealed}
  @see-slot{gtk:info-bar-show-close-button}
  @see-constructor{gtk:info-bar-new}
  @see-constructor{gtk:info-bar-new-with-buttons}
  @see-class{gtk:statusbar}
  @see-class{gtk:message-dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- info-bar-message-type ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "message-type" 'info-bar) t)
 "The @code{message-type} property of type @symbol{gtk:message-type}
  (Read / Write / Construct) @br{}
  The type of the message. The type may be used to determine the appearance of
  the info bar. @br{}
  Default value: @code{:info}")

#+liber-documentation
(setf (liber:alias-for-function 'info-bar-message-type)
      "Accessor"
      (documentation 'info-bar-message-type 'function)
 "@version{#2022-5-27}
  @syntax[]{(gtk:info-bar-message-type object) => message-type}
  @syntax[]{(setf (gtk:info-bar-message-type object) message-type)}
  @argument[object]{a @class{gtk:info-bar} widget}
  @argument[message-type]{a value of the @symbol{gtk:message-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:info-bar]{message-type} slot of the
    @class{gtk:info-bar} class.
  @end{short}
  The @sym{gtk:info-bar-message-type} function returns the message type of the
  message area. The @sym{(setf gtk:info-bar-message-type)} function sets the
  message type.

  GTK uses the message type to determine how the message is displayed.
  @see-class{gtk:info-bar}
  @see-symbol{gtk:message-type}")

;;; --- info-bar-revealed --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "revealed" 'info-bar) t)
 "The @code{revealed} property of type @code{:boolean} (Read / Write) @br{}
  Controls whether the action bar shows its contents or not. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'info-bar-revealed)
      "Accessor"
      (documentation 'info-bar-revealed 'function)
 "@version{#2022-5-27}
  @syntax[]{(gtk:info-bar-revealed object) => revealed}
  @syntax[]{(setf (gtk:info-bar-revealed object) revealed)}
  @argument[object]{a @class{gtk:info-bar} widget}
  @argument[revealed]{a boolean whether the action bar shows its contents}
  @begin{short}
    Accessor of the @slot[gtk:info-bar]{revealed} slot of the
    @class{gtk:info-bar} class.
  @end{short}
  The @sym{gtk:info-bar-revealed} function returns whether the info bar is
  currently revealed. The @sym{(setf gtk:info-bar-revealed)} function sets the
  property.

  Changing this will make the info bar reveal or conceal itself via a sliding
  transition. Note: this does not show or hide the info bar in the visible
  sense, so revealing has no effect if the @slot[widget]{visible} property
  is @em{false}.
  @see-class{gtk:info-bar}
  @see-function{gtk:widget-visible}")

;;; --- info-bar-show-close-button -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-close-button"
                                               'info-bar) t)
 "The @code{show-close-button} property of type @code{:boolean}
  (Read / Write / Construct) @br{}
  Whether to include a standard Close button. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'info-bar-show-close-button)
      "Accessor"
      (documentation 'info-bar-show-close-button 'function)
 "@version{#2022-5-27}
  @syntax[]{(gtk:info-bar-show-close-button object) => setting}
  @syntax[]{(setf (gtk:info-bar-show-close-button object) setting)}
  @argument[object]{a @class{gtk:info-bar} widget}
  @argument[setting]{@em{true} to include a Close button}
  @begin{short}
    Accessor of the @slot[gtk:info-bar]{show-close-button} slot of the
    @class{gtk:info-bar} class.
  @end{short}
  The @sym{gtk:info-bar-show-close-button} function returns whether the widget
  will display a standard Close button. If @em{true}, a standard Close button
  is shown. When clicked it emits the @code{:close} response.
  @see-class{gtk:info-bar}")

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline info-bar-new))

(defun info-bar-new ()
 #+liber-documentation
 "@version{#2022-5-27}
  @return{A new @class{gtk:info-bar} widget.}
  @short{Creates a new info bar.}
  @see-class{gtk:info-bar}"
  (make-instance 'info-bar))

(export 'info-bar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_new_with_buttons ()
;;; ----------------------------------------------------------------------------

(defun info-bar-new-with-buttons (&rest args)
 #+liber-documentation
 "@version{#2022-5-27}
  @argument[args]{first a string with the text and second an integer with the
    response ID for each button, then more pairs for each button}
  @return{A new @class{gtk:info-bar} widget.}
  @short{Creates a new info bar with buttons.}
  Button text/response ID pairs should be listed. Button text can be some
  arbitrary text. A response ID can be any positive number, or one of the values
  in the @symbol{gtk:response-type} enumeration. If the user clicks one of these
  dialog buttons, the info bar will emit the \"response\" signal with the
  corresponding response ID.
  @see-class{gtk:info-bar}
  @see-symbol{gtk:response-type}"
  (let ((infobar (make-instance 'info-bar)))
     (apply #'info-bar-add-buttons infobar args)
     infobar))

(export 'info-bar-new-with-buttons)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_add_action_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_info_bar_add_action_widget" info-bar-add-action-widget) :void
 #+liber-documentation
 "@version{#2022-5-27}
  @argument[infobar]{a @class{gtk:info-bar} widget}
  @argument[child]{an activatable @class{gtk:widget} child widget}
  @argument[response]{an integer with the response ID for @arg{child}}
  @begin{short}
    Add an activatable widget to the action area of an info bar, connecting a
    signal handler that will emit the \"response\" signal on the message area
    when the widget is activated.
  @end{short}
  The widget is appended to the end of the message areas action area.
  @see-class{gtk:info-bar}
  @see-class{gtk:widget}"
  (infobar (g:object info-bar))
  (child (g:object widget))
  (response :int))

(export 'info-bar-add-action-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_remove_action_widget ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_info_bar_remove_action_widget" info-bar-remove-action-widget)
    :void
 #+liber-documentation
 "@version{#2022-5-27}
  @argument[infobar]{a @class{gtk:info-bar} widget}
  @argument[widget]{a @class{gtk:widget} action widget to remove}
  @begin{short}
    Removes a widget from the action area of the info bar, after it been put
    there by a call to the @fun{gtk:info-bar-add-action-widget} or
    @fun{gtk:info-bar-add-button} function.
  @end{short}
  @see-class{gtk:info-bar}
  @see-class{gtk:widget}
  @see-function{gtk:info-bar-add-action-widget}
  @see-function{gtk:info-bar-add-button}"
  (infobar (g:object info-bar))
  (widget (g:object widget)))

(export 'info-bar-remove-action-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_add_button ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_info_bar_add_button" info-bar-add-button)
    (g:object widget)
 #+liber-documentation
 "@version{#2022-5-27}
  @argument[infobar]{a @class{gtk:info-bar} widget}
  @argument[text]{a string with the text of the button}
  @argument[response]{an integer with the response ID for the button}
  @return{The @class{gtk:button} widget that was added.}
  @begin{short}
    Adds a button with the given text, and sets things up so that clicking the
    button will emit the \"response\" signal with the given response ID.
  @end{short}
  The button is appended to the end of the action area of the info bar. The
  button widget is returned, but usually you do not need it.
  @see-class{gtk:info-bar}
  @see-class{gtk:button}
  @see-function{gtk:info-bar-add-buttons}"
  (infobar (g:object info-bar))
  (text :string)
  (response :int))

(export 'info-bar-add-button)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_add_buttons ()
;;; ----------------------------------------------------------------------------

(defun info-bar-add-buttons (infobar &rest args)
 #+liber-documentation
 "@version{#2022-5-27}
  @argument[infobar]{a @class{gtk:info-bar} widget}
  @argument[args]{first a string with a button text and second an integer with
    a response ID, then more pairs for each button}
  @begin{short}
    Adds more buttons, same as calling the @fun{gtk:info-bar-add-button}
    function repeatedly.
  @end{short}
  Each button must have both text and a response ID.
  @see-class{gtk:info-bar}
  @see-function{gtk:info-bar-add-button}"
  (loop for (text response) on args by #'cddr
        do (info-bar-add-button infobar text response)))

(export 'info-bar-add-buttons)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_set_response_sensitive ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_info_bar_set_response_sensitive"
           info-bar-set-response-sensitive) :void
 #+liber-documentation
 "@version{#2022-5-27}
  @argument[infobar]{a @class{gtk:info-bar} widget}
  @argument[response]{an integer with a response ID}
  @argument[setting]{@em{true} for sensitive}
  @begin{short}
    Calls the @fun{gtk:widget-sensitive} function for each widget in the action
    area of the info bar with the given response ID.
  @end{short}
  A convenient way to sensitize/desensitize dialog buttons.
  @see-class{gtk:info-bar}
  @see-function{gtk:widget-sensitive}"
  (infobar (g:object info-bar))
  (response :int)
  (setting :boolean))

(export 'info-bar-set-response-sensitive)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_set_default_response ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_info_bar_set_default_response" info-bar-set-default-response)
    :void
 #+liber-documentation
 "@version{#2022-5-27}
  @argument[infobar]{a @class{gtk:info-bar} widget}
  @argument[response]{an integer with a response ID}
  @begin{short}
    Sets the last widget in the action area of the info bar with the given
    response ID as the default widget for the info bar.
  @end{short}
  Pressing the @kbd{Enter} key usually activates the default widget.

  Note that this function currently requires the info bar to be added to a
  widget hierarchy.
  @see-class{gtk:info-bar}"
  (infobar (g:object info-bar))
  (response :int))

(export 'info-bar-set-default-response)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_response ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_info_bar_response" info-bar-response) :void
 #+liber-documentation
 "@version{#2022-5-27}
  @argument[infobar]{a @class{gtk:info-bar} widget}
  @argument[response]{an integer with a response ID}
  @short{Emits the \"response\" signal with the given response ID.}
  @see-class{gtk:info-bar}"
  (infobar (g:object info-bar))
  (response :int))

(export 'info-bar-response)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_add_child ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_info_bar_add_child" info-bar-add-child) :void
 #+liber-documentation
 "@version{#2022-5-27}
  @argument[infobar]{a @class{gtk:info-bar} widget}
  @argument[widget]{a @class{gtk:widget} child widget to be added}
  @short{Adds a widget to the content area of the info bar.}
  @see-class{gtk:info-bar}
  @see-class{gtk:widget}"
  (infobar (g:object info-bar))
  (widget (g:object widget)))

(export 'info-bar-add-child)

;;; ----------------------------------------------------------------------------
;;; gtk_info_bar_remove_child ()
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_info_bar_remove_child" info-bar-remove-child) :void
 #+liber-documentation
 "@version{#2022-5-27}
  @argument[infobar]{a @class{gtk:info-bar} widget}
  @argument[widget]{a @class{gtk:widget} child widget to be removed}
  @begin{short}
    Removes a widget from the content area of the info bar, after it has been
    added with the @fun{gtk:info-bar-add-child} function.
  @end{short}
  @see-class{gtk:info-bar}
  @see-class{gtk:widget}
  @see-function{gtk:info-bar-add-child}"
  (infobar (g:object info-bar))
  (widget (g:object widget)))

(export 'info-bar-remove-child)

;;; --- End of file gtk.info-bar.lisp ------------------------------------------
