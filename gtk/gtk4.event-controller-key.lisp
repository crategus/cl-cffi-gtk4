;;; ----------------------------------------------------------------------------
;;; gtk4.event-controller-key.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2023 Dieter Kaiser
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
;;; GtkEventControllerKey
;;;
;;;     Event controller for key events
;;;
;;; Types and Values
;;;
;;;     GtkEventControllerKey
;;;
;;; Functions
;;;
;;;     gtk_event_controller_key_new
;;;     gtk_event_controller_key_set_im_context
;;;     gtk_event_controller_key_get_im_context
;;;     gtk_event_controller_key_forward
;;;     gtk_event_controller_key_get_group
;;;
;;; Signals
;;;
;;;     im-update
;;;     key-pressed
;;;     key-released
;;;     modifiers
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkEventController
;;;         ╰── GtkEventControllerKey
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkEventControllerKey
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkEventControllerKey" event-controller-key
  (:superclass event-controller
   :export t
   :interfaces nil
   :type-initializer "gtk_event_controller_key_get_type")
  nil)

#+liber-documentation
(setf (documentation 'event-controller-key 'type)
 "@version{#2022-8-23}
  @begin{short}
    The @class{gtk:event-controller-key} object is an event controller meant for
    situations where you need access to key events.
  @end{short}
  @begin[Signal Details]{dictionary}
    @subheading{The \"im-update\" signal}
      @begin{pre}
lambda (controller)    :run-last
      @end{pre}
      The signal is emitted whenever the input method context filters away a
      keypress and prevents the controller receiving it. See the
      @fun{gtk:event-controller-key-im-context} and
      @fun{gtk:im-context-filter-keypress} functions.
      @begin[code]{table}
        @entry[controller]{The @class{gtk:event-controller-key} object which
          received the signal.}
      @end{table}
    @subheading{The \"key-pressed\" signal}
      @begin{pre}
lambda (controller keyval keycode state)    :run-last
      @end{pre}
      The signal is emitted whenever a key is pressed.
      @begin[code]{table}
        @entry[controller]{The @class{gtk:event-controller-key} object which
          received the signal.}
        @entry[keyval]{An unsigned integer with the pressed key.}
        @entry[keycode]{An unsigned integer with the raw code of the pressed
          key.}
        @entry[state]{The @symbol{gdk:modifier-type} bitmask representing the
          state of modifier keys and pointer buttons.}
        @entry[Returns]{@em{True} if the key press was handled, @em{false}
          otherwise.}
      @end{table}
    @subheading{The \"key-released\" signal}
      @begin{pre}
lambda (controller keyval keycode state)    :run-last
      @end{pre}
      The signal is emitted whenever a key is released.
      @begin[code]{table}
        @entry[controller]{The @class{gtk:event-controller-key} object which
          received the signal.}
        @entry[keyval]{An unsigned integer with the released key.}
        @entry[keycode]{An unsigned integer with the raw code of the released
          key.}
        @entry[state]{The @symbol{gdk:modifier-type} bitmask representing the
          state of modifier keys and pointer buttons.}
      @end{table}
    @subheading{The \"modifiers\" signal}
      @begin{pre}
lambda (controller state)    :run-last
      @end{pre}
      This signal is emitted whenever the state of modifier keys and pointer
      buttons change.
      @begin[code]{table}
        @entry[controller]{The @class{gtk:event-controller-key} object on
          which received the signal.}
        @entry[state]{The @symbol{gdk:modifier-type} bitmask, representing the
          state of modifier keys and pointer buttons.}
        @entry[Returns]{A not documented boolean.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:event-controller-key-new}
  @see-class{gtk:event-controller}")

;;; ----------------------------------------------------------------------------
;;; gtk_event_controller_key_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline event-controller-key-new))

(defun event-controller-key-new ()
 #+liber-documentation
 "@version{2023-10-18}
  @return{The new @class{gtk:event-controller-key} object.}
  @short{Creates a new event controller that will handle key events.}
  @see-class{gtk:event-controller-key}"
  (make-instance 'event-controller-key))

(export 'event-controller-key-new)

;;; ----------------------------------------------------------------------------
;;; gtk_event_controller_key_get_im_context ()
;;; gtk_event_controller_key_set_im_context ()
;;; -> event-controller-key-im-context
;;; ----------------------------------------------------------------------------

(defun (setf event-controller-key-im-context) (value controller)
  (cffi:foreign-funcall "gtk_event_controller_key_set_im_context"
                        (g:object event-controller-key) controller
                        (g:object im-context) value
                        :void)
  value)

(cffi:defcfun ("gtk_event_controller-key_get_im_context"
               event-controller-key-im-context) (g:object im-context)
 #+liber-documentation
 "@version{#2022-8-23}
  @syntax[]{(gtk:event-controller-key-im-context controller) => context}
  @syntax[]{(setf (gtk:event-controller-key-im-context controller) context)}
  @argument[controller]{a @class{gtk:event-controller-key} object}
  @argument[context]{a @class{gtk:im-context} object}
  @begin{short}
    Accessor of the input method context of the key controller.
  @end{short}
  The @fun{gtk:event-controller-key-im-context} function gets the input method
  context of the key controller. The @setf{gtk:event-controller-key-im-context}
  function sets the input method context.
  @see-class{gtk:event-controller-key}
  @see-class{gtk:im-context}"
  (controller (g:object event-controller-key)))

(export 'event-controller-key-im-context)

;;; ----------------------------------------------------------------------------
;;; gtk_event_controller_key_forward ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_event_controller_key_forward" event-controller-key-forward)
    :boolean
 #+liber-documentation
 "@version{#2022-8-23}
  @argument[controller]{a @class{gtk:event-controller-key} object}
  @argument[widget]{a @class{gtk:widget} object}
  @return{A boolean whether the widget handled the event}
  @begin{short}
    Forwards the current event of this controller to a widget.
  @end{short}
  This function can only be used in handlers for the \"key-pressed\",
  \"key-released\" or \"modifiers\" signals.
  @see-class{gtk:event-controller-key}
  @see-class{gtk:widget}"
  (controller (g:object event-controller-key))
  (widget (g:object widget)))

(export 'event-controller-key-forward)

;;; ----------------------------------------------------------------------------
;;; gtk_event_controller_key_get_group () -> event-controller-key-group
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_event_controller_key_get_group" event-controller-key-group)
    :uint
 #+liber-documentation
 "@version{#2022-8-23}
  @argument[controller]{a @class{gtk:event-controller-key} object}
  @return{An unsigned integer with the key group.}
  @begin{short}
    Gets the key group of the current event of the controller.
  @end{short}
  @see-class{gtk:event-controller-key}"
  (controller (g:object event-controller-key)))

(export 'event-controller-key-group)

;;; --- End of file gtk4.event-controller-key.lisp -----------------------------
