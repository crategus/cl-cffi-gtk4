;;; ----------------------------------------------------------------------------
;;; gtk4.message-dialog.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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
;;; GtkMessageDialog
;;;
;;;     A convenient message window
;;;
;;; Types and Values
;;;
;;;     GtkMessageDialog
;;;     GtkButtonsType
;;;
;;; Accessors
;;;
;;;     gtk_message_dialog_get_message_area
;;;
;;; Functions
;;;
;;;     gtk_message_dialog_new
;;;     gtk_message_dialog_new_with_markup
;;;     gtk_message_dialog_set_markup
;;;     gtk_message_dialog_format_secondary_text
;;;     gtk_message_dialog_format_secondary_markup
;;;
;;; Properties
;;;
;;;     buttons
;;;     message-area
;;;     message-type
;;;     secondary-text
;;;     secondary-use-markup
;;;     text
;;;     use-markup
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkWindow
;;;                 ╰── GtkDialog
;;;                     ╰── GtkMessageDialog
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkNative
;;;     GtkShortcutManager
;;;     GtkRoot
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkButtonsType
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GtkButtonsType" buttons-type
  (:export t
   :type-initializer "gtk_buttons_type_get_type")
  (:none 0)
  (:ok 1)
  (:close 2)
  (:cancel 3)
  (:yes-no 4)
  (:ok-cancel 5))

#+liber-documentation
(setf (liber:alias-for-symbol 'buttons-type)
      "GEnum"
      (liber:symbol-documentation 'buttons-type)
 "@version{2024-5-1}
  @begin{declaration}
(gobject:define-g-enum \"GtkButtonsType\" buttons-type
  (:export t
   :type-initializer \"gtk_buttons_type_get_type\")
  (:none 0)
  (:ok 1)
  (:close 2)
  (:cancel 3)
  (:yes-no 4)
  (:ok-cancel 5))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:none]{No buttons at all.}
      @entry[:ok]{An OK button.}
      @entry[:close]{A Close button.}
      @entry[:cancel]{A Cancel button.}
      @entry[:yes-no]{Yes and No buttons.}
      @entry[:ok-cancel]{OK and Cancel buttons.}
    @end{table}
  @end{values}
  @begin{short}
    Prebuilt sets of buttons for the dialog.
  @end{short}
  If none of these choices are appropriate, simply use the @code{:none} value
  and call the @fun{gtk:dialog-add-buttons} function to add your own buttons.

  Please note that the @code{:ok}, @code{:yes-no} and @code{:ok-cancel} values
  are discouraged by the @url[https://developer.gnome.org/hig/]{GNOME Human
  Interface Guidelines}.
  @see-class{gtk:message-dialog}
  @see-function{gtk:dialog-add-buttons}")

;;; ----------------------------------------------------------------------------
;;; GtkMessageDialog
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkMessageDialog" message-dialog
  (:superclass dialog
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkNative"
                "GtkRoot"
                "GtkShortcutManager")
   :type-initializer "gtk_message_dialog_get_type")
  ((buttons
    %message-dialog-buttons
    "buttons" "GtkButtonsType" nil nil)
   (message-area
    message-dialog-message-area
    "message-area" "GtkWidget" t nil)
   (message-type
    message-dialog-message-type
    "message-type" "GtkMessageType" t t)
   (secondary-text
    message-dialog-secondary-text
    "secondary-text" "gchararray" t t)
   (secondary-use-markup
    message-dialog-secondary-use-markup
    "secondary-use-markup" "gboolean" t t)
   (text
    message-dialog-text
    "text" "gchararray" t t)
   (use-markup
    message-dialog-use-markup
    "use-markup" "gboolean" t t)))

#+(and gtk-4-10 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj message-dialog) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:MESSAGE-DIALOG is deprecated since 4.10")))

#+liber-documentation
(setf (documentation 'message-dialog 'type)
 "@version{2024-5-2}
  @begin{short}
    The @class{gtk:message-dialog} widget presents a dialog with some message
    text.
  @end{short}
  It is simply a convenience widget. You could construct the equivalent of a
  message dialog from a @class{gtk:dialog} widget without too much effort, but
  the @class{gtk:message-dialog} widget saves typing.

  @image[message-dialog]{Figure: GtkMessageDialog}

  The easiest way to do a modal message dialog is to use the @code{:modal} flag
  of the @symbol{gtk:dialog-flags} flags. The message dialog will prevent
  interaction with the parent window until it is hidden or destroyed. You can
  use the @code{\"response\"} signal to know when the user dismissed the
  message dialog.
  @begin[Examples]{dictionary}
    An example for creating a modal message dialog.
    @begin{pre}
(defun create-message-dialog-simple (parent)
  (let ((dialog (make-instance 'gtk:message-dialog
                               :transient-for parent
                               :modal t
                               :message-type :info
                               :buttons :ok
                               :text \"Message Dialog\"
                               :secondary-text \"The secondary text.\")))
    ;; Handler for the \"response\" signal of the dialog
    (g:signal-connect dialog \"response\"
                      (lambda (dialog response)
                        (gtk:window-destroy dialog)))
    (gtk:window-present dialog)))
    @end{pre}
    This is a variant that uses the @fun{gtk:message-dialog-new} function. The
    first example is more lispy and the implementation more favorable.
    @begin{pre}
(defun create-message-dialog-simple2 (parent)
  (let ((dialog (gtk:message-dialog-new parent
                                        '(:modal)
                                        :info
                                        :ok-cancel
                                        \"Message Dialog\"
                                        parent)))
    ;; Set secondary text with the accessor
    (setf (gtk:message-dialog-secondary-text dialog)
          \"Created with constructor and with two buttons.\")
    ;; Handler for the \"response\" signal of the dialog
    (g:signal-connect dialog \"response\"
                      (lambda (dialog response)
                        (gtk:window-destroy dialog)))
    (gtk:window-present dialog)))
    @end{pre}
  @end{dictionary}
  @begin[GtkMessageDialog as GtkBuildable]{dictionary}
    The @class{gtk:message-dialog} implementation of the @class{gtk:buildable}
    interface exposes the message area as an internal child with the name
    @code{message_area}.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:message-dialog} widget is deprecated since 4.10. Use the
    @class{gtk:alert-dialog} widget instead.
  @end{dictionary}
  @see-constructor{gtk:message-dialog-new}
  @see-constructor{gtk:message-dialog-new-with-markup}
  @see-slot{gtk:message-dialog-message-area}
  @see-slot{gtk:message-dialog-message-type}
  @see-slot{gtk:message-dialog-secondary-text}
  @see-slot{gtk:message-dialog-secondary-use-markup}
  @see-slot{gtk:message-dialog-text}
  @see-slot{gtk:message-dialog-use-markup}
  @see-class{gtk:dialog}
  @see-class{gtk:alert-dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:message-dialog-buttons ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "buttons" 'message-dialog) t)
 "The @code{buttons} property of type @symbol{gtk:buttons-type}
  (Construct Only) @br{}
  The buttons shown in the message dialog. @br{}
  @em{Note:} The property can be set only in a constructor and is not
  readable or writable. There is no accessor. @br{}
  Default value: @code{:none}")

;; We have no accessor. Unexport the symbol.
(unexport 'message-dialog-buttons)

;;; --- gtk:message-dialog-message-area ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "message-area"
                                               'message-dialog) t)
 "The @code{message-area} property of type @class{gtk:widget} (Read) @br{}
  The @class{gtk:box} widget with @code{:vertical} orientation that corresponds
  to the message area of the message dialog. See the
  @fun{gtk:message-dialog-message-area} function for a detailed description of
  the message area.")

#+liber-documentation
(setf (liber:alias-for-function 'message-dialog-message-area)
      "Accessor"
      (documentation 'message-dialog-message-area 'function)
 "@version{2024-5-2}
  @syntax{(gtk:message-dialog-message-area object) => area}
  @argument[dialog]{a @class{gtk:message-dialog} widget}
  @argument[area]{a @class{gtk:box} widget with @code{:vertical} orientation}
  @begin{short}
    Accessor of the @slot[gtk:message-dialog]{message-area} slot of the
    @class{gtk:message-dialog} class.
  @end{short}
  The @fun{gtk:message-dialog-message-area} function returns the @class{gtk:box}
  widget with @code{:vertical} orientation corresponding to the \"message area\"
  in the message dialog. This is the box where the primary and secondary labels
  of the message dialog are packed.

  You can add your own extra content to that box and it will appear below those
  labels. See the @fun{gtk:dialog-content-area} function for the corresponding
  function in the parent @class{gtk:dialog} class.
  @begin[Warning]{dictionary}
    The @class{gtk:message-dialog} widget is deprecated since 4.10. Use the
    @class{gtk:alert-dialog} widget instead.
  @end{dictionary}
  @see-class{gtk:message-dialog}
  @see-class{gtk:box}
  @see-class{gtk:dialog}
  @see-class{gtk:alert-dialog}
  @see-function{gtk:dialog-content-area}")

;;; --- gtk:message-dialog-message-type ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "message-type"
                                               'message-dialog) t)
 "The @code{message-type} property of type @symbol{gtk:message-type}
  (Read / Write / Construct) @br{}
  The type of the message dialog. @br{}
  Default value: @code{:info}")

#+liber-documentation
(setf (liber:alias-for-function 'message-dialog-message-type)
      "Accessor"
      (documentation 'message-dialog-message-type 'function)
 "@version{2024-5-2}
  @syntax{(gtk:message-dialog-message-type object) => type}
  @argument[object]{a @class{gtk:message-dialog} widget}
  @argument[type]{a value of the @symbol{gtk:message-type} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:message-dialog]{message-type} slot of the
    @class{gtk:message-dialog} class.
  @end{short}
  The type of the message.
  @begin[Warning]{dictionary}
    The @class{gtk:message-dialog} widget is deprecated since 4.10. Use the
    @class{gtk:alert-dialog} widget instead.
  @end{dictionary}
  @see-class{gtk:message-dialog}
  @see-class{gtk:alert-dialog}
  @see-symbol{gtk:message-type}")

;;; --- gtk:message-dialog-secondary-text --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "secondary-text"
                                               'message-dialog) t)
 "The @code{secondary-text} property of type @code{:string} (Read / Write) @br{}
  The secondary text for the message dialog. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'message-dialog-secondary-text)
      "Accessor"
      (documentation 'message-dialog-secondary-text 'function)
 "@version{2024-5-2}
  @syntax{(gtk:message-dialog-secondary-text object) => text}
  @syntax{(setf (gtk:message-dialog-secondary-text object) text)}
  @argument[object]{a @class{gtk:message-dialog} widget}
  @argument[text]{a string with the secondary text for the message dialog}
  @begin{short}
    Accessor of the @slot[gtk:message-dialog]{secondary-text} slot of the
    @class{gtk:message-dialog} class.
  @end{short}
  The secondary text for the message dialog.
  @begin[Warning]{dictionary}
    The @class{gtk:message-dialog} widget is deprecated since 4.10. Use the
    @class{gtk:alert-dialog} widget instead.
  @end{dictionary}
  @see-class{gtk:message-dialog}
  @see-class{gtk:alert-dialog}
  @see-function{gtk:message-dialog-format-secondary-text}")

;;; --- gtk:message-dialog-secondary-use-markup --------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "secondary-use-markup"
                                               'message-dialog) t)
 "The @code{secondary-use-markup} property of type @code{:boolean}
  (Read / Write) @br{}
  @em{True} if the secondary text of the message dialog includes Pango markup.
  @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'message-dialog-secondary-use-markup)
      "Accessor"
      (documentation 'message-dialog-secondary-use-markup 'function)
 "@version{2024-5-2}
  @syntax{(gtk:message-dialog-secondary-use-markup object) => setting}
  @syntax{(setf (gtk:message-dialog-secondary-use-markup object) setting)}
  @argument[object]{a @class{gtk:message-dialog} widget}
  @argument[setting]{a boolean whether to use Pango markup}
  @begin{short}
    Accessor of the @slot[gtk:message-dialog]{secondary-use-markup} slot of the
    @class{gtk:message-dialog} class.
  @end{short}
  @em{True} if the secondary text for the message dialog includes Pango markup.
  @begin[Warning]{dictionary}
    The @class{gtk:message-dialog} widget is deprecated since 4.10. Use the
    @class{gtk:alert-dialog} widget instead.
  @end{dictionary}
  @see-class{gtk:message-dialog}
  @see-class{gtk:alert-dialog}
  @see-function{gtk:message-dialog-format-secondary-markup}")

;;; --- gtk:message-dialog-text ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "text" 'message-dialog) t)
 "The @code{text} property of type @code{:string} (Read / Write) @br{}
  The primary text for the message dialog. If the message dialog has a secondary
  text, this will appear as the title. @br{}
  Default value: \"\"")

#+liber-documentation
(setf (liber:alias-for-function 'message-dialog-text)
      "Accessor"
      (documentation 'message-dialog-text 'function)
 "@version{2024-5-2}
  @syntax{(gtk:message-dialog-text object) => text}
  @syntax{(setf (gtk:message-dialog-text object) text)}
  @argument[object]{a @class{gtk:message-dialog} widget}
  @argument[text]{a string with the primary text for the message dialog}
  @begin{short}
    Accessor of the @slot[gtk:message-dialog]{text} slot of the
    @class{gtk:message-dialog} class.
  @end{short}
  The primary text for the message dialog. If the message dialog has a secondary
  text, this will appear as the title.
  @begin[Warning]{dictionary}
    The @class{gtk:message-dialog} widget is deprecated since 4.10. Use the
    @class{gtk:alert-dialog} widget instead.
  @end{dictionary}
  @see-class{gtk:message-dialog}
  @see-class{gtk:alert-dialog}")

;;; --- gtk:message-dialog-use-markup ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-markup"
                                               'message-dialog) t)
 "The @code{use-markup} property of type @code{:boolean} (Read / Write) @br{}
  @em{True} if the primary text for the message dialog includes Pango markup.
  @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'message-dialog-use-markup)
      "Accessor"
      (documentation 'message-dialog-use-markup 'function)
 "@version{2024-5-2}
  @syntax{(gtk:message-dialog-use-markup object) => setting}
  @syntax{(setf (gtk:message-dialog-use-markup object) setting)}
  @argument[object]{a @class{gtk:message-dialog} widget}
  @argument[setting]{a boolean whether to use Pango markup}
  @begin{short}
    Accessor of the @slot[gtk:message-dialog]{use-markup} slot of the
    @class{gtk:message-dialog} class.
  @end{short}
  @em{True} if the primary text for the message dialog includes Pango markup.
  @begin[Warning]{dictionary}
    The @class{gtk:message-dialog} widget is deprecated since 4.10. Use the
    @class{gtk:alert-dialog} widget instead.
  @end{dictionary}
  @see-class{gtk:message-dialog}
  @see-class{gtk:alert-dialog}")

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_new
;;; ----------------------------------------------------------------------------

(defun message-dialog-new (parent flags type buttons message &rest args)
 #+liber-documentation
 "@version{2024-5-2}
  @argument[parent]{a transient @class{gtk:window} parent, or @code{nil} for
    none}
  @argument[flags]{a @symbol{gtk:dialog-flags} value for the flags to use}
  @argument[type]{a @symbol{gtk:message-type} value for the type of message}
  @argument[buttons]{a @symbol{gtk:buttons-type} value for the buttons to use}
  @argument[message]{a Lisp format string, or @code{nil}}
  @argument[args]{arguments for @arg{message}}
  @return{The new @class{gtk:message-dialog} widget.}
  @begin{short}
    Creates a new message dialog, which is a simple dialog with some text the
    user may want to see.
  @end{short}
  When the user clicks a button a @code{\"response\"} signal is emitted with
  response IDs from the @symbol{gtk:response-type} enumeration. See the
  @class{gtk:dialog} documentation for more details.
  @begin[Warning]{dictionary}
    The @class{gtk:message-dialog} widget is deprecated since 4.10. Use the
    @class{gtk:alert-dialog} widget instead.
  @end{dictionary}
  @see-class{gtk:message-dialog}
  @see-class{gtk:dialog}
  @see-class{gtk:window}
  @see-class{gtk:alert-dialog}
  @see-symbol{gtk:dialog-flags}
  @see-symbol{gtk:message-type}
  @see-symbol{gtk:buttons-type}
  @see-symbol{gtk:response-type}"
  (let ((dialog (make-instance 'message-dialog
                               :use-header-bar -1 ; no header bar
                               :message-type type
                               :buttons buttons)))
    (if message
        (setf (message-dialog-text dialog)
              (apply #'format nil message args)))
    (if parent
        (setf (window-transient-for dialog) parent))
    (if (member :modal flags)
        (setf (window-modal dialog) t))
    (if (member :destroy-with-parent flags)
        (setf (window-destroy-with-parent dialog) t))
    dialog))

(export 'message-dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_new_with_markup
;;; ----------------------------------------------------------------------------

(defun message-dialog-new-with-markup (parent flags type buttons message
                                           &rest args)
 #+liber-documentation
 "@version{2024-5-2}
  @argument[parent]{a transient @class{gtk:window} parent, or @code{nil} for
    none}
  @argument[flags]{a @symbol{gtk:dialog-flags} value for the flags to use}
  @argument[type]{a @symbol{gtk:message-type} value for the type of message}
  @argument[buttons]{a @symbol{gtk:buttons-type} value for the buttons to use}
  @argument[message]{a Lisp format string, or @code{nil}}
  @argument[args]{arguments for @arg{message}}
  @return{The new @class{gtk:message-dialog} widget.}
  @begin{short}
    Creates a new message dialog, which is a simple dialog with some text which
    is marked up with the Pango text markup language.
  @end{short}
  When the user clicks a button a @code{\"response\"} signal is emitted with
  response IDs from the @symbol{gtk:response-type} enumeration. See the
  @class{gtk:dialog} documentation for more details.

  Special XML characters in the message arguments passed to this function will
  automatically be escaped as necessary. Usually this is what you want, but if
  you have an existing Pango markup string that you want to use literally as the
  label, then you need to use the @fun{gtk:message-dialog-set-markup} function
  instead, since you cannot pass the markup string either as the format, it
  might contain '%' characters, or as a string argument.
  @begin[Examples]{dictionary}
    @begin{pre}
(defun create-message-dialog-new-with-markup (parent filename)
  (let ((dialog (gtk:message-dialog-new-with-markup
                                        parent
                                        '(:modal :destroy-with-parent)
                                        :error
                                        :close
                                        \"<b>Error loading file ~s</b>\"
                                        filename)))
    (g:signal-connect dialog \"response\"
                      (lambda (dialog response)
                        (declare (ignore response))
                        (gtk:window-destroy dialog)))
    (gtk:window-present dialog)))
    @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:message-dialog} widget is deprecated since 4.10. Use the
    @class{gtk:alert-dialog} widget instead.
  @end{dictionary}
  @see-class{gtk:message-dialog}
  @see-class{gtk:window}
  @see-class{gtk:dialog}
  @see-class{gtk:alert-dialog}
  @see-symbol{gtk:dialog-flags}
  @see-symbol{gtk:message-type}
  @see-symbol{gtk:buttons-type}
  @see-symbol{gtk:response-type}
  @see-function{gtk:message-dialog-set-markup}"
  (let ((dialog (make-instance 'message-dialog
                               :use-markup t
                               :use-header-bar -1 ; no header bar
                               :message-type type
                               :buttons buttons)))
    (if message
        (setf (message-dialog-text dialog)
              (apply #'format nil message args)))
    (if parent
        (setf (window-transient-for dialog) parent))
    (if (member :modal flags)
        (setf (window-modal dialog) t))
    (if (member :destroy-with-parent flags)
        (setf (window-destroy-with-parent dialog) t))
    dialog))

(export 'message-dialog-new-with-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_set_markup
;;; ----------------------------------------------------------------------------

(defun message-dialog-set-markup (dialog text)
 #+liber-documentation
 "@version{2024-5-2}
  @argument[dialog]{a @class{gtk:message-dialog} widget}
  @argument[text]{a markup string, see Pango markup format}
  @begin{short}
    Sets the text for the message dialog to be @arg{text}, which is marked
    up with the Pango text markup language.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:message-dialog} widget is deprecated since 4.10. Use the
    @class{gtk:alert-dialog} widget instead.
  @end{dictionary}
  @see-class{gtk:message-dialog}
  @see-class{gtk:alert-dialog}"
  (setf (message-dialog-use-markup dialog) t
        (message-dialog-text dialog) text))

(export 'message-dialog-set-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_format_secondary_text
;;; ----------------------------------------------------------------------------

(declaim (inline message-dialog-format-secondary-text))

(defun message-dialog-format-secondary-text (dialog message &rest args)
 #+liber-documentation
 "@version{2024-5-2}
  @argument[dialog]{a @class{gtk:message-dialog} widget}
  @argument[message]{a Lisp format string, or @code{nil}}
  @argument[args]{arguments for @arg{message}}
  @begin{short}
    Sets the secondary text for the message dialog to be @arg{message} with
    the arguments in @arg{args}.
  @end{short}
  Note that setting a secondary text makes the primary text become bold, unless
  you have provided explicit markup.
  @begin[Warning]{dictionary}
    The @class{gtk:message-dialog} widget is deprecated since 4.10. Use the
    @class{gtk:alert-dialog} widget instead.
  @end{dictionary}
  @see-class{gtk:message-dialog}
  @see-class{gtk:alert-dialog}
  @see-function{gtk:message-dialog-format-secondary-markup}"
  (setf (message-dialog-secondary-text dialog)
        (apply #'format nil message args)))

(export 'message-dialog-format-secondary-text)

;;; ----------------------------------------------------------------------------
;;; gtk_message_dialog_format_secondary_markup
;;; ----------------------------------------------------------------------------

(declaim (inline message-dialog-format-secondary-markup))

(defun message-dialog-format-secondary-markup (dialog message &rest args)
 #+liber-documentation
 "@version{2024-5-2}
  @argument[dialog]{a @class{gtk:message-dialog} widget}
  @argument[message]{a Lisp format string with markup, see Pango markup format,
    or @code{nil}}
  @argument[args]{arguments for @arg{message}}
  @begin{short}
    Sets the secondary text for the message dialog to be @arg{message} with
    the arguments in @arg{args}, which is marked up with the Pango text markup
    language.
  @end{short}
  Note that setting a secondary text makes the primary text become bold,
  unless you have provided explicit markup. Due to an oversight in the C
  implementation, this function does not escape special XML characters like the
  @fun{gtk:message-dialog-new-with-markup} function does.
  @begin[Warning]{dictionary}
    The @class{gtk:message-dialog} widget is deprecated since 4.10. Use the
    @class{gtk:alert-dialog} widget instead.
  @end{dictionary}
  @see-class{gtk:message-dialog}
  @see-class{gtk:alert-dialog}
  @see-function{gtk:message-dialog-new-with-markup}
  @see-function{gtk:message-dialog-format-secondary-text}"
  (setf (message-dialog-secondary-use-markup dialog) t
        (message-dialog-secondary-text dialog)
        (apply #'format nil message args)))

(export 'message-dialog-format-secondary-markup)

;;; --- End of file gtk4.message-dialog.lisp -----------------------------------
