;;; ----------------------------------------------------------------------------
;;; gtk4.dialog.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.12 and modified to document the Lisp binding to the GTK library,
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
;;; GtkDialog
;;;
;;;     Create popup windows
;;;
;;; Types and Values
;;;
;;;     GtkDialog
;;;     GtkDialogFlags
;;;
;;; Functions
;;;
;;;     gtk_dialog_new
;;;     gtk_dialog_new_with_buttons
;;;
;;;     gtk_dialog_response
;;;     gtk_dialog_add_button
;;;     gtk_dialog_add_buttons
;;;     gtk_dialog_add_action_widget
;;;     gtk_dialog_set_default_response
;;;     gtk_dialog_set_response_sensitive
;;;     gtk_dialog_get_response_for_widget
;;;     gtk_dialog_get_widget_for_response
;;;     gtk_dialog_get_content_area
;;;     gtk_dialog_get_header_bar
;;;
;;; Properties
;;;
;;;     use-header-bar
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
;;;             ╰── GtkWindow
;;;                 ╰── GtkDialog
;;;                     ├── GtkAppChooserDialog
;;;                     ├── GtkColorChooserDialog
;;;                     ├── GtkFileChooserDialog
;;;                     ├── GtkFontChooserDialog
;;;                     ├── GtkMessageDialog
;;;                     ├── GtkPageSetupUnixDialog
;;;                     ╰── GtkPrintUnixDialog
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkNative
;;;     GtkRoot
;;;     GtkShortcutManager
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkDialogFlags
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "GtkDialogFlags" dialog-flags
  (:export t
   :type-initializer "gtk_dialog_flags_get_type")
  (:modal               #.(ash 1 0))
  (:destroy-with-parent #.(ash 1 1))
  (:use-header-bar      #.(ash 1 2)))

#+liber-documentation
(setf (liber:alias-for-symbol 'dialog-flags)
      "GFlags"
      (liber:symbol-documentation 'dialog-flags)
 "@version{2025-2-26}
  @begin{declaration}
(gobject:define-gflags \"GtkDialogFlags\" dialog-flags
  (:export t
   :type-initializer \"gtk_dialog_flags_get_type\")
  (:modal               #.(ash 1 0))
  (:destroy-with-parent #.(ash 1 1))
  (:use-header-bar      #.(ash 1 2)))
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:modal]{Make the constructed dialog modal.}
      @entry[:destroy-with-parent]{Destroy the dialog when its parent is
        destroyed.}
      @entry[:use-header-bar]{Create the dialog with actions in the header bar
        instead of an action area.}
    @end{table}
  @end{values}
  @begin{short}
    Flags used to influence the @class{gtk:dialog} widget construction.
  @end{short}
  @see-class{gtk:dialog}")

;;; ----------------------------------------------------------------------------
;;; GtkDialog
;;; ----------------------------------------------------------------------------

;; FIXME: We have a subtle bug. The INITIALIZE-INSTANCE :after and
;; INITIAlIZE-GOBJECT-CLASS-G-TYPE methods are not called for GtkDialog, when
;; loading the library. If we rename DIALOG to MYDIALOG the methods are called.
;; - 2023-8-22 -

;; If we call the definition at the Lisp prompt after loading the library
;; the definition of the class is executed correctly:
;;
;;* (gobject:define-gobject "GtkDialog" dialog
;;  (:superclass window
;;   :export t
;;   :interfaces ("GtkAccessible"
;;                "GtkBuildable"
;;                "GtkConstraintTarget"
;;                "GtkNative"
;;                "GtkRoot"
;;                "GtkShortcutManager")
;;   :type-initializer "gtk_dialog_get_type")
;;  ((use-header-bar
;;    dialog-use-header-bar
;;    "use-header-bar" "gint" t t)))
;;
;; This is the debugging output:
;;
;; :subclass INITIALIZE-INSTANCE :after for class
;;     #<GOBJECT-CLASS COMMON-LISP-USER::DIALOG {10046F11C3}> GtkDialog DIALOG
;; :subclass setf symbol-for-gtype GtkDialog
;; :subclass INITIALIZE-GOBJECT-CLASS-G-TYPE for class
;;     #<GOBJECT-CLASS COMMON-LISP-USER::DIALOG {10046F11C3}>
;;     #<GTYPE :name "GtkDialog" :id 94907715158336>

;; As a workaround we add the symbol DIALOG for GtkDialog to the hash table of
;; symbols. This ensures that we can use the class in further code.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf (glib:symbol-for-gtype "GtkDialog") 'dialog))

(gobject:define-gobject "GtkDialog" dialog
  (:superclass window
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkNative"
                "GtkRoot"
                "GtkShortcutManager")
   :type-initializer "gtk_dialog_get_type")
  ((use-header-bar
    dialog-use-header-bar
    "use-header-bar" "gint" t t)))

#+(and gtk-4-10 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj dialog) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:DIALOG is deprecated since 4.10")))

#+liber-documentation
(setf (documentation 'dialog 'type)
 "@version{2025-2-26}
  @begin{short}
    Dialogs are a convenient way to prompt the user for a small amount of input.
  @end{short}
  Typicall uses are to display a message, ask a question, or anything else that
  does not require extensive effort on the part of the user.

  @image[dialog]{Figure: GtkDialog}

  The main area of a @class{gtk:dialog} widget is called the \"content area\",
  and is yours to populate with widgets such a @class{gtk:label} or a
  @class{gtk:entry} widget, to present your information, questions, or tasks to
  the user.

  In addition, dialogs allow you to add \"action widgets\". Most commonly,
  action widgets are buttons. Depending on the platform, action widgets may be
  presented in the header bar at the top of the window, or at the bottom of the
  window. To add action widgets, create your @class{gtk:dialog} widget using
  the @fun{gtk:dialog-new-with-buttons} function, or use the
  @fun{gtk:dialog-add-button}, @fun{gtk:dialog-add-buttons}, or
  @fun{gtk:dialog-add-action-widget} functions.

  The @class{gtk:dialog} widgets uses some heuristics to decide whether to add a
  Close button to the window decorations. If any of the action buttons use the
  @code{:close} or @code{:cancel} response ID, the Close button is omitted.

  Clicking a button that was added as an action widget will emit the
  @code{\"response\"} signal with a response ID that you specified. GTK will
  never assign a meaning to positive response IDs. These are entirely
  user-defined. But for convenience, you can use the response IDs in the
  @symbol{gtk:response-type} enumeration, these all have values less than zero.
  If a dialog receives a delete event, the @code{\"response\"} signal will be
  emitted with the @code{:delete-event} response ID.

  Dialogs are created with a call to the @fun{gtk:dialog-new} function or the
  @fun{gtk:dialog-new-with-buttons} function. The latter is recommended. It
  allows you to set the dialog title, some convenient flags, and add buttons.

  A \"modal\" dialog, that is, one which freezes the rest of the application
  from user input, can be created by calling the @fun{gtk:window-modal} function
  on the dialog. When using the @fun{gtk:dialog-new-with-buttons} function, you
  can also pass the @code{:modal} flag to make a dialog modal.
  @begin[Examples]{dictionary}
    For the simple dialog in the following example, a @class{gtk:message-dialog}
    widget would save some effort. But you would need to create the dialog
    contents manually if you had more than a simple message in the dialog.
    @begin{pre}
;; Function to open a dialog to display the message provided.
(defun create-quick-message (parent msg)
  (let ((dialog (gtk:dialog-new-with-buttons \"Message\"
                                             parent
                                             '(:destroy-with-parent :modal)
                                             \"OK\"
                                             :ok)))
    (g:signal-connect dialog \"response\"
                      (lambda (widget response)
                        (declare (ignore response))
                        (gtk:window-destroy widget)))
    (gtk:box-append (gtk:dialog-content-area dialog)
                    (make-instance 'gtk:label
                                   :label msg
                                   :margin-top 12
                                   :margin-bottom 12
                                   :margin-start 12
                                   :margin-end 12))
    (gtk:window-present dialog)))
    @end{pre}
  @end{dictionary}
  @begin[GtkDialog as GtkBuildable]{dictionary}
    The @class{gtk:dialog} implementation of the @class{gtk:buildable} interface
    exposes the content area as an internal children with the name
    @code{content_area}.

    The @class{gtk:dialog} implementation supports a custom
    @code{<action-widgets>} element, which can contain multiple
    @code{<action-widget>} elements. The @code{\"response\"} attribute specifies
    a numeric response, and the content of the element is the ID of the widget,
    which should be a child of the action area of the dialog. To mark a
    response as default, set the @code{\"default\"} attribute of the
    @code{<action-widget>} element to true.

    The @class{gtk:dialog} implementation supports adding action widgets by
    specifying @code{\"action\"} as the @code{\"type\"} attribute of a
    @code{<child>} element. The widget will be added either to the action area
    or the headerbar of the dialog, depending on the
    @slot[gtk:dialog]{use-header-bar} property. The response ID has to be
    associated with the action widget using the @code{<action-widgets>} element.

    @b{Example:} A @class{gtk:dialog} UI definition fragment.
    @begin{pre}
<object class=\"GtkDialog\" id=\"dialog1\">
  <child type=\"action\">
    <object class=\"GtkButton\" id=\"button_cancel\"/>
  </child>
  <child type=\"action\">
    <object class=\"GtkButton\" id=\"button_ok\">
      <property name=\"can-default\">True</property>
    </object>
  </child>
  <action-widgets>
    <action-widget response=\"cancel\">button_cancel</action-widget>
    <action-widget response=\"ok\" default=\"true\">button_ok</action-widget>
  </action-widgets>
</object>
    @end{pre}
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @class{gtk:dialog} implementation uses the @code{:dialog} role of the
    @symbol{gtk:accessible-role} enumeration.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:dialog} implementation is deprecated since 4.10. Use the
    @class{gtk:window} widget instead.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"close\" signal}
      @begin{pre}
lambda (dialog)    :action
      @end{pre}
      @begin[code]{table}
        @entry[dialog]{The @class{gtk:dialog} widget on which the signal is
          emitted.}
      @end{table}
      A keybinding signal which gets emitted when the user uses a keybinding to
      close the dialog. The default binding for this signal is the @kbd{Escape}
      key.
    @subheading{The \"response\" signal}
      @begin{pre}
lambda (dialog response)    :run-last
      @end{pre}
      @begin[code]{table}
        @entry[dialog]{The @class{gtk:dialog} widget on which the signal is
          emitted.}
        @entry[response]{The integer with the response ID.}
      @end{table}
      Emitted when an action widget is clicked. The signal is also emitted when
      the @fun{gtk:dialog-response} function is called. On a delete event, the
      response ID is the @code{:delete-event} value of the
      @symbol{gtk:response-type} enumeration. Otherwise, it depends on which
      action widget was clicked.
  @end{dictionary}
  @see-constructor{gtk:dialog-new}
  @see-constructor{gtk:dialog-new-with-buttons}
  @see-slot{gtk:dialog-use-header-bar}
  @see-class{gtk:message-dialog}
  @see-class{gtk:window}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:dialog-use-header-bar ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-header-bar" 'dialog) t)
 "The @code{use-header-bar} property of type @code{:int}
  (Read / Write / Construct only) @br{}
  @em{True} if the dialog uses a header bar for action buttons instead of the
  action area. For technical reasons, this property is declared as an integer
  property, use the value 1 for @em{true} or -1 for @em{false}. @br{}
  Allowed values: [-1, 1] @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'dialog-use-header-bar)
      "Accessor"
      (documentation 'dialog-use-header-bar 'function)
 "@version{2025-2-26}
  @syntax{(gtk:dialog-use-header-bar object) => setting}
  @syntax{(setf (gtk:dialog-use-header-bar object) setting)}
  @argument[object]{a @class{gtk:dialog} widget}
  @argument[setting]{@em{true} if the dialog uses a header bar}
  @begin{short}
    Accessor of the @slot[gtk:dialog]{use-header-bar} slot of the
    @class{gtk:dialog} class.
  @end{short}
  @em{True} if the dialog uses a header bar for action buttons instead of the
  action area. For technical reasons, this property is declared as an integer
  property, use the value 1 for @em{true} or -1 for @em{false}.

  Builtin @class{gtk:dialog} subclasses such as the
  @class{gtk:color-chooser-dialog} widget set the
  @slot[gtk:dialog]{use-header-bar} property according to platform conventions,
  using the @slot[gtk:settings]{gtk-dialogs-use-header} setting.

  Here is how you can achieve the same:
  @begin{pre}
(let* ((settings (gtk:settings-default))
       (setting (g:object-property settings \"gtk-dialogs-use-header\"))
       (dialog (make-instance 'gtk:dialog
                              :use-header-bar setting)))
  ... )
  @end{pre}
  @begin[Warning]{dictionary}
    The @class{gtk:dialog} implementation is deprecated since 4.10. Use the
    @class{gtk:window} widget instead.
  @end{dictionary}
  @see-class{gtk:dialog}
  @see-class{gtk:window}
  @see-class{gtk:header-bar}
  @see-function{gtk:settings-gtk-dialogs-use-header}")

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_new
;;; ----------------------------------------------------------------------------

(declaim (inline dialog-new))

(defun dialog-new ()
 #+liber-documentation
 "@version{2025-2-26}
  @return{The new @class{gtk:dialog} widget.}
  @short{Creates a new dialog.}
  Widgets should not be packed into the dialog directly, but into the content
  area, which can be accessed with the @fun{gtk:dialog-content-area} function.
  @begin[Warning]{dictionary}
    The @class{gtk:dialog} implementation is deprecated since 4.10. Use the
    @class{gtk:window} widget instead.
  @end{dictionary}
  @see-class{gtk:dialog}
  @see-class{gtk:window}
  @see-function{gtk:dialog-content-area}"
  (make-instance 'dialog))

(export 'dialog-new)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_new_with_buttons
;;; ----------------------------------------------------------------------------

(defun dialog-new-with-buttons (title parent flags &rest buttons)
 #+liber-documentation
 "@version{2025-2-26}
  @argument[title]{a string for the title of the dialog, or @code{nil}}
  @argument[parent]{a @class{gtk:window} transient parent for the dialog,
    or @code{nil}}
  @argument[flags]{a @symbol{gtk:dialog-flags} value for the flags of the
    dialog}
  @argument[buttons]{pairs with a button text and the response ID for the
    button, which is a positive integer or a value of the
    @symbol{gtk:response-type} enumeration}
  @return{The new @class{gtk:dialog} widget.}
  @begin{short}
    Creates a new dialog with title @arg{title}, or @code{nil} for the default
    title, and transient parent @arg{parent}, or @code{nil} for none.
  @end{short}
  The @arg{flags} argument can be used to make the dialog modal with the
  @code{:modal} flag of the @symbol{gtk:dialog-flags} flags and/or to have it
  destroyed along with its transient parent with the @code{:destroy-with-parent}
  flag.

  After the @arg{flags} argument, button text/response ID pairs should be
  listed. Button text can be arbitrary text. A response ID can be any positive
  number, or one of the values in the @symbol{gtk:response-type} enumeration.
  If the user clicks one of these dialog buttons, the @class{gtk:dialog} widget
  will emit the @code{\"response\"} signal with the corresponding response ID.
  If a @class{gtk:dialog} widget receives the @code{\"delete-event\"} signal, it
  will emit the @code{\"response\"} signal with a @code{:delete-event} response
  ID. However, destroying a dialog does not emit the @code{\"response\"}
  signal. So be careful relying on the @code{\"response\"} signal when using the
  @code{:destroy-with-parent} flag. Buttons are from left to right, so the first
  button in the list will be the leftmost button in the dialog.
  @begin[Examples]{dictionary}
    @begin{pre}
(let ((dialog (gtk:dialog-new-with-buttons \"My dialog\"
                                           main-app-window
                                           '(:modal :destroy-with-parent)
                                           \"_OK\"
                                           :accept
                                           \"_Cancel\"
                                           :reject)))
  ... )
    @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:dialog} implementation is deprecated since 4.10. Use the
    @class{gtk:window} widget instead.
  @end{dictionary}
  @see-class{gtk:dialog}
  @see-class{gtk:window}
  @see-symbol{gtk:dialog-flags}
  @see-symbol{gtk:response-type}"
  (let ((dialog (make-instance 'dialog))
        (flags (glib-sys:mklist flags)))
    (when title
      (setf (window-title dialog) title))
    (when parent
      (setf (window-transient-for dialog) parent))
    (when (member :modal flags)
      (setf (window-modal dialog) t))
    (when (member :destroy-with-parent flags)
      (setf (window-destroy-with-parent dialog) t))
    (when buttons
     (apply #'dialog-add-buttons dialog buttons))
    dialog))

(export 'dialog-new-with-buttons)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_response
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_dialog_response" dialog-response) :void
 #+liber-documentation
 "@version{#2025-2-26}
  @argument[dialog]{a @class{gtk:dialog} widget}
  @argument[response]{a response ID, which is a positive integer or a value of
    the @symbol{gtk:response-type} enumeration}
  @begin{short}
    Emits the @code{\"response\"} signal with the given response ID.
  @end{short}
  Used to indicate that the user has responded to the dialog in some way.
  @begin[Warning]{dictionary}
    The @class{gtk:dialog} implementation is deprecated since 4.10. Use the
    @class{gtk:window} widget instead.
  @end{dictionary}
  @see-class{gtk:dialog}
  @see-class{gtk:window}
  @see-symbol{gtk:response-type}"
  (dialog (g:object dialog))
  (response response-type))

(export 'dialog-response)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_add_button
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_dialog_add_button" dialog-add-button) (g:object widget)
 #+liber-documentation
 "@version{#2025-2-26}
  @argument[dialog]{a @class{gtk:dialog} widget}
  @argument[text]{a string for the text of the button}
  @argument[response]{response ID for the button, which is a positive integer
    or a value of the @symbol{gtk:response-type} enumeration}
  @return{The @class{gtk:button} widget that was added.}
  @begin{short}
    Adds a button with the given text and sets things up so that clicking the
    button will emit the @code{\"response\"} signal with the given
    @arg{response} value.
  @end{short}
  The button is appended to the end of the action area of the dialog.
  @begin[Warning]{dictionary}
    The @class{gtk:dialog} implementation is deprecated since 4.10. Use the
    @class{gtk:window} widget instead.
  @end{dictionary}
  @see-class{gtk:dialog}
  @see-class{gtk:window}
  @see-class{gtk:button}
  @see-symbol{gtk:response-type}
  @see-function{gtk:dialog-add-buttons}
  @see-function{gtk:dialog-add-action-widget}"
  (dialog (g:object dialog))
  (text :string)
  (response response-type))

(export 'dialog-add-button)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_add_buttons
;;; ----------------------------------------------------------------------------

(defun dialog-add-buttons (dialog &rest buttons)
 #+liber-documentation
 "@version{#2025-2-26}
  @argument[dialog]{a @class{gtk:dialog} widget}
  @argument[buttons]{pairs with a button text and the response ID, which is a
    positive integer or a value of the @symbol{gtk:response-type} enumeration}
  @begin{short}
    Adds more buttons, same as calling the @fun{gtk:dialog-add-button} function
    repeatedly.
  @end{short}
  Each button must have both text and response ID.
  @begin[Notes]{dictionary}
    The Lisp implementation does not call the C function, but the
    @fun{gtk:dialog-add-button} function is called in a loop to add the buttons.
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:dialog} implementation is deprecated since 4.10. Use the
    @class{gtk:window} widget instead.
  @end{dictionary}
  @see-class{gtk:dialog}
  @see-class{gtk:window}
  @see-symbol{gtk:response-type}
  @see-function{gtk:dialog-add-button}
  @see-function{gtk:dialog-add-action-widget}"
  (iter (for (text id) on buttons by #'cddr)
        (dialog-add-button dialog text id)))

(export 'dialog-add-buttons)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_add_action_widget
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_dialog_add_action_widget" dialog-add-action-widget) :void
 #+liber-documentation
 "@version{#2025-2-26}
  @argument[dialog]{a @class{gtk:dialog} widget}
  @argument[child]{an activatable @class{gtk:widget} object}
  @argument[response]{response ID for @arg{child}, which is a positive
    integer or a value of the @symbol{gtk:response-type} enumeration}
  @begin{short}
    Adds an activatable child widget to the action area of the dialog,
    connecting a signal handler that will emit the @code{\"response\"} signal
    on the dialog when the child widget is activated.
  @end{short}
  The child widget is appended to the end of the action area of the dialog. If
  you want to add a non-activatable widget, simply pack it into the action area
  of the dialog.
  @begin[Warning]{dictionary}
    The @class{gtk:dialog} implementation is deprecated since 4.10. Use the
    @class{gtk:window} widget instead.
  @end{dictionary}
  @see-class{gtk:dialog}
  @see-class{gtk:window}
  @see-class{gtk:widget}
  @see-symbol{gtk:response-type}
  @see-function{gtk:dialog-add-button}
  @see-function{gtk:dialog-add-buttons}"
  (dialog (g:object dialog))
  (child (g:object widget))
  (response response-type))

(export 'dialog-add-action-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_set_default_response
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_dialog_set_default_response" dialog-set-default-response)
    :void
 #+liber-documentation
 "@version{#2025-2-26}
  @argument[dialog]{a @class{gtk:dialog} widget}
  @argument[response]{a response ID, which is a positive integer or a value
    of the @symbol{gtk:response-type} enumeration}
  @begin{short}
    Sets the last widget in the action area of the dialog with the given
    @arg{response} value as the default widget for the dialog.
  @end{short}
  Pressing the @kbd{Enter} key normally activates the default widget.
  @begin[Warning]{dictionary}
    The @class{gtk:dialog} implementation is deprecated since 4.10. Use the
    @class{gtk:window} widget instead.
  @end{dictionary}
  @see-class{gtk:dialog}
  @see-class{gtk:window}
  @see-symbol{gtk:response-type}"
  (dialog (g:object dialog))
  (response response-type))

(export 'dialog-set-default-response)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_set_response_sensitive
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_dialog_set_response_sensitive"
               dialog-set-response-sensitive) :void
 #+liber-documentation
 "@version{#2025-2-26}
  @argument[dialog]{a @class{gtk:dialog} widget}
  @argument[response]{a response ID, which is a positive integer or a value
    of the @symbol{gtk:response-type} enumeration}
  @argument[setting]{@em{true} for sensitive}
  @begin{short}
    Calls the @fun{gtk:widget-sensitive} function for each widget in the
    action area of the dialog with the given @arg{response} value.
  @end{short}
  This is a convenient way to sensitize/desensitize dialog buttons.
  @begin[Warning]{dictionary}
    The @class{gtk:dialog} implementation is deprecated since 4.10. Use the
    @class{gtk:window} widget instead.
  @end{dictionary}
  @see-class{gtk:dialog}
  @see-class{gtk:window}
  @see-symbol{gtk:response-type}
  @see-function{gtk:widget-sensitive}"
  (dialog (g:object dialog))
  (response response-type)
  (setting :boolean))

(export 'dialog-set-response-sensitive)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_get_response_for_widget
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_dialog_get_response_for_widget" dialog-response-for-widget)
    :int
 #+liber-documentation
 "@version{#2025-2-26}
  @argument[dialog]{a @class{gtk:dialog} widget}
  @argument[widget]{a @class{gtk:widget} object in the action area of
    @arg{dialog}}
  @return{The response ID of @arg{widget}, which is a positive integer or a
    value of the @symbol{gtk:response-type} enumeration, the value is
    @code{:none} if @arg{widget} does not have a response ID set.}
  @begin{short}
    Gets the response ID of the widget in the action area of the dialog.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:dialog} implementation is deprecated since 4.10. Use the
    @class{gtk:window} widget instead.
  @end{dictionary}
  @see-class{gtk:dialog}
  @see-class{gtk:window}
  @see-class{gtk:widget}
  @see-symbol{gtk:response-type}
  @see-function{gtk:dialog-widget-for-response}"
  (dialog (g:object dialog))
  (widget (g:object widget)))

(export 'dialog-response-for-widget)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_get_widget_for_response
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_dialog_get_widget_for_response" dialog-widget-for-response)
    (g:object widget)
 #+liber-documentation
 "@version{#2025-2-26}
  @argument[dialog]{a @class{gtk:dialog} widget}
  @argument[response]{a response ID, which is a positive integer or a value
    of the @symbol{gtk:response-type} enumeration}
  @return{The @class{gtk:widget} button that uses the given @arg{response}
    value, or @code{nil}.}
  @begin{short}
    Gets the button that uses the given response ID in the action area of the
    dialog.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:dialog} implementation is deprecated since 4.10. Use the
    @class{gtk:window} widget instead.
  @end{dictionary}
  @see-class{gtk:dialog}
  @see-class{gtk:window}
  @see-class{gtk:widget}
  @see-symbol{gtk:response-type}
  @see-function{gtk:dialog-response-for-widget}"
  (dialog (g:object dialog))
  (response response-type))

(export 'dialog-widget-for-response)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_get_content_area
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_dialog_get_content_area" dialog-content-area)
    (g:object widget)
 #+liber-documentation
 "@version{#2025-2-26}
  @argument[dialog]{a @class{gtk:dialog} widget}
  @begin{return}
    The vertical @class{gtk:box} widget with the content area.
  @end{return}
  @short{Returns the content area of the dialog.}
  @begin[Warning]{dictionary}
    The @class{gtk:dialog} implementation is deprecated since 4.10. Use the
    @class{gtk:window} widget instead.
  @end{dictionary}
  @see-class{gtk:dialog}
  @see-class{gtk:window}
  @see-class{gtk:box}"
  (dialog (g:object dialog)))

(export 'dialog-content-area)

;;; ----------------------------------------------------------------------------
;;; gtk_dialog_get_header_bar
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_dialog_get_header_bar" dialog-header-bar) (g:object widget)
 #+liber-documentation
 "@version{#2025-2-26}
  @argument[dialog]{a @class{gtk:dialog} widget}
  @return{The @class{gtk:header-bar} widget.}
  @begin{short}
    Returns the header bar of the dialog.
  @end{short}
  Note that the header bar is only used by the dialog if the
  @slot[gtk:dialog]{use-header-bar} property is @em{true}.
  @begin[Warning]{dictionary}
    The @class{gtk:dialog} implementation is deprecated since 4.10. Use the
    @class{gtk:window} widget instead.
  @end{dictionary}
  @see-class{gtk:dialog}
  @see-class{gtk:window}
  @see-class{gtk:header-bar}
  @see-function{gtk:dialog-use-header-bar}"
  (dialog (g:object dialog)))

(export 'dialog-header-bar)

;;; --- End of file gtk4.dialog.lisp -------------------------------------------
