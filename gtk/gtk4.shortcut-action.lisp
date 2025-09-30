;;; ----------------------------------------------------------------------------
;;; gtk4.shortcut-action.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2025 Dieter Kaiser
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
;;; GtkShortcutAction
;;;
;;;     Tracking if shortcuts should be activated
;;;
;;; Types and Values
;;;
;;;     GtkShortcutActionFlags
;;;     GtkShortcutAction
;;;     GtkNothingAction
;;;     GtkCallbackAction
;;;     GtkMnemonicAction
;;;     GtkActivateAction
;;;     GtkSignalAction
;;;     GtkNamedAction
;;;
;;; Functions
;;;
;;;     GtkShortcutFunc
;;;
;;;     gtk_shortcut_action_to_string
;;;     gtk_shortcut_action_print
;;;     gtk_shortcut_action_parse_string
;;;     gtk_shortcut_action_activate
;;;
;;;     gtk_nothing_action_get
;;;
;;;     gtk_callback_action_new
;;;
;;;     gtk_mnemonic_action_get
;;;
;;;     gtk_activate_action_get
;;;
;;;     gtk_signal_action_new
;;;     gtk_signal_action_get_signal_name                  Accessor
;;;
;;;     gtk_named_action_new
;;;     gtk_named_action_get_action_name                   Accessor
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkShortcutAction
;;;         ├── GtkSignalAction
;;;         ├── GtkNothingAction
;;;         ├── GtkNamedAction
;;;         ╰── GtkCallbackAction
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkShortcutActionFlags
;;; ----------------------------------------------------------------------------

(gobject:define-gflags "GtkShortcutActionFlags" shortcut-action-flags
  (:export t
   :type-initializer "gtk_shortcut_action_flags_get_type")
  (:exclusive #.(ash 1 0)))

#+liber-documentation
(setf (liber:alias-for-symbol 'shortcut-action-flags)
      "GFlags"
      (liber:symbol-documentation 'shortcut-action-flags)
 "@version{2025-07-22}
  @begin{declaration}
(gobject:define-gflags \"GtkShortcutActionFlags\" shortcut-action-flags
  (:export t
   :type-initializer \"gtk_shortcut_action_flags_get_type\")
  (:exclusive #.(ash 1 0)))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:exclusive]{The action is the only action that can be activated.
        If this flag is not set, a future activation may select a different
        action.}
    @end{simple-table}
  @end{values}
  @begin{short}
    List of flags that can be passed to action activation.
  @end{short}
  More flags may be added in the future.
  @see-class{gtk:shortcut-action}")

;;; ----------------------------------------------------------------------------
;;; GtkShortcutAction
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkShortcutAction" shortcut-action
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_shortcut_action_get_type")
  nil)

#+liber-documentation
(setf (documentation 'shortcut-action 'type)
 "@version{2025-07-12}
  @begin{short}
    The @class{gtk:shortcut-action} object is the object used to describe what a
    @class{gtk:shortcut} object should do when triggered.
  @end{short}
  To activate a @class{gtk:shortcut-action} object manually, the
  @fun{gtk:shortcut-action-activate} function can be called.

  The @class{gtk:shortcut-action} implementation contain functions that allow
  easy presentation to end users as well as being printed for debugging.

  All @class{gtk:shortcut-action} objects are immutable, you can only specify
  their properties during construction. If you want to change an action, you
  have to replace it with a new one. If you need to pass arguments to an action,
  these are specified by the higher-level @class{gtk:shortcut} object.

  GTK provides various actions:
  @begin[code]{simple-table}
    @entry[GtkMnemonicAction]{A shortcut action that calls the
      @fun{gtk:widget-mnemonic-activate} function.}
    @entry[GtkCallbackAction]{A shortcut action that invokes a given callback.}
    @entry[GtkSignalAction]{A shortcut action that emits a given signal.}
    @entry[GtkActivateAction]{A shortcut action that calls the
      @fun{gtk:widget-activate} function.}
    @entry[GtkNamedAction]{A shortcut action that calls the
      @fun{gtk:widget-activate-action} function.}
    @entry[GtkNothingAction]{A shortcut action that does nothing.}
  @end{simple-table}
  @see-constructor{gtk:shortcut-action-parse-string}
  @see-class{gtk:shortcut}")

;;; ----------------------------------------------------------------------------
;;; gtk_shortcut_action_parse_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_shortcut_action_parse_string" shortcut-action-parse-string)
    (g:object shortcut-action :return)
 #+liber-documentation
 "@version{2024-11-01}
  @argument[str]{a string to parse}
  @return{The new @class{gtk:shortcut-action} object, or @code{nil} on error.}
  @begin{short}
    Tries to parse the given string into an action.
  @end{short}
  On success, the parsed action is returned. When parsing failed, @code{nil}
  is returned. The accepted strings are:
  @begin[code]{table}
    @entry[nothing]{for a @class{gtk:nothing-action} object}
    @entry[activate]{for a @class{gtk:activate-action} object}
    @entry[mnemonic-activate]{for a @class{gtk:mnenomic-action} object}
    @entry[action(name)]{for a @class{gtk:named-action} object for the action
      named @code{name}}
    @entry[signal(name)]{for a @class{gtk:signal-action} object for the signal
      @code{name}}
  @end{table}
  @see-class{gtk:shortcut-action}
  @see-class{gtk:nothing-action}
  @see-class{gtk:activate-action}
  @see-class{gtk:mnemonic-action}
  @see-class{gtk:named-action}
  @see-class{gtk:signal-action}"
  (str :string))

(export 'shortcut-action-parse-string)

;;; ----------------------------------------------------------------------------
;;; gtk_shortcut_action_to_string
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_shortcut_action_to_string" shortcut-action-to-string)
    :string
 #+liber-documentation
 "@version{2024-11-01}
  @argument[shortcut]{a @class{gtk:shortcut-action} object}
  @return{The human-readable string.}
  @begin{short}
    Prints the given action into a human-readable string.
  @end{short}
  @see-class{gtk:shortcut-action}"
  (action (g:object shortcut-action)))

(export 'shortcut-action-to-string)

;;; ----------------------------------------------------------------------------
;;; gtk_shortcut_action_print                               not needed
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_shortcut_action_activate
;;; ----------------------------------------------------------------------------

;; TODO: This is not a complete implementation. We have to pass in an array of
;; G:VARIANT values

(cffi:defcfun ("gtk_shortcut_action_activated" shortcut-action-activate)
    :boolean
 #+liber-documentation
 "@version{#2025-07-27}
  @argument[shortcut]{a @class{gtk:shortcut-action} object}
  @argument[flags]{a @sym{gtk:shortcut-action-flags} value}
  @argument[widget]{a @class{gtk:widget} object}
  @argument[args]{an array of @sym{g:variant} parameters to pass}
  @return{@em{True} if the action was activaed successfully.}
  @begin{short}
    Activates the action on the widget with the given @arg{args}.
  @end{short}
  Note that some actions ignore the passed in @arg{flags}, @arg{widget} or
  @arg{args}.

  Activation of an action can fail for various reasons. If the action is not
  supported by the widget, if the @arg{args} do not match the action or if the
  activation otherwise had no effect, @em{false} will be returned.
  @see-class{gtk:shortcut-action}
  @see-class{gtk:widget}
  @see-symbol{g:variant}
  @see-symbol{gtk:shortcut-action-flags}"
  (action (g:object shortcut-action))
  (flags shortcut-action-flags)
  (widget (g:object widget))
  (args (:pointer (:struct g:variant))))

(export 'shortcut-action-activate)

;;; ----------------------------------------------------------------------------
;;; GtkNothingAction
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkNothingAction" nothing-action
  (:superclass shortcut-action
   :export t
   :interfaces nil
   :type-initializer "gtk_nothing_action_get_type")
  nil)

#+liber-documentation
(setf (documentation 'nothing-action 'type)
 "@version{2024-11-01}
  @begin{short}
    A @class{gtk:shortcut-action} object that does nothing.
  @end{short}
  @see-class{gtk:shortcut-action}")

;;; ----------------------------------------------------------------------------
;;; gtk_nothing_action_get
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_nothing_action_get" nothing-action-get)
    (g:object shortcut-action)
 #+liber-documentation
 "@version{2024-11-01}
  @return{The @class{gtk:nothing-action} object.}
  @begin{short}
    Gets the nothing action.
  @end{short}
  This is an action that does nothing and where activating it always fails.
  @see-class{gtk:nothing-action}")

(export 'nothing-action-get)

;;; ----------------------------------------------------------------------------
;;; GtkCallbackAction
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkCallbackAction" callback-action
  (:superclass shortcut-action
   :export t
   :interfaces nil
   :type-initializer "gtk_callback_action_get_type")
  nil)

#+liber-documentation
(setf (documentation 'callback-action 'type)
 "@version{2024-11-01}
  @begin{short}
    A @class{gtk:shortcut-action} object that invokes a callback.
  @end{short}
  @see-constructor{gtk:callback-action-new}
  @see-class{gtk:shortcut-action}")

;;; ----------------------------------------------------------------------------
;;; GtkShortcutFunc
;;; ----------------------------------------------------------------------------

(cffi:defcallback shortcut-func :boolean
    ((widget (g:object widget))
     (args (:pointer (:struct g:variant)))
     (data :pointer))
  (let ((func (glib:get-stable-pointer-value data)))
    (funcall func widget args)))

#+liber-documentation
(setf (liber:alias-for-symbol 'shortcut-func)
      "Callback"
      (liber:symbol-documentation 'shortcut-func)
 "@version{2025-07-27}
  @syntax{lambda (widget args)}
  @argument[widget]{a @class{gtk:widget} object passed to the activation}
  @argument[args]{an array of @sym{g:variant} parameters passed to the
    activation}
  @begin{short}
    Prototype for shortcuts based on user callbacks.
  @end{short}
  @see-class{gtk:callback-action}")

(export 'shortcut-func)

;;; ----------------------------------------------------------------------------
;;; gtk_callback_action_new
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_callback_action_new" %callback-action-new)
    (g:object shortcut-action)
  (callback :pointer)
  (data :pointer)
  (destroy :pointer))

(defun callback-action-new (func)
 #+liber-documentation
 "@version{2025-07-27}
  @argument[func]{a @sym{gtk:shortcut-func} callback function to call}
  @return{The new @class{gtk:callback-action} object.}
  @short{Create a custom action that calls the given callback when activated.}
  @see-class{gtk:callback-action}
  @see-symbol{gtk:shortcut-func}"
  (glib:with-stable-pointer (ptr func)
    (%callback-action-new (cffi:callback shortcut-func)
                           ptr
                           (cffi:callback glib:stable-pointer-destroy-notify))))

(export 'callback-action-new)

;;; ----------------------------------------------------------------------------
;;; GtkMnemonicAction
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkMnemonicAction" mnemonic-action
  (:superclass shortcut-action
   :export t
   :interfaces nil
   :type-initializer "gtk_mnemonic_action_get_type")
  nil)

#+liber-documentation
(setf (documentation 'mnemonic-action 'type)
 "@version{2024-11-01}
  @begin{short}
    A @class{gtk:shortcut-action} that calls the
    @fun{gtk:widget-mnemonic-activate} function.
  @end{short}
  @see-class{gtk:shortcut-action}
  @see-function{gtk:widget-mnemonic-activate}")

;;; ----------------------------------------------------------------------------
;;; gtk_mnemonic_action_get
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_mnemonic_action_get" mnemonic-action-get)
    (g:object shortcut-action)
 #+liber-documentation
 "@version{2024-11-01}
  @return{The @class{gtk:shortcut-action} mnemonic action.}
  @begin{short}
    Gets the mnemonic action.
  @end{short}
  This is an action that calls the @fun{gtk:widget-mnemonic-activate} function
  on the given widget upon activation.
  @see-class{gtk:shortcut-action}
  @see-function{gtk:widget-mnemonic-activate}")

(export 'mnemonic-action-get)

;;; ----------------------------------------------------------------------------
;;; GtkActivateAction
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkActivateAction" activate-action
  (:superclass shortcut-action
   :export t
   :interfaces nil
   :type-initializer "gtk_activate_action_get_type")
  nil)

#+liber-documentation
(setf (documentation 'activate-action 'type)
 "@version{2024-11-01}
  @begin{short}
    A @class{gtk:shortcut-action} that calls the @fun{gtk:widget-activate}
    function.
  @end{short}
  @see-class{gtk:shortcut-action}
  @see-function{gtk:widget-activate}")

;;; ----------------------------------------------------------------------------
;;; gtk_activate_action_get
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_activate_action_get" activate-action-get)
    (g:object shortcut-action)
 #+liber-documentation
 "@version{2024-11-01}
  @return{The @class{gtk:shortcut-action} activate action.}
  @begin{short}
    Gets the activate action.
  @end{short}
  This is an action that calls the @fun{gtk:widget-activate} function on the
  given widget upon activation.
  @see-class{gtk:shortcut-action}
  @see-function{gtk:widget-activate}")

(export 'activate-action-get)

;;; ----------------------------------------------------------------------------
;;; GtkSignalAction
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkSignalAction" signal-action
  (:superclass shortcut-action
   :export t
   :interfaces nil
   :type-initializer "gtk_signal_action_get_type")
  ((signal-name
    signal-action-signal-name
    "signal-name" "gchararray" t nil)))

#+liber-documentation
(setf (documentation 'signal-action 'type)
 "@version{2025-07-26}
  @begin{short}
    A @class{gtk:shortcut-action} that emits a signal.
  @end{short}
  Signals that are used in this way are referred to as keybinding signals,
  and they are expected to be defined with the @val[g:signal-flags]{:action}
  value of the @sym{g:signal-flags} flags.
  @see-slot{gtk:signal-action-signal-name}
  @see-constructor{gtk:signal-action-new}
  @see-class{gtk:shortcut-action}
  @see-symbol{g:signal-flags}")

;;; --- gtk:signal-action-signal-name ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "signal-name" 'signal-action) t)
 "The @code{signal-name} property of type @code{:string}
  (Read / Write / Construct only) @br{}
  The name of the signal to emit.")

#+liber-documentation
(setf (liber:alias-for-function 'signal-action-signal-name)
      "Accessor"
      (documentation 'signal-action-signal-name 'function)
 "@version{2025-09-28}
  @syntax{(gtk:signal-action-signal-name object) => name}
  @argument[object]{a @class{gtk:signal-action} object}
  @argument[name]{a string for the name of the signal to emit}
  @begin{short}
    The accessor for the @slot[gtk:signal-action]{signal-name} slot of the
    @class{gtk:signal-action} class returns the name of the signal that will be
    emitted.
  @end{short}
  @see-class{gtk:signal-action}")

;;; ----------------------------------------------------------------------------
;;; gtk_signal_action_new
;;; ----------------------------------------------------------------------------

(declaim (inline signal-action-new))

(defun signal-action-new (name)
 #+liber-documentation
 "@version{2025-07-27}
  @argument[name]{a string for the name of the signal to emit}
  @return{The @class{gtk:signal-action} object.}
  @begin{short}
    Creates an action that when activated, emits the given action signal on the
    provided widget unpacking the given @arg{args} into arguments passed to the
    signal.
  @end{short}
  @see-class{gtk:signal-action}"
  (make-instance 'signal-action
                 :signal-name name))

(export 'signal-action-new)

;;; ----------------------------------------------------------------------------
;;; GtkNamedAction
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkNamedAction" named-action
  (:superclass shortcut-action
   :export t
   :interfaces nil
   :type-initializer "gtk_named_action_get_type")
  ((action-name
    named-action-action-name
    "action-name" "gchararray" t nil)))

#+liber-documentation
(setf (documentation 'named-action 'type)
 "@version{2024-11-01}
  @begin{short}
    A @class{gtk:shortcut-action} that activates an action by name.
  @end{short}
  @see-slot{gtk:named-action-action-name}
  @see-constructor{gtk:named-action-new}
  @see-class{gtk:shortcut-action}")

;;; --- gtk:named-action-action-name -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "action-name"
                                               'named-action) t)
 "The @code{action-name} property of type @code{:string}
  (Read / Write / Construct only) @br{}
  The name of the action to activate.")

#+liber-documentation
(setf (liber:alias-for-function 'named-action-action-name)
      "Accessor"
      (documentation 'named-action-action-name 'function)
 "@version{2025-09-28}
  @syntax{(gtk:named-action-action-name object) => name}
  @argument[object]{a @class{gtk:named-action} object}
  @argument[name]{a string for the name of the actopm that will be activated}
  @begin{short}
    The accessor for the @slot[gtk:named-action]{action-name} slot of the
    @class{gtk:named-action} class returns the name of the action that will be
    activated.
  @end{short}
  @see-class{gtk:named-action}")

;;; ----------------------------------------------------------------------------
;;; gtk_named_action_new
;;; ----------------------------------------------------------------------------

(declaim (inline named-action-new))

(defun named-action-new (name)
 #+liber-documentation
 "@version{2025-07-27}
  @argument[name]{a string for the detailed name of the action}
  @return{The new @class{gtk:named-action} object.}
  @begin{short}
    Creates an action that when activated, activates the action given by the
    detailed name on the widget passing the given arguments to it.
  @end{short}
  See the @fun{gtk:widget-insert-action-group} function for how to add actions
  to widgets.
  @see-class{gtk:named-action}
  @see-function{gtk:widget-insert-action-group}"
  (make-instance 'named-action
                 :action-name name))

(export 'named-action-new)

;;; --- End of file gtk4.shortcut-action.lisp ----------------------------------
