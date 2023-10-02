;;; ----------------------------------------------------------------------------
;;; gtk4.actionable.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2023 Dieter Kaiser
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
;;; GtkActionable
;;;
;;;     An interface for widgets that can be associated with actions
;;;
;;; Types and Values
;;;
;;;     GtkActionable
;;;
;;; Accessors
;;;
;;;     gtk_actionable_get_action_name
;;;     gtk_actionable_set_action_name
;;;     gtk_actionable_get_action_target_value
;;;     gtk_actionable_set_action_target_value
;;;
;;; Functions
;;;
;;;     gtk_actionable_set_action_target
;;;     gtk_actionable_set_detailed_action_name
;;;
;;; Properties
;;;
;;;     action-name
;;;     action-target
;;;
;;; Hierarchy
;;;
;;;    GInterface
;;;    ╰── GtkActionable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkActionable
;;; ----------------------------------------------------------------------------

(gobject:define-g-interface "GtkActionable" actionable
  (:export t
   :type-initializer "gtk_actionable_get_type")
  ((action-name
    actionable-action-name
    "action-name" "gchararray" t t)
   (action-target
    actionable-action-target
    "action-target" "GVariant" t t)))

#+liber-documentation
(setf (liber:alias-for-class 'actionable)
      "Interface"
      (documentation 'actionable 'type)
 "@version{2023-8-4}
  @begin{short}
    The @class{gtk:actionable} interface provides a convenient way of
    associating widgets with actions on a @class{gtk:application-window} widget
    or @class{gtk:application} instance.
  @end{short}
  It primarily consists of two properties: @code{action-name} and
  @code{action-target}. There are also some convenience APIs for setting
  these properties.

  The action will be looked up in action groups that are found among the widgets
  ancestors. Most commonly, these will be the actions with the \"win.\" or
  \"app.\" prefix that are associated with the @class{gtk:application-window}
  widget or the @class{gtk:application} instance, but other action groups that
  are added with the @fun{gtk:widget-insert-action-group} function will be
  consulted as well.
  @see-slot{gtk:actionable-action-name}
  @see-slot{gtk:actionable-action-target}
  @see-class{gtk:application}
  @see-class{gtk:application-window}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- actionable-action-name -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "action-name" 'actionable) t)
 "The @code{action-name} property of type @code{:string} (Read / Write) @br{}
  The name of the associated action, like \"app.quit\". @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'actionable-action-name)
      "Accessor"
      (documentation 'actionable-action-name 'function)
 "@version{2023-8-4}
  @syntax[]{(gtk:actionable-action-name object) => name}
  @syntax[]{(setf (gtk:actionable-action-name object) name)}
  @argument[object]{a @class{gtk:actionable} widget}
  @argument[name]{a string with the action name, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gtk:actionable]{action-name} slot of the
    @class{gtk:actionable} inferface.
  @end{short}
  The @fun{gtk:actionable-action-name} function gets the action name for
  @arg{object}, or @code{nil} if none is set. The
  @setf{gtk:actionable-action-name} function specifies the name of the action
  with which this widget should be associated. If the @arg{name} argument is
  @code{nil} then the widget will be unassociated from any previous action.
  Usually this function is used when the widget is located, or will be located,
  within the hierarchy of a @class{gtk:application-window} widget.

  Names are of the form \"win.save\" or \"app.quit\" for actions on the
  containing @class{gtk:application-window} widget or its associated
  @class{gtk:application} instance, respectively. This is the same form used
  for actions in the @class{g:menu} object associated with the window.
  @begin[Example]{dictionary}
    @begin{pre}
(let ((button (make-instance 'gtk:button)))
  (setf (gtk:actionable-action-name button) \"win.save\")
  (gtk:actionable-action-name button))
=> \"win.save\"
    @end{pre}
  @end{dictionary}
  @see-class{gtk:actionable}
  @see-class{gtk:application}
  @see-class{gtk:application-window}
  @see-class{g:menu}")

;;; --- actionable-action-target -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "action-target" 'actionable) t)
 "The @code{action-target} property of type @type{glib:variant} (Read / Write)
  @br{}
  The parameter for action invocations. @br{}
  Allowed values: a @type{glib:variant} parameter @br{}
  Default value: @code{null-pointer}")

#+liber-documentation
(setf (liber:alias-for-function 'actionable-action-target)
      "Accessor"
      (documentation 'actionable-action-target 'function)
 "@version{2023-8-4}
  @syntax[]{(gtk:actionable-action-target object) => value}
  @syntax[]{(setf (gtk:actionable-action-target object) value)}
  @argument[object]{a @class{gtk:actionable} widget}
  @argument[value]{a @type{glib:variant} parameter as the target value, or
    @code{null-pointer}}
  @begin{short}
    Accessor of the @slot[gtk:actionable]{action-target} slot of the
    @class{gtk:actionable} inferface.
  @end{short}
  The @fun{gtk:actionable-action-target} function gets the current target value
  of an actionable widget. The @setf{gtk:actionable-action-target} function sets
  the target value. If the @arg{value} argument is a @code{null-pointer} then
  the target value is unset.

  The target value has two purposes. First, it is used as the parameter to
  activation of the action associated with the @class{gtk:actionable} widget.
  Second, it is used to determine if the widget should be rendered as \"active\"
  - the widget is active if the state is equal to the given target.

  Consider the example of associating a set of buttons with a @class{g:action}
  object with string state in a typical radio button situation. Each button
  will be associated with the same action, but with a different target value for
  that action. Clicking on a particular button will activate the action with
  the target of that button, which will typically cause the state of the action
  to change to that value. Since the state of the action is now equal to the
  target value of the button, the button will now be rendered as active and the
  other buttons, with different targets, rendered inactive.
  @begin[Example]{dictionary}
    @begin{pre}
(let ((button (make-instance 'gtk:button)))
  (setf (gtk:actionable-action-target button) (g:variant-new-int16 128))
  (g:variant-int16 (gtk:actionable-action-target button)))
=> 128
    @end{pre}
  @end{dictionary}
  @begin[Note]{dictionary}
    The C implementation knows in addition the
    @code{gtk_actionable_get_action_target_value ()} and
    @code{gtk_actionable_set_action_target_value ()} functions. In the Lisp
    implementation these functions are replaced by the
    @fun{gtk:actionable-action-target} function.
  @end{dictionary}
  @see-class{gtk:actionable}
  @see-type{glib:variant}
  @see-class{g:action}")

;;; ----------------------------------------------------------------------------
;;; gtk_actionable_set_action_target ()
;;;
;;; void
;;; gtk_actionable_set_action_target (GtkActionable *actionable,
;;;                                   const char *format_string,
;;;                                   ...);
;;;
;;; Sets the target of an actionable widget.
;;;
;;; This is a convenience function that calls g_variant_new() for format_string
;;; and uses the result to call gtk_actionable_set_action_target_value().
;;;
;;; If you are setting a string-valued target and want to set the action name at
;;; the same time, you can use gtk_actionable_set_detailed_action_name().
;;;
;;; actionable :
;;;     a GtkActionable widget
;;;
;;; format_string :
;;;     a GVariant format string
;;;
;;; ... :
;;;     arguments appropriate for format_string
;;; ----------------------------------------------------------------------------

;; TODO: Consider to implement g:variant-new to allow this implementation.

;;; ----------------------------------------------------------------------------
;;; gtk_actionable_set_detailed_action_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_actionable_set_detailed_action_name"
               actionable-set-detailed-action-name) :void
 #+liber-documentation
 "@version{2023-3-19}
  @argument[actionable]{a @class{gtk:actionable} widget}
  @argument[name]{a string with the detailed action name}
  @begin{short}
    Sets the action name and associated string target value of an actionable
    widget.
  @end{short}
  This allows for the effect of both the @fun{gtk:actionable-action-name} and
  @fun{gtk:actionable-action-target} functions in the common case that the
  target is string-valued.

  The @arg{name} argument is a string of the form \"action::target\" where
  \"action\" is the action name and \"target\" is the string to use as the
  target.
  @begin[Example]{dictionary}
    @begin{pre}
(setq button (make-instance 'gtk:button))
=> #<GTK-BUTTON {1004A8C973@}>
(gtk:actionable-set-detailed-action-name button \"win.justify::left\")
(values (gtk:actionable-action-name button)
        (g:variant-string (gtk:actionable-action-target button)))
=> \"win.justify\"
=> \"left\"
    @end{pre}
  @end{dictionary}
  @see-class{gtk:actionable}
  @see-function{gtk:actionable-action-name}
  @see-function{gtk:actionable-action-target}"
  (actionable (g:object actionable))
  (name :string))

(export 'actionable-set-detailed-action-name)

;;; --- End of file gtk4.actionable.lisp ---------------------------------------
