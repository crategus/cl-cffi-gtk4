;;; ----------------------------------------------------------------------------
;;; gtk.check-button.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2022 Dieter Kaiser
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
;;; GtkCheckButton
;;;
;;;     Create widgets with a discrete toggle button
;;;
;;; Types and Values
;;;
;;;     GtkCheckButton
;;;
;;; Accessors
;;;
;;;     gtk_check_button_get_active
;;;     gtk_check_button_set_active
;;;     gtk_check_button_get_inconsistent
;;;     gtk_check_button_set_inconsistent
;;;     gtk_check_button_get_label
;;;     gtk_check_button_set_label
;;;     gtk_check_button_get_use_underline
;;;     gtk_check_button_set_use_underline
;;;     gtk_check_button_get_child                         Since 4.8
;;;     gtk_check_button_set_child                         Since 4.8
;;;
;;; Functions
;;;
;;;     gtk_check_button_new
;;;     gtk_check_button_new_with_label
;;;     gtk_check_button_new_with_mnemonic
;;;     gtk_check_button_set_group
;;;
;;; Properties
;;;
;;;     active
;;;     child                                              Since 4.8
;;;     group
;;;     inconsistent
;;;     label
;;;     use-underline
;;;
;;; Signals
;;;
;;;     activate                                           Since 4.2
;;;     toggled
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkCheckButton
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
;;; GtkCheckButton
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCheckButton" check-button
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkActionable"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_check_button_get_type")
  ((active
    check-button-active
    "active" "gboolean" t t)
   #+gtk-4-8
   (child
   check-button-child
   "child" "GtkWidget" t t)
   (group
    check-button-group
    "group" "GtkCheckButton" nil t)
   (inconsistent
    check-button-inconsistent
    "inconsistent" "gboolean" t t)
   (label
    check-button-label
    "label" "gchararray" t t)
   (use-underline
    check-button-use-underline
    "use-underline" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'check-button 'type)
 "@version{#2022-9-8}
  @begin{short}
    A @sym{gtk:check-button} widget places a label next to an indicator.
  @end{short}

  @image[check-button]{Figure: GtkCheckButton}

  A @sym{gtk:check-button} widget is created by calling either the
  @fun{gtk:check-button-new} or @fun{gtk:check-button-new-with-label} functions.

  The state of a check button can be set specifically using the
  @fun{gtk:check-button-active} function.

  @subheading{Inconsistent state}
  In addition to \"on\" and \"off\", check buttons can be an \"in between\"
  state that is neither on nor off. This can be used e.g. when the user has
  selected a range of elements (such as some text or spreadsheet cells) that are
  affected by a check button, and the current values in that range are
  inconsistent. To set a check button to inconsistent state, use the
  @fun{gtk:check-button-inconsistent} function.

  @subheading{Grouping}
  Check buttons can be grouped together, to form mutually exclusive groups -
  only one of the buttons can be toggled at a time, and toggling another one
  will switch the currently toggled one off. Grouped check buttons use a
  different indicator, and are commonly referred to as radio buttons. To add a
  @sym{gtk:check-button} widget to a group, use the @fun{gtk:check-button-group}
  function.
  @begin[Example]{dictionary}
    This examples shows check and radio buttons.
    @begin{pre}
(defun do-check-button (&optional application)
  (let* ((grid (make-instance 'gtk:grid
                              :halign :center
                              :column-spacing 12
                              :row-spacing 12
                              :margin-top 12
                              :margin-bottom 12
                              :margin-start 12
                              :margin-end 12))
         (window (make-instance 'gtk:window
                                :title \"Check Buttons\"
                                :child grid
                                :application application
                                :resizable nil)))
    ;; Create three radio buttons and put the buttons into the grid
    (let ((group nil)
          (button (gtk:check-button-new-with-label \"Radio Button 1\")))
      (gtk:grid-attach grid button 0 0 1 1)
      (setf group button)
      ;; Create and add the second radio button to the group
      (setf button (gtk:check-button-new-with-label \"Radio Button 2\"))
      (setf (gtk:check-button-group button) group)
      (gtk:grid-attach grid button 0 1 1 1)
      ;; Make the second button active
      (setf (gtk:check-button-active button) t)
      ;; Create and add the third radio button to the group
      (setf button (gtk:check-button-new-with-label \"Radio Button 3\"))
      (setf (gtk:check-button-group button) group)
      (gtk:grid-attach grid button 0 2 1 1))
    ;; Create three check buttons and put the buttons into the grid
    (gtk:grid-attach grid
                     (gtk:check-button-new-with-label \"Check Button 1\")
                     1 0 1 1)
    (gtk:grid-attach grid
                     (gtk:check-button-new-with-label \"Check Button 2\")
                       1 1 1 1)
    (gtk:grid-attach grid
                     (gtk:check-button-new-with-label \"Check Button 3\")
                     1 2 1 1)
    ;; Make the first check button active
    (setf (gtk:check-button-active (gtk:grid-child-at grid 1 0)) t)
    ;; Show the window and its content
    (gtk:widget-show window)))
    @end{pre}
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
checkbutton[.text-button]
├── check
╰── [label]
    @end{pre}
    The @sym{gtk:check-button} implementation has a main node with name
    @code{checkbutton}. If the @code{label} property is set, it contains a label
    child. The indicator node is named @code{check} when no group is set, and
    @code{radio} if the check button is grouped together with other check
    buttons.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @sym{gtk:check-button} implementation uses the @code{:checkbox} role
    of the @symbol{gtk:accessible-role} enumeration.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activated\" signal}
      @begin{pre}
lambda (checkbutton)    :action
      @end{pre}
      Emitted when the check button is activated. The signal is an action signal
      and emitting it causes the button to animate press then release.
      Applications should never connect to this signal, but use the \"toggled\"
      signal. Since 4.2
      @begin[code]{table}
        @entry[checkbutton]{The @sym{gtk:check-button} widget which received
          the signal.}
      @end{table}
    @subheading{The \"toggled\" signal}
      @begin{pre}
lambda (checkbutton)    :run-first
      @end{pre}
      Emitted when the @code{active} property of the check button changes.
      @begin[code]{table}
        @entry[checkbutton]{The @sym{gtk:check-button} widget which received
          the signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:check-button-active}
  @see-slot{gtk:check-button-group}
  @see-slot{gtk:check-button-inconsistent}
  @see-slot{gtk:check-button-label}
  @see-slot{gtk:check-button-use-underline}
  @see-class{gtk:button}
  @see-class{gtk:toggle-button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- check-button-active ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "active" 'check-button) t)
 "The @code{active} property of type @code{:boolean} (Read / Write) @br{}
  If the check button should be pressed in. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'check-button-active)
      "Accessor"
      (documentation 'check-button-active 'function)
 "@version{#2022-9-8}
  @syntax[]{(gtk:check-button-active object) => active}
  @syntax[]{(setf (gtk:check-button-active object) active)}
  @argument[object]{a @class{gtk:check-button} widget}
  @argument[active]{a boolean whether the check button should be pressed in}
  @begin{short}
    Accessor of the @slot[gtk:check-button]{active} slot of the
    @class{gtk:check-button} class.
  @end{short}
  Setting the @code{active} property to @em{true} will add the @code{:checked}
  state to both the check button and the indicator CSS node.
  @see-class{gtk:check-button}")

;;; --- check-button-child -------------------------------------------------

#+(and gtk-4-8 liber-documentation)
(setf (documentation (liber:slot-documentation "child" 'check-button) t)
 "The @code{child} property of type @class{gkt-widget} (Read / Write) @br{}
  The child widget. Since 4.8")

#+(and gtk-4-8 liber-documentation)
(setf (liber:alias-for-function 'check-button-child)
      "Accessor"
      (documentation 'check-button-child 'function)
 "@version{#2022-9-8}
  @syntax[]{(gtk:check-button-child object) => child}
  @syntax[]{(setf (gtk:check-button-child object) child)}
  @argument[object]{a @class{gtk:check-button} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Accessor of the @slot[gtk:check-button]{child} slot of the
    @class{gtk:check-button} class.
  @end{short}
  The @sym{gtk:check-button-child} function gets the child widget of the check
  button or @code{nil} if the @slot[gtk:check-button]{label} property is set.
  The @sym{(setf gtk:check-button-child)} functionn sets the child widget.

  Note that by using this API, you take full responsibility for setting up the
  proper accessibility label and description information for the check button.
  Most likely, you will either set the accessibility label or description for
  the check button explicitly, or you will set a labelled-by or described-by
  relations from the child widget to the check button.

  Since 4.8
  @see-class{gtk:check-button}
  @see-function{gtk:check-button-label}")

;;; --- check-button-group -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "group" 'check-button) t)
 "The @code{group} property of type @sym{gtk:check-button} (Write) @br{}
  The check button whose group this widget belongs to.")

#+liber-documentation
(setf (liber:alias-for-function 'check-button-group)
      "Accessor"
      (documentation 'check-button-group 'function)
 "@version{#2022-9-8}
  @syntax[]{(setf (gtk:check-button-group object) group)}
  @argument[object]{a @class{gtk:check-button} widget}
  @argument[group]{a @class{gtk:check-button} widget}
  @begin{short}
    Accessor of the @slot[gtk:check-button]{group} slot of the
    @class{gtk:check-button} class.
  @end{short}
  Adds the check button to the group of @arg{group}. In a group of multiple
  check buttons, only one button can be active at a time.

  Setting the group of a check button also changes the CSS name of the indicator
  widget's CSS node to @code{radio}. The behavior of a check button in a group
  is also commonly known as a \"radio button\".

  Note that the same effect can be achieved via the @class{gtk:actionable} API,
  by using the same action with parameter type and state type \"s\" for all
  buttons in the group, and giving each button its own target value.
  @see-class{gtk:check-button}
  @see-function{gtk:actionable}")

;;; --- check-button-inconsistent ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "inconsistent"
                                               'check-button) t)
 "The @code{inconsistent} property of type @code{:boolean} (Read / Write) @br{}
  If the check button is in an \"in between\" state. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'check-button-inconsistent)
      "Accessor"
      (documentation 'check-button-inconsistent 'function)
 "@version{#2022-9-8}
  @syntax[]{(gtk:check-button-inconsistent object) => inconsistent}
  @syntax[]{(setf (gtk:check-button-inconsistent object) inconsistent)}
  @argument[object]{a @class{gtk:check-button} widget}
  @argument[inconsistent]{a boolean whether the check button is in an \"in
    between\" state}
  @begin{short}
    Accessor of the @slot[gtk:check-button]{inconsistent} slot of the
    @class{gtk:check-button} class.
  @end{short}
  If the user has selected a range of elements (such as some text or spreadsheet
  cells) that are affected by a check button, and the current values in that
  range are inconsistent, you may want to display the toggle in an
  \"in between\" state. Normally you would turn off the inconsistent state again
  if the user checks the check button. This has to be done manually,
  the @slot[gtk:check-button]{inconsistent} property only affects visual
  appearance, not the semantics of the button.
  @see-class{gtk:check-button}")

;;; --- check-button-label -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label" 'check-button) t)
 "The @code{label} property of type @code{:string} (Read / Write) @br{}
  Text of the label widget inside the button, if the button contains a label
  widget. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'check-button-label)
      "Accessor"
      (documentation 'check-button-label 'function)
 "@version{#2022-9-8}
  @syntax[]{(gtk:check-button-label object) => label}
  @syntax[]{(setf (gtk:check-button-label object) label)}
  @argument[object]{a @class{gtk:check-button} widget}
  @argument[label]{a string with the text of the label widget inside the button}
  @begin{short}
    Accessor of the @slot[gtk:check-button]{label} slot of the
    @class{gtk:check-button} class.
  @end{short}
  @see-class{gtk:check-button}")

;;; --- check-button-use-underline -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-underline"
                                               'check-button) t)
 "The @code{use-underline} property of type @code{:boolean} (Read / Write) @br{}
  If set, an underline in the text indicates the next character should be used
  for the mnemonic accelerator key. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'check-button-use-underline)
      "Accessor"
      (documentation 'check-button-use-underline 'function)
 "@version{#2022-9-8}
  @syntax[]{(gtk:check-button-use-underline object) => setting}
  @syntax[]{(setf (gtk:check-button-use-underline object) setting)}
  @argument[object]{a @class{gtk:check-button} widget}
  @argument[setting]{a boolean whether an underline in the text indicates the
    next character should be used for the mnemonic accelerator key}
  @begin{short}
    Accessor of the @slot[gtk:check-button]{use-underline} slot of the
    @class{gtk:check-button} class.
  @end{short}
  @see-class{gtk:check-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_check_button_new
;;; ----------------------------------------------------------------------------

(declaim (inline check-button-new))

(defun check-button-new ()
 #+liber-documentation
 "@version{#2022-9-8}
  @return{A @class{gtk:check-button} widget.}
  @short{Creates a new check button.}
  @see-class{gtk:check-button}
  @see-function{gtk:check-button-new-with-label}
  @see-function{gtk:check-button-new-with-mnemonic}"
  (make-instance 'check-button))

(export 'check-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_check_button_new_with_label
;;; ----------------------------------------------------------------------------

(declaim (inline check-button-new-with-label))

(defun check-button-new-with-label (label)
 #+liber-documentation
 "@version{#2022-9-8}
  @argument[label]{a string with the text for the check button}
  @return{A @class{gtk:check-button} widget.}
  @begin{short}
    Creates a new check button with a @class{gtk:label} widget to the right
    of it.
  @end{short}
  @see-class{gtk:check-button}
  @see-class{gtk:label}
  @see-function{gtk:check-button-new}
  @see-function{gtk:check-button-new-with-mnemonic}"
  (make-instance 'check-button
                 :label label))

(export 'check-button-new-with-label)

;;; ----------------------------------------------------------------------------
;;; gtk_check_button_new_with_mnemonic
;;; ----------------------------------------------------------------------------

;; TODO: Rewrite the implementation in terms of the function make-instance

(defcfun ("gtk_check_button_new_with_mnemonic"
           check-button-new-with-mnemonic)
    (g:object check-button)
#+liber-documentation
 "@version{#2022-9-8}
  @argument[label]{a string with the text of the button, with an underscore in
    front of the mnemonic character}
  @return{A @class{gtk:check-button} widget.}
  @begin{short}
    Creates a new check button widget containing a label.
  @end{short}
  The label will be created using the @fun{gtk:label-new-with-mnemonic}
  function, so underscores in label indicate the mnemonic for the check button.
  @see-class{gtk:check-button}
  @see-function{gtk:check-button-new}
  @see-function{gtk:check-button-new-with-label}
  @see-function{gtk:label-new-with-mnemonic}"
  (label :string))

(export 'check-button-new-with-mnemonic)

;;; --- End of file gtk.check-button.lisp --------------------------------------
