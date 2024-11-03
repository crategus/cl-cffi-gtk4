;;; ----------------------------------------------------------------------------
;;; gtk4.toggle-button.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library.
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
;;; GtkToggleButton
;;;
;;;     Create buttons which retain their state
;;;
;;; Types and Values
;;;
;;;     GtkToggleButton
;;;
;;; Accessors
;;;
;;;     gtk_toggle_button_get_active
;;;     gtk_toggle_button_set_active
;;;     gtk_toogle_button_set_group
;;;
;;; Functions
;;;
;;;     gtk_toggle_button_new
;;;     gtk_toggle_button_new_with_label
;;;     gtk_toggle_button_new_with_mnemonic
;;;     gtk_toggle_button_toggled                           Deprecated 4.10
;;;
;;; Properties
;;;
;;;     active
;;;     group
;;;
;;; Signals
;;;
;;;     toggled
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkButton
;;;                 ╰── GtkToggleButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstrainttarget
;;;     GtkActionable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTogleButton
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkToggleButton" toggle-button
  (:superclass button
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkActionable")
   :type-initializer "gtk_toggle_button_get_type")
  ((active
    toggle-button-active
    "active" "gboolean" t t)
   (group
    toggle-button-group
    "group" "GtkToggleButton" nil t)))

#+liber-documentation
(setf (documentation 'toggle-button 'type)
 "@version{2024-11-3}
  @begin{short}
    The @class{gtk:toggle-button} widget is a @class{gtk:button} widget which
    will remain \"pressed-in\" when clicked.
  @end{short}
  Clicking again will cause the toggle button to return to its normal state.

  @image[toggle-button]{Figure: GtkToggleButton}

  A toggle button is created by calling either the @fun{gtk:toggle-button-new}
  or @fun{gtk:toggle-button-new-with-label} functions. If using the former, it
  is advisable to pack a widget, such as a @class{gtk:label} or a
  @class{gtk:image} widget, into the container of the toggle button. See the
  @class{gtk:button} documentation for more information.

  The state of a @class{gtk:toggle-button} widget can be set and retrieved using
  the @fun{gtk:toggle-button-active} function.

  @subheading{Grouping}
  Toggle buttons can be grouped together, to form mutually exclusive groups.
  Only one of the buttons can be toggled at a time, and toggling another one
  will switch the currently toggled one off. To add a toggle button to a group,
  use the @fun{gtk:toggle-button-group} function.
  @begin[Examples]{dictionary}
    In this example three toggle buttons are grouped together using the
    @class{gtk:actionable} API, by using the same action with parameter type
    and @code{\"s\"} state type for all buttons in the group, and giving each
    button its own target value.
    @begin{pre}
(defun do-toggle-button-action (&optional application)
  (let* ((vbox (make-instance 'gtk:box
                              :orientation :vertical
                              :spacing 12
                              :margin-top 12
                              :margin-bottom 12
                              :margin-start 48
                              :margin-end 48))
         (window (make-instance 'gtk:window
                                :title \"Toggle Buttons with Action\"
                                :child vbox
                                :application application
                                :resizable nil))
         (label (make-instance 'gtk:label
                               :margin-top 9
                               :margin-bottom 6
                               :label \"<b>Button ? is active</b>\"
                               :use-markup t
                               :use-underline t))
         (action (g:simple-action-new-stateful \"toggled\"
                                               (g:variant-type-new \"s\")
                                               (g:variant-new-string \"top\")))
         (button nil))
    ;; Configure the \"toggled\" action
    (g:action-map-add-action application action)
    (g:signal-connect action \"activate\"
            (lambda (action parameter)
              (g:action-change-state action parameter)))
    (g:signal-connect action \"change-state\"
            (lambda (action parameter)
              (let ((str (g:variant-string parameter)))
                (cond ((string= str \"top\")
                       (setf (gtk:label-label label)
                             \"<b>Button Top is active</b>\"))
                      ((string= str \"center\")
                       (setf (gtk:label-label label)
                             \"<b>Button Center is active</b>\"))
                      (t
                       (setf (gtk:label-label label)
                             \"<b>Button Bottom is active</b>\")))
                (setf (g:action-state action) parameter))))
    ;; Create three grouped toggle buttons
    (setf button (gtk:toggle-button-new-with-mnemonic \"Button _Top\"))
    (gtk:actionable-set-detailed-action-name button \"app.toggled::top\")
    (gtk:box-append vbox button)
    ;; Create and add the second radio button to the group
    (setf button (gtk:toggle-button-new-with-mnemonic \"Button _Center\"))
    (gtk:actionable-set-detailed-action-name button \"app.toggled::center\")
    (gtk:box-append vbox button)
    ;; Create and add the third toggle button to the group
    (setf button (gtk:toggle-button-new-with-mnemonic \"Button _Bottom\"))
    (gtk:actionable-set-detailed-action-name button \"app.toggled::bottom\")
    (gtk:box-append vbox button)
    ;; Add a label which shows the status of the toggle buttons
    (gtk:box-append vbox label)
    ;; Make the \"app.toggled::center\" action active
    (g:action-activate (g:action-map-lookup-action application \"toggled\")
                       (g:variant-new-string \"center\"))
    ;; Present window
    (gtk:window-present window)))
    @end{pre}
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    The @class{gtk:toggle-button} implementation has a single CSS node with name
    @code{button}. To differentiate it from a plain button, it gets the
    @code{.toggle} style class.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"toggled\" signal}
      @begin{pre}
lambda (toggle)    :run-first
      @end{pre}
      @begin[code]{table}
        @entry[toggle]{The @class{gtk:toggle-button} widget which received the
          signal.}
      @end{table}
      Should be connected if you wish to perform an action whenever the state
      of the toggle button is changed.
  @end{dictionary}
  @see-constructor{gtk:toggle-button-new}
  @see-constructor{gtk:toggle-button-new-with-label}
  @see-constructor{gtk:toggle-button-new-with-mnemonic}
  @see-slot{gtk:toggle-button-active}
  @see-slot{gtk:toggle-button-group}
  @see-class{gtk:button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:toggle-button-active -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "active" 'toggle-button) t)
 "The @code{active} property of type @code{:boolean} (Read / Write) @br{}
  Whether the toggle button should be pressed in. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'toggle-button-active)
      "Accessor"
      (documentation 'toggle-button-active 'function)
 "@version{2024-5-4}
  @syntax{(gtk:toggle-button-active object) => active}
  @syntax{(setf (gtk:toggle-button-active object) active)}
  @argument[object]{a @class{gtk:toggle-button} widget}
  @argument[active]{@em{true} if the toggle button should be pressed in}
  @begin{short}
    Accessor of the @slot[gtk:toggle-button]{active} slot of the
    @class{gtk:toggle-button} class.
  @end{short}
  The @fun{gtk:toggle-button-active} function queries a toggle button and
  returns its current state. Returns @em{true} if the toggle button is pressed
  in and @em{false} if it is raised. The @setf{gtk:toggle-button-active}
  function sets the status of the toggle button.

  If the status of the toggle button changes, this action causes the
  @code{\"toggled\"} signal to be emitted.
  @see-class{gtk:toggle-button}")

;;; --- gtk:toggle-button-group ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "group" 'toggle-button) t)
 "The @code{group} property of type @class{gtk:toggle-button} (Write) @br{}
  The toggle button whose group this widget belongs to.")

#+liber-documentation
(setf (liber:alias-for-function 'toggle-button-group)
      "Accessor"
      (documentation 'toggle-button-group 'function)
 "@version{2024-5-4}
  @syntax{(setf (gtk:toggle-button-group object) group)}
  @argument[object]{a @class{gtk:toggle-button} widget}
  @argument[group]{another @class{gtk:toggle-button} widget to form a group
    with}
  @begin{short}
    Accessor of the @slot[gtk:toggle-button]{group} slot of the
    @class{gtk:toggle-button} class.
  @end{short}
  The @setf{gtk:toggle-button-group} function adds @arg{object} to the group of
  @arg{group}. In a group of multiple toggle buttons, only one button can be
  active at a time.

  Note that the same effect can be achieved via the @class{gtk:actionable} API,
  by using the same action with parameter type and @code{\"s\"} state type for
  all buttons in the group, and giving each button its own target value.
  @see-class{gtk:toggle-button}
  @see-class{gtk:actionable}")

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_button_new
;;; ----------------------------------------------------------------------------

(declaim (inline toggle-button-new))

(defun toggle-button-new ()
 #+liber-documentation
 "@version{2024-11-3}
  @return{The new @class{gtk:toggle-button} widget.}
  @begin{short}
    Creates a new toggle button.
  @end{short}
  A widget should be packed into the toggle button, as in the
  @fun{gtk:button-new} function.
  @see-class{gtk:toggle-button}
  @see-function{gtk:toggle-button-new-with-label}
  @see-function{gtk:toggle-button-new-with-mnemonic}
  @see-function{gtk:button-new}"
  (make-instance 'toggle-button))

(export 'toggle-button-new)

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_button_new_with_label
;;; ----------------------------------------------------------------------------

(declaim (inline toggle-button-new-with-label))

(defun toggle-button-new-with-label (label)
 #+liber-documentation
 "@version{2024-5-4}
  @argument[label]{a string containing the message to be placed in the toggle
    button}
  @return{The new @class{gtk:toggle-button} widget.}
  @short{Creates a new toggle button with a text label.}
  @see-class{gtk:toggle-button}
  @see-function{gtk:toggle-button-new}
  @see-function{gtk:toggle-button-new-with-mnemonic}"
  (make-instance 'toggle-button
                 :label label))

(export 'toggle-button-new-with-label)

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_button_new_with_mnemonic
;;; ----------------------------------------------------------------------------

(declaim (inline toggle-button-new-with-mnemonic))

(defun toggle-button-new-with-mnemonic (label)
 #+liber-documentation
 "@version{2024-5-4}
  @argument[label]{a string with the text of the button, with an underscore in
    front of the mnemonic character}
  @return{The new @class{gtk:toggle-button} widget.}
  @begin{short}
    Creates a new toggle button containing a label.
  @end{short}
  The label will be created using the @fun{gtk:label-new-with-mnemonic}
  function, so underscores in label indicate the mnemonic for the button.
  @see-class{gtk:toggle-button}
  @see-function{gtk:toggle-button-new}
  @see-function{gtk:toggle-button-new-with-label}
  @see-function{gtk:label-new-with-mnemonic}"
  (make-instance 'toggle-button
                 :label label
                 :use-underline t))

(export 'toggle-button-new-with-mnemonic)

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_button_toggled
;;; ----------------------------------------------------------------------------

(declaim (inline toggle-button-toggled))

(cffi:defcfun ("gtk_toggle_button_toggled" %toggle-button-toggled) :void
  (button (g:object toggle-button)))

(defun toggle-button-toggled (button)
 #+liber-documentation
 "@version{2024-5-4}
  @argument[button]{a @class{gtk:toggle-button} widget}
  @begin{short}
    Emits the @code{\"toggled\"} signal on the toggle button.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gtk:toggle-button-toggled} function is deprecated since 4.10.
    There is no good reason for an application ever to call this function.
  @end{dictionary}
  @see-class{gtk:toggle-button}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:TOGGLE-BUTTON-TOGGLED is deprecated since 4.10"))
  (%toggle-button-toggled button))

(export 'toggle-button-toggled)

;;; --- End of file gtk4.toggle-button.lisp ------------------------------------
