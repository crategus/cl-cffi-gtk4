;;; ----------------------------------------------------------------------------
;;; gtk.toggle-button.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2021 Dieter Kaiser
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
;;;     gtk_toggle_button_toggled
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

(define-g-object-class "GtkToggleButton" toggle-button
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
 "@version{#2022-9-8}
  @begin{short}
    A @sym{gtk:toggle-button} widget is a @class{gtk:button} widget which will
    remain \"pressed-in\" when clicked.
  @end{short}
  Clicking again will cause the toggle button to return to its normal state.

  @image[toggle-button]{}

  A toggle button is created by calling either the @fun{gtk:toggle-button-new}
  or @fun{gtk:toggle-button-new-with-label} functions. If using the former, it
  is advisable to pack a widget, such as a @class{gtk:label} or a
  @class{gtk:image} widget, into the container of the toggle button. See the
  @class{gtk:button} documentation for more information.

  The state of a @sym{gtk:toggle-button} widget can be set and retrieved using
  the @fun{gtk:toggle-button-active} function. To simply switch the state of a
  toggle button, use the @fun{gtk:toggle-button-toggled} function.

  @subheading{Grouping}
  Toggle buttons can be grouped together, to form mutually exclusive groups -
  only one of the buttons can be toggled at a time, and toggling another one
  will switch the currently toggled one off. To add a toggle button to a group,
  use the @fun{gtk:toggle-button-set-group} function.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:toggle-button} implementation has a single CSS node with name
    @code{button}. To differentiate it from a plain button, it gets the
    @code{.toggle} style class.
  @end{dictionary}
  @begin[Example]{dictionary}
    This example has two toggle buttons. The toggle buttons are used to switch
    the column and row spacing of a grid.
    @begin{pre}
(defun do-grid-spacing (&optional (application nil))
  (let* ((grid (make-instance 'gtk:grid
                              :margin-top 12
                              :margin-bottom 12
                              :margin-start 12
                              :margin-end 12
                              :column-homogeneous t
                              :column-spacing 6
                              :row-homogeneous t
                              :row-spacing 6))
         (window (make-instance 'gtk:window
                                :title \"Grid Spacing\"
                                :application application
                                :child grid
                                :default-width 320))
         (button1 (make-instance 'gtk:toggle-button
                                 :label \"More Row Spacing\"))
         (button2 (make-instance 'gtk:toggle-button
                                 :label \"More Col Spacing\"))
         (button3 (make-instance 'gtk:button
                                 :label \"Button\")))
    ;; Signal handler for the first toggle button
    (g-signal-connect button1 \"toggled\"
       (lambda (widget)
         (if (gtk:toggle-button-active widget)
             (progn
               (setf (gtk:grid-row-spacing grid) 48)
               (setf (gtk:button-label widget) \"Less Row Spacing\"))
             (progn
               (setf (gtk:grid-row-spacing grid) 6)
               (setf (gtk:button-label widget) \"More Row Spacing\")))))
    ;; Signal handler for the second toggle button
    (g-signal-connect button2 \"toggled\"
       (lambda (widget)
         (if (gtk:toggle-button-active widget)
             (progn
               (setf (gtk:grid-column-spacing grid) 48)
               (setf (gtk:button-label widget) \"Less Col Spacing\"))
             (progn
               (setf (gtk:grid-column-spacing grid) 6)
               (setf (gtk:button-label widget) \"More Col Spacing\")))))
    ;; Pack and show the widgets
    (gtk:grid-attach grid button1 0 0 1 1)
    (gtk:grid-attach grid button2 1 0 1 1)
    (gtk:grid-attach grid button3 0 1 2 1)
    (gtk:widget-show window)))
    @end{pre}
    In this example three toggle buttons are grouped together.
    @begin{pre}
(defun do-toggle-button (&optional application)
  (let* ((vbox (make-instance 'gtk:box
                              :orientation :vertical
                              :halign :center
                              :spacing 12
                              :margin-top 12
                              :margin-bottom 12))
         (window (make-instance 'gtk:window
                                :title \"Toggle Buttons\"
                                :child vbox
                                :application application
                                :resizable nil)))
    ;; Create three toggle buttons and put the buttons into the box
    (let ((group nil)
          (button (gtk:toggle-button-new-with-label \"Toggle Button 1\")))
      (gtk:box-append vbox button)
      (setf group button)
      ;; Create and add the second radio button to the group
      (setf button (gtk:toggle-button-new-with-label \"Toggle Button 2\"))
      (setf (gtk:toggle-button-group button) group)
      (gtk:box-append vbox button)
      ;; Make the second button active
      (setf (gtk:toggle-button-active button) t)
      ;; Create and add the third toggle button to the group
      (setf button (gtk:toggle-button-new-with-label \"Toggle Button 3\"))
      (setf (gtk:toggle-button-group button) group)
      (gtk:box-append vbox button))
    (gtk:widget-show window)))
    @end{pre}
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"toggled\" signal}
      @begin{pre}
lambda (toggle)    :run-first
      @end{pre}
      Should be connected if you wish to perform an action whenever the state
      of the toggle button is changed.
      @begin[code]{table}
        @entry[toggle]{The @sym{gtk:toggle-button} widget which received the
          signal.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:toggle-button-active}
  @see-slot{gtk:toggle-button-group}
  @see-constructor{gtk:toggle-button-new}
  @see-constructor{gtk:toggle-button-new-with-label}
  @see-constructor{gtk:toggle-button-new-with-mnemonic}
  @see-class{gtk:button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- toggle-button-active -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "active" 'toggle-button) t)
"The @code{active} property of type @code{:boolean} (Read / Write) @br{}
  Whether the toggle button should be pressed in. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'toggle-button-active)
      "Accessor"
      (documentation 'toggle-button-active 'function)
 "@version{#2022-9-8}
  @syntax[]{(gtk:toggle-button-active object) => active}
  @syntax[]{(setf (gtk:toggle-button-active object) active)}
  @argument[object]{a @class{gtk:toggle-button} widget}
  @argument[active]{@em{true} if the toggle button should be pressed in}
  @begin{short}
    Accessor of the @slot[gtk:toggle-button]{active} slot of the
    @class{gtk:toggle-button} class.
  @end{short}
  The @sym{gtk:toggle-button-active} function queries a toggle button and
  returns its current state. Returns @em{true} if the toggle button is pressed
  in and @em{false} if it is raised. The @sym{(setf gtk:toggle-button-active)}
  function sets the status of the toggle button.

  If the status of the toggle button changes, this action causes the \"toggled\"
  signal to be emitted.
  @see-class{gtk:toggle-button}")

;;; --- toggle-button-group ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "group" 'toggle-button) t)
"The @code{group} property of type @sym{gtk:toggle-button} (Write) @br{}
  The toggle button whose group this widget belongs to.")

#+liber-documentation
(setf (liber:alias-for-function 'toggle-button-group)
      "Accessor"
      (documentation 'toggle-button-group 'function)
 "@version{#2022-9-8}
  @syntax[]{(setf (gtk:toggle-button-group object) group)}
  @argument[object]{a @class{gtk:toggle-button} widget}
  @argument[group]{another @class{gtk:toggle-button} widget to form a group
    with}
  @begin{short}
    Accessor of the @slot[gtk:toggle-button]{group} slot of the
    @class{gtk:toggle-button} class.
  @end{short}
  The @sym{(setf gtk:toggle-button-group)} function adds @arg{object} to the
  group of @arg{group}. In a group of multiple toggle buttons, only one button
  can be active at a time.

  Note that the same effect can be achieved via the @class{gtk:actionable} API,
  by using the same action with parameter type and \"s\" state type for all
  buttons in the group, and giving each button its own target value.
  @see-class{gtk:toggle-button}
  @see-class{gtk:actionable}")

;;; ----------------------------------------------------------------------------
;;; gtk_toggle_button_new
;;; ----------------------------------------------------------------------------

(declaim (inline toggle-button-new))

(defun toggle-button-new ()
 #+liber-documentation
 "@version{#2022-9-8}
  @return{A new @class{gtk:toggle-button} widget.}
  @begin{short}
    Creates a new toggle button.
  @end{short}
  A widget should be packed into the button, as in the @fun{gtk:button-new}
  function.
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
 "@version{#2022-9-8}
  @argument[label]{a string containing the message to be placed in the toggle
    button}
  @return{A new @class{gtk:toggle-button} widget.}
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
 "@version{#2022-9-8}
  @argument[label]{a string with the text of the button, with an underscore in
    front of the mnemonic character}
  @return{A new @class{gtk:toggle-button} widget.}
  @begin{short}
    Creates a new toggle button containing a label.
  @end{short}
  The label will be created using the
  @fun{gtk:label-new-with-mnemonic} function, so underscores in label indicate
  the mnemonic for the button.
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

(defcfun ("gtk_toggle_button_toggled" toggle-button-toggled) :void
 #+liber-documentation
 "@version{#2022-9-8}
  @argument[button]{a @class{gtk:toggle-button} widget}
  @begin{short}
    Emits the \"toggled\" signal on the toggle button.
  @end{short}
  There is no good reason for an application ever to call this function.
  @see-class{gtk:toggle-button}"
  (button (g:object toggle-button)))

(export 'toggle-button-toggled)

;;; --- End of file gtk.toggle-button.lisp -------------------------------------
