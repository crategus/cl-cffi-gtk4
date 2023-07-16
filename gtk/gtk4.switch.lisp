;;; ----------------------------------------------------------------------------
;;; gtk4.switch.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.10 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;; GtkSwitch
;;;
;;;     A "light switch" style toggle
;;;
;;; Types and Values
;;;
;;;     GtkSwitch
;;;
;;; Accessors
;;;
;;;     gtk_switch_set_active
;;;     gtk_switch_get_active
;;;     gtk_switch_set_state
;;;     gtk_switch_get_state
;;;
;;; Functions
;;;
;;;     gtk_switch_new
;;;
;;; Properties
;;;
;;;     active
;;;     state
;;;
;;; Signals
;;;
;;;     activate
;;;     state-set
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkSwitch
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
;;; struct GtkSwitch
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkSwitch" switch
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkActionable")
   :type-initializer "gtk_switch_get_type")
  ((active
    switch-active
    "active" "gboolean" t t)
   (state
    switch-state
    "state" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'switch 'type)
 "@version{2023-3-26}
  @begin{short}
    The @sym{gtk:switch} widget is a widget that has two states: on or off.
  @end{short}

  @image[switch]{Figure: GtkSwitch}

  The user can control which state should be active by clicking the switch,
  or by dragging the handle.

  The @sym{gtk:switch} widget can also handle situations where the underlying
  state changes with a delay. See the \"state-set\" signal for details.
  @begin[CSS nodes]{dictionary}
    @begin{pre}
 switch
 ├── label
 ├── label
 ╰── slider
    @end{pre}
    The @sym{gtk:switch} implementation has four CSS nodes, the main node with
    the name @code{switch} and subnodes for the slider and the on and off
    labels. Neither of them is using any style classes.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @sym{gtk:switch} implementation uses the @code{:switch} role of the
    @symbol{gtk:accessible-role} enumeration.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @subheading{The \"activate\" signal}
      @begin{pre}
lambda (widget)    :action
      @end{pre}
      The signal on the switch is an action signal and emitting it causes the
      switch to animate. Applications should never connect to this signal, but
      use the \"notify::active\" signal.
      @begin[code]{table}
        @entry[widget]{The @sym{gtk:switch} widget which received the signal.}
      @end{table}
    @subheading{The \"state-set\" signal}
      @begin{pre}
lambda (widget state)    :run-last
      @end{pre}
      The signal on the switch is emitted to change the underlying state. It is
      emitted when the user changes the switch position. The default handler
      keeps the state in sync with the @code{active} property.

      To implement delayed state change, applications can connect to this
      signal, initiate the change of the underlying state, and call the
      @fun{gtk:switch-state} function when the underlying state change is
      complete. The signal handler should return @em{true} to prevent the
      default handler from running.

      Visually, the underlying state is represented by the trough color of the
      switch, while the @code{active} property is represented by the position
      of the switch.
      @begin[code]{table}
        @entry[widget]{The @sym{gtk:switch} widget which received the signal.}
        @entry[state]{A boolean with the state of the switch.}
        @entry[Returns]{@em{True} to stop the signal emission.}
      @end{table}
  @end{dictionary}
  @see-constructor{gtk:switch-new}
  @see-slot{gtk:switch-active}
  @see-slot{gtk:switch-state}
  @see-class{gtk:toggle-button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accesor Details
;;; ----------------------------------------------------------------------------

;;; --- switch-active ----------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "active" 'switch) t)
 "The @code{active} property of type @code{:boolean} (Read / Write) @br{}
  Whether the switch is in its on or off state. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'switch-active)
      "Accessor"
      (documentation 'switch-active 'function)
 "@version{2023-3-26}
  @syntax[]{(gtk:switch-active object) => active)}
  @syntax[]{(setf (gtk:switch-active object) active)}
  @argument[object]{a @class{gtk:switch} widget}
  @argument[active]{@em{true} if the switch should be active, and @em{false}
    otherwise}
  @begin{short}
    Accessor of the @slot[gtk:switch]{active} slot of the @class{gtk:switch}
    class.
  @end{short}
  The @sym{gtk:switch-active} function gets whether the switch is in its \"on\"
  or \"off\" state. The @sym{(setf gtk:switch-active)} function changes the
  state of the switch to the desired one.
  @see-class{gtk:switch}")

;;; --- switch-state -----------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "state" 'switch) t)
 "The @code{state} property of type @code{:boolean} (Read / Write) @br{}
  The backend state that is controlled by the switch. See the \"state-set\"
  signal for details. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'switch-state)
      "Accessor"
      (documentation 'switch-state 'function)
 "@version{2023-3-26}
  @syntax[]{(gtk:switch-state object) => state)}
  @syntax[]{(setf (gtk:switch-state object) state)}
  @argument[object]{a @class{gtk:switch} widget}
  @argument[state]{a boolean with the state}
  @begin{short}
    Accessor of the @slot[gtk:switch]{state} slot of the @class{gtk:switch}
    class.
  @end{short}
  The @sym{gtk:switch-active} function gets the underlying state of the switch.
  The @sym{(setf gtk:switch-active)} function sets the underlying state.

  Normally, this is the same as the @slot[gtk:switch]{active} property, unless
  the switch is set up for delayed state changes. This function is typically
  called from a \"state-set\" signal handler. See the \"state-set\" signal for
  details.
  @see-class{gtk:switch}
  @see-function{gtk:switch-active}")

;;; ----------------------------------------------------------------------------
;;; gtk_switch_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline switch-new))

(defun switch-new ()
 #+liber-documentation
 "@version{2023-3-26}
  @return{The newly created @class{gtk:switch} widget.}
  @short{Creates a new switch.}
  @see-class{gtk:switch}"
  (make-instance 'switch))

(export 'switch-new)

;;; --- End of file gtk4.switch.lisp -------------------------------------------
