;;; ----------------------------------------------------------------------------
;;; gtk4.cell-renderer-toggle.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
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
;;; GtkCellRendererToggle
;;;
;;;     Renders a toggle button in a cell
;;;
;;; Types and Values
;;;
;;;     GtkCellRendererToggle
;;;
;;; Accessors
;;;
;;;     gtk_cell_renderer_toggle_get_activatable
;;;     gtk_cell_renderer_toggle_set_activatable
;;;     gtk_cell_renderer_toggle_get_active
;;;     gtk_cell_renderer_toggle_set_active
;;;     gtk_cell_renderer_toggle_get_radio
;;;     gtk_cell_renderer_toggle_set_radio
;;;
;;; Functions
;;;
;;;     gtk_cell_renderer_toggle_new
;;;
;;; Properties
;;;
;;;     activatable
;;;     active
;;;     inconsistent
;;;     radio
;;;
;;; Signals
;;;
;;;     toggled
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkCellRenderer
;;;             ╰── GtkCellRendererToggle
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCellRendererToggle
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkCellRendererToggle" cell-renderer-toggle
  (:superclass cell-renderer
    :export t
    :interfaces nil
    :type-initializer "gtk_cell_renderer_toggle_get_type")
  ((activatable
    cell-renderer-toggle-activatable
    "activatable" "gboolean" t t)
   (active
    cell-renderer-toggle-active
    "active" "gboolean" t t)
   (inconsistent
    cell-renderer-toggle-inconsistent
    "inconsistent" "gboolean" t t)
   (radio
    cell-renderer-toggle-radio
    "radio" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'cell-renderer-toggle 'type)
 "@version{#2020-6-13}
  @begin{short}
    @sym{gtk:cell-renderer-toggle} renders a toggle button in a cell.
  @end{short}
  The button is drawn as a radio button or a check button, depending on the
  @code{radio} property. When activated, it emits the \"toggled\" signal.
  @begin[Signal Details]{dictionary}
    @subheading{The \"toggled\" signal}
      @begin{pre}
lambda (cell-renderer path)    :run-last
      @end{pre}
      The \"toggled\" signal is emitted when the cell is toggled.
      @begin[code]{table}
        @entry[cell-renderer]{The @sym{gtk:cell-renderer-toggle} object which
          received the signal.}
        @entry[path]{String representation of the @class{gtk:tree-path}
          structure describing the event location.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:cell-renderer-toggle-activatable}
  @see-slot{gtk:cell-renderer-toggle-active}
  @see-slot{gtk:cell-renderer-toggle-inconsistent}
  @see-slot{gtk:cell-renderer-toggle-radio}
  @see-constructor{gtk:cell-renderer-toggle-new}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- cell-renderer-toggle-activatable ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "activatable"
                                               'cell-renderer-toggle) t)
 "The @code{activatable} property of type @code{:boolean} (Read / Write) @br{}
  Whether the toggle button can be activated. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-toggle-activatable)
      "Accessor"
      (documentation 'cell-renderer-toggle-activatable 'function)
 "@version{#2020-6-13}
  @syntax[]{(gtk:cell-renderer-toggle-activatable object) => setting}
  @syntax[]{(setf (gtk:cell-renderer-toggle-activatable object) setting)}
  @argument[toggle]{a @class{gtk:cell-renderer-toggle} object}
  @argument[setting]{a boolean value to set}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-toggle]{activatable} slot of the
    @class{gtk:cell-renderer-toggle} class.
  @end{short}

  The slot access function @sym{gtk:cell-renderer-toggle-activatable} returns
  whether the cell renderer is activatable. The slot access function
  @sym{(setf gtk:cell-renderer-toggle-activatable)} makes the cell renderer
  activatable.
  @see-class{gtk:cell-renderer-toggle}")

;;; --- cell-renderer-toggle-active --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "active"
                                               'cell-renderer-toggle) t)
 "The @code{active} property of type @code{:boolean} (Read / Write) @br{}
  The toggle state of the button. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-toggle-active)
      "Accessor"
      (documentation 'cell-renderer-toggle-active 'function)
 "@version{#2020-6-13}
  @syntax[]{(gtk:cell-renderer-toggle-active object) => setting}
  @syntax[]{(setf (gtk:cell-renderer-toggle-active object) setting)}
  @argument[toggle]{a @class{gtk:cell-renderer-toggle} object}
  @argument[setting]{a boolean value to set}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-toggle]{active} slot of the
    @class{gtk:cell-renderer-toggle} class.
  @end{short}

  The slot access function @sym{gtk:cell-renderer-toggle-active} returns
  whether the cell renderer is active. The slot access function
  @sym{(setf gtk:cell-renderer-toggle-active)} activates or deactivates a cell
  renderer.
  @see-class{gtk:cell-renderer-toggle}")

;;; --- cell-renderer-toggle-inconsistent --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "inconsistent"
                                               'cell-renderer-toggle) t)
 "The @code{inconsistent} property of type @code{:boolean} (Read / Write) @br{}
  The inconsistent state of the button. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-toggle-inconsistent)
      "Accessor"
      (documentation 'cell-renderer-toggle-inconsistent 'function)
 "@version{#2020-6-13}
  @syntax[]{(gtk:cell-renderer-toggle-inconsistent object) => setting}
  @syntax[]{(setf (gtk:cell-renderer-toggle-inconsistent object) setting)}
  @argument[toggle]{a @class{gtk:cell-renderer-toggle} object}
  @argument[setting]{a boolean value to set}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-toggle]{inconsistent} slot of the
    @class{gtk:cell-renderer-toggle} class.
  @end{short}

  The inconsistent state of the button.
  @see-class{gtk:cell-renderer-toggle}")

;;; --- cell-renderer-toggle-radio ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "radio"
                                               'cell-renderer-toggle) t)
 "The @code{radio} property of type @code{:boolean} (Read / Write) @br{}
  Draw the toggle button as a radio button. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-toggle-radio)
      "Accessor"
      (documentation 'cell-renderer-toggle-radio 'function)
 "@version{#2020-6-13}
  @syntax[]{(gtk:cell-renderer-toggle-radio object) => radio}
  @syntax[]{(setf (gtk:cell-renderer-toggle-radio object) radio)}
  @argument[toggle]{a @class{gtk:cell-renderer-toggle} object}
  @argument[radio]{@em{true} to make the toggle look like a radio button}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-toggle]{radio} slot of the
    @class{gtk:cell-renderer-toggle} class.
  @end{short}

  If @arg{radio} is @em{true}, the cell renderer renders a radio toggle, i.e.
  a toggle in a group of mutually-exclusive toggles. If @em{false}, it renders
  a check toggle, a standalone boolean option.

  This can be set globally for the cell renderer, or changed just before
  rendering each cell in the model, for @class{gtk:tree-view}, you set up a
  per-row setting using @class{gtk:tree-view-column} to associate model columns
  with cell renderer properties.
  @see-class{gtk:cell-renderer-toggle}
  @see-class{gtk:tree-view}
  @see-class{gtk:tree-view-column}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_toggle_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline cell-renderer-toggle-new))

(defun cell-renderer-toggle-new ()
 #+liber-documentation
 "@version{#2020-6-13}
  @return{The new @class{gtk:cell-renderer-toggle} object.}
  @begin{short}
    Creates a new cell renderer toggle.
  @end{short}
  Adjust rendering parameters using object properties. Object properties can
  be set globally, with the function @fun{g:object-property}. Also, with
  @class{gtk:tree-view-column}, you can bind a property to a value in a
  @class{gtk:tree-model}. For example, you can bind the
  @slot[gtk:cell-renderer-toggle]{active} property on the cell renderer to
  a boolean value in the model, thus causing the check button to reflect the
  state of the model.
  @see-class{gtk:cell-renderer-toggle}
  @see-class{gtk:tree-view-column}
  @see-class{gtk:tree-model}
  @see-function{g:object-property}"
  (make-instance 'cell-renderer-toggle))

(export 'cell-renderer-toggle-new)

;;; --- End of file gtk4.cell-renderer-toggle.lisp -----------------------------
