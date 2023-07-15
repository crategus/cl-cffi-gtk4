;;; ----------------------------------------------------------------------------
;;; gtk4.cell-renderer-accel.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
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
;;; GtkCellRendererAccel
;;;
;;;     Renders a keyboard accelerator in a cell
;;;
;;; Types and Values
;;;
;;;     GtkCellRendererAccel
;;;     GtkCellRendererAccelMode
;;;
;;; Functions
;;;
;;;     gtk_cell_renderer_accel_new
;;;
;;; Properties
;;;
;;;     accel-key
;;;     accel-mode
;;;     accel-mods
;;;     keycode
;;;
;;; Signals
;;;
;;;     accel-cleared
;;;     accel-edited
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkCellRenderer
;;;             ╰── GtkCellRendererText
;;;                 ╰── GtkCellRendererAccel
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; enum GtkCellRendererAccelMode
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GtkCellRendererAccelMode" cell-renderer-accel-mode
  (:export t
   :type-initializer "gtk_cell_renderer_accel_mode_get_type")
  (:gtk 0)
  (:other 1))

#+liber-documentation
(setf (liber:alias-for-symbol 'cell-renderer-accel-mode)
      "GEnum"
      (liber:symbol-documentation 'cell-renderer-accel-mode)
 "@version{#2020-6-20}
  @begin{short}
    Determines if the edited accelerators are GTK+ accelerators.
  @end{short}
  If they are, consumed modifiers are suppressed, only accelerators accepted by
  GTK+ are allowed, and the accelerators are rendered in the same way as they
  are in menus.
  @begin{pre}
(gobject:define-g-enum \"GtkCellRendererAccelMode\" cell-renderer-accel-mode
  (:export t
   :type-initializer \"gtk_cell_renderer_accel_mode_get_type\")
  (:gtk 0)
  (:other 1))
  @end{pre}
  @begin[code]{table}
    @entry[:gtk]{GTK+ accelerators mode.}
    @entry[:other]{Other accelerator mode.}
  @end{table}
  @see-class{gtk:cell-renderer-accel}")

;;; ----------------------------------------------------------------------------
;;; struct GtkCellRendererAccel
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkCellRendererAccel" cell-renderer-accel
  (:superclass cell-renderer-text
   :export t
   :interfaces nil
   :type-initializer "gtk_cell_renderer_accel_get_type")
  ((accel-key
    cell-renderer-accel-accel-key
    "accel-key" "guint" t t)
   (accel-mode
    cell-renderer-accel-accel-mode
    "accel-mode" "GtkCellRendererAccelMode" t t)
   (accel-mods
    cell-renderer-accel-accel-mods
    "accel-mods" "GdkModifierType" t t)
   (keycode
    cell-renderer-accel-keycode
    "keycode" "guint" t t)))

#+liber-documentation
(setf (documentation 'cell-renderer-accel 'type)
 "@version{#2020-6-20}
  @begin{short}
    @sym{gtk:cell-renderer-accel} displays a keyboard accelerator, i.e. a key
    combination like @code{Control+a}.
  @end{short}
  If the cell renderer is editable, the accelerator can be changed by simply
  typing the new combination.
  @begin[Signal Details]{dictionary}
    @subheading{The \"accel-cleared\" signal}
      @begin{pre}
lambda (accel path)    :run-last
      @end{pre}
      Gets emitted when the user has removed the accelerator.
      @begin[code]{table}
        @entry[accel]{The @sym{gtk:cell-renderer-accel} object reveiving the
          signal.}
        @entry[path]{A string with the path identifying the row of the edited
          cell.}
      @end{table}
    @subheading{The \"accel-edited\" signal}
      @begin{pre}
lambda (accel path accel-key accel-mods hardware-keycode)    :run-last
      @end{pre}
      Gets emitted when the user has selected a new accelerator.
      @begin[code]{table}
        @entry[accel]{The @sym{gtk:cell-renderer-accel} object reveiving the
          signal.}
        @entry[path]{A string with the path identifying the row of the edited
          cell.}
        @entry[accel-key]{An unsigned integer with the new accelerator keyval.}
        @entry[accel-mods]{A @class{gdk:modifier-type} value with the new
          acclerator modifier mask.}
        @entry[hardware-keycode]{An unsignend integer with the keycode of the
          new accelerator.}
      @end{table}
  @end{dictionary}
  @see-slot{gtk:cell-renderer-accel-accel-key}
  @see-slot{gtk:cell-renderer-accel-accel-mode}
  @see-slot{gtk:cell-renderer-accel-accel-mods}
  @see-slot{gtk:cell-renderer-accel-keycode}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- cell-renderer-accel-accel-key ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accel-key"
                                               'cell-renderer-accel) t)
 "The @code{accel-key} property of type @code{:uint} (Read / Write) @br{}
  The keyval of the accelerator. @br{}
  Allowed values: <= @code{G_MAXINT} @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-accel-accel-key)
      "Accessor"
      (documentation 'cell-renderer-accel-accel-key 'function)
 "@version{#2020-6-20}
  @syntax[]{(gtk:cell-renderer-accel-accel-key object) => accel-key}
  @syntax[]{(setf (gtk:cell-renderer-accel-accel-key object) accel-key)}
  @argument[object]{a @class{gtk:cell-renderer-accel} object}
  @argument[accel-key]{an unsigned integer with the keyval of the accelerator}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer]{accel-key} slot of the
    @class{gtk:cell-renderer-accel} class.
  @end{short}

  The keyval of the accelerator.
  @see-class{gtk:cell-renderer-accel}")

;;; --- cell-renderer-accel-accel-mode -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accel-mode"
                                               'cell-renderer-accel) t)
 "The @code{accel-mode} property of type @symbol{gtk:cell-renderer-accel-mode}
  (Read / Write) @br{}
  Determines if the edited accelerators are GTK+ accelerators. If they are,
  consumed modifiers are suppressed, only accelerators accepted by GTK+ are
  allowed, and the accelerators are rendered in the same way as they are in
  menus. @br{}
  Default value: @code{:gtk}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-accel-accel-mode)
      "Accessor"
      (documentation 'cell-renderer-accel-accel-mode 'function)
 "@version{#2020-6-20}
  @syntax[]{(gtk:cell-renderer-accel-accel-mode object) => accel-mode}
  @syntax[]{(setf (gtk:cell-renderer-accel-accel-mode object) accel-mode)}
  @argument[object]{a @class{gtk:cell-renderer-accel} object}
  @argument[accel-mode]{a value of the @symbol{gtk:cell-renderer-accel-mode}
    enumeration}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-accel]{accel-mode} slot of the
    @class{gtk:cell-renderer-accel} class.
  @end{short}

  Determines if the edited accelerators are GTK+ accelerators. If they are,
  consumed modifiers are suppressed, only accelerators accepted by GTK+ are
  allowed, and the accelerators are rendered in the same way as they are in
  menus.
  @see-class{gtk:cell-renderer-accel}
  @see-symbol{gtk:cell-renderer-accel-mode}")

;;; --- cell-renderer-accel-accel-mods -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accel-mods"
                                               'cell-renderer-accel) t)
 "The @code{accel-mods} property of type @symbol{gdk:modifier-type}
  (Read / Write) @br{}
  The modifier mask of the accelerator.")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-accel-accel-mods)
      "Accessor"
      (documentation 'cell-renderer-accel-accel-mods 'function)
 "@version{#2020-6-20}
  @syntax[]{(gtk:cell-renderer-accel-accel-mods object) => accel-mods}
  @syntax[]{(setf (gtk:cell-renderer-accel-accel-mods object) accel-mods)}
  @argument[object]{a @class{gtk:cell-renderer-accel} object}
  @argument[accel-mode]{a @symbol{gtk:modifier-type} value}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-accel]{accel-mods} slot of the
    @class{gtk:cell-renderer-accel} class.
  @end{short}

  The modifier mask of the accelerator.
  @see-class{gtk:cell-renderer-accel}
  @see-symbol{gtk:modifier-type}")

;;; --- cell-renderer-accel-keycode --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "keycode"
                                               'cell-renderer-accel) t)
 "The @code{keycode} property of type @code{:uint} (Read / Write) @br{}
  The hardware keycode of the accelerator. Note that the hardware keycode is
  only relevant if the key does not have a keyval. Normally, the keyboard
  configuration should assign keyvals to all keys. @br{}
  Allowed values: <= @code{G_MAXINT} @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-accel-keycode)
      "Accessor"
      (documentation 'cell-renderer-accel-keycode 'function)
 "@version{#2020-6-20}
  @syntax[]{(gtk:cell-renderer-accel-keycode object) => keycode}
  @syntax[]{(setf (gtk:cell-renderer-accel-keycode object) keycode)}
  @argument[object]{a @class{gtk:cell-renderer-accel} object}
  @argument[keycode]{an unsigned integer with the hardware keycode of the
    accelerator}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-accel]{keycode} slot of the
    @class{gtk:cell-renderer-accel} class.
  @end{short}

  The hardware keycode of the accelerator. Note that the hardware keycode is
  only relevant if the key does not have a keyval. Normally, the keyboard
  configuration should assign keyvals to all keys.
  @see-class{gtk:cell-renderer-accel}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_accel_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline cell-renderer-accel-new))

(defun cell-renderer-accel-new ()
 #+liber-documentation
 "@version{#2020-6-20}
  @returns{The new @class{gtk:cell-renderer-accel} object.}
  @short{Creates a new cell renderer accel object.}
  @see-class{gtk:cell-renderer-accel}"
  (make-instance 'cell-renderer-accel))

(export 'cell-renderer-accel-new)

;;; --- gtk4.cell-renderer-accel.lisp ------------------------------------------
