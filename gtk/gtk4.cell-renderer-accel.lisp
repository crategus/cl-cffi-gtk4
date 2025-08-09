;;; ----------------------------------------------------------------------------
;;; gtk4.cell-renderer-accel.lisp
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
;;; GtkCellRendererAccelMode
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GtkCellRendererAccelMode" cell-renderer-accel-mode
  (:export t
   :type-initializer "gtk_cell_renderer_accel_mode_get_type")
  (:gtk 0)
  (:other 1))

#+liber-documentation
(setf (liber:alias-for-symbol 'cell-renderer-accel-mode)
      "GEnum"
      (liber:symbol-documentation 'cell-renderer-accel-mode)
 "@version{2025-07-22}
  @begin{declaration}
(gobject:define-genum \"GtkCellRendererAccelMode\" cell-renderer-accel-mode
  (:export t
   :type-initializer \"gtk_cell_renderer_accel_mode_get_type\")
  (:gtk 0)
  (:other 1))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:gtk]{GTK accelerators mode.}
      @entry[:other]{Other accelerator mode.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Determines if the edited accelerators are GTK accelerators.
  @end{short}
  If they are, consumed modifiers are suppressed, only accelerators accepted by
  GTK are allowed, and the accelerators are rendered in the same way as they
  are in menus.
  @see-class{gtk:cell-renderer-accel}")

;;; ----------------------------------------------------------------------------
;;; GtkCellRendererAccel
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkCellRendererAccel" cell-renderer-accel
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
 "@version{2025-07-22}
  @begin{short}
    The @class{gtk:cell-renderer-accel} object displays a keyboard accelerator,
    that is a key combination like @kbd{Control+a}.
  @end{short}
  If the cell renderer is editable, the accelerator can be changed by simply
  typing the new combination.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-accel} implementation is deprecated since 4.10.
    Applications editing keyboard accelerators should provide their own
    implementation according to platform design guidelines.
  @end{dictionary}
  @begin[Signal Details]{dictionary}
    @begin[cell-renderer-accel::accel-cleared]{signal}
      @begin{pre}
lambda (accel path)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[accel]{The @class{gtk:cell-renderer-accel} object reveiving the
          signal.}
        @entry[path]{The string for the path identifying the row of the edited
          cell.}
      @end{simple-table}
      Gets emitted when the user has removed the accelerator.
    @end{signal}
    @begin[cell-renderer-accel::accel-edited]{signal}
      @begin{pre}
lambda (accel path key mods keycode)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[accel]{The @class{gtk:cell-renderer-accel} object reveiving the
          signal.}
        @entry[path]{The string for the path identifying the row of the edited
          cell.}
        @entry[key]{The unsigned integer for the new accelerator keyval.}
        @entry[mods]{The @class{gdk:modifier-type} value for the new
          acclerator modifier mask.}
        @entry[keycode]{The unsigned integer for the keycode of the new
          accelerator.}
      @end{simple-table}
      Gets emitted when the user has selected a new accelerator.
    @end{signal}
  @end{dictionary}
  @see-constructor{gtk:cell-renderer-accel-new}
  @see-slot{gtk:cell-renderer-accel-accel-key}
  @see-slot{gtk:cell-renderer-accel-accel-mode}
  @see-slot{gtk:cell-renderer-accel-accel-mods}
  @see-slot{gtk:cell-renderer-accel-keycode}
  @see-class{gtk:cell-renderer}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:cell-renderer-accel-accel-key --------------------------------------

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
 "@version{2025-07-22}
  @syntax{(gtk:cell-renderer-accel-accel-key object) => key}
  @syntax{(setf (gtk:cell-renderer-accel-accel-key object) key)}
  @argument[object]{a @class{gtk:cell-renderer-accel} object}
  @argument[key]{an unsigned integer for the keyval of the accelerator}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer]{accel-key} slot of the
    @class{gtk:cell-renderer-accel} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-accel} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-accel}")

;;; --- gtk:cell-renderer-accel-accel-mode -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accel-mode"
                                               'cell-renderer-accel) t)
 "The @code{accel-mode} property of type @sym{gtk:cell-renderer-accel-mode}
  (Read / Write) @br{}
  Determines if the edited accelerators are GTK accelerators. If they are,
  consumed modifiers are suppressed, only accelerators accepted by GTK are
  allowed, and the accelerators are rendered in the same way as they are in
  menus. @br{}
  Default value: @val[gtk:cell-renderer-accel-mode]{:gtk}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-accel-accel-mode)
      "Accessor"
      (documentation 'cell-renderer-accel-accel-mode 'function)
 "@version{2025-07-22}
  @syntax{(gtk:cell-renderer-accel-accel-mode object) => mode}
  @syntax{(setf (gtk:cell-renderer-accel-accel-mode object) mode)}
  @argument[object]{a @class{gtk:cell-renderer-accel} object}
  @argument[mode]{a @sym{gtk:cell-renderer-accel-mode} value}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-accel]{accel-mode} slot of the
    @class{gtk:cell-renderer-accel} class.
  @end{short}
  Determines if the edited accelerators are GTK accelerators. If they are,
  consumed modifiers are suppressed, only accelerators accepted by GTK are
  allowed, and the accelerators are rendered in the same way as they are in
  menus.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-accel} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-accel}
  @see-symbol{gtk:cell-renderer-accel-mode}")

;;; --- gtk:cell-renderer-accel-accel-mods -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accel-mods"
                                               'cell-renderer-accel) t)
 "The @code{accel-mods} property of type @sym{gdk:modifier-type} (Read / Write)
  @br{}
  The modifier mask of the accelerator.")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-accel-accel-mods)
      "Accessor"
      (documentation 'cell-renderer-accel-accel-mods 'function)
 "@version{2025-07-22}
  @syntax{(gtk:cell-renderer-accel-accel-mods object) => accel-mods}
  @syntax{(setf (gtk:cell-renderer-accel-accel-mods object) accel-mods)}
  @argument[object]{a @class{gtk:cell-renderer-accel} object}
  @argument[accel-mods]{a @sym{gdk:modifier-type} value for the modifier mask
    of the accelerator}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-accel]{accel-mods} slot of the
    @class{gtk:cell-renderer-accel} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-accel} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-accel}
  @see-symbol{gdk:modifier-type}")

;;; --- gtk:cell-renderer-accel-keycode ----------------------------------------

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
 "@version{2025-07-22}
  @syntax{(gtk:cell-renderer-accel-keycode object) => keycode}
  @syntax{(setf (gtk:cell-renderer-accel-keycode object) keycode)}
  @argument[object]{a @class{gtk:cell-renderer-accel} object}
  @argument[keycode]{an unsigned integer for the hardware keycode of the
    accelerator}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-accel]{keycode} slot of the
    @class{gtk:cell-renderer-accel} class.
  @end{short}
  The hardware keycode of the accelerator. Note that the hardware keycode is
  only relevant if the key does not have a keyval. Normally, the keyboard
  configuration should assign keyvals to all keys.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-accel} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-accel}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_accel_new
;;; ----------------------------------------------------------------------------

(declaim (inline cell-renderer-accel-new))

(defun cell-renderer-accel-new ()
 #+liber-documentation
 "@version{2024-02-21}
  @return{The new @class{gtk:cell-renderer-accel} object.}
  @short{Creates a new cell renderer accel object.}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-accel} implementation is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-accel}"
  (make-instance 'cell-renderer-accel))

(export 'cell-renderer-accel-new)

;;; --- gtk4.cell-renderer-accel.lisp ------------------------------------------
