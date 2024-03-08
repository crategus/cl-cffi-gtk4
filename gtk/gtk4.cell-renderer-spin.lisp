;;; ----------------------------------------------------------------------------
;;; gtk4.cell-renderer-spin.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
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
;;; GtkCellRendererSpin
;;;
;;;     Renders a spin button in a cell
;;;
;;; Types and Values
;;;
;;;     GtkCellRendererSpin
;;;
;;; Functions
;;;
;;;     gtk_cell_renderer_spin_new
;;;
;;; Properties
;;;
;;;     adjustment
;;;     climb-rate
;;;     digits
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkCellRenderer
;;;             ╰── GtkCellRendererText
;;;                 ╰── GtkCellRendererSpin
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCellRendererSpin
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkCellRendererSpin" cell-renderer-spin
  (:superclass cell-renderer-text
    :export t
    :interfaces nil
    :type-initializer "gtk_cell_renderer_spin_get_type")
  ((adjustment
    cell-renderer-spin-adjustment
    "adjustment" "GtkAdjustment" t t)
   (climb-rate
    cell-renderer-spin-climb-rate
    "climb-rate" "gdouble" t t)
   (digits
    cell-renderer-spin-digits
    "digits" "guint" t t)))

#+liber-documentation
(setf (documentation 'cell-renderer-spin 'type)
 "@version{2024-2-22}
  @begin{short}
    The @class{gtk:cell-renderer-spin} object renders text in a cell like
    @class{gtk:cell-renderer-text} object from which it is derived.
  @end{short}
  But while the @class{gtk:cell-renderer-text} object offers a simple entry to
  edit the text, the @class{gtk:cell-renderer-spin} object offers a
  @class{gtk:spin-button} widget. Of course, that means that the text has to be
  parseable as a floating point number.

  The range of the spin button is taken from the
  @slot[gtk:cell-renderer-spin]{adjustment} property of the cell renderer, which
  can be set explicitly or mapped to a column in the tree model, like all
  properties of cell renders. The @class{gtk:cell-renderer-spin} object also has
  the @slot[gtk:cell-renderer-spin]{climb-rate} and
  @slot[gtk:cell-renderer-spin]{digits} properties. Other
  @class{gtk:spin-button} properties can be set in a handler for the
  @code{\"editing-started\"} signal.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-spin} object is deprecated since 4.10. List
    views use widgets to display their contents. You should use the
    @class{gtk:spin-button} widget instead.
  @end{dictionary}
  @see-constructor{gtk:cell-renderer-spin-new}
  @see-slot{gtk:cell-renderer-spin-adjustment}
  @see-slot{gtk:cell-renderer-spin-climb-rate}
  @see-slot{gtk:cell-renderer-spin-digits}
  @see-class{gtk:cell-renderer-text}
  @see-class{gtk:spin-button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:cell-renderer-spin-adjustment --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "adjustment"
                                               'cell-renderer-spin) t)
 "The @code{adjustment} property of type @class{gtk:adjustment} (Read / Write)
  @br{}
  The adjustment that holds the value of the spin button. This must be
  non-@code{nil} for the cell renderer to be editable.")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-spin-adjustment)
      "Accessor"
      (documentation 'cell-renderer-spin-adjustment 'function)
 "@version{2024-2-22}
  @syntax[]{(gtk:cell-renderer-spin-adjustment object) => adjustment}
  @syntax[]{(setf (gtk:cell-renderer-spin-adjustment object) adjustment)}
  @argument[object]{a @class{gtk:cell-renderer-spin} object}
  @argument[adjustment]{a @class{gtk:adjustment} object}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-spin]{adjustment} slot of the
    @class{gtk:cell-renderer-spin} class.
  @end{short}
  The adjustment that holds the value of the spin button. This must be
  non-@code{nil} for the cell renderer to be editable.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-spin} object is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-spin}
  @see-class{gtk:adjustment}")

;;; --- gtk:cell-renderer-spin-climb-rate --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "climb-rate"
                                               'cell-renderer-spin) t)
 "The @code{climb-rate} property of type @code{:double} (Read / Write) @br{}
  The acceleration rate when you hold down a button. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-spin-climb-rate)
      "Accessor"
      (documentation 'cell-renderer-spin-climb-rate 'function)
 "@version{2024-2-22}
  @syntax[]{(gtk:cell-renderer-spin-climb-rate object) => climb-rate}
  @syntax[]{(setf (gtk:cell-renderer-spin-climb-rate object) climb-rate)}
  @argument[object]{a @class{gtk:cell-renderer-spin} object}
  @argument[climb-rate]{a double float with the acceleration rate}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-spin]{climb-rate} slot of the
    @class{gtk:cell-renderer-spin} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-spin} object is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-spin}")

;;; --- gtk:cell-renderer-spin-digits ------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "digits"
                                               'cell-renderer-spin) t)
 "The @code{digits} property of type @code{:uint} (Read / Write) @br{}
  The number of decimal places to display. @br{}
  Allowed values: <= 20 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-spin-digits)
      "Accessor"
      (documentation 'cell-renderer-spin-digits 'function)
 "@version{2024-2-22}
  @syntax[]{(gtk:cell-renderer-spin-digits object) => digits}
  @syntax[]{(setf (gtk:cell-renderer-spin-digits object) digits)}
  @argument[object]{a @class{gtk:cell-renderer-spin} object}
  @argument[digits]{an unsigned integer with the number of decimal places to
    display}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-spin]{digits} slot of the
    @class{gtk:cell-renderer-spin} class.
  @end{short}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-spin} object is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-spin}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_spin_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline cell-renderer-spin-new))

(defun cell-renderer-spin-new ()
 #+liber-documentation
 "@version{2024-2-22}
  @return{The new @class{gtk:cell-renderer-spin} object.}
  @short{Creates a new cell renderer spin object.}
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-spin} object is deprecated since 4.10.
    Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-spin}"
  (make-instance 'cell-renderer-spin))

(export 'cell-renderer-spin-new)

;;; --- End of file gtk4.cell-renderer-spin.lisp -------------------------------
