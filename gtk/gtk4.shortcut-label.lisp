;;; ----------------------------------------------------------------------------
;;; gtk4.shortcut-label.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2024 Dieter Kaiser
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
;;; GtkShortcutLabel
;;;
;;;     Displays a keyboard shortcut
;;;
;;; Types and Values
;;;
;;;     GtkShortcutLabel
;;;
;;; Accessors
;;;
;;;     gtk_shortcut_label_get_accelerator
;;;     gtk_shortcut_label_set_accelerator
;;;     gtk_shortcut_label_get_disabled_text
;;;     gtk_shortcut_label_set_disabled_text
;;;
;;; Functions
;;;
;;;     gtk_shortcut_label_new
;;;
;;; Properties
;;;
;;;     accelerator
;;;     disabled-text
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkShortcutLabel
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkShortcutLabel
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkShortcutLabel" shortcut-label
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_shortcut_label_get_type")
  ((accelerator
    shortcut-label-accelerator
    "accelerator" "gchararray" t t)
   (disabled-text
    shortcut-label-disabled-text
    "disabled-text" "gchararray" t t)))

#+liber-documentation
(setf (documentation 'shortcut-label 'type)
 "@version{2024-10-27}
  @begin{short}
    The @class{gtk:shortcut-label} widget is a widget that represents a single
    keyboard shortcut or gesture in the user interface.
  @end{short}
  @see-slot{gtk:shortcut-label-accelerator}
  @see-slot{gtk:shortcut-label-disabled-text}
  @see-constructor{gtk:shortcut-label-new}
  @see-class{gtk:cell-renderer-accel}")

;;; ----------------------------------------------------------------------------
;;; Property and Accesor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:shortcut-label-accelerator -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accelerator" 'shortcut-label) t)
 "The @code{accelerator} property of type @code{:string} (Read / Write) @br{}
  The accelerator that the shortcut label displays. See the
  @slot[gtk:shortcuts-shortcut]{accelerator} property for the accepted syntax.
  @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'shortcut-label-accelerator)
      "Accessor"
      (documentation 'shortcut-label-accelerator 'function)
 "@version{2024-10-27}
  @syntax{(gtk:shortcut-label-accelerator object) => accelerator}
  @syntax{(setf (gtk:shortcut-label-accelerator object) accelerator)}
  @argument[object]{a @class{gtk:shortcut-label} widget}
  @argument[accelerator]{a string with the accelerator}
  @begin{short}
    Accessor of the @slot[gtk:shortcut-label]{accelerator} slot of the
    @class{gtk:shortcut-label} class.
  @end{short}
  The @fun{gtk:shortcut-label-accelerator} function retrieves the current
  accelerator of the shortcut label. The
  @setf{gtk:shortcut-label-accelerator} function sets the accelerator.
  @see-class{gtk:shortcut-label}")

;;; --- gtk:shortcut-label-disabled-text ---------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "disabled-text"
                                               'shortcut-label) t)
 "The @code{disabled-text} property of type @code{:string} (Read / Write) @br{}
  The text that is displayed when no accelerator is set. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'shortcut-label-disabled-text)
      "Accessor"
      (documentation 'shortcut-label-disabled-text 'function)
 "@version{2024-10-27}
  @syntax{(gtk:shortcut-label- object) => disabled-text}
  @syntax{(setf (gtk:shortcut-label-accelerator object) disabled-text)}
  @argument[object]{a @class{gtk:shortcut-label} widget}
  @argument[accelerator]{a string with text to be displayed when no accelerator
    is set}
  @begin{short}
    Accessor of the @slot[gtk:shortcut-label]{accelerator} slot of the
    @class{gtk:shortcut-label} class.
  @end{short}
  The @fun{gtk:shortcut-label-disabled-text} function retrieves the text that
  is displayed when no accelerator is set. The
  @setf{gtk:shortcut-label-disabled-text} function sets the text.
  @see-class{gtk:shortcut-label}")

;;; ----------------------------------------------------------------------------
;;; gtk_shortcut_label_new
;;; ----------------------------------------------------------------------------

(defun shortcut-label-new (accelerator)
 #+liber-documentation
 "@version{2024-10-27}
  @argument[accelerator]{a string with the initial accelerator}
  @return{The newly @class{gtk:shortcut-label} widget.}
  @begin{short}
    Creates a new shortcut label with @arg{accelerator} set.
  @end{short}
  @see-class{gtk:shortcut-label}"
  (make-instance 'shortcut-label
                 :accelerator accelerator))

(export 'shortcut-label-new)

;;; --- End of file gtk4.shortcut-label.lisp -----------------------------------
