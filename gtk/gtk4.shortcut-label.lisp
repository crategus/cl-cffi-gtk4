;;; ----------------------------------------------------------------------------
;;; gtk.shortcut-label.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 Dieter Kaiser
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

(define-g-object-class "GtkShortcutLabel" shortcut-label
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
 "@version{#2022-7-12}
  @begin{short}
    A @sym{gtk:shortcut-label} widget is a widget that represents a single
    keyboard shortcut or gesture in the user interface.
  @end{short}
  @see-slot{gtk:shortcut-label-accelerator}
  @see-slot{gtk:shortcut-label-disabled-text}
  @see-constructor{gtk:shortcut-label-new}
  @see-class{gtk:cell-renderer-accel}")

;;; ----------------------------------------------------------------------------
;;; Property and Accesor Details
;;; ----------------------------------------------------------------------------

;;; --- shortcut-label-accelerator -----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "accelerator"
                                               'shortcut-label) t)
 "The @code{accelerator} property of type @code{:string} (Read / Write) @br{}
  The accelerator that the shortcut label displays. See the
  @slot[gtk:shortcuts-shortcut]{accelerator} property for the accepted syntax.
  @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'shortcut-label-accelerator)
      "Accessor"
      (documentation 'shortcut-label-accelerator 'function)
 "@version{#2022-7-24}
  @syntax[]{(gtk:shortcut-label-accelerator object) => accelerator}
  @syntax[]{(setf (gtk:shortcut-label-accelerator object) accelerator)}
  @argument[object]{a @class{gtk:shortcut-label} widget}
  @argument[accelerator]{a string with the accelerator}
  @begin{short}
    Accessor of the @slot[gtk:shortcut-label]{accelerator} slot of the
    @class{gtk:shortcut-label} class.
  @end{short}
  The @sym{gtk:shortcut-label-accelerator} function retrieves the current
  accelerator of the shortcut label. The
  @sym{(setf gtk:shortcut-label-accelerator)} function sets the accelerator.
  @see-class{gtk:shortcut-label}")

;;; --- shortcut-label-disabled-text ---------------------------------------

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
 "@version{#2022-7-24}
  @syntax[]{(gtk:shortcut-label- object) => disabled-text}
  @syntax[]{(setf (gtk:shortcut-label-accelerator object) disabled-text)}
  @argument[object]{a @class{gtk:shortcut-label} widget}
  @argument[accelerator]{a string with text to be displayed when no accelerator
    is set}
  @begin{short}
    Accessor of the @slot[gtk:shortcut-label]{accelerator} slot of the
    @class{gtk:shortcut-label} class.
  @end{short}
  The @sym{gtk:shortcut-label-disabled-text} function retrieves the text that
  is displayed when no accelerator is set. The
  @sym{(setf gtk:shortcut-label-disabled-text)} function sets the text.
  @see-class{gtk:shortcut-label}")

;;; ----------------------------------------------------------------------------
;;; gtk_shortcut_label_new ()
;;; ----------------------------------------------------------------------------

(defun shortcut-label-new (accelerator)
 #+liber-documentation
 "@version{#2022-7-24}
  @argument[accelerator]{a string with the initial accelerator}
  @return{A newly @class{gtk:shortcut-label} widget.}
  @begin{short}
    Creates a new shortcut label with @arg{accelerator} set.
  @end{short}
  @see-class{gtk:shortcut-label}"
  (make-instance 'shortcut-label
                 :accelerator accelerator))

(export 'shortcut-label-new)

;;; --- End of file gtk.shortcut-label.lisp ------------------------------------
