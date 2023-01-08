;;; ----------------------------------------------------------------------------
;;; gtk.stack-switcher.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2022 Dieter Kaiser
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
;;; GtkStackSwitcher
;;;
;;;     A controller for GtkStack
;;;
;;; Types and Values
;;;
;;;     GtkStackSwitcher
;;;
;;; Accessors
;;;
;;;     gtk_stack_switcher_set_stack
;;;     gtk_stack_switcher_get_stack
;;;
;;; Functions
;;;
;;;     gtk_stack_switcher_new
;;;
;;; Properties
;;;
;;;     stack
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkStackSwitcher
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkStackSwitcher
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkStackSwitcher" stack-switcher
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_stack_switcher_get_type")
  ((stack
    stack-switcher-stack
    "stack" "GtkStack" t t)))

#+liber-documentation
(setf (documentation 'stack-switcher 'type)
 "@version{#2022-2-6}
  @begin{short}
    The @sym{gtk:stack-switcher} widget acts as a controller for a
    @class{gtk:stack} widget. It shows a row of buttons to switch between
    the various pages of the associated stack widget.
  @end{short}

  @image[stack-switcher]{Figure: GtkStackSwitcher}

  All the content for the buttons comes from the child properties of the
  @class{gtk:stack} widget. The button visibility in a @sym{gtk:stack-switcher}
  widget is controlled by the visibility of the child widget in the
  @class{gtk:stack} widget.

  It is possible to associate multiple @sym{gtk:stack-switcher} widgets with
  the same @class{gtk:stack} widget.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:stack-switcher} implementation has a single CSS node named
    @code{stackswitcher} and @code{.stack-switcher} style class. When
    circumstances require it, the @sym{gtk:stack-switcher} widget adds the
    @code{.needs-attention} style class to the widgets representing the stack
    pages.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    The @sym{gtk:stack-switcher} implementation uses the @code{:tab-list} role
    and uses the @code{:tab} role of the @symbol{gtk:accessible-role}
    enumeration for its buttons.
  @end{dictionary}
  @see-slot{gtk:stack-switcher-icon-size}
  @see-slot{gtk:stack-switcher-stack}
  @see-class{gtk:stack}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- stack-switcher-stack -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "stack"
                                               'stack-switcher) t)
 "The @code{stack} property of type @class{gtk:stack} (Read / Write) @br{}
  The stack to control. @br{}")

#+liber-documentation
(setf (liber:alias-for-function 'stack-switcher-stack)
      "Accessor"
      (documentation 'stack-switcher-stack 'function)
 "@version{#2021-12-8}
  @syntax[]{(gtk:stack-switcher-stack object) => stack}
  @syntax[]{(setf (gtk:stack-switcher-stack object) stack)}
  @argument[object]{a @class{gtk:stack-switcher} widget}
  @argument[stack]{a @class{gtk:stack} widget}
  @begin{short}
    Accessor of the @slot[gtk:stack-switcher]{stack} slot of the
    @class{gtk:stack-switcher} class.
  @end{short}

  The @sym{gtk:stack-switcher-stack} function retrieves the stack. The
  @sym{(setf gtk:stack-switcher-stack)} function sets the stack to control.
  @see-class{gtk:stack-switcher}
  @see-class{gtk:stack}")

;;; ----------------------------------------------------------------------------
;;; gtk_stack_switcher_new
;;; ----------------------------------------------------------------------------

(declaim (inline stack-switcher-new))

(defun stack-switcher-new ()
 #+liber-documentation
 "@version{#2021-12-8}
  @return{The new @class{gtk:stack-switcher} widget.}
  @short{Creates a new stack switcher.}
  @see-class{gtk:stack-switcher}"
  (make-instance 'stack-switcher))

(export 'stack-switcher-new)

;;; --- gtk.stack-switcher.lisp ------------------------------------------------
