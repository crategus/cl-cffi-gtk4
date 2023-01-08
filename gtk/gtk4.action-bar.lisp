;;; ----------------------------------------------------------------------------
;;; gtk.action-bar.lisp
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
;;; GtkActionBar
;;;
;;;     A full width bar for presenting contextual actions
;;;
;;; Types and Values
;;;
;;;     GtkActionBar
;;;
;;; Accessors
;;;
;;;     gtk_action_bar_get_revealed
;;;     gtk_action_bar_set_revealed
;;;
;;; Functions
;;;
;;;     gtk_action_bar_new
;;;     gtk_action_bar_pack_start
;;;     gtk_action_bar_pack_end
;;;     gtk_action_bar_remove
;;;     gtk_action_bar_get_center_widget
;;;     gtk_action_bar_set_center_widget
;;;
;;; Properties
;;;
;;;     revealed
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkActionBar
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkActionBar
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkActionBar" action-bar
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_action_bar_get_type")
  ((revealed
    action-bar-revealed
    "revealed" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'action-bar 'type)
 "@version{#2022-2-6}
  @begin{short}
    The @sym{gtk:action-bar} widget is designed to present contextual actions.
  @end{short}
  It is expected to be displayed below the content and expand horizontally to
  fill the area.

  @image[action-bar]{Figure: GtkActionBar}

  It allows placing children at the start or the end. In addition, it contains
  an internal centered box which is centered with respect to the full width of
  the box, even if the children at either side take up different amounts of
  space.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:action-bar} implementation has a single CSS node with name
    @code{actionbar}.
  @end{dictionary}
  @see-class{gtk:box}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "revealed" 'action-bar) t)
 "The @code{revealed} property of type @code{:boolean} (Read / Write) @br{}
  Controls whether the action bar shows its contents or not. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'action-bar-revealed)
      "Accessor"
      (documentation 'action-bar-revealed 'function)
 "@version{#2022-2-6}
  @syntax[]{(gtk:action-bar-revealed object) => revealed}
  @syntax[]{(setf (gtk:action-bar-revealed object) revealed)}
  @argument[object]{a @class{gtk:action-bar} widget}
  @argument[position]{a boolean whether the action bar shows its contents}
  @begin{short}
    Accessor of the @slot[gtk:action-bar]{revealed} slot of the
    @class{gtk:action-bar} class.
  @end{short}

  Changing the @slot[gtk:action-bar]{revealed} property will make the action bar
  reveal (@em{true}) or conceal (@em{false}) itself via a sliding transition.
  @begin[Note]{dictionary}
    This does not show or hide the action bar in the
    @slot[widget]{visible} property sense, so revealing has no effect if
    the @slot[widget]{visible} property is @em{false}.
  @end{dictionary}
  @see-class{gtk:action-bar}
  @see-function{gtk:widget-visible}")

;;; ----------------------------------------------------------------------------
;;; gtk_action_bar_new
;;; ----------------------------------------------------------------------------

(declaim (inline action-bar-new))

(defun action-bar-new ()
 #+liber-documentation
 "@version{#2021-12-8}
  @return{The new @class{gtk:action-bar} widget.}
  @short{Creates a new action bar.}
  @see-class{gtk:action-bar}"
  (make-instance 'action-bar))

(export 'action-bar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_action_bar_pack_start
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_bar_pack_start" action-bar-pack-start) :void
 #+liber-documentation
 "@version{#2021-12-17}
  @argument[actionbar]{a @class{gtk:action-bar} widget}
  @argument[child]{a @class{gtk:widget} child widget to be added to
  @arg{actionbar}}
  @begin{short}
    Adds the child widget to the action bar, packed with reference to the start
    of the action bar.
  @end{short}
  @see-class{gtk:action-bar}
  @see-class{gtk:widget}"
  (actionbar (g:object action-bar))
  (child (g:object widget)))

(export 'action-bar-pack-start)

;;; ----------------------------------------------------------------------------
;;; gtk_action_bar_pack_end
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_bar_pack_end" action-bar-pack-end) :void
 #+liber-documentation
 "@version{#2021-12-8}
  @argument[actionbar]{a @class{gtk:action-bar} widget}
  @argument[child]{a @class{gtk:widget} child widget to be added to
    @arg{actionbar}}
  @begin{short}
    Adds the child widget to the action bar, packed with reference to the end
    of the action bar.
  @end{short}
  @see-class{gtk:action-bar}
  @see-class{gtk:widget}"
  (actionbar (g:object action-bar))
  (child (g:object widget)))

(export 'action-bar-pack-end)

;;; ----------------------------------------------------------------------------
;;;gtk_action_bar_remove
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_action_bar_remove" action-bar-remove) :void
 #+liber-documentation
 "@version{#2022-2-6}
  @argument[actionbar]{a @class{gtk:action-bar} widget}
  @argument[child]{a @class{gtk:widget} child widget to be removed}
  @short{Removes a child widget from the action bar.}
  @see-class{gtk:action-bar}
  @see-class{gtk:widget}"
  (actionbar (g:object action-bar))
  (child (g:object widget)))

(export 'action-bar-remove)

;;; ----------------------------------------------------------------------------
;;; gtk_action_bar_get_center_widget
;;; gtk_action_bar_set_center_widget -> action-bar-center-widget
;;; ----------------------------------------------------------------------------

(defun (setf action-bar-center-widget) (widget actionbar)
  (foreign-funcall "gtk_action_bar_set_center_widget"
                   (g:object action-bar) actionbar
                   (g:object widget) widget
                   :void)
  widget)

(defcfun ("gtk_action_bar_get_center_widget" action-bar-center-widget)
    (g:object widget)
 #+liber-documentation
 "@version{#2021-12-8}
  @syntax[]{(gtk:action-bar-center-widget actionbar) => widget}
  @syntax[]{(setf (gtk:action-bar-center-widget actionbar) widget)}
  @argument[actionbar]{a @class{gtk:action-bar} widget}
  @argument[widget]{a @class{gtk:widget} object to use for the center widget}
  @begin{short}
    Accessor of the center widget of the action bar.
  @end{short}

  The @sym{gtk:action-bar-center-widget} function retrieves the center widget of
  the action bar. The @sym{(setf gtk:action-bar-center-widget)} function sets
  the center widget for the action bar.
  @see-class{gtk:action-bar}
  @see-class{gtk:widget}"
  (actionbar (g:object action-bar)))

(export 'action-bar-center-widget)

;;; --- End of file gtk.action-bar.lisp ----------------------------------------
