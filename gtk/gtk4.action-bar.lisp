;;; ----------------------------------------------------------------------------
;;; gtk4.action-bar.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2025 Dieter Kaiser
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

(gobject:define-gobject "GtkActionBar" action-bar
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
 "@version{2025-04-24}
  @begin{short}
    The @class{gtk:action-bar} widget is designed to present contextual actions.
  @end{short}
  It is expected to be displayed below the content and expand horizontally to
  fill the area.

  @image[action-bar]{Figure: GtkActionBar}

  It allows placing children at the start or the end. In addition, it contains
  an internal centered box which is centered with respect to the full width of
  the box, even if the children at either side take up different amounts of
  space.
  @begin[GtkActionBar as GtkBuildable]{dictionary}
    The @class{gtk:action-bar} implementation of the @class{gtk:buildable}
    interface supports adding children at the start or end sides by specifying
    @code{\"start\"} or @code{\"end\"} as the @code{\"type\"} attribute of a
    @code{<child>} element, or setting the center widget by specifying
    @code{\"center\"} value.
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
actionbar
╰── revealer
    ╰── box
        ├── box.start
        │   ╰── [start children]
        ├── [center widget]
        ╰── box.end
            ╰── [end children]
    @end{pre}
    The @class{gtk:action-bar} implementation has a single CSS node with name
    @code{actionbar}. It contains a revealer subnode, which contains a box
    subnode, which contains two box subnodes at the start and end of the action
    bar, with @code{start} and @code{end} style classes respectively, as well
    as a center node that represents the center child. Each of the boxes
    contains children packed for that side.
  @end{dictionary}
  @see-constructor{gtk:action-bar-new}
  @see-slot{gtk:action-bar-revealed}
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
 "@version{2025-04-24}
  @syntax{(gtk:action-bar-revealed object) => revealed}
  @syntax{(setf (gtk:action-bar-revealed object) revealed)}
  @argument[object]{a @class{gtk:action-bar} widget}
  @argument[revealed]{a boolean whether the action bar shows its contents}
  @begin{short}
    Accessor of the @slot[gtk:action-bar]{revealed} slot of the
    @class{gtk:action-bar} class.
  @end{short}
  Changing the @slot[gtk:action-bar]{revealed} property will make the action bar
  reveal (@em{true}) or conceal (@em{false}) itself via a sliding transition.
  @begin[Notes]{dictionary}
    This does not show or hide the action bar in the
    @slot[gtk:widget]{visible} property sense, so revealing has no effect if
    the @slot[gtk:widget]{visible} property is @em{false}.
  @end{dictionary}
  @see-class{gtk:action-bar}
  @see-function{gtk:widget-visible}")

;;; ----------------------------------------------------------------------------
;;; gtk_action_bar_new
;;; ----------------------------------------------------------------------------

(declaim (inline action-bar-new))

(defun action-bar-new ()
 #+liber-documentation
 "@version{2025-04-24}
  @return{The new @class{gtk:action-bar} widget.}
  @short{Creates a new action bar.}
  @see-class{gtk:action-bar}"
  (make-instance 'action-bar))

(export 'action-bar-new)

;;; ----------------------------------------------------------------------------
;;; gtk_action_bar_pack_start
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_action_bar_pack_start" action-bar-pack-start) :void
 #+liber-documentation
 "@version{2025-04-24}
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

(cffi:defcfun ("gtk_action_bar_pack_end" action-bar-pack-end) :void
 #+liber-documentation
 "@version{2025-04-24}
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

(cffi:defcfun ("gtk_action_bar_remove" action-bar-remove) :void
 #+liber-documentation
 "@version{2025-04-24}
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
;;; gtk_action_bar_set_center_widget
;;; ----------------------------------------------------------------------------

(defun (setf action-bar-center-widget) (widget actionbar)
  (cffi:foreign-funcall "gtk_action_bar_set_center_widget"
                        (g:object action-bar) actionbar
                        (g:object widget) widget
                        :void)
  widget)

(cffi:defcfun ("gtk_action_bar_get_center_widget" action-bar-center-widget)
    (g:object widget)
 #+liber-documentation
 "@version{2025-04-24}
  @syntax{(gtk:action-bar-center-widget actionbar) => widget}
  @syntax{(setf (gtk:action-bar-center-widget actionbar) widget)}
  @argument[actionbar]{a @class{gtk:action-bar} widget}
  @argument[widget]{a @class{gtk:widget} object to use for the center widget}
  @begin{short}
    The @fun{gtk:action-bar-center-widget} function retrieves the center widget
    of the action bar.
  @end{short}
  The @setf{gtk:action-bar-center-widget} function sets the center widget.
  @see-class{gtk:action-bar}
  @see-class{gtk:widget}"
  (actionbar (g:object action-bar)))

(export 'action-bar-center-widget)

;;; --- End of file gtk4.action-bar.lisp ---------------------------------------
