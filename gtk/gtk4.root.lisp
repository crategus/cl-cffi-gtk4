;;; ----------------------------------------------------------------------------
;;; gtk.root.lisp
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
;;; GtkRoot
;;;
;;;     Interface for root widgets
;;;
;;; Types and Values
;;;
;;;     GtkRoot
;;;
;;; Functions
;;;
;;;     gtk_root_get_display
;;;     gtk_root_get_focus
;;;     gtk_root_set_focus
;;;
;;; Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkRoot
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkRoot
;;; ----------------------------------------------------------------------------

(define-g-interface "GtkRoot" root
  (:export t
   :type-initializer "gtk_root_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'root)
      "Interface"
      (documentation 'root 'type)
 "@version{#2022-7-11}
  @begin{short}
    The @sym{gtk:root} interface is the interface implemented by all widgets
    that can act as a toplevel widget to a hierarchy of widgets.
  @end{short}
  The root widget takes care of providing the connection to the windowing system
  and manages layout, drawing and event delivery for its widget hierarchy. The
  obvious example of a @sym{gtk:root} widget is the @class{gtk:window} widget.
  @see-class{gtk:window}")

;;; ----------------------------------------------------------------------------
;;; gtk_root_get_display -> root-display
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_root_get_display" root-display) (g:object gdk:display)
 #+liber-documentation
 "@version{#2022-7-11}
  @argument[root]{a @class{gtk:root} widget}
  @return{The @class{gdk:display} object of @arg{root}.}
  @begin{short}
    Returns the display that the root widget is on.
  @end{short}
  @see-class{gtk:root}
  @see-class{gdk:display}"
  (root (g:object root)))

(export 'root-display)

;;; ----------------------------------------------------------------------------
;;; gtk_root_get_focus
;;; gtk_root_set_focus -> root-focus
;;; ----------------------------------------------------------------------------

(defun (setf root-focus) (value root)
  (foreign-funcall "gtk_root_set_focus"
                   (g:object root) root
                   (g:object widget) value
                   :void)
  value)

(defcfun ("gtk_root_get_focus" root-focus) (g:object widget)
 #+liber-documentation
 "@version{#2022-7-11}
  @syntax[]{(gtk:root-focus root) => widget}
  @syntax[]{(setf (gtk:root-focus root) widget)}
  @argument[root]{a @class{gtk:root} widget}
  @argument[widget]{a @class{gtk:widget} focus widget, or @code{nil} if there
    is none}
  @begin{short}
    The accessor function for the focus widget of the root widget.
  @end{short}
  The @sym{gtk:root-focus} function retrieves the current focused widget within
  the root widget. Note that this is the widget that would have the focus if
  the root widget is active. If the root widget is not focused then the
  @fun{gtk:widget-has-focus} function will return @em{false} for the widget.

  If the @arg{widget} argument is not the current focus widget, and is
  focusable, the @sym{(setf gtk:root-focus)} function sets it as the focus
  widget for the root widget. If the @arg{widget} argument is @code{nil},
  unsets the focus widget for the root widget.

  To set the focus to a particular widget in the root widget, it is usually
  more convenient to use the @fun{gtk:widget-grab-focus} function instead of
  this function.
  @see-class{gtk:root}
  @see-class{gtk:widget}
  @see-function{gtk:widget-has-focus}
  @see-function{gtk:widget-grab-focus}"
  (root (g:object widget)))

(export 'root-focus)

;;; --- End of file gtk.root.lisp ----------------------------------------------
