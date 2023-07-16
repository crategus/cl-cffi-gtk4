;;; ----------------------------------------------------------------------------
;;; gtk4.root.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2023 Dieter Kaiser
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

(gobject:define-g-interface "GtkRoot" root
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

(cffi:defcfun ("gtk_root_get_display" root-display) (g:object gdk:display)
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
  (cffi:foreign-funcall "gtk_root_set_focus"
                        (g:object root) root
                        (g:object widget) value
                        :void)
  value)

(cffi:defcfun ("gtk_root_get_focus" root-focus) (g:object widget)
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

;;; --- End of file gtk4.root.lisp ---------------------------------------------
