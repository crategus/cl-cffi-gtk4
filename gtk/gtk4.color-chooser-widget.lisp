;;; ----------------------------------------------------------------------------
;;; gtk4.color-chooser-widget.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.12 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2025 Dieter Kaiser
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
;;; GtkColorChooserWidget
;;;
;;;     A widget for choosing colors
;;;
;;; Types and Values
;;;
;;;     GtkColorChooserWidget
;;;
;;; Functions
;;;
;;;     gtk_color_chooser_widget_new
;;;
;;; Properties
;;;
;;;     show-editor
;;;
;;; Actions
;;;
;;;     color.customize
;;;     color.select
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkColorChooserWidget
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkColorChooser
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkColorChooserWidget
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkColorChooserWidget" color-chooser-widget
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkColorChooser")
   :type-initializer "gtk_color_chooser_widget_get_type")
  ((show-editor
    color-chooser-widget-show-editor
    "show-editor" "gboolean" t t)))

#+(and gtk-4-10 gtk-warn-deprecated)
(defmethod initialize-instance :after ((obj color-chooser-widget) &key)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GTK:COLOR-CHOOSER-WIDGET is deprecated since 4.10")))

#+liber-documentation
(setf (documentation 'color-chooser-widget 'type)
 "@version{2024-05-21}
  @begin{short}
    The @class{gtk:color-chooser-widget} widget lets the user select a color.
  @end{short}
  By default, the chooser presents a prefined palette of colors, plus a small
  number of settable custom colors. It is also possible to select a different
  color with the single-color editor. To enter the single-color editing mode,
  use the context menu of any color of the palette, or use the '+' button to
  add a new custom color.

  The chooser automatically remembers the last selection, as well as custom
  colors. To change the initially selected color or to get the selected color
  use the @fun{gtk:color-chooser-rgba} function.

  The @class{gtk:color-chooser-widget} widget is used in the
  @class{gtk:color-chooser-dialog} widget to provide a dialog for selecting
  colors.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:color-chooser-widget} class has a single CSS node with name
    @code{colorchooser}.
  @end{dictionary}
  @begin[Examples]{dictionary}
    This example shows a color chooser widget in a window. The selected color
    is print on the console.
    @begin{pre}
(defun do-color-chooser-widget (&optional application)
  (let* ((color-chooser (make-instance 'gtk:color-chooser-widget
                                       :margin-top 12
                                       :margin-bottom 12
                                       :margin-start 12
                                       :margin-end 12))
         (window (make-instance 'gtk:window
                                 :application application
                                 :child color-chooser
                                 :title \"Color Chooser Widget\"
                                 :border-width 12
                                 :default-width 400)))
    (g:signal-connect color-chooser \"color-activated\"
        (lambda (chooser color)
          (declare (ignore chooser))
          (format t \"Selected color is ~a~%\" (gdk:rgba-to-string color))))
    (gtk:window-present window)))
    @end{pre}
  @end{dictionary}
  @begin[Warning]{dictionary}
    The @class{gtk:color-chooser-widget} implementation is deprecated
    since 4.10.
  @end{dictionary}
  @see-constructor{gtk:color-chooser-widget-new}
  @see-slot{gtk:color-chooser-widget-show-editor}
  @see-class{gtk:color-chooser}
  @see-class{gtk:color-chooser-dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "show-editor"
                                               'color-chooser-widget) t)
 "The @code{show-editor} property of type @code{:boolean} (Read / Write) @br{}
  @em{True} when the color chooser is showing the single-color editor. It can
  be set to switch the color chooser into single-color editing mode. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'color-chooser-widget-show-editor)
      "Accessor"
      (documentation 'color-chooser-widget-show-editor 'function)
 "@version{2025-09-29}
  @syntax{(gtk:color-chooser-widget-show-editor object) => show-editor}
  @syntax{(setf (gtk:color-chooser-widget-show-editor object) show-editor)}
  @argument[object]{a @class{gtk:color-chooser-widget} widget}
  @argument[show-editor]{a boolean whether to show the single-color editor}
  @begin{short}
    The accessor for the @slot[gtk:color-chooser-widget]{show-editor} slot of
    the @class{gtk:color-chooser-widget} class gets or sets whether the color
    chooser is showing the single-color editor.
  @end{short}
  It can be set to switch the color chooser into single-color editing mode.
  @begin[Warning]{dictionary}
    The @class{gtk:color-chooser-widget} implementation is deprecated
    since 4.10.
  @end{dictionary}
  @see-class{gtk:color-chooser-widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_color_chooser_widget_new
;;; ----------------------------------------------------------------------------

(declaim (inline color-chooser-widget-new))

(defun color-chooser-widget-new ()
 #+liber-documentation
 "@version{2024-02-22}
  @return{The new @class{gtk:color-chooser-widget} widget.}
  @short{Creates a new color chooser widget.}
  @begin[Warning]{dictionary}
    The @class{gtk:color-chooser-widget} implementation is deprecated
    since 4.10.
  @end{dictionary}
  @see-class{gtk:color-chooser-widget}"
  (make-instance 'color-chooser-widget))

(export 'color-chooser-widget-new)

;;; --- End of file gtk4.color-chooser-widget.lisp -----------------------------
