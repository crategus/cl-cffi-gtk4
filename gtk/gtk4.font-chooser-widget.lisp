;;; ----------------------------------------------------------------------------
;;; gtk.font-chooser-widget.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2012 - 2022 Dieter Kaiser
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
;;; GtkFontChooserWidget
;;;
;;;     A widget for selecting fonts
;;;
;;; Types and Values
;;;
;;;     GtkFontChooserWidget
;;;
;;; Functions
;;;
;;;     gtk_font_chooser_widget_new
;;;
;;; Properties
;;;
;;;     tweak-action
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkFontChooserWidget
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkFontChooser
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFontChooserWidget
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFontChooserWidget" font-chooser-widget
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkFontChooser")
   :type-initializer "gtk_font_chooser_widget_get_type")
  ((tweak-action
    font-chooser-widget-tweak-action
    "tweak-action" "GAction" t nil)))

#+liber-documentation
(setf (documentation 'font-chooser-widget 'type)
 "@version{#2020-6-6}
  @begin{short}
    The @sym{gtk:font-chooser-widget} widget lists the available fonts, styles
    and sizes, allowing the user to select a font.
  @end{short}
  It is used in the @class{gtk:font-chooser-dialog} widget to provide a dialog
  box for selecting fonts.

  To set or to get the font which is initially selected, use the
  @fun{gtk:font-chooser-font} or @fun{gtk:font-chooser-font-desc} functions.

  To change the text which is shown in the preview area, use the
  @fun{gtk:font-chooser-preview-text} function.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:font-chooser-widget} class has a single CSS node with name
    @code{fontchooser}.
  @end{dictionary}
  @see-class{gtk:font-chooser-dialog}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- font-chooser-widget-tweak-action -----------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tweak-action"
                                               'font-chooser-widget) t)
 "The @code{tweak-action} property of type @class{g-action} (Read) @br{}
  A toggle action that can be used to switch to the tweak page of the font
  chooser widget, which lets the user tweak the OpenType features and variation
  axes of the selected font. The action will be enabled or disabled depending
  on whether the selected font has any features or axes.")

#+liber-documentation
(setf (liber:alias-for-function 'font-chooser-widget-tweak-action)
      "Accessor"
      (documentation 'font-chooser-widget-tweak-action 'function)
 "@version{#2020-6-6}
  @syntax[]{(gtk:font-chooser-widget-tweak-action object) => tweak-action}
  @syntax[]{(setf (gtk:font-chooser-widget-tweak-action object) tweak-action)}
  @argument[object]{a @class{gtk:font-chooser-widget} widget}
  @argument[tweak-action]{a @class{g-action} toggle action}
  @begin{short}
    Accessor of the @slot[gtk:font-chooser-widget]{tweak-action} slot of the
    @class{gtk:font-chooser-widget} class.
  @end{short}

  A toggle action that can be used to switch to the tweak page of the font
  chooser widget, which lets the user tweak the OpenType features and variation
  axes of the selected font. The action will be enabled or disabled depending
  on whether the selected font has any features or axes.
  @see-class{gtk:font-chooser-widget}")

;;; ----------------------------------------------------------------------------
;;; gtk_font_chooser_widget_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline font-chooser-widget-new))

(defun font-chooser-widget-new ()
 #+liber-documentation
 "@version{#2020-6-6}
  @return{A new @class{gtk:font-chooser-widget} widget.}
  @short{Creates a new font chooser widget.}
  @see-class{gtk:font-chooser-widget}"
  (make-instance 'font-chooser-widget))

(export 'font-chooser-widget-new)

;;; --- End of file gtk.font-chooser-widget.lisp -------------------------------
