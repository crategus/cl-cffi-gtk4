;;; ----------------------------------------------------------------------------
;;; gtk4.tooltip.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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
;;; GtkTooltip
;;;
;;;     Add tips to your widgets
;;;
;;; Types and Values
;;;
;;;     GtkTooltip
;;;
;;; Functions
;;;
;;;     gtk_tooltip_set_markup
;;;     gtk_tooltip_set_text
;;;     gtk_tooltip_set_icon
;;;     gtk_tooltip_set_icon_from_icon_name
;;;     gtk_tooltip_set_icon_from_gicon
;;;     gtk_tooltip_set_custom
;;;     gtk_tooltip_set_tip_area
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkTooltip
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkTooltip
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GtkTooltip" tooltip
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gtk_tooltip_get_type")
  nil)

(setf (documentation 'tooltip 'type)
 "@version{#2020-6-3}
  @short{Add tips to your widgets.}

  Basic tooltips can be realized simply by using the functions
  @fun{gtk:widget-tooltip-text} or @fun{gtk:widget-tooltip-markup}
  without any explicit tooltip object.

  When you need a tooltip with a little more fancy contents, like adding an
  image, or you want the tooltip to have different contents per
  @class{gtk:tree-view} row or cell, you will have to do a little more work:
  @begin{itemize}
    @begin{item}
      Set the @slot[widget]{has-tooltip} property to @em{true}, this will
      make GTK monitor the widget for motion and related events which are
      needed to determine when and where to show a tooltip.
    @end{item}
    @begin{item}
      Connect to the \"query-tooltip\" signal. The signal will be emitted when
      a tooltip is supposed to be shown. One of the arguments passed to the
      signal handler is a @sym{gtk:tooltip} object. This is the object that we
      are about to display as a tooltip, and can be manipulated in your callback
      function using functions like the @fun{gtk:tooltip-set-icon} function.
      There are functions for setting the markup of the tooltip, setting an
      image from a stock icon, or even putting in a custom widget.
    @end{item}
    @begin{item}
      Return @em{true} from your query-tooltip handler. This causes the tooltip
      to be show. If you return @em{false}, it will not be shown.
    @end{item}
  @end{itemize}
  @see-function{gtk:widget-tooltip-text}
  @see-function{gtk:widget-tooltip-markup}
  @see-function{gtk:tooltip-set-icon}")

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_markup ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tooltip_set_markup" tooltip-set-markup) :void
 #+liber-documentation
 "@version{#2020-6-3}
  @argument[tooltip]{a @class{gtk:tooltip} object}
  @argument[markup]{a markup string, see Pango markup format, or @code{nil}}
  @begin{short}
    Sets the text of the tooltip to be @arg{markup}, which is marked up
    with the Pango text markup language.
  @end{short}
  If @arg{markup} is @code{nil}, the label will be hidden.
  @see-class{gtk:tooltip}
  @see-function{gtk:tooltip-set-text}"
  (tooltip (g:object tooltip))
  (markup :string))

(export 'tooltip-set-markup)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_text ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tooltip_set_text" tooltip-set-text) :void
 #+liber-documentation
 "@version{#2020-6-3}
  @argument[tooltip]{a @class{gtk:tooltip} object}
  @argument[text]{a text string or @code{nil}}
  @begin{short}
    Sets the text of the tooltip to be @arg{text}.
  @end{short}
  If @arg{text} is @code{nil}, the label will be hidden. See also the function
  @fun{gtk:tooltip-set-markup}.
  @see-class{gtk:tooltip}
  @see-function{gtk:tooltip-set-markup}"
  (tooltip (g:object tooltip))
  (text :string))

(export 'tooltip-set-text)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_icon ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tooltip_set_icon" tooltip-set-icon) :void
 #+liber-documentation
 "@version{#2022-7-10}
  @argument[tooltip]{a @class{gtk:tooltip} object}
  @argument[paintable]{a @class{gdk:paintable} object, or @code{nil}}
  @begin{short}
    Sets the icon of the tooltip, which is in front of the text, to be
    @arg{paintable}.
  @end{short}
  If the @arg{paintable} argument is @code{nil}, the image will be hidden.
  @see-class{gtk:tooltip}
  @see-class{gdk-pixbuf:pixbuf}"
  (tooltip (g:object tooltip))
  (paintable (g:object gdk:paintable)))

(export 'tooltip-set-icon)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_icon_from_icon_name ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tooltip_set_icon_from_icon_name"
               tooltip-set-icon-from-icon-name) :void
 #+liber-documentation
 "@version{#2022-7-10}
  @argument[tooltip]{a @class{gtk:tooltip} object}
  @argument[name]{a string with the icon name, or @code{nil}}
  @begin{short}
    Sets the icon of the tooltip, which is in front of the text, to be the icon
    indicated by @arg{name}.
  @end{short}
  If @arg{name} is @code{nil}, the image will be hidden.
  @see-class{gtk:tooltip}"
  (tooltip (g:object tooltip))
  (name :string))

(export 'tooltip-set-icon-from-icon-name)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_icon_from_gicon ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tooltip_set_icon_from_gicon" tooltip-set-icon-from-gicon)
    :void
 #+liber-documentation
 "@version{#2022-7-10}
  @argument[tooltip]{a @class{gtk:tooltip} widget}
  @argument[gicon]{a @class{g-icon} representing the icon, or @code{nil}}
  @begin{short}
    Sets the icon of the tooltip, which is in front of the text, to be the
    icon indicated by @arg{gicon}.
  @end{short}
  If @arg{gicon} is @code{nil}, the image will be hidden.
  @see-class{gtk:tooltip}
  @see-class{g-icon}"
  (tooltip (g:object tooltip))
  (gicon (g:object g-icon)))

(export 'tooltip-set-icon-from-gicon)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_custom ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tooltip_set_custom" tooltip-set-custom) :void
 #+liber-documentation
 "@version{#2022-7-10}
  @argument[tooltip]{a @class{gtk:tooltip} object}
  @argument[widget]{a @class{gtk:widget} custom widget, or @code{nil} to unset
    the old custom widget}
  @begin{short}
    Replaces the widget packed into the tooltip with @arg{widget}.
  @end{short}
  @arg{widget} does not get destroyed when @arg{tooltip} goes away. By default
  a box with a @class{gtk:image} widget and @class{gtk:label} widget is
  embedded in the tooltip, which can be configured using the
  @fun{gtk:tooltip-set-markup} and @fun{gtk:tooltip-set-icon} functions.
  @see-class{gtk:tooltip}
  @see-function{gtk:tooltip-set-markup}
  @see-function{gtk:tooltip-set-icon}"
  (tooltip (g:object tooltip))
  (widget (g:object widget)))

(export 'tooltip-set-custom)

;;; ----------------------------------------------------------------------------
;;; gtk_tooltip_set_tip_area ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_tooltip_set_tip_area" tooltip-set-tip-area) :void
 #+liber-documentation
 "@version{#2021-12-11}
  @argument[tooltip]{a @class{gtk:tooltip} object}
  @argument[rectangle]{a @class{gdk:rectangle} instance}
  @begin{short}
    Sets the area of the widget, where the contents of the tooltip apply, to be
    @arg{rectangle} in widget coordinates.
  @end{short}
  This is especially useful for properly setting tooltips on
  @class{gtk:tree-view} rows and cells, @class{gtk:icon-view} widgets, etc.

  For setting tooltips on the @class{gtk:tree-view} widget, please refer to the
  convenience functions for this: @fun{gtk:tree-view-set-tooltip-row} and
  @fun{gtk:tree-view-set-tooltip-cell}.
  @see-class{gtk:tooltip}
  @see-function{gtk:tree-view-set-tooltip-row}
  @see-function{gtk:tree-view-set-tooltip-cell}"
  (tooltip (g:object tooltip))
  (rectangle (g:boxed gdk:rectangle)))

(export 'tooltip-set-tip-area)

;;; --- End of file gtk4.tooltip.lisp ------------------------------------------
