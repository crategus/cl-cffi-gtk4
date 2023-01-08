;;; ----------------------------------------------------------------------------
;;; gtk.frame.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2009 - 2011 Kalyanov Dmitry
;;; Copyright (C) 2011 - 2022 Dieter Kaiser
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
;;; GtkFrame
;;;
;;;     A widget with a decorative frame and optional label
;;;
;;; Types and Values
;;;
;;;     GtkFrame
;;;
;;; Accessors
;;;
;;;     gtk_frame_set_child
;;;     gtk_frame_get_child
;;;     gtk_frame_set_label
;;;     gtk_frame_get_label
;;;     gtk_frame_set_label_widget
;;;     gtk_frame_get_label_widget
;;;
;;; Functions
;;;
;;;     gtk_frame_new
;;;     gtk_frame_set_label_align                          not implemented
;;;     gtk_frame_get_label_align                          not implemented
;;;
;;; Properties
;;;
;;;     child
;;;     label
;;;     label-widget
;;;     label-xalign
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkFrame
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkFrame
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkFrame" frame
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_frame_get_type")
  ((child
    frame-child
    "child" "GtkWidget" t t)
   (label
    frame-label
    "label" "gchararray" t t)
   (label-widget
    frame-label-widget
    "label-widget" "GtkWidget" t t)
   (label-xalign
    frame-label-xalign
    "label-xalign" "gfloat" t t)))

#+liber-documentation
(setf (documentation 'frame 'type)
 "@version{#2022-9-9}
  @begin{short}
    The frame widget is a widget that surrounds its child with a decorative
    frame and an optional label.
  @end{short}

  @image[frame]{Figure: GtkFrame}

  If present, the label is drawn inside the top edge of the frame. The
  horizontal position of the label can be controlled with the
  @fun{gtk:frame-label-align} function.

  The @sym{gtk:frame} widget clips its child. You can use this to add rounded
  corners to widgets, but be aware that it also cuts off shadows.
  @begin[GtkFrame as GtkBuildable]{dictionary}
    The @sym{gtk:frame} implementation of the @class{gtk:buildable} interface
    supports placing a child in the label position by specifying @code{label}
    as the @code{type} attribute of a @code{<child>} element. A normal content
    child can be specified without specifying a @code{<child>} type attribute.

    @b{Example:} A UI definition fragment with a @sym{gtk:frame} widget
    @begin{pre}
<object class=\"GtkFrame\">
 <child type=\"label\">
   <object class=\"GtkLabel\" id=\"frame-label\"/>
 </child>
 <child>
   <object class=\"GtkEntry\" id=\"frame-content\"/>
 </child>
</object>
    @end{pre}
  @end{dictionary}
  @begin[CSS nodes]{dictionary}
    @begin{pre}
frame
├── <label widget>
╰── <child>
    @end{pre}
    The @sym{gtk:frame} implementation has a main CSS node with name
    @code{frame}, which is used to draw the visible border. You can set the
    appearance of the border using CSS properties like @code{border-style}
    on this node.
  @end{dictionary}
  @see-slot{gtk:frame-child}
  @see-slot{gtk:frame-label}
  @see-slot{gtk:frame-label-widget}
  @see-slot{gtk:frame-label-xalign}
  @see-constructor{gtk:frame-new}
  @see-function{gtk:frame-label-align}")

;;; ----------------------------------------------------------------------------
;;; Property Details
;;; ----------------------------------------------------------------------------

;;; --- frame-child --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'frame) t)
 "The @code{child} property of type  @class{gtk:widget} (Read / Write) @br{}
  The child widget of the frame.")

#+liber-documentation
(setf (liber:alias-for-function 'frame-child)
      "Accessor"
      (documentation 'frame-child 'function)
 "@version{#2022-9-9}
  @syntax[]{(gtk:frame-child object) => child}
  @syntax[]{(setf (gtk:frame-child object) child)}
  @argument[object]{a @class{gtk:frame} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Accessor of the @slot[gtk:frame]{child} slot of the @class{gtk:frame} class.
  @end{short}
  The @sym{gtk:frame-child} function gets the child widget of the frame. The
  @sym{(setf gtk:frame-child)} function sets the child widget.
  @see-class{gtk:frame}")

;;; --- frame-label --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label" 'frame) t)
 "The @code{label} property of type  @code{:string} (Read / Write) @br{}
  Text of the frame's label. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'frame-label)
      "Accessor"
      (documentation 'frame-label 'function)
 "@version{#2022-9-9}
  @syntax[]{(gtk:frame-label object) => label}
  @syntax[]{(setf (gtk:frame-label object) label)}
  @argument[object]{a @class{gtk:frame} widget}
  @argument[label]{a @code{:string} with the text to use as the label of the
    frame}
  @begin{short}
    Accessor of the @slot[gtk:frame]{label} slot of the @class{gtk:frame} class.
  @end{short}
  The @sym{gtk:frame-label} function returns the text in the label, or
  @code{nil} if there was no label widget or the label widget was not a
  @class{gtk:label} widget. The @sym{(setf gtk:frame-label)} function sets
  the text of the label. If the @arg{label} argument is @code{nil}, the current
  label is removed.

  The frame will have a @class{gtk:label} widget for the label widget if a
  non-@code{nil} argument was passed to the @fun{gtk:frame-new} function.
  @see-class{gtk:frame}
  @see-function{gtk:frame-new}
  @see-function{gtk:frame-label-widget}")

;;; --- frame-label-widget -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label-widget" 'frame) t)
 "The @code{label-widget} property of type @class{gtk:widget} (Read / Write)
  @br{}
  A widget to display in place of the usual frame label.")

#+liber-documentation
(setf (liber:alias-for-function 'frame-label-widget)
      "Accessor"
      (documentation 'frame-label-widget 'function)
 "@version{#2022-9-9}
  @syntax[]{(gtk:frame-label-widget object) => widget}
  @syntax[]{(setf (gtk:frame-label-widget object) widget)}
  @argument[object]{a @class{gtk:frame} widget}
  @argument[widget]{a new @class{gtk:widget} label widget}
  @begin{short}
    Accessor of the @slot[gtk:frame]{label-widget} slot of the @class{gtk:frame}
    class.
  @end{short}
  The @sym{gtk:frame-label-widget} function retrieves the label widget for the
  frame. The @sym{(setf gtk:frame-label-widget)} function sets the label widget.
  This is the widget that will appear embedded in the top edge of the frame as
  a title.
  @see-class{gtk:frame}
  @see-function{gtk:frame-label}")

;;; --- frame-label-xalign -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label-xalign" 'frame) t)
 "The @code{label-xalign} property of type @code{:float} (Read / Write) @br{}
  The horizontal alignment of the label. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 0.0")

#+liber-documentation
(setf (liber:alias-for-function 'frame-label-xalign)
      "Accessor"
      (documentation 'frame-label-xalign 'function)
 "@version{#2022-9-9}
  @syntax[]{(gtk:frame-label-xalign object) => xalign}
  @syntax[]{(setf (gtk:frame-label-xalign object) xalign)}
  @argument[object]{a @class{gtk:frame} widget}
  @argument[xalign]{a float with the position of the label along the top edge
    of the widget}
  @begin{short}
    Accessor of the @slot[gtk:frame]{label-xalign} slot of the @class{gtk:frame}
    class.
  @end{short}
  The @sym{gtk:frame-label-xalign} function retrieves the X alignment of the
  label of the frame. The @sym{gtk:frame-label-align} function sets the X
  alignment. The default value for a newly created frame is 0.0. A value of 0.0
  represents left alignment, 1.0 represents right alignment.
  @see-class{gtk:frame}")

;;; ----------------------------------------------------------------------------
;;; gtk_frame_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline frame-new))

(defun frame-new (&optional label)
 #+liber-documentation
 "@version{#2022-9-9}
  @argument[label]{an optional string with the text to use as the label of the
    frame}
  @return{A new @class{gtk:frame} widget.}
  @begin{short}
    Creates a new frame widget, with an optional label.
  @end{short}
  If @arg{label} is @code{nil}, the label is omitted.
  @see-class{gtk:frame}"
  (make-instance 'frame
                 :label (if label label (null-pointer))))

(export 'frame-new)

;;; ----------------------------------------------------------------------------
;;; gtk_frame_set_label_align ()
;;;
;;; void
;;; gtk_frame_set_label_align (GtkFrame *frame, float xalign);
;;;
;;; Sets the X alignment of the frame widget’s label. The default value for a
;;; newly created frame is 0.0.
;;;
;;; frame :
;;;     a GtkFrame
;;;
;;; xalign :
;;;     The position of the label along the top edge of the widget. A value of
;;;     0.0 represents left alignment; 1.0 represents right alignment.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_frame_get_label_align ()
;;;
;;; float
;;; gtk_frame_get_label_align (GtkFrame *frame);
;;;
;;; Retrieves the X alignment of the frame’s label. See
;;; gtk_frame_set_label_align().
;;;
;;; frame :
;;;     a GtkFrame
;;; ----------------------------------------------------------------------------

;;; --- End of file gtk.frame.lisp ---------------------------------------------
