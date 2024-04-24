;;; ----------------------------------------------------------------------------
;;; gtk4.frame.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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
;;; GtkFrame
;;;
;;;     A widget with a frame and optional label
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
;;;     gtk_frame_set_label_align
;;;     gtk_frame_get_label_align
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

(gobject:define-g-object-class "GtkFrame" frame
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
 "@version{2024-4-19}
  @begin{short}
    The @class{gtk:frame} widget is a widget that surrounds its child widget
    with a frame and an optional label.
  @end{short}

  @image[frame]{Figure: GtkFrame}

  If present, the label is drawn inside the top edge of the frame. The
  horizontal position of the label can be controlled with the
  @fun{gtk:frame-label-xalign} function.

  The @class{gtk:frame} widget clips its child widget. You can use this to add
  rounded corners to widgets, but be aware that it also cuts off shadows. See
  the @class{gtk:aspect-frame} widget for a frame that constrains its child to
  a particular aspect ratio.
  @begin[GtkFrame as GtkBuildable]{dictionary}
    The @class{gtk:frame} implementation of the @class{gtk:buildable} interface
    supports placing a child in the label position by specifying @code{label}
    as the @code{type} attribute of a @code{<child>} element. A normal content
    child can be specified without specifying a @code{<child>} type attribute.

    @b{Example:} A UI definition fragment with a @class{gtk:frame} widget
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
    The @class{gtk:frame} implementation has a main CSS node with name
    @code{frame}, which is used to draw the visible border. You can set the
    appearance of the border using CSS properties like @code{border-style}
    on this node.
  @end{dictionary}
  @see-constructor{gtk:frame-new}
  @see-slot{gtk:frame-child}
  @see-slot{gtk:frame-label}
  @see-slot{gtk:frame-label-widget}
  @see-slot{gtk:frame-label-xalign}
  @see-class{gtk:aspect-frame}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:frame-child --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'frame) t)
 "The @code{child} property of type  @class{gtk:widget} (Read / Write) @br{}
  The child widget of the frame.")

#+liber-documentation
(setf (liber:alias-for-function 'frame-child)
      "Accessor"
      (documentation 'frame-child 'function)
 "@version{2024-4-19}
  @syntax{(gtk:frame-child object) => child}
  @syntax{(setf (gtk:frame-child object) child)}
  @argument[object]{a @class{gtk:frame} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Accessor of the @slot[gtk:frame]{child} slot of the @class{gtk:frame} class.
  @end{short}
  The @fun{gtk:frame-child} function gets the child widget of the frame. The
  @setf{gtk:frame-child} function sets the child widget.
  @see-class{gtk:frame}
  @see-class{gtk:widget}")

;;; --- gtk:frame-label --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label" 'frame) t)
 "The @code{label} property of type  @code{:string} (Read / Write) @br{}
  The text of the label of the frame. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'frame-label)
      "Accessor"
      (documentation 'frame-label 'function)
 "@version{2024-4-19}
  @syntax{(gtk:frame-label object) => label}
  @syntax{(setf (gtk:frame-label object) label)}
  @argument[object]{a @class{gtk:frame} widget}
  @argument[label]{a string with the text to use as the label of the frame}
  @begin{short}
    Accessor of the @slot[gtk:frame]{label} slot of the @class{gtk:frame} class.
  @end{short}
  The @fun{gtk:frame-label} function returns the text in the label, or
  @code{nil} if there was no label widget or the label widget was not a
  @class{gtk:label} widget. The @setf{gtk:frame-label} function sets the text
  of the label. If the @arg{label} argument is @code{nil}, the current label is
  removed.

  The frame will have a @class{gtk:label} widget for the label widget if a
  non-@code{nil} argument was passed to the @fun{gtk:frame-new} function.
  @see-class{gtk:frame}
  @see-class{gtk:label}
  @see-function{gtk:frame-new}
  @see-function{gtk:frame-label-widget}")

;;; --- gtk:frame-label-widget -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "label-widget" 'frame) t)
 "The @code{label-widget} property of type @class{gtk:widget} (Read / Write)
  @br{}
  The widget to display in place of the usual frame label.")

#+liber-documentation
(setf (liber:alias-for-function 'frame-label-widget)
      "Accessor"
      (documentation 'frame-label-widget 'function)
 "@version{2024-4-19}
  @syntax{(gtk:frame-label-widget object) => widget}
  @syntax{(setf (gtk:frame-label-widget object) widget)}
  @argument[object]{a @class{gtk:frame} widget}
  @argument[widget]{a @class{gtk:widget} label widget}
  @begin{short}
    Accessor of the @slot[gtk:frame]{label-widget} slot of the @class{gtk:frame}
    class.
  @end{short}
  The @class{gtk:frame-label-widget} function retrieves the label widget for
  the frame. The @setf{gtk:frame-label-widget} function sets the label widget.
  This is the widget that will appear embedded in the top edge of the frame as
  a title.
  @see-class{gtk:frame}
  @see-class{gtk:widget}
  @see-function{gtk:frame-label}")

;;; --- gtk:frame-label-xalign -------------------------------------------------

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
 "@version{2024-4-19}
  @syntax{(gtk:frame-label-xalign object) => xalign}
  @syntax{(setf (gtk:frame-label-xalign object) xalign)}
  @argument[object]{a @class{gtk:frame} widget}
  @argument[xalign]{a float with the position of the label along the top edge
    of the widget}
  @begin{short}
    Accessor of the @slot[gtk:frame]{label-xalign} slot of the @class{gtk:frame}
    class.
  @end{short}
  The @fun{gtk:frame-label-xalign} function retrieves the x alignment of the
  label of the frame. The @setf{gtk:frame-label-xalign} function sets the x
  alignment. The default value for a newly created frame is 0.0. A value of 0.0
  represents left alignment, 1.0 represents right alignment. The change of the
  property is ignored if the value is not in the range of [0.0, 1.0].
  @see-class{gtk:frame}")

;;; ----------------------------------------------------------------------------
;;; gtk_frame_new
;;; ----------------------------------------------------------------------------

(defun frame-new (&optional label)
 #+liber-documentation
 "@version{2024-4-19}
  @argument[label]{an optional string with the text to use as the label of the
    frame}
  @return{The new @class{gtk:frame} widget.}
  @begin{short}
    Creates a new frame widget, with an optional label.
  @end{short}
  If the @arg{label} argument is @code{nil}, the label is omitted.
  @see-class{gtk:frame}"
  (make-instance 'frame
                 :label (or label (cffi:null-pointer))))

(export 'frame-new)

;;; ----------------------------------------------------------------------------
;;; gtk_frame_get_label_align
;;; gtk_frame_set_label_align
;;; ----------------------------------------------------------------------------

(defun (setf frame-label-align) (value frame)
  (setf (frame-label-xalign frame) value))

(defun frame-label-align (frame)
 "@version{2024-4-19}
  @syntax{(gtk:frame-label-align object) => align}
  @syntax{(setf (gtk:frame-label-align object) align)}
  @argument[frame]{a @class{gtk:frame} widget}
  @argument[align]{a float with the position of the label along the top edge
    of the widget}
  @begin{short}
    The @fun{gtk:frame-label-align} function retrieves the x alignment of the
    label of the frame.
  @end{short}
  The @setf{gtk:frame-label-align} function sets the x alignment. The default
  value for a newly created frame is 0.0. A value of 0.0 represents left
  alignment, 1.0 represents right alignment. The change of the
  property is ignored if the value is not in the range of [0.0, 1.0].
  @begin{notes}
    This function is a variant of the @fun{gtk:frame-label-xalign} function,
    which is the accessor function of the @slot[gtk:frame]{label-xalign}
    property.
  @end{notes}
  @see-class{gtk:frame}
  @see-function{gtk:frame-label-xalign}"
  (frame-label-xalign frame))

(export 'frame-label-align)

;;; --- End of file gtk4.frame.lisp --------------------------------------------
