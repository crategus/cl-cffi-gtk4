;;; ----------------------------------------------------------------------------
;;; gtk4.aspect-frame.lisp
;;;
;;; The documentation in this file is taken from the GTK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GTK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2026 Dieter Kaiser
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
;;; GtkAspectFrame
;;;
;;;     A frame that constrains its child to a particular aspect ratio
;;;
;;; Types and Values
;;;
;;;     GtkAspectFrame
;;;
;;; Accessors
;;;
;;;     gtk_aspect_frame_set_child
;;;     gtk_aspect_frame_get_child
;;;     gtk_aspect_frame_set_obey_child
;;;     gtk_aspect_frame_get_obey_child
;;;     gtk_aspect_frame_set_ratio
;;;     gtk_aspect_frame_get_ratio
;;;     gtk_aspect_frame_set_xalign
;;;     gtk_aspect_frame_get_xalign
;;;     gtk_aspect_frame_set_yalign
;;;     gtk_aspect_frame_get_yalign
;;;
;;; Functions
;;;
;;;     gtk_aspect_frame_new
;;;
;;; Properties
;;;
;;;     child
;;;     obey-child
;;;     ratio
;;;     xalign
;;;     yalign
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkAspectFrame
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkAspectFrame
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GtkAspectFrame" aspect-frame
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_aspect_frame_get_type")
  ((child
    aspect-frame-child
    "child" "GtkWidget" t t)
   (obey-child
    aspect-frame-obey-child
    "obey-child" "gboolean" t t)
   (ratio
    aspect-frame-ratio
    "ratio" "gfloat" t t)
   (xalign
    aspect-frame-xalign
    "xalign" "gfloat" t t)
   (yalign
    aspect-frame-yalign
    "yalign" "gfloat" t t)))

#+liber-documentation
(setf (documentation 'aspect-frame 'type)
 "@version{2025-05-30}
  @begin{short}
    The @class{gtk:aspect-frame} widget preserves the aspect ratio of its child.
  @end{short}
  The frame can respect the aspect ratio of the child widget, or use its own
  aspect ratio.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:aspect-frame} implementation uses a CSS node with name
    @code{aspectframe}.
  @end{dictionary}
  @begin[Accessibility]{dictionary}
    Until GTK 4.10, the @class{gtk:aspect-frame} implementation used the
    @val[gtk:accessible-role]{:group} role of the @sym{gtk:accessible-role}
    enumeration. Starting from GTK 4.12, it uses the
    @val[gtk:accessible-role]{:generic} role.
  @end{dictionary}
  @see-constructor{gtk:aspect-frame-new}
  @see-slot{gtk:aspect-frame-child}
  @see-slot{gtk:aspect-frame-obey-child}
  @see-slot{gtk:aspect-frame-ratio}
  @see-slot{gtk:aspect-frame-xalign}
  @see-slot{gtk:aspect-frame-yalign}
  @see-class{gtk:frame}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gtk:aspect-frame-child -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'aspect-frame) t)
 "The @code{child} property of type @class{gtk:widget} (Read / Write) @br{}
  The child widget.")

#+liber-documentation
(setf (liber:alias-for-function 'aspect-frame-child)
      "Accessor"
      (documentation 'aspect-frame-child 'function)
 "@version{2025-07-31}
  @syntax{(gtk:aspect-frame-child object) => child}
  @syntax{(setf (gtk:aspect-frame-child object) child)}
  @argument[object]{a @class{gtk:aspect-frame} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    The accessor for the @slot[gtk:aspect-frame]{child} slot of the
    @class{gtk:aspect-frame} class gets or sets the child widget.
  @end{short}
  @see-class{gtk:aspect-frame}
  @see-class{gtk:widget}")

;;; --- gtk:aspect-frame-obey-child --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "obey-child" 'aspect-frame) t)
 "The @code{obey-child} property of type @code{:boolean} (Read / Write) @br{}
  Whether to force the aspect ratio to match that of the child widget of the
  aspect frame. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'aspect-frame-obey-child)
      "Accessor"
      (documentation 'aspect-frame-obey-child 'function)
 "@version{2025-07-31}
  @syntax{(gtk:aspect-frame-obey-child object) => obey}
  @syntax{(setf (gtk:aspect-frame-obey-child object) obey)}
  @argument[object]{a @class{gtk:aspect-frame} widget}
  @argument[obey]{a boolean whether to force the aspect ratio}
  @begin{short}
    The accessor for the @slot[gtk:aspect-frame]{obey-child} slot of the
    @class{gtk:aspect-frame} class gets or sets whether to force the aspect
    ratio to match that of the child widget of the aspect frame.
  @end{short}
  @see-class{gtk:aspect-frame}")

;;; --- gtk:aspect-frame-ratio -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "ratio" 'aspect-frame) t)
 "The @code{ratio} property of type @code{:float} (Read / Write) @br{}
  The aspect ratio if the @code{obey-child} property is @em{false}. @br{}
  Allowed values: [0.0001, 10000.0] @br{}
  Default value: 1.0")

#+liber-documentation
(setf (liber:alias-for-function 'aspect-frame-ratio)
      "Accessor"
      (documentation 'aspect-frame-ratio 'function)
 "@version{2025-07-31}
  @syntax{(gtk:aspect-frame-ratio object) => ratio}
  @syntax{(setf (gtk:aspect-frame-ratio object) ratio)}
  @argument[object]{a @class{gtk:aspect-frame} widget}
  @argument[ratio]{a number coerced to a single float for an aspect ratio}
  @begin{short}
    The accessor for the @slot[gtk:aspect-frame]{ratio} slot of the
    @class{gtk:aspect-frame} class gets or sets the aspect ratio if the
    @slot[gtk:aspect-frame]{obey-child} property is @em{false}.
  @end{short}
  Allowed values are in [0.0001, 10000.0]. The default value is 1.0.
  @see-class{gtk:aspect-frame}
  @see-function{gtk:aspect-frame-obey-child}")

;;; --- gtk:aspect-frame-xalign ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "xalign" 'aspect-frame) t)
 "The @code{xalign} property of type @code{:float} (Read / Write) @br{}
  The x alignment of the child. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 0.5")

#+liber-documentation
(setf (liber:alias-for-function 'aspect-frame-xalign)
      "Accessor"
      (documentation 'aspect-frame-xalign 'function)
 "@version{2025-07-31}
  @syntax{(gtk:aspect-frame-xalign object) => xalign}
  @syntax{(setf (gtk:aspect-frame-xalign object) xalign)}
  @argument[object]{a @class{gtk:aspect-frame} widget}
  @argument[xalign]{a number coerced to a single float for the x alignment of
    the child widget}
  @begin{short}
    The accessor for the @slot[gtk:aspect-frame]{xalign} slot of the
    @class{gtk:aspect-frame} class gets or sets the x alignment of the child
    widget in the aspect frame container.
  @end{short}
  @see-class{gtk:aspect-frame}")

;;; --- gtk:aspect-frame-yalign ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "yalign" 'aspect-frame) t)
 "The @code{yalign} property of type @code{:float} (Read / Write) @br{}
  The y alignment of the child. @br{}
  Allowed values: [0.0, 1.0] @br{}
  Default value: 0.5")

#+liber-documentation
(setf (liber:alias-for-function 'aspect-frame-yalign)
      "Accessor"
      (documentation 'aspect-frame-yalign 'function)
 "@version{2025-07-31}
  @syntax{(gtk:aspect-frame-yalign object) => yalign}
  @syntax{(setf (gtk:aspect-frame-yalign object) yalign)}
  @argument[object]{a @class{gtk:aspect-frame} widget}
  @argument[yalign]{a number coerced to a single float for the y alignment of
    the child widget}
  @begin{short}
    The accessor for the @slot[gtk:aspect-frame]{yalign} slot of the
    @class{gtk:aspect-frame} class gets or sets the y alignment of the child
    widget in the aspect frame container.
  @end{short}
  @see-class{gtk:aspect-frame}")

;;; ----------------------------------------------------------------------------
;;; gtk_aspect_frame_new
;;; ----------------------------------------------------------------------------

(declaim (inline aspect-frame-new))

(defun aspect-frame-new (xalign yalign ratio obey)
 #+liber-documentation
 "@version{2025-06-29}
  @argument[xalign]{a number coerced to a single float for the horizontal
    alignment of the child within the allocation of the aspect frame, this
    ranges from 0.0 (left aligned) to 1.0 (right aligned)}
  @argument[yalign]{a number coerced to a single float for the vertical
    alignment of the child within the allocation of the aspect frame, this
    ranges from 0.0 (left aligned) to 1.0 (right aligned)}
  @argument[ratio]{a number coerced to a single float for the desired aspect
    ratio}
  @argument[obey]{if @em{true}, @arg{ratio} is ignored, and the aspect ratio is
    taken from the requistion of the child}
  @return{The new @class{gtk:aspect-frame} widget.}
  @begin{short}
    Create a new aspect frame.
  @end{short}
  @see-class{gtk:aspect-frame}"
  (make-instance 'aspect-frame
                 :xalign xalign
                 :yalign yalign
                 :ratio ratio
                 :obey-child obey))

(export 'aspect-frame-new)

;;; --- End of file gtk4.aspect-frame.lisp -------------------------------------
