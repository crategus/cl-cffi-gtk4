;;; ----------------------------------------------------------------------------
;;; gtk.aspect-frame.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
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

(define-g-object-class "GtkAspectFrame" aspect-frame
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
 "@version{#2022-2-6}
  @begin{short}
    The @sym{gtk:aspect-frame} widget is useful when you want pack a widget so
    that it can resize but always retains the same aspect ratio.
  @end{short}
  For instance, one might be drawing a small preview of a larger image.
  The @sym{gtk:aspect-frame} class derives from the @class{gtk:frame} class, so
  it can draw a label and a frame around the child. The frame will be
  \"shrink-wrapped\" to the size of the child.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:aspect-frame} implementation uses a CSS node with name
    @code{frame}.
  @end{dictionary}
  @see-slot{gtk:aspect-frame-obey-child}
  @see-slot{gtk:aspect-frame-ratio}
  @see-slot{gtk:aspect-frame-xalign}
  @see-slot{gtk:aspect-frame-yalign}
  @see-class{gtk:frame}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- aspect-frame-child -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "child" 'aspect-frame) t)
 "The @code{child} property of type @class{gtk:widget} (Read / Write) @br{}
  The child widget.")

#+liber-documentation
(setf (liber:alias-for-function 'aspect-frame-child)
      "Accessor"
      (documentation 'aspect-frame-child 'function)
 "@version{#2022-2-6}
  @syntax[]{(gtk:aspect-frame-child object) => child}
  @syntax[]{(setf (gtk:aspect-frame-child object) child)}
  @argument[object]{a @class{gtk:aspect-frame} widget}
  @argument[child]{a @class{gtk:widget} child widget}
  @begin{short}
    Accessor of the @slot[gtk:aspect-frame]{child} slot of the
    @class{gtk:aspect-frame} class.
  @end{short}
  @see-class{gtk:aspect-frame}
  @see-class{gtk:widget}")

;;; --- aspect-frame-obey-child --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "obey-child"
                                               'aspect-frame) t)
 "The @code{obey-child} property of type @code{:boolean} (Read / Write) @br{}
  Force aspect ratio to match that of the child widget of the aspect frame.@br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'aspect-frame-obey-child)
      "Accessor"
      (documentation 'aspect-frame-obey-child 'function)
 "@version{#2021-12-17}
  @syntax[]{(gtk:aspect-frame-obey-child object) => obey-child}
  @syntax[]{(setf (gtk:aspect-frame-obey-child object) obey-child)}
  @argument[object]{a @class{gtk:aspect-frame} widget}
  @argument[obey-child]{a boolean whether to force the aspect ratio}
  @begin{short}
    Accessor of the @slot[gtk:aspect-frame]{obey-child} slot of the
    @class{gtk:aspect-frame} class.
  @end{short}

  WHether to force the aspect ratio to match that of the child widget of the
  aspect frame.
  @see-class{gtk:aspect-frame}")

;;; --- aspect-frame-ratio -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "ratio" 'aspect-frame) t)
 "The @code{ratio} property of type @code{:float} (Read / Write) @br{}
  Aspect ratio if the @code{obey-child} property is @em{false}. @br{}
  Allowed values: [0.0001, 10000.0] @br{}
  Default value: 1.0")

#+liber-documentation
(setf (liber:alias-for-function 'aspect-frame-ratio)
      "Accessor"
      (documentation 'aspect-frame-ratio 'function)
 "@version{#2021-12-17}
  @syntax[]{(gtk:aspect-frame-ratio object) => ratio}
  @syntax[]{(setf (gtk:aspect-frame-ratio object) ratio)}
  @argument[object]{a @class{gtk:aspect-frame} widget}
  @argument[ratio]{a float with an aspect ratio}
  @begin{short}
    Accessor of the @slot[gtk:aspect-frame]{ratio} slot of the
    @class{gtk:aspect-frame} class.
  @end{short}

  The aspect ratio if the @slot[gtk:aspect-frame]{obey-child} property is
  @em{false}. Allowed values are in [0.0001, 10000.0]. The default value is 1.0.
  @see-class{gtk:aspect-frame}")

;;; --- aspect-frame-xalign ------------------------------------------------

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
 "@version{#2021-12-17}
  @syntax[]{(gtk:aspect-frame-xalign object) => xalign}
  @syntax[]{(setf (gtk:aspect-frame-xalign object) xalign)}
  @argument[object]{a @class{gtk:aspect-frame} widget}
  @argument[xalign]{a float with the x alignment of the child widget}
  @begin{short}
    Accessor of the @slot[gtk:aspect-frame]{xalign} slot of the
    @class{gtk:aspect-frame} class.
  @end{short}

  The x alignment of the child widget in the aspect frame container.
  @see-class{gtk:aspect-frame}")

;;; --- aspect-frame-yalign ------------------------------------------------

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
 "@version{#2021-12-17}
  @syntax[]{(gtk:aspect-frame-yalign object) => yalign}
  @syntax[]{(setf (gtk:aspect-frame-yalign object) yalign)}
  @argument[object]{a @class{gtk:aspect-frame} widget}
  @argument[yalign]{a float with the y alignment of the child widget}
  @begin{short}
    Accessor of the @slot[gtk:aspect-frame]{yalign} slot of the
    @class{gtk:aspect-frame} class.
  @end{short}

  The y alignment of the child widget in the aspect frame container.
  @see-class{gtk:aspect-frame}")

;;; ----------------------------------------------------------------------------
;;; gtk_aspect_frame_new
;;; ----------------------------------------------------------------------------

(declaim (inline aspect-frame-new))

(defun aspect-frame-new (label xalign yalign ratio obey-child)
 #+liber-documentation
 "@version{#2021-12-17}
  @argument[label]{a string with the label text}
  @argument[xalign]{a float with the horizontal alignment of the child within
    the allocation of the aspect frame, this ranges from 0.0 (left aligned) to
    1.0 (right aligned)}
  @argument[yalign]{a float with the vertical alignment of the child within the
    allocation of the aspect frame, this ranges from 0.0 (left aligned) to 1.0
    (right aligned)}
  @argument[ratio]{a float with the desired aspect ratio}
  @argument[obey-child]{if @em{true}, @arg{ratio} is ignored, and the aspect
    ratio is taken from the requistion of the child}
  @return{The new @class{gtk:aspect-frame} widget.}
  @begin{short}
    Create a new aspect frame container.
  @end{short}
  @see-class{gtk:aspect-frame}"
  (make-instance 'aspect-frame
                 :label label
                 :xalign xalign
                 :yalign yalign
                 :ratio ratio
                 :obey-child obey-child))

(export 'aspect-frame-new)

;;; --- End of file gtk.aspect-frame.lisp --------------------------------------
