;;; ----------------------------------------------------------------------------
;;; gtk.cell-renderer-progress.lisp
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
;;; GtkCellRendererProgress
;;;
;;;     Renders numbers as progress bars
;;;
;;; Types and Values
;;;
;;;     GtkCellRendererProgress
;;;
;;; Functions
;;;
;;;     gtk_cell_renderer_progress_new
;;;
;;; Properties
;;;
;;;     inverted
;;;     pulse
;;;     text
;;;     text-xalign
;;;     text-yalign
;;;     value
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkCellRenderer
;;;             ╰── GtkCellRendererProgress
;;;
;;; Implemented Interfaces
;;;
;;;     GtkOrientable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCellRendererProgress
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCellRendererProgress" cell-renderer-progress
  (:superclass cell-renderer
   :export t
   :interfaces ("GtkOrientable")
   :type-initializer "gtk_cell_renderer_progress_get_type")
  ((inverted
    cell-renderer-progress-inverted
    "inverted" "gboolean" t t)
   (pulse
    cell-renderer-progress-pulse
    "pulse" "gint" t t)
   (text
    cell-renderer-progress-text
    "text" "gchararray" t t)
   (text-xalign
    cell-renderer-progress-text-xalign
    "text-xalign" "gfloat" t t)
   (text-yalign
    cell-renderer-progress-text-yalign
    "text-yalign" "gfloat" t t)
   (value
    cell-renderer-progress-value
    "value" "gint" t t)))

#+liber-documentation
(setf (documentation 'cell-renderer-progress 'type)
 "@version{#2020-6-14}
  @begin{short}
    The @sym{gtk:cell-renderer-progress} class renders a numeric value as a
    progress par in a cell.
  @end{short}
  Additionally, it can display a text on top of the progress bar.
  @see-slot{gtk:cell-renderer-progress-inverted}
  @see-slot{gtk:cell-renderer-progress-pulse}
  @see-slot{gtk:cell-renderer-progress-text}
  @see-slot{gtk:cell-renderer-progress-text-xalign}
  @see-slot{gtk:cell-renderer-progress-text-yalign}
  @see-slot{gtk:cell-renderer-progress-value}
  @see-constructor{gtk:cell-renderer-progress-new}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- cell-renderer-progress-inverted ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "inverted"
                                               'cell-renderer-progress) t)
 "The @code{inverted} property of type @code{:boolean} (Read / Write) @br{}
  Invert the direction in which the progress bar grows. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-progress-inverted)
      "Accessor"
      (documentation 'cell-renderer-progress-inverted 'function)
 "@version{#2020-6-13}
  @syntax[]{(gtk:cell-renderer-progress-inverted object) => setting}
  @syntax[]{(setf (gtk:cell-renderer-progress-inverted object) setting)}
  @argument[object]{a @class{gtk:cell-renderer-progress} object}
  @argument[setting]{a boolean whether to invert the direction in which the
    progress bar grows}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-progress]{inverted} slot of the
    @class{gtk:cell-renderer-progress} class.
  @end{short}

  Invert the direction in which the progress bar grows.
  @see-class{gtk:cell-renderer-progress}")

;;; --- cell-renderer-progress-pulse -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pulse"
                                               'cell-renderer-progress) t)
 "The @code{pulse} property of type @code{:int} (Read / Write) @br{}
  Setting this to a non-negative value causes the cell renderer to enter
  \"activity mode\", where a block bounces back and forth to indicate that some
  progress is made, without specifying exactly how much. Each increment of the
  property causes the block to move by a little bit. To indicate that the
  activity has not started yet, set the property to zero. To indicate
  completion, set the property to @code{G_MAXINT}. @br{}
  Allowed values: >= -1 @br{}
  Default value: -1")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-progress-pulse)
      "Accessor"
      (documentation 'cell-renderer-progress-pulse 'function)
 "@version{#2020-6-14}
  @syntax[]{(gtk:cell-renderer-progress-pulse object) => pulse}
  @syntax[]{(setf (gtk:cell-renderer-progress-pulse object) pulse)}
  @argument[object]{a @class{gtk:cell-renderer-progress} object}
  @argument[pulse]{an integer for the pulse value}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-progress]{pulse} slot of the
    @class{gtk:cell-renderer-progress} class.
  @end{short}

  Setting this to a non-negative value causes the cell renderer to enter
  \"activity mode\", where a block bounces back and forth to indicate that some
  progress is made, without specifying exactly how much. Each increment of the
  property causes the block to move by a little bit. To indicate that the
  activity has not started yet, set the property to zero. To indicate
  completion, set the property to @code{G_MAXINT}.

  @see-class{gtk:cell-renderer-progress}")

;;; --- cell-renderer-progress-text --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "text"
                                               'cell-renderer-progress) t)
 "The @code{text} property of type @code{:string} (Read / Write) @br{}
  Determines the label which will be drawn over the progress bar. Setting this
  property to @code{nil} causes the default label to be displayed. Setting this
  property to an empty string causes no label to be displayed. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-progress-text)
      "Accessor"
      (documentation 'cell-renderer-progress-text 'function)
 "@version{#2020-6-14}
  @syntax[]{(gtk:cell-renderer-progress-text object) => text}
  @syntax[]{(setf (gtk:cell-renderer-progress-text object) text)}
  @argument[object]{a @class{gtk:cell-renderer-progress} object}
  @argument[text]{a string for the label which will be drawn over the progress
    bar}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-progress]{text} slot of the
    @class{gtk:cell-renderer-progress} class.
  @end{short}

  Determines the label which will be drawn over the progress bar. Setting this
  property to @code{nil} causes the default label to be displayed. Setting this
  property to an empty string causes no label to be displayed.
  @see-class{gtk:cell-renderer-progress}")

;;; --- cell-renderer-progress-text-xalign -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "text-xalign"
                                               'cell-renderer-progress) t)
 "The @code{text-xalign} property of type @code{:float} (Read / Write) @br{}
  Controls the horizontal alignment of the text in the progress bar. Valid
  values range from 0 (left) to 1 (right). Reserved for RTL layouts. @br{}
  Allowed values: [0,1] @br{}
  Default value: 0.5")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-progress-text-xalign)
      "Accessor"
      (documentation 'cell-renderer-progress-text-xalign 'function)
 "@version{#2020-6-14}
  @syntax[]{(gtk:cell-renderer-progress-text-xalign object) => align}
  @syntax[]{(setf (gtk:cell-renderer-progress-text-xalign object) align)}
  @argument[object]{a @class{gtk:cell-renderer-progress} object}
  @argument[align]{a @code{:float} which controls the horizontal alignment}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-progress]{text-xalign} slot of the
    @class{gtk:cell-renderer-progress} class.
  @end{short}

  Controls the horizontal alignment of the text in the progress bar. Valid
  values range from 0 (left) to 1 (right). Reserved for RTL layouts.
  @see-class{gtk:cell-renderer-progress}")

;;; --- cell-renderer-progress-text-yalign -------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "text-yalign"
                                               'cell-renderer-progress) t)
 "The @code{text-yalign} property of type @code{:float} (Read / Write) @br{}
  Controls the vertical alignment of the text in the progress bar. Valid values
  range from 0 (top) to 1 (bottom). @br{}
  Allowed values: [0,1] @br{}
  Default value: 0.5")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-progress-text-yalign)
      "Accessor"
      (documentation 'cell-renderer-progress-text-yalign 'function)
 "@version{#2020-6-14}
  @syntax[]{(gtk:cell-renderer-progress-text-yalign object) => align}
  @syntax[]{(setf (gtk:cell-renderer-progress-text-yalign object) align)}
  @argument[object]{a @class{gtk:cell-renderer-progress} object}
  @argument[align]{a @code{:float} which controls the vertical alignment}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-progress]{text-yalign} slot of the
    @class{gtk:cell-renderer-progress} class.
  @end{short}

  Controls the vertical alignment of the text in the progress bar. Valid values
  range from 0 (top) to 1 (bottom).
  @see-class{gtk:cell-renderer-progress}")

;;; --- cell-renderer-progress-value -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "value"
                                               'cell-renderer-progress) t)
 "The @code{value} property of type @code{:int} (Read / Write) @br{}
  Determines the percentage to which the progress bar will be \"filled in\".
  @br{}
  Allowed values: [0,100] @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-progress-value)
      "Accessor"
      (documentation 'cell-renderer-progress-value 'function)
 "@version{#2020-6-14}
  @syntax[]{(gtk:cell-renderer-progress-value object) => value}
  @syntax[]{(setf (gtk:cell-renderer-progress-value object) value)}
  @argument[object]{a @class{gtk:cell-renderer-progress} object}
  @argument[value]{an integer which determines the percentage the progress bar
    will be filled in}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-progress]{value} slot of the
    @class{gtk:cell-renderer-progress} class.
  @end{short}

  Determines the percentage to which the progress bar will be \"filled in\".
  @see-class{gtk:cell-renderer-progress}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_progress_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline cell-renderer-progress-new))

(defun cell-renderer-progress-new ()
 #+liber-documentation
 "@version{#2020-6-14}
  @return{The new @class{gtk:cell-renderer-progress} object.}
  @begin{short}
    Creates a new cell renderer progress object.
  @end{short}
  @see-class{gtk:cell-renderer-progress}"
  (make-instance 'cell-renderer-progress))

(export 'cell-renderer-progress-new)

;;; --- End of file gtk.cell-renderer-progress.lisp ----------------------------
