;;; ----------------------------------------------------------------------------
;;; gtk.cell-renderer-spinner.lisp
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
;;; GtkCellRendererSpinner
;;;
;;;     Renders a spinning animation in a cell
;;;
;;; Types and Values
;;;
;;;     GtkCellRendererSpinner
;;;
;;; Functions
;;;
;;;     gtk_cell_renderer_spinner_new
;;;
;;; Properties
;;;
;;;     active
;;;     pulse
;;;     size
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkCellRenderer
;;;             ╰── GtkCellRendererSpinner
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkCellRendererSpinner
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkCellRendererSpinner" cell-renderer-spinner
  (:superclass cell-renderer
   :export t
   :interfaces nil
   :type-initializer "gtk_cell_renderer_spinner_get_type")
  ((active
    cell-renderer-spinner-active
    "active" "gboolean" t t)
   (pulse
    cell-renderer-spinner-pulse
    "pulse" "guint" t t)
   (size
    cell-renderer-spinner-size
    "size" "GtkIconSize" t t)))

#+liber-documentation
(setf (documentation 'cell-renderer-spinner 'type)
 "@version{#2020-6-13}
  @begin{short}
    The @sym{gtk:cell-renderer-spinner} class renders a spinning animation in a
    cell, very similar to the @class{gtk:spinner} widget.
  @end{short}
  It can often be used as an alternative to a the
  @class{gtk:cell-renderer-progress} object for displaying indefinite activity,
  instead of actual progress.

  To start the animation in a cell, set the @code{active} property to @em{true}
  and increment the @code{pulse} property at regular intervals. The usual way to
  set the cell renderer properties for each cell is to bind them to columns in
  your tree model using e.g. the @fun{gtk:tree-view-column-add-attribute}
  function.
  @see-slot{gtk:cell-renderer-spinner-active}
  @see-slot{gtk:cell-renderer-spinner-pulse}
  @see-slot{gtk:cell-renderer-spinner-size}
  @see-class{gtk:spinner}
  @see-class{gtk:cell-renderer-progress}
  @see-function{gtk:tree-view-column-add-attribute}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- cell-renderer-spinner-active -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "active"
                                               'cell-renderer-spinner) t)
 "The @code{active} property of type @code{:boolean} (Read / Write) @br{}
  Whether the spinner is active, i.e. shown, in the cell. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-spinner-active)
      "Accessor"
      (documentation 'cell-renderer-spinner-active 'function)
 "@version{#2020-6-13}
  @syntax[]{(gtk:cell-renderer-spinner-active object) => active}
  @syntax[]{(setf (gtk:cell-renderer-spinner-active object) active)}
  @argument[object]{a @class{gtk:cell-renderer-spinner} object}
  @argument[active]{a boolean whether the spinner is active}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-spinner]{active} slot of the
    @class{gtk:cell-renderer-spinner} class.
  @end{short}
  Whether the spinner is active, i.e. shown, in the cell.
  @see-class{gtk:cell-renderer-spinner}")

;;; --- cell-renderer-spinner-pulse --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "pulse"
                                               'cell-renderer-spinner) t)
 "The @code{pulse} property of type @code{:uint} (Read / Write) @br{}
  Pulse of the spinner. Increment this value to draw the next frame of the
  spinner animation. Usually, you would update this value in a timeout. By
  default, the @class{gtk:spinner} widget draws one full cycle of the animation,
  consisting of 12 frames, in 750 milliseconds. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-spinner-pulse)
      "Accessor"
      (documentation 'cell-renderer-spinner-pulse 'function)
 "@version{#2020-6-13}
  @syntax[]{(gtk:cell-renderer-spinner-pulse object) => pulse}
  @syntax[]{(setf (gtk:cell-renderer-spinner-pulse object) pulse)}
  @argument[object]{a @class{gtk:cell-renderer-spinner} object}
  @argument[pulse]{an unsigned integer with the pulse of the spinner}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-spinner]{pulse} slot of the
    @class{gtk:cell-renderer-spinner} class.
  @end{short}
  Pulse of the spinner. Increment this value to draw the next frame of the
  spinner animation. Usually, you would update this value in a timeout. By
  default, the @class{gtk:spinner} widget draws one full cycle of the animation,
  consisting of 12 frames, in 750 milliseconds.
  @see-class{gtk:cell-renderer-spinner}")

;;; --- cell-renderer-spinner-size ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "size"
                                               'cell-renderer-spinner) t)
 "The @code{size} property of type @symbol{gtk:icon-size} (Read / Write) @br{}
  The icon size that specifies the size of the rendered spinner. @br{}
  Default value: @code{:menu}")

#+liber-documentation
(setf (liber:alias-for-function 'cell-renderer-spinner-size)
      "Accessor"
      (documentation 'cell-renderer-spinner-size 'function)
 "@version{#2020-6-13}
  @syntax[]{(gtk:cell-renderer-spinner-size object) => size}
  @syntax[]{(setf (gtk:cell-renderer-spinner-size object) size)}
  @argument[object]{a @class{gtk:cell-renderer-spinner} object}
  @argument[size]{a value of the @symbol{gtk:icon-size} enumeration}
  @begin{short}
    Accessor of the @slot[gtk:cell-renderer-spinner]{size} slot of the
    @class{gtk:cell-renderer-spinner} class.
  @end{short}
  The icon size that specifies the size of the rendered spinner.
  @see-class{gtk:cell-renderer-spinner}")

;;; ----------------------------------------------------------------------------
;;; gtk_cell_renderer_spinner_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline cell-renderer-spinner-new))

(defun cell-renderer-spinner-new ()
 #+liber-documentation
 "@version{#2020-6-13}
  @return{A new @class{gtk:cell-renderer} object.}
  @begin{short}
    Returns a new cell renderer which will show a spinner to indicate activity.
  @end{short}
  @see-class{gtk:cell-renderer-spinner}"
  (make-instance 'cell-renderer-spinner))

(export 'cell-renderer-spinner-new)

;;; --- End of file gtk.cell-renderer-spinner.lisp -----------------------------
