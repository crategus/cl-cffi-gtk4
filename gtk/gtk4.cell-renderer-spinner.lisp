;;; ----------------------------------------------------------------------------
;;; gtk4.cell-renderer-spinner.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
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

(gobject:define-g-object-class "GtkCellRendererSpinner" cell-renderer-spinner
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
    The @class{gtk:cell-renderer-spinner} class renders a spinning animation in
    a cell, very similar to the @class{gtk:spinner} widget.
  @end{short}
  It can often be used as an alternative to a the
  @class{gtk:cell-renderer-progress} object for displaying indefinite activity,
  instead of actual progress.

  To start the animation in a cell, set the @code{active} property to @em{true}
  and increment the @code{pulse} property at regular intervals. The usual way to
  set the cell renderer properties for each cell is to bind them to columns in
  your tree model using e.g. the @fun{gtk:tree-view-column-add-attribute}
  function.
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-spinner} implementation is deprecated since
    4.10. List views use widgets to display their contents. You should use
    the @class{gtk:spinner} widget instead.
  @end{dictionary}
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
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-spinner} implementation is deprecated since
    4.10. Please do not use it in newly written code.
  @end{dictionary}
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
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-spinner} implementation is deprecated since
    4.10. Please do not use it in newly written code.
  @end{dictionary}
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
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-spinner} implementation is deprecated since
    4.10. Please do not use it in newly written code.
  @end{dictionary}
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
  @begin[Warning]{dictionary}
    The @class{gtk:cell-renderer-spinner} implementation is deprecated since
    4.10. Please do not use it in newly written code.
  @end{dictionary}
  @see-class{gtk:cell-renderer-spinner}"
  (make-instance 'cell-renderer-spinner))

(export 'cell-renderer-spinner-new)

;;; --- End of file gtk4.cell-renderer-spinner.lisp ----------------------------
