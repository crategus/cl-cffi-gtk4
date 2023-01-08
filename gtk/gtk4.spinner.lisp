;;; ----------------------------------------------------------------------------
;;; gtk.spinner.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2022 Dieter Kaiser
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
;;; GtkSpinner
;;;
;;;     Show a spinner animation
;;;
;;; Types and Values
;;;
;;;     GtkSpinner
;;;
;;; Accessors
;;;
;;;     gtk_spinner_set_spinning
;;;     gtk_spinner_get_spinning
;;;
;;; Functions
;;;
;;;     gtk_spinner_new
;;;     gtk_spinner_start
;;;     gtk_spinner_stop
;;;
;;; Properties
;;;
;;;     spinning
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkSpinner
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkSpinner
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkSpinner" spinner
  (:superclass widget
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget")
   :type-initializer "gtk_spinner_get_type")
  ((spinning
    spinner-spinning
    "spinning" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'spinner 'type)
 "@version{#2022-5-27}
  @begin{short}
    A @sym{gtk:spinner} widget displays an icon size spinning animation.
  @end{short}

  @image[spinner]{Figure: GtkSpinner}

  It is often used as an alternative to a @class{gtk:progress-bar} widget for
  displaying indefinite activity, instead of actual progress.

  To start the animation, use the @fun{gtk:spinner-start} function, to stop it
  use the @fun{gtk:spinner-stop} function.
  @begin[CSS nodes]{dictionary}
    The @sym{gtk:spinner} implementation has a single CSS node with the name
    @code{spinner}. When the animation is active, the @code{:checked}
    pseudoclass is added to this node.
  @end{dictionary}
  @see-slot{gtk:spinner-spinning}
  @see-class{gtk:cell-renderer-spinner}
  @see-class{gtk:progress-bar}
  @see-function{gtk:spinner-start}
  @see-function{gtk:spinner-stop}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "spinning" 'spinner) t)
 "The @code{spinning} property of type @code{:boolean} (Read / Write) @br{}
  Whether the spinner is spinning. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'spinner-spinning)
      "Accessor"
      (documentation 'spinner-spinning 'function)
 "@version{#2022-5-27}
  @syntax[]{(gtk:spinner-spinning object) => spinning}
  @syntax[]{(setf (gtk:spinner-spinning object) spinning)}
  @argument[object]{a @class{gtk:spinner} widget}
  @argument[spinning]{a boolean whether the spinner is spinning}
  @begin{short}
    Accessor of the @slot[gtk:spinner]{spinning} slot of the @class{gtk:spinner}
    class.
  @end{short}

  The @sym{gtk:spinner-spinning} function returns whether the spinner is
  spinning. The @sym{(setf gtk:spinner-spinning)} function sets the activity
  of the spinner.
  @see-class{gtk:spinner}")

;;; ----------------------------------------------------------------------------
;;; gtk_spinner_new
;;; ----------------------------------------------------------------------------

(declaim (inline spinner-new))

(defun spinner-new ()
 #+liber-documentation
 "@version{#2022-5-27}
  @return{A new @class{gtk:spinner} widget.}
  @short{Returns a new spinner.}
  Not yet started.
  @see-class{gtk:spinner}
  @see-class{gtk:spinner-start}"
  (make-instance 'spinner))

(export 'spinner-new)

;;; ----------------------------------------------------------------------------
;;; gtk_spinner_start
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_spinner_start" spinner-start) :void
 #+liber-documentation
 "@version{#2022-5-27}
  @argument[spinner]{a @class{gtk:spinner} widget}
  @short{Starts the animation of the spinner.}
  @see-class{gtk:spinner}
  @see-function{gtk:spinner-stop}"
  (spinner (g:object spinner)))

(export 'spinner-start)

;;; ----------------------------------------------------------------------------
;;; gtk_spinner_stop
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_spinner_stop" spinner-stop) :void
 #+liber-documentation
 "@version{#2022-5-27}
  @argument[spinner]{a @class{gtk:spinner} widget}
  @short{Stops the animation of the spinner.}
  @see-class{gtk:spinner}
  @see-function{gtk:spinner-start}"
  (spinner (g:object spinner)))

(export 'spinner-stop)

;;; --- End of file gtk.spinner.lisp -------------------------------------------
