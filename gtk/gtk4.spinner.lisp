;;; ----------------------------------------------------------------------------
;;; gtk4.spinner.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2024 Dieter Kaiser
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

(gobject:define-g-object-class "GtkSpinner" spinner
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
 "@version{2024-4-25}
  @begin{short}
    The @class{gtk:spinner} widget displays an icon size spinning animation.
  @end{short}

  @image[spinner]{Figure: GtkSpinner}

  It is often used as an alternative to the @class{gtk:progress-bar} widget for
  displaying indefinite activity, instead of actual progress.

  To start the animation, use the @fun{gtk:spinner-start} function, to stop it
  use the @fun{gtk:spinner-stop} function.
  @begin[CSS nodes]{dictionary}
    The @class{gtk:spinner} implementation has a single CSS node with the name
    @code{spinner}. When the animation is active, the @code{:checked}
    pseudoclass is added to this node.
  @end{dictionary}
  @see-constructor{gtk:spinner-new}
  @see-slot{gtk:spinner-spinning}
  @see-class{gtk:progress-bar}")

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
 "@version{2024-4-25}
  @syntax{(gtk:spinner-spinning object) => spinning}
  @syntax{(setf (gtk:spinner-spinning object) spinning)}
  @argument[object]{a @class{gtk:spinner} widget}
  @argument[spinning]{a boolean whether the spinner is spinning}
  @begin{short}
    Accessor of the @slot[gtk:spinner]{spinning} slot of the @class{gtk:spinner}
    class.
  @end{short}
  The @fun{gtk:spinner-spinning} function returns whether the spinner is
  spinning. The @setf{gtk:spinner-spinning} function sets the activity of the
  spinner.
  @see-class{gtk:spinner}")

;;; ----------------------------------------------------------------------------
;;; gtk_spinner_new
;;; ----------------------------------------------------------------------------

(declaim (inline spinner-new))

(defun spinner-new ()
 #+liber-documentation
 "@version{2024-4-25}
  @return{The new @class{gtk:spinner} widget.}
  @short{Returns a new spinner.}
  Not yet started.
  @see-class{gtk:spinner}
  @see-class{gtk:spinner-start}"
  (make-instance 'spinner))

(export 'spinner-new)

;;; ----------------------------------------------------------------------------
;;; gtk_spinner_start
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_spinner_start" spinner-start) :void
 #+liber-documentation
 "@version{2023-8-24}
  @argument[spinner]{a @class{gtk:spinner} widget}
  @short{Starts the animation of the spinner.}
  @see-class{gtk:spinner}
  @see-function{gtk:spinner-stop}"
  (spinner (g:object spinner)))

(export 'spinner-start)

;;; ----------------------------------------------------------------------------
;;; gtk_spinner_stop
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_spinner_stop" spinner-stop) :void
 #+liber-documentation
 "@version{2023-8-24}
  @argument[spinner]{a @class{gtk:spinner} widget}
  @short{Stops the animation of the spinner.}
  @see-class{gtk:spinner}
  @see-function{gtk:spinner-start}"
  (spinner (g:object spinner)))

(export 'spinner-stop)

;;; --- End of file gtk4.spinner.lisp ------------------------------------------
