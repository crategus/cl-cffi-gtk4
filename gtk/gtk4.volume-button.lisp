;;; ----------------------------------------------------------------------------
;;; gtk.volume-button.lisp
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
;;; GtkVolumeButton
;;;
;;;     A button which pops up a volume control
;;;
;;; Types and Values
;;;
;;;     GtkVolumeButton
;;;
;;; Functions
;;;
;;;     gtk_volume_button_new
;;;
;;; Properties
;;;
;;;     use-symbolic
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GInitiallyUnowned
;;;         ╰── GtkWidget
;;;             ╰── GtkScaleButton
;;;                 ╰── GtkVolumeButton
;;;
;;; Implemented Interfaces
;;;
;;;     GtkAccessible
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkOrientable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkVolumeButton
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkVolumeButton" volume-button
  (:superclass scale-button
   :export t
   :interfaces ("GtkAccessible"
                "GtkBuildable"
                "GtkConstraintTarget"
                "GtkOrientable")
   :type-initializer "gtk_volume_button_get_type")
  ((use-symbolic
    volume-button-use-symbolic
    "use-symbolic" "gboolean" t t)))

#+liber-documentation
(setf (documentation 'volume-button 'type)
 "@version{#2021-12-23}
  @begin{short}
    The @sym{gtk:volume-button} class is a subclass of the
    @class{gtk:scale-button} class that has been tailored for use as a volume
    control widget with suitable icons, tooltips and accessible labels.
  @end{short}

  @image[volume-button]{Figure: GtkVolumeButton}
  @see-slot{gtk:volume-button-use-symbolic}
  @see-class{gtk:scale-button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-symbolic"
                                               'volume-button) t)
 "The @code{use-symbolic} property of type @code{:boolean} (Read / Write) @br{}
  Whether to use symbolic icons as the icons. Note that if the symbolic icons
  are not available in your installed theme, then the normal, potentially
  colorful icons will be used. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'volume-button-use-symbolic)
      "Accessor"
      (documentation 'volume-button-use-symbolic 'function)
 "@version{#2021-12-23}
  @syntax[]{(gtk:volume-button-use-symbolic object) => use-symbolic}
  @syntax[]{(setf (gtk:volume-button-use-symbolic object) use-symbolic)}
  @argument[object]{a @class{gtk:volume-button} widget}
  @argument[use-symbolic]{a boolean whether to use symbolic icons}
  @begin{short}
    Accessor of the @slot[gtk:volume-button]{use-symbolic} slot of the
    @class{gtk:volume-button} class.
  @end{short}

  Whether to use symbolic icons as the icons. Note that if the symbolic icons
  are not available in your installed theme, then the normal, potentially
  colorful icons will be used.
  @see-class{gtk:volume-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_volume_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline volume-button-new))

(defun volume-button-new ()
 #+liber-documentation
 "@version{#2021-12-23}
  @return{A new @class{gtk:volume-button} widget.}
  @begin{short}
    Creates a volume button, with a range between 0.0 and 1.0, with
    a stepping of 0.02.
  @end{short}
  Volume values can be obtained and modified using the functions from
  the @class{gtk:scale-button} class.
  @see-class{gtk:volume-button}
  @see-class{gtk:scale-button}"
  (make-instance 'volume-button))

(export 'volume-button-new)

;;; --- End of file gtk.volume-button.lisp -------------------------------------
