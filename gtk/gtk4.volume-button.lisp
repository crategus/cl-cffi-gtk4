;;; ----------------------------------------------------------------------------
;;; gtk4.volume-button.lisp
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
;;;     GtkAccessibleRange                                 Since 4.10
;;;     GtkBuildable
;;;     GtkConstraintTarget
;;;     GtkOrientable
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; struct GtkVolumeButton
;;; ----------------------------------------------------------------------------

;; TODO: Implement the GtkAccessibleRange interface

(gobject:define-g-object-class "GtkVolumeButton" volume-button
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
 "@version{2023-3-26}
  @begin{short}
    The @class{gtk:volume-button} class is a subclass of the
    @class{gtk:scale-button} class that has been tailored for use as a volume
    control widget with suitable icons, tooltips and accessible labels.
  @end{short}

  @image[volume-button]{Figure: GtkVolumeButton}
  @begin[Warning]{dictionary}
    The @class{gtk:volume-button} implementation has been deprecated since
    version 4.10 and should not be used in newly written code. This widget will
    be removed in GTK 5.
  @end{dictionary}
  @see-constructor{gtk:volume-button-new}
  @see-slot{gtk:volume-button-use-symbolic}
  @see-class{gtk:scale-button}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "use-symbolic" 'volume-button) t)
 "The @code{use-symbolic} property of type @code{:boolean} (Read / Write) @br{}
  Whether to use symbolic icons as the icons. Note that if the symbolic icons
  are not available in your installed theme, then the normal, potentially
  colorful icons will be used. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'volume-button-use-symbolic)
      "Accessor"
      (documentation 'volume-button-use-symbolic 'function)
 "@version{2023-3-26}
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
  @begin[Warning]{dictionary}
    The @class{gtk:volume-button} implementation has been deprecated since
    version 4.10 and should not be used in newly written code. This widget will
    be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:volume-button}")

;;; ----------------------------------------------------------------------------
;;; gtk_volume_button_new ()
;;; ----------------------------------------------------------------------------

(declaim (inline volume-button-new))

(defun volume-button-new ()
 #+liber-documentation
 "@version{2023-3-26}
  @return{A new @class{gtk:volume-button} widget.}
  @begin{short}
    Creates a volume button, with a range between 0.0 and 1.0, with
    a stepping of 0.02.
  @end{short}
  Volume values can be obtained and modified using the functions from
  the @class{gtk:scale-button} class.
  @begin[Warning]{dictionary}
    The @class{gtk:volume-button} implementation has been deprecated since
    version 4.10 and should not be used in newly written code. This widget will
    be removed in GTK 5.
  @end{dictionary}
  @see-class{gtk:volume-button}
  @see-class{gtk:scale-button}"
  (make-instance 'volume-button))

(export 'volume-button-new)

;;; --- End of file gtk4.volume-button.lisp ------------------------------------
