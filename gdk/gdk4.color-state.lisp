;;; ----------------------------------------------------------------------------
;;; gdk4.color-state.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.16 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2024 Dieter Kaiser
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
;;; Types and Values
;;;
;;;     GdkColorState
;;;
;;; Functions
;;;
;;;     gdk_color_state_get_oklab                           Since 4.18
;;;     gdk_color_state_get_oklch                           Since 4.18
;;;     gdk_color_state_get_rec2100_linear
;;;     gdk_color_state_get_rec2100_pq
;;;     gdk_color_state_get_srgb
;;;     gdk_color_state_get_srgb_linear
;;;
;;;     gdk_color_state_create_cicp_params
;;;     gdk_color_state_equal
;;;     gdk_color_state_ref
;;;     gdk_color_state_unref
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkColorState
;;; ----------------------------------------------------------------------------

(glib:define-gboxed-opaque color-state "GdkColorState"
  :export t
  :type-initializer "gdk_color_state_get_type"
  :alloc
  (error "GdkColorState cannot be created from the Lisp side"))

#+liber-documentation
(setf (liber:alias-for-class 'color-state)
      "GBoxed"
      (documentation 'color-state 'type)
 "@version{2024-11-6}
  @begin{declaration}
(glib:define-gboxed-opaque color-state \"GdkColorState\"
  :export t
  :type-initializer \"gdk_color_state_get_type\"
  :alloc (error \"GdkColorState cannot be created from the Lisp side\"))
  @end{declaration}
  @begin{short}
    The @class{gdk:color-state} instance provides the information to interpret
    colors and pixels in a variety of ways.
  @end{short}
  They are also known as color spaces.

  Crucially, GTK knows how to convert colors from one color state to another.
  The @class{gdk:color-state} instances are immutable and therefore threadsafe.

  Since 4.16
  @see-class{gdk:texture}")

;;; ----------------------------------------------------------------------------
;;; gdk_color_state_get_oklab                               Since 4.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_color_state_get_oklch                               Since 4.18
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_color_state_get_rec2100_linear
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_color_state_get_rec2100_linear" color-state-rec2100-linear)
    (g:boxed color-state :return)
)

(export 'color-state-rec2100-linear)

;;; ----------------------------------------------------------------------------
;;; gdk_color_state_get_rec2100_pq
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_color_state_get_rec2100_pq" color-state-rec2100-pq)
    (g:boxed color-state :return)
)

(export 'color-state-rec2100-pq)

;;; ----------------------------------------------------------------------------
;;; gdk_color_state_get_srgb
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_color_state_get_srgb" color-state-srgb)
    (g:boxed color-state :return)
)

(export 'color-state-srgb)

;;; ----------------------------------------------------------------------------
;;; gdk_color_state_get_srgb_linear
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_color_state_get_srgb_linear" color-state-srgb-linear)
    (g:boxed color-state :return)
)

(export 'color-state-srgb-linear)

;;; ----------------------------------------------------------------------------
;;; gdk_color_state_create_cicp_params
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_color_state_create_cicp_params"
               color-state-create-cicp-params)
    (g:object cicp-params :already-referenced)
  (color (g:boxed color-state)))

(export 'color-state-create-cicp-params)

;;; ----------------------------------------------------------------------------
;;; gdk_color_state_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_color_state_equal" color-state-equal) :boolean
  (color1 (g:boxed color-state))
  (color2 (g:boxed color-state)))

(export 'color-state-equal)

;;; --- End of file gdk4.color-state.lisp --------------------------------------
