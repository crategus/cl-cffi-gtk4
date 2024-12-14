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
 "@version{2024-11-28}
  @begin{declaration}
(glib:define-gboxed-opaque color-state \"GdkColorState\"
  :export t
  :type-initializer \"gdk_color_state_get_type\"
  :alloc (error \"GdkColorState cannot be created from the Lisp side\"))
  @end{declaration}
  @begin{short}
    The @class{gdk:color-state} structure provides the information to interpret
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
 #+liber-documentation
 "@version{2024-11-10}
  @return{The @class{gdk:color-state} instance for linearized rec2100.}
  @begin{short}
    Returns the color state object representing the linear rec2100 color space.
  @end{short}
  This color state uses the primaries defined by BT.2020-2 and BT.2100-0 and a
  linear transfer function. It is equivalent to the Cicp tuple 9/8/0/1. See
  for example the
  @url[https://drafts.csswg.org/css-color-hdr/#valdef-color-rec2100-linear]{CSS HDR Module}
  for details about this colorstate.

  Since 4.16
  @see-class{gdk:color-state}")

(export 'color-state-rec2100-linear)

;;; ----------------------------------------------------------------------------
;;; gdk_color_state_get_rec2100_pq
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_color_state_get_rec2100_pq" color-state-rec2100-pq)
    (g:boxed color-state :return)
 #+liber-documentation
 "@version{2024-11-10}
  @return{The @class{gdk:color-state} instance for rec2100-pq.}
  @begin{short}
    Returns the color state object representing the rec2100-pq color space.
  @end{short}
  This color state uses the primaries defined by BT.2020-2 and BT.2100-0 and the
  transfer function defined by SMPTE ST 2084 and BT.2100-2. It is equivalent to
  the Cicp tuple 9/16/0/1. See for example the
  @url[https://drafts.csswg.org/css-color-hdr/#valdef-color-rec2100-pq]{CSS HDR Module}
  for details about
  this colorstate.

  Since 4.16
  @see-class{gdk:color-state}")

(export 'color-state-rec2100-pq)

;;; ----------------------------------------------------------------------------
;;; gdk_color_state_get_srgb
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_color_state_get_srgb" color-state-srgb)
    (g:boxed color-state :return)
 #+liber-documentation
 "@version{2024-11-10}
  @return{The @class{gdk:color-state} instance for sRGB.}
  @begin{short}
    Returns the color state object representing the sRGB color space.
  @end{short}
  This color state uses the primaries defined by BT.709-6 and the transfer
  function defined by IEC 61966-2-1. It is equivalent to the Cicp tuple
  1/13/0/1. See for example the
  @url[https://www.w3.org/TR/css-color-4/#predefined-sRGB]{CSS HDR Module}
  for details about this colorstate.

  Since 4.16
  @see-class{gdk:color-state}")

(export 'color-state-srgb)

;;; ----------------------------------------------------------------------------
;;; gdk_color_state_get_srgb_linear
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_color_state_get_srgb_linear" color-state-srgb-linear)
    (g:boxed color-state :return)
 #+liber-documentation
 "@version{2024-11-10}
  @return{The @class{gdk:color-state} instance for linearized sRGB.}
  @begin{short}
    Returns the color state object representing the linearized sRGB color space.
  @end{short}
  This color state uses the primaries defined by BT.709-6 and a linear transfer
  function. It is equivalent to the Cicp tuple 1/8/0/1. See for example the
  @url[https://www.w3.org/TR/css-color-4/#predefined-sRGB-linear]{CSS HDR Module}
  for details about this colorstate.

  Since 4.16
  @see-class{gdk:color-state}")

(export 'color-state-srgb-linear)

;;; ----------------------------------------------------------------------------
;;; gdk_color_state_create_cicp_params
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_color_state_create_cicp_params"
               color-state-create-cicp-params) (g:object cicp-params :return)
 #+liber-documentation
 "@version{2024-11-10}
  @argument[color]{a @class{gdk:color-state} instance}
  @return{The new @class{gdk:cicp-params} object.}
  @begin{short}
    Create a @class{gdk:cicp-params} object representing the colorstate.
  @end{short}
  It is not guaranteed that every @class{gdk:color-state} instance can be
  represented with Cicp parameters. If that is the case, this function returns
  @code{nil}.

  Since 4.16
  @see-class{gdk:color-state}
  @see-class{gdk:cicp-params}"
  (color (g:boxed color-state)))

(export 'color-state-create-cicp-params)

;;; ----------------------------------------------------------------------------
;;; gdk_color_state_equal
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_color_state_equal" color-state-equal) :boolean
 #+liber-documentation
 "@version{2024-11-10}
  @argument[color1]{a @class{gdk:color-state} instance}
  @argument[color2]{another @class{gdk:color-state} instance}
  @begin{short}
    Compares two color states for equality.
  @end{short}
  Note that this function is not guaranteed to be perfect and two objects
  describing the same color state may compare not equal. However, different
  color states will never compare equal.

  Since 4.16
  @see-class{gdk:color-state}"
  (color1 (g:boxed color-state))
  (color2 (g:boxed color-state)))

(export 'color-state-equal)

;;; --- End of file gdk4.color-state.lisp --------------------------------------
