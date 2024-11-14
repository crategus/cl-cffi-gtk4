;;; ----------------------------------------------------------------------------
;;; gdk4.cicp-params.lisp
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
;;;     GdkCicpParams
;;;     GdkCicpRange
;;;
;;; Accessors
;;;
;;;     gdk_cicp_params_get_color-primaries
;;;     gdk_cicp_params_set_color_primaries
;;;     gdk_cicp_params_get_matrix_coefficients
;;;     gdk_cicp_params_set_matrix_coefficients
;;;     gdk_cicp_params_get_range
;;;     gdk_cicp_params_set_range
;;;     gdk_cicp_params_get_transfer_function
;;;     gdk_cicp_params_set_transfer_function
;;;
;;; Functions
;;;
;;;     gdk_cicp_params_new
;;;     gdk_cicp_params_build_color_state
;;;
;;; Properties
;;;
;;;     color-primaries
;;;     matrix-coefficients
;;;     range
;;;     transfer-function
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkCicpRange
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GdkCicpRange" cicp-range
  (:export t
   :type-initializer "gdk_cicp_range_get_type")
  :narrow
  :full)

#+liber-documentation
(setf (liber:alias-for-symbol 'cicp-range)
      "GEnum"
      (liber:symbol-documentation 'cicp-range)
 "@version{2024-11-10}
  @begin{declaration}
(gobject:define-genum \"GdkCicpRange\" cicp-range
  (:export t
   :type-initializer \"gdk_cicp_range_get_type\")
  :narrow
  :full)
  @end{declaration}
  @begin{values}
    @begin[code]{table}
      @entry[:narrow]{The values use the range of 16-235 (for Y) and 16-240
        for u and v.}
      @entry[:full]{The values use the full range.}
    @end{table}
  @end{values}
  @begin{short}
  The values of this enumeration describe whether image data uses the full range
  of 8-bit values.
  @end{short}

  In digital broadcasting, it is common to reserve the lowest and highest
  values. Typically the allowed values for the narrow range are 16-235 for Y and
  16-240 for u,v (when dealing with YUV data).

  Since 4.16
  @see-class{gdk:cicp-params}")

;;; ----------------------------------------------------------------------------
;;; GdkCicpParams
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkCicpParams" cicp-params
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_cicp_params_get_type")
  ((color-primaries
    cicp-params-color-primaries
    "color-primaries" "guint" t t)
   (matrix-coefficients
    cicp-params-matrix-coefficients
    "matrix-coefficients" "guint" t t)
   (range
    cicp-params-range
    "range" "GdkCicpRange" t t)
   (transfer-function
    cicp-params-transfer-function
    "transfer-function" "guint" t t)))

#+liber-documentation
(setf (documentation 'cicp-params 'type)
 "@version{2024-11-10}
  @begin{short}
   The @class{gdk:cicp-params} object contains the parameters that define a
   colorstate according to the
   @url[https://www.itu.int/rec/T-REC-H.273/en]{ITU-T H.273} specification.
  @end{short}
  See the documentation of individual properties for supported values.

  The 'unspecified' value (2) is not treated in any special way, and must be
  replaced by a different value before creating a color state.

  The @class{gdk:cicp-params} object can be used as a builder object to
  construct a color state from Cicp data with the
  @fun{gdk:cicp-params-build-color-state} function. The function will return an
  error if the given parameters are not supported.

  You can obtain a @class{gdk:cicp-params} object from a color state with the
  @fun{gdk:color-state-create-cicp-params} function. This can be used to create
  a variant of a color state, by changing just one of the Cicp parameters, or
  just to obtain information about the color state.

  Since 4.16
  @see-constructor{gdk:cicp-params-new}
  @see-slot{gdk:cicp-params-color-primaries}
  @see-slot{gdk:cicp-params-matrix-coefficients}
  @see-slot{gdk:cicp-params-range}
  @see-slot{gdk:cicp-params-transfer-function}
  @see-class{gdk:color-state}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk:cicp-params-color-primaries ----------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "color-primaries"
                                               'cicp-params) t)
 "The @code{color-primaries} property of type @code{:uint} (Read / Write) @br{}
  The color primaries to use. @br{}
  Default value: 2")

#+liber-documentation
(setf (liber:alias-for-function 'cicp-params-color-primaries)
      "Accessor"
      (documentation 'cicp-params-color-primaries 'function)
 "@version{#2024-11-10}
  @syntax{(gdk:cicp-params-color-primaries object) => primaries}
  @syntax{(setf (gdk:cicp-params-color-primaries object) primaries)}
  @argument[object]{a @class{gdk:cicp-params} object}
  @argument[primaries]{an unsigned integer with the color primaries value}
  @begin{short}
    Accessor of the @slot[gdk:cicp-params]{color-primaries} slot of the
    @class{gdk:cicp-params} class.
  @end{short}
  The @fun{gdk:cicp-params-color-primaries} function returns the color
  primaries value. The @setf{gdk:cicp-params-color-primaries} function sets
  the value.

  Supported values are:
  @begin{itemize}
    @item{1: BT.709 / sRGB}
    @item{2: unspecified}
    @item{5: PAL}
    @item{6,7: BT.601 / NTSC}
    @item{9: BT.2020}
    @item{12: Display P3}
  @end{itemize}
  Since 4.16
  @see-class{gdk:cicp-params}")

;;; --- gdk:cicp-params-matrix-coefficients ------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "matrix-coefficients"
                                               'cicp-params) t)
 "The @code{matrix-coefficients} property of type @code{:uint} (Read / Write)
  @br{}
  The matrix coefficients (for YUV to RGB conversion). @br{}
  Default value: 2")

#+liber-documentation
(setf (liber:alias-for-function 'cicp-params-matrix-coefficients)
      "Accessor"
      (documentation 'cicp-params-matrix-coefficients 'function)
 "@version{#2024-11-10}
  @syntax{(gdk:cicp-params-matrix-coefficients object) => coefficients}
  @syntax{(setf (gdk:cicp-params-matrix-coefficients object) coefficients)}
  @argument[object]{a @class{gdk:cicp-params} object}
  @argument[coefficients]{an unsigned integer with the matrix coefficients
    value}
  @begin{short}
    Accessor of the @slot[gdk:cicp-params]{matrix-coefficients} slot of the
    @class{gdk:cicp-params} class.
  @end{short}
  The @fun{gdk:cicp-params-matrix-coefficients} function gets the matrix
  coefficients of @arg{object}. The @setf{gdk:cicp-params-matrix-coefficients}
  function sets the matrix coefficients.

  Supported values are:
  @begin{itemize}
    @item{0: RGB}
    @item{2: unspecified}
  @end{itemize}
  Since 4.16
  @see-class{gdk:cicp-params}")

;;; --- gdk:cicp-params-range --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "range" 'cicp-params) t)
 "The @code{range} property of type @symbol{gdk:cicp-range} (Read / Write) @br{}
  Whether the data is using the full range of values. @br{}
  Default value: @code{:narrow}")

#+liber-documentation
(setf (liber:alias-for-function 'cicp-params-range)
      "Accessor"
      (documentation 'cicp-params-range 'function)
 "@version{#2024-11-10}
  @syntax{(gdk:cicp-params-range object) => range}
  @syntax{(setf (gdk:cicp-params-range object) range)}
  @argument[object]{a @class{gdk:cicp-params} object}
  @argument[range]{a @symbol{gdk:cicp-range} value}
  @begin{short}
    Accessor of the @slot[gdk:cicp-params]{range} slot of the
    @class{gdk:cicp-params} class.
  @end{short}
  The @fun{gdk:cicp-params-range} function gets the range value of @arg{object}.
  The @setf{gdk:cicp-params-range} function sets the range value.
  @see-class{gdk:cicp-params}
  @see-symbol{gdk:cicp-range}")

;;; --- gdk:cicp-params-transfer-function --------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "transfer-function"
                                               'cicp-params) t)
 "The @code{transfer-function} property of type @code{:uint} (Read / Write)
  @br{}
  The transfer function to use. @br{}
  Default value: 2")

#+liber-documentation
(setf (liber:alias-for-function 'cicp-params-transfer-function)
      "Accessor"
      (documentation 'cicp-params-transfer-function 'function)
 "@version{#2024-11-10}
  @syntax{(gdk:cicp-params-transfer-function object) => function}
  @syntax{(setf (gdk:cicp-params-transfer-function object) function)}
  @argument[object]{a @class{gdk:cicp-params} object}
  @argument[function]{an unsigned integer with the transfer function value}
  @begin{short}
    Accessor of the @slot[gdk:cicp-params]{transfer-function} slot of the
    @class{gdk:cicp-params} class.
  @end{short}
  The @fun{gdk:cicp-params-transfer-function} function gets the transfer
  function value of @arg{object}. The @setf{gdk:cicp-params-transfer-function}
  function sets the transfer function value.

  Supported values are:
  @begin{itemize}
    @item{1,6,14,15: BT.709, BT.601, BT.2020}
    @item{2: unspecified}
    @item{4: gamma 2.2}
    @item{5: gamma 2.8}
    @item{8: linear}
    @item{13: sRGB}
    @item{16: BT.2100 PQ}
    @item{18: BT.2100 HLG}
  @end{itemize}
  Since 4.16
  @see-class{gdk:cicp-params}")

;;; ----------------------------------------------------------------------------
;;; gdk_cicp_params_new
;;; ----------------------------------------------------------------------------

(defun cicp-params-new ()
 #+liber-documentation
 "@version{#2024-11-10}
  @return{The new @class{gdk:cicp-params} object.}
  @begin{short}
    Creates a new @class{gdk:cicp-params} object.
  @end{short}
  The initial values of the properties are the values for 'undefined' and need
  to be set before a color state object can be built.

  Since 4.16
  @see-class{gdk:cicp-params}"
  (make-instance 'cicp-params))

(export 'cicp-params-new)

;;; ----------------------------------------------------------------------------
;;; gdk_cicp_params_build_color_state
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_cicp_params_build_color_state"
               %cicp-params-build-color-state) (g:boxed color-state :return)
  (params (g:object cicp-params))
  (err :pointer))

(defun cicp-params-build-color-state (params)
 #+liber-documentation
 "@version{#2024-11-10}
  @argument[params]{a @class{gdk:cicp-params} object}
  @return{The newly allocated @class{gdk:color-state} instance.}
  @begin{short}
    Creates a new @class{gdk:color-state} object for the Cicp parameters in
    @arg{params}.
  @end{short}
  Note that this may fail if the Cicp parameters in @arg{params} are not
  supported by GTK. In that case, @code{nil} is returned.

  Since 4.16
  @see-class{gdk:cicp-params}
  @see-class{gdk:color-state}"
  (glib:with-error (err)
    (%cicp-params-build-color-state params err)))

(export 'cicp-params-build-color-state)

;;; --- End of file gdk4.cicp-params.lisp --------------------------------------
