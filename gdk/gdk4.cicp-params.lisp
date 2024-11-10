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

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;;     color-primaries
;;;     matrix-coefficients
;;;     range
;;;     transfer-function

;;; ----------------------------------------------------------------------------
;;; gdk_cicp_params_new
;;; ----------------------------------------------------------------------------

(defun cicp-params-new ()
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
  (glib:with-error (err)
    (%cicp-params-build-color-state params err)))

(export 'cicp-params-build-color-state)

;;; --- End of file gdk4.cicp-params.lisp --------------------------------------
