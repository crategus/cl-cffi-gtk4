;;; ----------------------------------------------------------------------------
;;; gtk4.version.lisp
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
;;; Version Information
;;;
;;;     Variables and functions to check the GTK version
;;;
;;; Types and Values
;;;
;;;     GTK_MAJOR_VERSION                                  not implemented
;;;     GTK_MINOR_VERSION                                  not implemented
;;;     GTK_MICRO_VERSION                                  not implemented
;;;     GTK_BINARY_AGE                                     not implemented
;;;     GTK_INTERFACE_AGE                                  not implemented
;;;     GTK_CHECK_VERSION                                  not implemented

;;; Functions
;;;
;;;     gtk_get_major_version
;;;     gtk_get_minor_version
;;;     gtk_get_micro_version
;;;     gtk_get_binary_age                                 not implemented
;;;     gtk_get_interface_age                              not implemented
;;;     gtk_check_version
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GTK_MAJOR_VERSION                                      not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_MINOR_VERSION                                      not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_MICRO_VERSION                                      not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_BINARY_AGE                                         not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_INTERFACE_AGE                                      not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; GTK_CHECK_VERSION()                                    not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_get_major_version -> major-version
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_get_major_version" major-version) :uint
 #+liber-documentation
 "@version{2023-3-24}
  @return{An unsigned integer with the major version number of the GTK library.}
  @begin{short}
    Returns the major version number of the GTK library.
  @end{short}
  This function is in the library, so it represents the GTK library your code
  is running against.
  @see-function{gtk:check-version}")

(export 'major-version)

;;; ----------------------------------------------------------------------------
;;; gtk_get_minor_version -> minor-version
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_get_minor_version" minor-version) :uint
 #+liber-documentation
 "@version{2023-3-24}
  @return{An unsigned integer with the minor version number of the GTK library.}
  @begin{short}
    Returns the minor version number of the GTK library.
  @end{short}
  This function is in the library, so it represents the GTK library your code
  is running against.
  @see-function{gtk:check-version}")

(export 'minor-version)

;;; ----------------------------------------------------------------------------
;;; gtk_get_micro_version -> micro-version
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_get_micro_version" micro-version) :uint
 #+liber-documentation
 "@version{2023-3-24}
  @return{An unsigned integer with the micro version number of the GTK library.}
  @begin{short}
    Returns the micro version number of the GTK library.
  @end{short}
  This function is in the library, so it represents the GTK library your code
  is running against.
  @see-function{gtk:check-version}")

(export 'micro-version)

;;; ----------------------------------------------------------------------------
;;; gtk_get_binary_age -> binary-age                       not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_get_interface_age -> interface-age                 not implemented
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gtk_check_version
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gtk_check_version" check-version) :string
 #+liber-documentation
 "@version{2023-3-24}
  @argument[major]{an unsigned integer with the required major version}
  @argument[minor]{an unsigned integer with the required minor version}
  @argument[micro]{an unsigned integer with the required micro version}
  @begin{return}
    The @code{nil} value if the GTK library is compatible with the given
    version, or a string describing the version mismatch.
  @end{return}
  @begin{short}
    Checks that the GTK library in use is compatible with the given version.
  @end{short}
  Compatibility is defined by two things: first the version of the running
  library is newer than the @arg{major}.@arg{minor}.@arg{micro} version.
  Second the running library must be binary compatible with the
  @arg{major}.@arg{minor}.@arg{micro} version (same major version).
  @see-function{gtk:major-version}
  @see-function{gtk:minor-version}
  @see-function{gtk:micro-version}"
  (major :uint)
  (minor :uint)
  (micro :uint))

(export 'check-version)

;;; ----------------------------------------------------------------------------

(defun cl-cffi-gtk-build-info (&optional (out *standard-output*))
 #+liber-documentation
 "@version{2023-3-24}
  @argument[out]{an optional stream, the default is @code{*standard-output*}}
  @begin{short}
    Provides informations about the installation and the versions of the
    loaded libraries.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(gtk:cl-cffi-gtk-build-info)
=>
cl-cffi-gtk version: 0.2.0
cl-cffi-gtk build date: 20:25 3/24/2023
GTK version: 4.6.6
GLIB version: 2.72.4
GDK-Pixbuf version: 2.42.8
Pango version: 1.50.6
Cairo version: 1.16.0
Machine type: X86-64
Machine version: Intel(R) Core(TM) i5-5200U CPU @@ 2.20GHz
Software type: Linux
Software version: 5.19.0-35-generic
Lisp implementation type: SBCL
Lisp implementation version: 2.1.11.debian
NIL
    @end{pre}
  @end{dictionary}
  @see-function{gtk:major-version}
  @see-function{gtk:minor-version}
  @see-function{gtk:micro-version}
  @see-symbol{glib:+glib-major-version+}
  @see-symbol{glib:+glib-minor-version+}
  @see-symbol{glib:+glib-micro-version+}
  @see-symbol{gdk-pixbuf:+gdk-pixbuf-version+}
  @see-function{pango:version-string}
  @see-function{cairo:version-string}"
  (format out "cl-cffi-gtk version: ~a~%" *cl-cffi-gtk-version*)
  (format out "cl-cffi-gtk build date: ~a:~a ~a/~a/~a~%"
          (third *cl-cffi-gtk-build-time*)
          (second *cl-cffi-gtk-build-time*)
          (fifth *cl-cffi-gtk-build-time*)
          (fourth *cl-cffi-gtk-build-time*)
          (sixth *cl-cffi-gtk-build-time*))
  (format out "GTK version: ~a.~a.~a~%"
          (major-version)
          (minor-version)
          (micro-version))
  (format out "GLIB version: ~a.~a.~a~%"
          glib:+glib-major-version+
          glib:+glib-minor-version+
          glib:+glib-micro-version+)
  (format out "GDK-Pixbuf version: ~a~%" gdk-pixbuf:+gdk-pixbuf-version+)
  (format out "Pango version: ~a~%" (pango:version-string))
  (format out "Cairo version: ~a~%" (cairo:version-string))
  (format out "Machine type: ~a~%" (machine-type))
  (format out "Machine version: ~a~%" (machine-version))
  (format out "Software type: ~a~%" (software-type))
  (format out "Software version: ~A~%" (software-version))
  (format out "Lisp implementation type: ~a~%" (lisp-implementation-type))
  (format out "Lisp implementation version: ~a~%" (lisp-implementation-version))
  nil)

(export 'cl-cffi-gtk-build-info)

;;; --- End of file gtk4.version.lisp ------------------------------------------
