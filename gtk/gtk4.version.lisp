;;; ----------------------------------------------------------------------------
;;; gtk.version.lisp
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
;;;     gtk_get_binary_age
;;;     gtk_get_interface_age
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

(defcfun ("gtk_get_major_version" major-version) :uint
 #+liber-documentation
 "@version{#2022-1-4}
  @return{An unsigned integer with the major version number of the GTK library.}
  @begin{short}
    Returns the major version number of the GTK library.
  @end{short}
  This function is in the library, so it represents the GTK library your code
  is running against.
  @see-function{check-version}")

(export 'major-version)

;;; ----------------------------------------------------------------------------
;;; gtk_get_minor_version -> minor-version
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_get_minor_version" minor-version) :uint
 #+liber-documentation
 "@version{#2022-1-4}
  @return{An unsigned integer with the minor version number of the GTK library.}
  @begin{short}
    Returns the minor version number of the GTK library.
  @end{short}
  This function is in the library, so it represents the GTK library your code
  is are running against.
  @see-function{check-version}")

(export 'minor-version)

;;; ----------------------------------------------------------------------------
;;; gtk_get_micro_version -> micro-version
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_get_micro_version" micro-version) :uint
 #+liber-documentation
 "@version{#2022-1-4}
  @return{An unsigned integer with the micro version number of the GTK library.}
  @begin{short}
    Returns the micro version number of the GTK library.
  @end{short}
  This function is in the library, so it represents the GTK library your code
  is are running against.
  @see-function{check-version}")

(export 'micro-version)

;;; ----------------------------------------------------------------------------
;;; gtk_get_binary_age -> binary-age                       not implemented
;;; ----------------------------------------------------------------------------

#+nil
(defcfun ("gtk_get_binary_age" binary-age) :uint
 #+liber-documentation
 "@version{#2022-1-4}
  @return{An unsigned integer with the binary age of the GTK library.}
  @begin{short}
    Returns the binary age as passed to @code{libtool} when building the GTK
    library the process is running against.
  @end{short}
  If @code{libtool} means nothing to you, do not worry about it.
  @see-function{interface-age}")

#+nil
(export 'binary-age)

;;; ----------------------------------------------------------------------------
;;; gtk_get_interface_age -> interface-age                 not implemented
;;; ----------------------------------------------------------------------------

#+nil
(defcfun ("gtk_get_interface_age" interface-age) :uint
 #+liber-documentation
 "@version{#2022-1-4}
  @return{An unsigned integer with the interface age of the GTK library.}
  @begin{short}
    Returns the interface age as passed to @code{libtool} when building the
    GTK library the process is running against.
  @end{short}
  If @code{libtool} means nothing to you, do not worry about it.
  @see-function{binary-age}")

#+nil
(export 'interface-age)

;;; ----------------------------------------------------------------------------
;;; gtk_check_version
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_check_version" check-version) :string
 #+liber-documentation
 "@version{#2022-1-4}
  @argument[major]{an unsigned integer with the required major version}
  @argument[minor]{an unsigned integer with the required minor version}
  @argument[micro]{an unsigned integer with the required micro version}
  @begin{return}
    The @code{Nil} value if the GTK library is compatible with the given
    version, or a string describing the version mismatch.
  @end{return}
  @begin{short}
    Checks that the GTK library in use is compatible with the given version.
  @end{short}
  Compatibility is defined by two things: first the version of the running
  library is newer than the version @arg{major}.@arg{minor}.@arg{micro}.
  Second the running library must be binary compatible with the version
  @arg{major}.@arg{minor}.@arg{micro} (same major version).
  @see-function{major-version}
  @see-function{minor-version}
  @see-function{micro-version}"
  (major :uint)
  (minor :uint)
  (micro :uint))

(export 'check-version)

;;; ----------------------------------------------------------------------------

;; TODO: Rewrite the function to allow writing into a string

(defun cl-cffi-gtk-build-info (&optional (out *standard-output*))
 #+liber-documentation
 "@version{#2022-11-6}
  @argument[out]{an optional stream, the default is @code{*standard-output*}}
  @begin{short}
    Provides informations about the installation and the versions of the
    loaded libraries.
  @end{short}
  @begin[Example]{dictionary}
    @begin{pre}
(* (cl-cffi-gtk-build-info)
cl-cffi-gtk version: 1.0.0
cl-cffi-gtk build date: 15:55 12/28/2021
GTK version: 4.4.0
GLIB version: 2.68.4
GDK-Pixbuf version: 2.42.6
Pango version: 1.48.10
Cairo version: 1.16.0
Machine type: X86-64
Machine version: Intel(R) Core(TM) i5-4210U CPU @@ 1.70GHz
Software type: Linux
Software version: 5.13.0-22-generic
Lisp implementation type: SBCL
Lisp implementation version: 2.1.1.debian
NIL
    @end{pre}
  @end{dictionary}
  @see-function{major-version}
  @see-function{minor-version}
  @see-function{micro-version}
  @see-symbol{+glib-major-version+}
  @see-symbol{+glib-minor-version+}
  @see-symbol{+glib-micro-version+}
  @see-symbol{+gdk-pixbuf-version+}
  @see-function{pango:version-string}
  @see-function{cairo-version-string}"
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
          +glib-major-version+
          +glib-minor-version+
          +glib-micro-version+)
  (format out "GDK-Pixbuf version: ~a~%" +gdk-pixbuf-version+)
  (format out "Pango version: ~a~%" (pango:version-string))
  (format out "Cairo version: ~a~%" (cairo:version-string))
  (format out "Machine type: ~a~%" (machine-type))
  (format out "Machine version: ~a~%" (machine-version))
  (format out "Software type: ~a~%" (software-type))
  (format out "Software version: ~A~%" (software-version))
  (format out "Lisp implementation type: ~a~%" (lisp-implementation-type))
  (format out "Lisp implementation version: ~a~%" (lisp-implementation-version))
  nil)

;;; --- End of file gtk.version.lisp -------------------------------------------
