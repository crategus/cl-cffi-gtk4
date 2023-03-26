;;; ----------------------------------------------------------------------------
;;; cl-cffi-gtk4-init.lisp
;;;
;;; Copyright (C) 2011 - 2023 Dieter Kaiser
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

(defpackage :gtk4-init
  (:use :cl :cffi))

(in-package :gtk4-init)

(glib-init:at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (define-foreign-library gtk4
      ((:and :unix (:not :darwin))
       (:or "libgtk-4.so.1" "libgtk-4.so.0" "libgtk-4.so"))
      (:darwin (:or "libgtk-4.0.dylib"
                    "libgtk-4.dylib"
                    "libgtk-x11-4.0.0.dylib"
                    "libgtk-x11-4.0.dylib"))
      (:windows (:or "libgtk-4-1.dll" "libgtk-win32-4.0-0.dll"))
      (t "libgtk-4-0"))
    (pushnew :gtk4 *features*))
  (use-foreign-library gtk4))

(glib-init::push-library-version-features gtk
    ;; We can not call the Lisp implementations gtk-major-version and
    ;; gtk-minor-version because GTK is not compiled at this time.
    (cffi:foreign-funcall "gtk_get_major_version" :int)
    (cffi:foreign-funcall "gtk_get_minor_version" :int)
    4 0
    4 2
    4 4
    4 6
    )

;;; --- End of file cl-cffi-gtk4-init.lisp -------------------------------------
