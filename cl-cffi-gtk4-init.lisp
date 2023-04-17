;;; ----------------------------------------------------------------------------
;;; cl-cffi-gtk4-init.lisp
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
    4 8
    4 10
    4 12
    )

;;; --- End of file cl-cffi-gtk4-init.lisp -------------------------------------
