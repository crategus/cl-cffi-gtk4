;;; ----------------------------------------------------------------------------
;;; cl-cffi-gtk4-init.lisp
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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

(defpackage :gtk-init
  (:use :cl)
  (:export *gtk-warn-deprecated*))

(in-package :gtk-init)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *gtk-warn-deprecated* t)
  (when *gtk-warn-deprecated*
    (pushnew :gtk-warn-deprecated *features*)))

#+sbcl
(when (and (find-package "SB-EXT")
           (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT")))
  (funcall (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT"))
           :traps nil))

(glib-init:at-init ()
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (cffi:define-foreign-library gtk4
      ((:and :unix (:not :darwin))
       (:or "libgtk-4.so.1" "libgtk-4.so.0" "libgtk-4.so"))
      (:darwin (:or "libgtk-4.0.dylib"
                    "libgtk-4.dylib"
                    "libgtk-x11-4.0.0.dylib"
                    "libgtk-x11-4.0.dylib"))
      (:windows (:or "libgtk-4-1.dll" "libgtk-4.dll"))
      (t "libgtk-4-0"))
    ;; push the hostname on *features*
    (pushnew (intern (string-upcase (machine-instance)) :keyword) *features*)
    (pushnew :gtk4 *features*))
  (cffi:use-foreign-library gtk4))

(glib-init::push-library-version-features gtk
    ;; We can not call the Lisp implementations gtk-major-version and
    ;; gtk-minor-version because GTK is not compiled at this time.
    (cffi:foreign-funcall "gtk_get_major_version" :int)
    (cffi:foreign-funcall "gtk_get_minor_version" :int)
    4 0
    4 2
    4 4   ; Since 10-08-2021
    4 6   ; Since 30-12-2021
    4 8   ; Since 06-09-2022
    4 10  ; Since 04-03-2023
    4 12  ; Since 05-08-2023
    4 14  ; Since 12-03-2024
    4 16  ; Since 07-09-2024
    4 18
    )

;;; --- End of file cl-cffi-gtk4-init.lisp -------------------------------------
