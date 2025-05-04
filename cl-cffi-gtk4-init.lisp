;;; ----------------------------------------------------------------------------
;;; cl-cffi-gtk4-init.lisp
;;;
;;; Copyright (C) 2011 - 2025 Dieter Kaiser
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
(when (and (find-package "SB-INT")
           (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-INT")))
  (funcall (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-INT"))
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
    ;; We cannot call the Lisp implementations gtk:major-version and
    ;; gtk:minor-version because GTK is not compiled at this time.
    (cffi:foreign-funcall "gtk_get_major_version" :int)
    (cffi:foreign-funcall "gtk_get_minor_version" :int)
    4 0   ; Since 2021-01-09
    4 2   ; Since 2021-03-27
    4 4   ; Since 2021-08-10
    4 6   ; Since 2021-12-30
    4 8   ; Since 2022-09-06
    4 10  ; Since 2023-03-04
    4 12  ; Since 2023-08-05
    4 14  ; Since 2024-04-12
    4 16  ; Since 2024-09-07
    4 18  ; Since 2025-03-14
    )

;;; --- End of file cl-cffi-gtk4-init.lisp -------------------------------------
