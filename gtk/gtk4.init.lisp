;;; ----------------------------------------------------------------------------
;;; gtk.init.lisp
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

(in-package :gtk)

;; FIXME: We can get the floating point error: "arithmetic error
;; DIVISION-BY-ZERO signalled". This code disables floating point errors for
;; SBCL. Check which part of the library can produce floating point errors.

#+nil
(when (and (find-package "SB-EXT")
           (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT")))
  (funcall (find-symbol "SET-FLOATING-POINT-MODES" (find-package "SB-EXT"))
           :traps nil))

(defun finalize-subclasses (class)
  (c2mop:ensure-finalized class)
  (iter (for subclass in (c2mop:class-direct-subclasses class))
        (finalize-subclasses subclass)))

(defun finalize-gtk-classes ()
  (finalize-subclasses (find-class 'g:object)))

(finalize-gtk-classes)

;; Initialize the GTK main loop
(glib-init:at-init ()
  (init))

;;; --- End of file gtk.init.lisp ----------------------------------------------
