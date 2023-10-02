;;; ----------------------------------------------------------------------------
;;; gtk4.shortcut-manager.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2023 Dieter Kaiser
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
;;; GtkShortcutManager
;;;
;;;     Interface for managing shortcuts
;;;
;;; Types and Values
;;;
;;;     GtkShortcutManager
;;;
;;; Hierarchy
;;;
;;;     GInterface
;;;     ╰── GtkShortcutManager
;;;
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkShortcutManager
;;; ----------------------------------------------------------------------------

(gobject:define-g-interface "GtkShortcutManager" shortcut-manager
  (:export t
   :type-initializer "gtk_shortcut_manager_get_type")
  nil)

#+liber-documentation
(setf (liber:alias-for-class 'shortcut-manager)
      "Interface"
      (documentation 'shortcut-manager 'type)
 "@version{#2022-2-1}
  @begin{short}
    The @class{gtk:shortcut-manager} interface is used to implement shortcut
    scopes.
  @end{short}
  This is important for @class{gtk:native} widgets that have their own surface,
  since the event controllers that are used to implement managed and global
  scopes are limited to the same native.

  Every widget that implements the @class{gtk:shortcut-manager} interface will 
  be used with the @code{:managed} value of the @symbol{gtk:shortcut-scope}
  enumeration.

  Examples for widgets implementing the @class{gtk:shortcut-manager} interface 
  are the @class{gtk:window} and @class{gtk:popover} widgets.
  @see-class{gtk:shortcut-scopes}
  @see-class{gtk:native}
  @see-class{gtk:window}
  @see-class{gtk:popover}
  @see-symbol{gtk:shortcut-scope}")

;;; --- End of file gtk4.shortcut-manager.lisp ---------------------------------
