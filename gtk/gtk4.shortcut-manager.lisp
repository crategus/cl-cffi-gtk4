;;; ----------------------------------------------------------------------------
;;; gtk4.shortcut-manager.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2023 Dieter Kaiser
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
    The @sym{gtk:shortcut-manager} interface is used to implement shortcut
    scopes.
  @end{short}
  This is important for @class{gtk:native} widgets that have their own surface,
  since the event controllers that are used to implement managed and global
  scopes are limited to the same native.

  Every widget that implements the @sym{gtk:shortcut-manager} interface will be
  used with the @code{:managed} value of the @symbol{gtk:shortcut-scope}
  enumeration.

  Examples for widgets implementing the @sym{gtk:shortcut-manager} interface are
  the @class{gtk:window} and @class{gtk:popover} widgets.
  @see-class{gtk:shortcut-scopes}
  @see-class{gtk:native}
  @see-class{gtk:window}
  @see-class{gtk:popover}
  @see-symbol{gtk:shortcut-scope}")

;;; --- End of file gtk4.shortcut-manager.lisp ---------------------------------
