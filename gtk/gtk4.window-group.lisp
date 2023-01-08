;;; ----------------------------------------------------------------------------
;;; gtk.window-group.lisp
;;;
;;; The documentation of this file is taken from the GTK 4 Reference Manual
;;; Version 4.6 and modified to document the Lisp binding to the GTK library.
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
;;; GtkWindowGroup
;;;
;;;     Limit the effect of grabs
;;;
;;; Types and Values
;;;
;;;     GtkWindowGroup
;;;
;;; Functions
;;;
;;;     gtk_window_group_new
;;;     gtk_window_group_add_window
;;;     gtk_window_group_remove_window
;;;     gtk_window_group_list_windows
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GtkWindowGroup
;;; ----------------------------------------------------------------------------

(in-package :gtk)

;;; ----------------------------------------------------------------------------
;;; GtkWindowGroup
;;; ----------------------------------------------------------------------------

(define-g-object-class "GtkWindowGroup" window-group
  (:superclass g:object
    :export t
    :interfaces nil
    :type-initializer "gtk_window_group_get_type")
  nil)

#+liber-documentation
(setf (documentation 'window-group 'type)
 "@version{#2021-11-2}
  @begin{short}
    A @sym{gtk:window-group} object restricts the effect of grabs to windows in
    the same group, thereby making window groups almost behave like separate
    applications.
  @end{short}

  A window can be a member in at most one window group at a time. Windows that
  have not been explicitly assigned to a window group are implicitly treated
  like windows of the default window group.

  Window groups are referenced by each window in the window group. If the
  windows in the window group are subsequently destroyed, then they will be
  removed from the window group and drop their references on the window group.
  When all window have been removed, the window group will be freed.
  @see-constructor{gtk:window-group-new}")

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_new
;;; ----------------------------------------------------------------------------

(declaim (inline window-group-new))

(defun window-group-new ()
 #+liber-documentation
 "@version{#2021-11-2}
  @return{A new @class{gtk:window-group} object.}
  @begin{short}
    Creates a new window group.
  @end{short}
  Grabs added with the @fun{gtk:grab-add} function only affect windows within
  the same window group.
  @see-class{gtk:window-group}
  @see-function{gtk:grab-add}"
  (make-instance 'window-group))

(export 'window-group-new)

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_add_window
;;; ----------------------------------------------------------------------------

(defcfun (window-group-add-window "gtk_window_group_add_window") :void
 #+liber-documentation
 "@version{#2021-10-26}
  @argument[group]{a @class{gtk:window-group} object}
  @argument[window]{the @class{gtk:window} widget to add}
  @begin{short}
    Adds a window to a window group.
  @end{short}
  @see-class{gtk:window-group}
  @see-class{gtk:window}"
  (group (g:object window-group))
  (window (g:object window)))

(export 'window-group-add-window)

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_remove_window
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_group_remove_window" window-group-remove-window) :void
 #+liber-documentation
 "@version{#2021-10-26}
  @argument[group]{a @class{gtk:window-group} object}
  @argument[window]{the @class{gtk:window} widget to remove}
  @begin{short}
    Removes a window from a window group.
  @end{short}
  @see-class{gtk:window-group}
  @see-class{gtk:window}"
  (group (g:object window-group))
  (window (g:object window)))

(export 'window-group-remove-window)

;;; ----------------------------------------------------------------------------
;;; gtk_window_group_list_windows
;;; ----------------------------------------------------------------------------

(defcfun ("gtk_window_group_list_windows" window-group-list-windows)
    (g:list-t (g:object window))
 #+liber-documentation
 "@version{#2021-10-26}
  @argument[group]{a @class{gtk:window-group} object}
  @return{A list of windows inside the window group.}
  @begin{short}
    Returns a list of windows that belong to the window group.
  @end{short}
  @see-class{gtk:window-group}
  @see-class{gtk:window}"
  (group (g:object window-group)))

(export 'window-group-list-windows)

;;; --- gtk.window-group.lisp --------------------------------------------------
