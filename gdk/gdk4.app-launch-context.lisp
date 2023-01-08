;;; ----------------------------------------------------------------------------
;;; gdk.app-launch-context.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
;;;
;;; Copyright (C) 2012 - 2022 Dieter Kaiser
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
;;; Application launching
;;;
;;;     Startup notification for applications
;;;
;;; Types and Values
;;;
;;;     GdkAppLaunchContext
;;;
;;; Accessors
;;;
;;;     gdk_app_launch_context_get_display
;;;
;;; Functions
;;;
;;;     gdk_app_launch_context_set_desktop
;;;     gdk_app_launch_context_set_timestamp
;;;     gdk_app_launch_context_set_icon
;;;     gdk_app_launch_context_set_icon_name
;;;
;;; Properties
;;;
;;;     display
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GAppLaunchContext
;;;         ╰── GdkAppLaunchContext
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkAppLaunchContext
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkAppLaunchContext" app-launch-context
  (:superclass g:app-launch-context
   :export t
   :interfaces nil
   :type-initializer "gdk_app_launch_context_get_type")
  ((display
    app-launch-context-display
    "display" "GdkDisplay" t t)))

#+liber-documentation
(setf (documentation 'app-launch-context 'type)
 "@version{#2021-12-11}
  @begin{short}
    The @sym{gdk:app-launch-context} object is an implementation of the
    @class{g-app-launch-context} object that handles launching an application
    in a graphical context.
  @end{short}
  It provides startup notification and allows to launch applications on a
  specific screen or workspace.
  @begin[Example]{dictionary}
    Launching an application.
    @begin{pre}
GdkAppLaunchContext *context;

context = gdk_display_get_app_launch_context (display);

gdk_app_launch_context_set_display (display);
gdk_app_launch_context_set_timestamp (gdk_event_get_time (event));

if (!g_app_info_launch_default_for_uri (\"http://www.gtk.org\",
                                        context, &error))
  g_warning (\"Launching failed: %s\n\", error->message);

g_object_unref (context);
    @end{pre}
  @end{dictionary}
  @see-class{g:app-launch-context}
  @see-slot{gdk:app-launch-context-display}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- app-launch-context-display ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "display"
                                               'app-launch-context) t)
 "The @code{display} property of type @class{gdk:display}
  (Read / Write / Construct Only) @br{}
  The display that the launch context is for.")

#+liber-documentation
(setf (liber:alias-for-function 'app-launch-context-display)
      "Accessor"
      (documentation 'app-launch-context-display 'function)
 "@version{#2021-12-11}
  @syntax[]{(gdk:app-launch-context-display object) => display}
  @syntax[]{(setf (gdk:app-launch-context-display object) display)}
  @argument[object]{a @class{gdk:app-launch-context} object}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    Accessor of the @code{display} slot of the @class{gdk:app-launch-context}
    class.
  @end{short}
  The @sym{(setf gdk:app-launch-context-display)} function gets the display
  that the launch context is for.
  @see-class{gdk:app-launch-context}
  @see-class{gdk:display}")

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_set_desktop ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_app_launch_context_set_desktop" app-launch-context-set-desktop)
    :void
 #+liber-documentation
 "@version{#2021-12-11}
  @argument[context]{a @class{gdk:app-launch-context} object}
  @argument[desktop]{an integer with the number of a workspace, or -1}
  @begin{short}
    Sets the workspace on which applications will be launched when using this
    context when running under a window manager that supports multiple
    workspaces, as described in the Extended Window Manager Hints.
  @end{short}

  When the workspace is not specified or the @arg{desktop} argument is set to
  -1, it is up to the window manager to pick one, typically it will be the
  current workspace.
  @see-class{gdk:app-launch-context}"
  (context (g:object app-launch-context))
  (desktop :int))

(export 'app-launch-context-set-desktop)

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_set_timestamp ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_app_launch_context_set_timestamp"
           app-launch-context-set-timestamp) :void
 #+liber-documentation
 "@version{#2021-12-11}
  @argument[context]{a @class{gdk:app-launch-context} object}
  @argument[timestamp]{an unsigned integer with a timestamp}
  @begin{short}
    Sets the timestamp of @arg{context}.
  @end{short}
  The timestamp should ideally be taken from the event that triggered the
  launch.

  Window managers can use this information to avoid moving the focus to the
  newly launched application when the user is busy typing in another window.
  This is also known as 'focus stealing prevention'.
  @see-class{gdk:app-launch-context}"
  (context (g:object app-launch-context))
  (timestamp :uint32))

(export 'app-launch-context-set-timestamp)

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_set_icon ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_app_launch_context_set_icon" app-launch-context-set-icon) :void
 #+liber-documentation
 "@version{#2021-12-11}
  @argument[context]{a @class{gdk:app-launch-context} object}
  @argument[icon]{a @class{g:icon} object, or @code{nil}}
  @begin{short}
    Sets the icon for applications that are launched with this context.
  @end{short}
  Window Managers can use this information when displaying startup
  notification. See also the @fun{gdk:app-launch-context-set-icon-name}
  function.
  @see-class{gdk:app-launch-context}
  @see-class{g:icon}
  @see-function{gdk:app-launch-context-set-icon-name}"
  (context (g:object app-launch-context))
  (icon (g:object g:icon)))

(export 'app-launch-context-set-icon)

;;; ----------------------------------------------------------------------------
;;; gdk_app_launch_context_set_icon_name ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_app_launch_context_set_icon_name"
           app-launch-context-set-icon-name) :void
 #+liber-documentation
 "@version{#2021-12-11}
  @argument[context]{a @class{gdk:app-launch-context} object}
  @argument[name]{a string with an icon name, or @code{nil}}
  @begin{short}
    Sets the icon for applications that are launched with this context.
  @end{short}
  The icon name will be interpreted in the same way as the icon field in
  desktop files. See also the @fun{gdk:app-launch-context-set-icon} function.

  If both an icon and an icon name are set, the @arg{name} argument takes
  priority. If neither an icon or an icon name is set, the icon is taken from
  either the file that is passed to launched application or from the
  @class{g-app-info} object for the launched application itself.
  @see-class{gdk:app-launch-context}
  @see-class{g:app-info}
  @see-function{gdk:app-launch-context-set-icon}"
  (context (g:object app-launch-context))
  (name :string))

(export 'app-launch-context-set-icon-name)

;;; -- End of file gdk.app-launch-context.lisp ---------------------------------
