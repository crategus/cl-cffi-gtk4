;;; ----------------------------------------------------------------------------
;;; gdk.toplevel-layout.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 Dieter Kaiser
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
;;; GdkToplevelLayout
;;;
;;;     Information for presenting toplevels
;;;
;;; Types and Values
;;;
;;;     GdkToplevelLayout
;;;
;;; Functions
;;;
;;;     gdk_toplevel_layout_new
;;;     gdk_toplevel_layout_ref
;;;     gdk_toplevel_layout_unref
;;;     gdk_toplevel_layout_copy
;;;     gdk_toplevel_layout_equal
;;;     gdk_toplevel_layout_set_maximized
;;;     gdk_toplevel_layout_get_maximized
;;;     gdk_toplevel_layout_set_fullscreen
;;;     gdk_toplevel_layout_get_fullscreen
;;;     gdk_toplevel_layout_get_fullscreen_monitor
;;;     gdk_toplevel_layout_set_resizable
;;;     gdk_toplevel_layout_get_resizable
;;;
;;; Object Hierarchy
;;;
;;;     GBoxed
;;;     ╰── GdkToplevelLayout
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;;Description
;;;Toplevel surfaces are sovereign windows that can be presented to the user in various states (maximized, on all workspaces, etc).

;;;The GdkToplevelLayout struct contains information that is necessary to do so, and is passed to gdk_toplevel_present().

;;;Functions
;;;gdk_toplevel_layout_new ()
;;;GdkToplevelLayout *
;;;gdk_toplevel_layout_new (void);
;;;Create a toplevel layout description.

;;;Used together with gdk_toplevel_present() to describe how a toplevel surface should be placed and behave on-screen.

;;;The size is in ”application pixels”, not ”device pixels” (see gdk_surface_get_scale_factor()).

;;;[constructor]

;;;Returns
;;;newly created instance of GdkToplevelLayout.

;;;[transfer full]

;;;gdk_toplevel_layout_ref ()
;;;GdkToplevelLayout *
;;;gdk_toplevel_layout_ref (GdkToplevelLayout *layout);
;;;Increases the reference count of layout .

;;;Parameters
;;;layout

;;;a GdkToplevelLayout

;;;
;;;Returns
;;;the same layout

;;;gdk_toplevel_layout_unref ()
;;;void
;;;gdk_toplevel_layout_unref (GdkToplevelLayout *layout);
;;;Decreases the reference count of layout .

;;;Parameters
;;;layout

;;;a GdkToplevelLayout

;;;
;;;gdk_toplevel_layout_copy ()
;;;GdkToplevelLayout *
;;;gdk_toplevel_layout_copy (GdkToplevelLayout *layout);
;;;Create a new GdkToplevelLayout and copy the contents of layout into it.

;;;Parameters
;;;layout

;;;a GdkToplevelLayout

;;;
;;;Returns
;;;a copy of layout .

;;;[transfer full]

;;;gdk_toplevel_layout_equal ()
;;;gboolean
;;;gdk_toplevel_layout_equal (GdkToplevelLayout *layout,
;;;                           GdkToplevelLayout *other);
;;;Check whether layout and other has identical layout properties.

;;;Parameters
;;;layout

;;;a GdkToplevelLayout

;;;
;;;other

;;;another GdkToplevelLayout

;;;
;;;Returns
;;;TRUE if layout and other have identical layout properties, otherwise FALSE.

;;;gdk_toplevel_layout_set_maximized ()
;;;void
;;;gdk_toplevel_layout_set_maximized (GdkToplevelLayout *layout,
;;;                                   gboolean maximized);
;;;Sets whether the layout should cause the surface to be maximized when presented.

;;;Parameters
;;;layout

;;;a GdkToplevelLayout

;;;
;;;maximized

;;;TRUE to maximize

;;;
;;;gdk_toplevel_layout_get_maximized ()
;;;gboolean
;;;gdk_toplevel_layout_get_maximized (GdkToplevelLayout *layout,
;;;                                   gboolean *maximized);
;;;If the layout specifies whether to the toplevel should go maximized, the value pointed to by maximized is set to TRUE if it should go fullscreen, or FALSE, if it should go unmaximized.

;;;Parameters
;;;layout

;;;a GdkToplevelLayout

;;;
;;;maximized

;;;set to TRUE if the toplevel should be maximized.

;;;[out]
;;;Returns
;;;whether the layout specifies the maximized state for the toplevel

;;;gdk_toplevel_layout_set_fullscreen ()
;;;void
;;;gdk_toplevel_layout_set_fullscreen (GdkToplevelLayout *layout,
;;;                                    gboolean fullscreen,
;;;                                    GdkMonitor *monitor);
;;;Sets whether the layout should cause the surface to be fullscreen when presented.

;;;Parameters
;;;layout

;;;a GdkToplevelLayout

;;;
;;;fullscreen

;;;TRUE to fullscreen the surface

;;;
;;;monitor

;;;the monitor to fullscreen on.

;;;[nullable]
;;;gdk_toplevel_layout_get_fullscreen ()
;;;gboolean
;;;gdk_toplevel_layout_get_fullscreen (GdkToplevelLayout *layout,
;;;                                    gboolean *fullscreen);
;;;If the layout specifies whether to the toplevel should go fullscreen, the value pointed to by fullscreen is set to TRUE if it should go fullscreen, or FALSE, if it should go unfullscreen.

;;;Parameters
;;;layout

;;;a GdkToplevelLayout

;;;
;;;fullscreen

;;;location to store whether the toplevel should be fullscreen.

;;;[out]
;;;Returns
;;;whether the layout specifies the fullscreen state for the toplevel

;;;gdk_toplevel_layout_get_fullscreen_monitor ()
;;;GdkMonitor *
;;;gdk_toplevel_layout_get_fullscreen_monitor
;;;                               (GdkToplevelLayout *layout);
;;;Returns the monitor that the layout is fullscreening the surface on.

;;;Parameters
;;;layout

;;;a GdkToplevelLayout

;;;
;;;Returns
;;;the monitor on which layout fullscreens.

;;;[nullable][transfer none]

;;;gdk_toplevel_layout_set_resizable ()
;;;void
;;;gdk_toplevel_layout_set_resizable (GdkToplevelLayout *layout,
;;;                                   gboolean resizable);
;;;Sets whether the layout should allow the user to resize the surface after it has been presented.

;;;Parameters
;;;layout

;;;a GdkToplevelLayout

;;;
;;;resizable

;;;TRUE to allow resizing

;;;
;;;gdk_toplevel_layout_get_resizable ()
;;;gboolean
;;;gdk_toplevel_layout_get_resizable (GdkToplevelLayout *layout);
;;;Returns whether the layout should allow the user to resize the surface.

;;;Parameters
;;;layout

;;;a GdkToplevelLayout

;;;
;;;Returns
;;;TRUE if the layout is resizable

;;;Types and Values
;;;GdkToplevelLayout
;;;typedef struct _GdkToplevelLayout GdkToplevelLayout;
;;;Struct containing information for gdk_toplevel_present()

;;; --- End of file gdk.toplevel-layout.lisp -----------------------------------
