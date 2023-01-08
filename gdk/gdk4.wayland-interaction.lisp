;;; ----------------------------------------------------------------------------
;;; gdk.wayland-interaction.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk/>.
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

;;;Wayland Interaction
;;;Wayland Interaction — Wayland backend-specific functions

;;;Functions
;;;struct wl_display *	gdk_wayland_display_get_wl_display ()
;;;struct wl_compositor *	gdk_wayland_display_get_wl_compositor ()
;;;gboolean	gdk_wayland_display_query_registry ()
;;;void	gdk_wayland_display_set_cursor_theme ()
;;;const char *	gdk_wayland_display_get_startup_notification_id ()
;;;void	gdk_wayland_display_set_startup_notification_id ()
;;;struct wl_seat *	gdk_wayland_seat_get_wl_seat ()
;;;struct wl_seat *	gdk_wayland_device_get_wl_seat ()
;;;struct wl_pointer *	gdk_wayland_device_get_wl_pointer ()
;;;struct wl_keyboard *	gdk_wayland_device_get_wl_keyboard ()
;;;const char *	gdk_wayland_device_get_node_path ()
;;;struct wl_output *	gdk_wayland_monitor_get_wl_output ()
;;;struct wl_surface *	gdk_wayland_surface_get_wl_surface ()
;;;void	(*GdkWaylandToplevelExported) ()
;;;gboolean	gdk_wayland_toplevel_export_handle ()
;;;void	gdk_wayland_toplevel_unexport_handle ()
;;;gboolean	gdk_wayland_toplevel_set_transient_for_exported ()
;;;void	gdk_wayland_toplevel_set_application_id ()
;;;Includes
;;;#include <gdk/wayland/gdkwayland.h>
;;;Description
;;;The functions in this section are specific to the GDK Wayland backend. To use them, you need to include the <gdk/wayland/gdkwayland.h> header and use the Wayland-specific pkg-config gtk4-wayland file to build your application.

;;;To make your code compile with other GDK backends, guard backend-specific calls by an ifdef as follows. Since GDK may be built with multiple backends, you should also check for the backend that is in use (e.g. by using the GDK_IS_WAYLAND_DISPLAY() macro).

;;;#ifdef GDK_WINDOWING_WAYLAND
;;;  if (GDK_IS_WAYLAND_DISPLAY (display))
;;;    {
;;;      // make Wayland-specific calls here
;;;    }
;;;  else
;;;#endif
;;;#ifdef GDK_WINDOWING_X11
;;;  if (GDK_IS_X11_DISPLAY (display))
;;;    {
;;;      // make X11-specific calls here
;;;    }
;;;  else
;;;#endif
;;;  g_error ("Unsupported GDK backend");
;;;Functions
;;;gdk_wayland_display_get_wl_display ()
;;;struct wl_display *
;;;gdk_wayland_display_get_wl_display (GdkDisplay *display);
;;;Returns the Wayland wl_display of a GdkDisplay.

;;;[skip]

;;;Parameters
;;;display

;;;a GdkDisplay.

;;;[type GdkWaylandDisplay]
;;;Returns
;;;a Wayland wl_display.

;;;[transfer none]

;;;gdk_wayland_display_get_wl_compositor ()
;;;struct wl_compositor *
;;;gdk_wayland_display_get_wl_compositor (GdkDisplay *display);
;;;Returns the Wayland global singleton compositor of a GdkDisplay.

;;;[skip]

;;;Parameters
;;;display

;;;a GdkDisplay.

;;;[type GdkWaylandDisplay]
;;;Returns
;;;a Wayland wl_compositor.

;;;[transfer none]

;;;gdk_wayland_display_query_registry ()
;;;gboolean
;;;gdk_wayland_display_query_registry (GdkDisplay *display,
;;;                                    const char *global);
;;;Returns TRUE if the the interface was found in the display wl_registry.global handler.

;;;Parameters
;;;display

;;;a GdkDisplay.

;;;[type GdkWaylandDisplay]
;;;global

;;;global interface to query in the registry

;;;
;;;Returns
;;;TRUE if the global is offered by the compositor

;;;gdk_wayland_display_set_cursor_theme ()
;;;void
;;;gdk_wayland_display_set_cursor_theme (GdkDisplay *display,
;;;                                      const char *name,
;;;                                      int size);
;;;Sets the cursor theme for the given display .

;;;Parameters
;;;display

;;;a GdkDisplay.

;;;[type GdkWaylandDisplay]
;;;name

;;;the new cursor theme

;;;
;;;size

;;;the size to use for cursors

;;;
;;;gdk_wayland_display_get_startup_notification_id ()
;;;const char *
;;;gdk_wayland_display_get_startup_notification_id
;;;                               (GdkDisplay *display);
;;;Gets the startup notification ID for a Wayland display, or NULL if no ID has been defined.

;;;Parameters
;;;display

;;;a GdkDisplay.

;;;[type GdkWaylandDisplay]
;;;Returns
;;;the startup notification ID for display , or NULL.

;;;[nullable]

;;;gdk_wayland_display_set_startup_notification_id ()
;;;void
;;;gdk_wayland_display_set_startup_notification_id
;;;                               (GdkDisplay *display,
;;;                                const char *startup_id);
;;;Sets the startup notification ID for a display.

;;;This is usually taken from the value of the DESKTOP_STARTUP_ID environment variable, but in some cases (such as the application not being launched using exec()) it can come from other sources.

;;;The startup ID is also what is used to signal that the startup is complete (for example, when opening a window or when calling gdk_display_notify_startup_complete()).

;;;Parameters
;;;display

;;;a GdkDisplay.

;;;[type GdkWaylandDisplay]
;;;startup_id

;;;the startup notification ID (must be valid utf8)

;;;
;;;gdk_wayland_seat_get_wl_seat ()
;;;struct wl_seat *
;;;gdk_wayland_seat_get_wl_seat (GdkSeat *seat);
;;;Returns the Wayland wl_seat of a GdkSeat.

;;;[skip]

;;;Parameters
;;;seat

;;;a GdkSeat.

;;;[type GdkWaylandSeat]
;;;Returns
;;;a Wayland wl_seat.

;;;[transfer none]

;;;gdk_wayland_device_get_wl_seat ()
;;;struct wl_seat *
;;;gdk_wayland_device_get_wl_seat (GdkDevice *device);
;;;Returns the Wayland wl_seat of a GdkDevice.

;;;[skip]

;;;Parameters
;;;device

;;;a GdkDevice.

;;;[type GdkWaylandDevice]
;;;Returns
;;;a Wayland wl_seat.

;;;[transfer none]

;;;gdk_wayland_device_get_wl_pointer ()
;;;struct wl_pointer *
;;;gdk_wayland_device_get_wl_pointer (GdkDevice *device);
;;;Returns the Wayland wl_pointer of a GdkDevice.

;;;[skip]

;;;Parameters
;;;device

;;;a GdkDevice.

;;;[type GdkWaylandDevice]
;;;Returns
;;;a Wayland wl_pointer.

;;;[transfer none]

;;;gdk_wayland_device_get_wl_keyboard ()
;;;struct wl_keyboard *
;;;gdk_wayland_device_get_wl_keyboard (GdkDevice *device);
;;;Returns the Wayland wl_keyboard of a GdkDevice.

;;;[skip]

;;;Parameters
;;;device

;;;a GdkDevice.

;;;[type GdkWaylandDevice]
;;;Returns
;;;a Wayland wl_keyboard.

;;;[transfer none]

;;;gdk_wayland_device_get_node_path ()
;;;const char *
;;;gdk_wayland_device_get_node_path (GdkDevice *device);
;;;Returns the /dev/input/event* path of this device.

;;;For GdkDevices that possibly coalesce multiple hardware devices (eg. mouse, keyboard, touch,...), this function will return NULL.

;;;This is most notably implemented for devices of type GDK_SOURCE_PEN, GDK_SOURCE_TABLET_PAD.

;;;Parameters
;;;device

;;;a GdkDevice.

;;;[type GdkWaylandDevice]
;;;Returns
;;;the /dev/input/event* path of this device.

;;;[nullable][transfer none]

;;;gdk_wayland_monitor_get_wl_output ()
;;;struct wl_output *
;;;gdk_wayland_monitor_get_wl_output (GdkMonitor *monitor);
;;;Returns the Wayland wl_output of a GdkMonitor.

;;;[skip]

;;;Parameters
;;;monitor

;;;a GdkMonitor.

;;;[type GdkWaylandMonitor]
;;;Returns
;;;a Wayland wl_output.

;;;[transfer none]

;;;gdk_wayland_surface_get_wl_surface ()
;;;struct wl_surface *
;;;gdk_wayland_surface_get_wl_surface (GdkSurface *surface);
;;;Returns the Wayland surface of a GdkSurface.

;;;[skip]

;;;Parameters
;;;surface

;;;a GdkSurface.

;;;[type GdkWaylandSurface]
;;;Returns
;;;a Wayland wl_surface.

;;;[transfer none]

;;;GdkWaylandToplevelExported ()
;;;void
;;;(*GdkWaylandToplevelExported) (GdkToplevel *toplevel,
;;;                               const char *handle,
;;;                               gpointer user_data);
;;;Callback that gets called when the handle for a surface has been obtained from the Wayland compositor. The handle can be passed to other processes, for the purpose of marking surfaces as transient for out-of-process surfaces.

;;;Parameters
;;;toplevel

;;;the GdkToplevel that is exported.

;;;[type GdkWaylandToplevel]
;;;handle

;;;the handle

;;;
;;;user_data

;;;user data that was passed to gdk_wayland_toplevel_export_handle()

;;;
;;;gdk_wayland_toplevel_export_handle ()
;;;gboolean
;;;gdk_wayland_toplevel_export_handle (GdkToplevel *toplevel,
;;;                                    GdkWaylandToplevelExported callback,
;;;                                    gpointer user_data,
;;;                                    GDestroyNotify destroy_func);
;;;Asynchronously obtains a handle for a surface that can be passed to other processes. When the handle has been obtained, callback will be called.

;;;It is an error to call this function on a surface that is already exported.

;;;When the handle is no longer needed, gdk_wayland_toplevel_unexport_handle() should be called to clean up resources.

;;;The main purpose for obtaining a handle is to mark a surface from another surface as transient for this one, see gdk_wayland_toplevel_set_transient_for_exported().

;;;Note that this API depends on an unstable Wayland protocol, and thus may require changes in the future.

;;;Parameters
;;;toplevel

;;;the GdkToplevel to obtain a handle for.

;;;[type GdkWaylandToplevel]
;;;callback

;;;callback to call with the handle

;;;
;;;user_data

;;;user data for callback .

;;;[closure]
;;;destroy_func

;;;destroy notify for user_data

;;;
;;;Returns
;;;TRUE if the handle has been requested, FALSE if an error occurred.

;;;gdk_wayland_toplevel_unexport_handle ()
;;;void
;;;gdk_wayland_toplevel_unexport_handle (GdkToplevel *toplevel);
;;;Destroys the handle that was obtained with gdk_wayland_toplevel_export_handle().

;;;It is an error to call this function on a surface that does not have a handle.

;;;Note that this API depends on an unstable Wayland protocol, and thus may require changes in the future.

;;;Parameters
;;;toplevel

;;;the GdkToplevel to unexport.

;;;[type GdkWaylandToplevel]
;;;gdk_wayland_toplevel_set_transient_for_exported ()
;;;gboolean
;;;gdk_wayland_toplevel_set_transient_for_exported
;;;                               (GdkToplevel *toplevel,
;;;                                const char *parent_handle_str);
;;;Marks toplevel as transient for the surface to which the given parent_handle_str refers. Typically, the handle will originate from a gdk_wayland_toplevel_export_handle() call in another process.

;;;Note that this API depends on an unstable Wayland protocol, and thus may require changes in the future.

;;;Parameters
;;;toplevel

;;;the GdkToplevel to make as transient.

;;;[type GdkWaylandToplevel]
;;;parent_handle_str

;;;an exported handle for a surface

;;;
;;;Returns
;;;TRUE if the surface has been marked as transient, FALSE if an error occurred.

;;;gdk_wayland_toplevel_set_application_id ()
;;;void
;;;gdk_wayland_toplevel_set_application_id
;;;                               (GdkToplevel *toplevel,
;;;                                const char *application_id);
;;;Sets the application id on a GdkToplevel.

;;;Parameters
;;;toplevel

;;;a GdkToplevel.

;;;[type GdkWaylandToplevel]
;;;application_id

;;;the application id for the toplevel

;;;

;;; --- End of file gdk.wayland-interaction.lisp -------------------------------

