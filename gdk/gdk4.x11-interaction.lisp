;;; ----------------------------------------------------------------------------
;;; gdk.x11-interaction.lisp
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

;;;X Window System Interaction
;;;X Window System Interaction — X backend-specific functions

;;;Functions
;;;#define	GDK_SURFACE_XID()
;;;#define	GDK_DISPLAY_XDISPLAY()
;;;#define	GDK_POINTER_TO_XID()
;;;#define	GDK_XID_TO_POINTER()
;;;GdkDisplay *	gdk_x11_lookup_xdisplay ()
;;;guint32	gdk_x11_get_server_time ()
;;;int	gdk_x11_device_get_id ()
;;;GdkDevice *	gdk_x11_device_manager_lookup ()
;;;GdkDisplay *	gdk_x11_display_open ()
;;;void	gdk_x11_display_set_program_class ()
;;;guint32	gdk_x11_display_get_user_time ()
;;;void	gdk_x11_display_broadcast_startup_message ()
;;;const char *	gdk_x11_display_get_startup_notification_id ()
;;;void	gdk_x11_display_set_startup_notification_id ()
;;;Display *	gdk_x11_display_get_xdisplay ()
;;;Screen *	gdk_x11_display_get_xscreen ()
;;;Window	gdk_x11_display_get_xrootwindow ()
;;;Cursor	gdk_x11_display_get_xcursor ()
;;;void	gdk_x11_display_grab ()
;;;void	gdk_x11_display_ungrab ()
;;;GdkSurface *	gdk_x11_display_get_default_group ()
;;;void	gdk_x11_display_error_trap_push ()
;;;int	gdk_x11_display_error_trap_pop ()
;;;void	gdk_x11_display_error_trap_pop_ignored ()
;;;void	gdk_x11_display_set_cursor_theme ()
;;;void	gdk_x11_display_set_surface_scale ()
;;;gboolean	gdk_x11_display_get_glx_version ()
;;;GdkMonitor *	gdk_x11_display_get_primary_monitor ()
;;;GdkX11Screen *	gdk_x11_display_get_screen ()
;;;XID	gdk_x11_monitor_get_output ()
;;;void	gdk_x11_monitor_get_workarea ()
;;;int	gdk_x11_screen_get_screen_number ()
;;;Screen *	gdk_x11_screen_get_xscreen ()
;;;const char *	gdk_x11_screen_get_window_manager_name ()
;;;XID	gdk_x11_screen_get_monitor_output ()
;;;gboolean	gdk_x11_screen_supports_net_wm_hint ()
;;;guint32	gdk_x11_screen_get_number_of_desktops ()
;;;guint32	gdk_x11_screen_get_current_desktop ()
;;;GdkSurface *	gdk_x11_surface_lookup_for_display ()
;;;Window	gdk_x11_surface_get_xid ()
;;;void	gdk_x11_surface_set_theme_variant ()
;;;void	gdk_x11_surface_set_user_time ()
;;;void	gdk_x11_surface_move_to_current_desktop ()
;;;void	gdk_x11_surface_move_to_desktop ()
;;;guint32	gdk_x11_surface_get_desktop ()
;;;void	gdk_x11_surface_set_utf8_property ()
;;;void	gdk_x11_surface_set_frame_sync_enabled ()
;;;void	gdk_x11_surface_set_group ()
;;;GdkSurface *	gdk_x11_surface_get_group ()
;;;void	gdk_x11_surface_set_skip_pager_hint ()
;;;void	gdk_x11_surface_set_skip_taskbar_hint ()
;;;void	gdk_x11_surface_set_urgency_hint ()
;;;Atom	gdk_x11_get_xatom_by_name_for_display ()
;;;const char *	gdk_x11_get_xatom_name_for_display ()
;;;void	gdk_x11_set_sm_client_id ()
;;;int	gdk_x11_display_text_property_to_text_list ()
;;;void	gdk_x11_free_text_list ()
;;;int	gdk_x11_display_string_to_compound_text ()
;;;gboolean	gdk_x11_display_utf8_to_compound_text ()
;;;void	gdk_x11_free_compound_text ()


(in-package :gdk)


;;;Description
;;;The functions in this section are specific to the GDK X11 backend. To use them, you need to include the <gdk/x11/gdkx.h> header and use the X11-specific pkg-config file gtk4-x11 to build your application.

;;;To make your code compile with other GDK backends, guard backend-specific calls by an ifdef as follows. Since GDK may be built with multiple backends, you should also check for the backend that is in use (e.g. by using the GDK_IS_X11_DISPLAY() macro).

;;;#ifdef GDK_WINDOWING_X11
;;;  if (GDK_IS_X11_DISPLAY (display))
;;;    {
;;;      // make X11-specific calls here
;;;    }
;;;  else
;;;#endif
;;;#ifdef GDK_WINDOWING_MACOS
;;;  if (GDK_IS_MACOS_DISPLAY (display))
;;;    {
;;;      // make MacOS-specific calls here
;;;    }
;;;  else
;;;#endif
;;;  g_error ("Unsupported GDK backend");
;;;Functions
;;;GDK_SURFACE_XID()
;;;#define GDK_SURFACE_XID(win)           (gdk_x11_surface_get_xid (win))
;;;Returns the X window belonging to a GdkSurface.

;;;Parameters
;;;win

;;;a GdkSurface.

;;;
;;;Returns
;;;the Xlib Window of win .

;;;GDK_DISPLAY_XDISPLAY()
;;;#define GDK_DISPLAY_XDISPLAY(display) (gdk_x11_display_get_xdisplay (display))
;;;Returns the display of a GdkDisplay.

;;;Parameters
;;;display

;;;a GdkDisplay

;;;
;;;Returns
;;;an Xlib Display*

;;;GDK_POINTER_TO_XID()
;;;#define GDK_POINTER_TO_XID(pointer) GPOINTER_TO_UINT(pointer)
;;;Converts a gpointer back to an XID that was previously converted using GDK_XID_TO_POINTER().

;;;Parameters
;;;pointer

;;;pointer to extract an XID from

;;;
;;;GDK_XID_TO_POINTER()
;;;#define GDK_XID_TO_POINTER(xid) GUINT_TO_POINTER(xid)
;;;Converts an XID into a gpointer . This is useful with data structures that use pointer arguments such as GHashTable. Use GDK_POINTER_TO_XID() to convert the argument back to an XID.

;;;Parameters
;;;xid

;;;XID to stuff into the pointer

;;;
;;;gdk_x11_lookup_xdisplay ()
;;;GdkDisplay *
;;;gdk_x11_lookup_xdisplay (Display *xdisplay);
;;;Find the GdkDisplay corresponding to xdisplay , if any exists.

;;;Parameters
;;;xdisplay

;;;a pointer to an X Display

;;;
;;;Returns
;;;the GdkDisplay, if found, otherwise NULL.

;;;[transfer none][type GdkX11Display]

;;;gdk_x11_get_server_time ()
;;;guint32
;;;gdk_x11_get_server_time (GdkSurface *surface);
;;;Routine to get the current X server time stamp.

;;;Parameters
;;;surface

;;;a GdkSurface, used for communication with the server. The surface must have GDK_PROPERTY_CHANGE_MASK in its events mask or a hang will result.

;;;[type GdkX11Surface]
;;;Returns
;;;the time stamp.

;;;gdk_x11_device_get_id ()
;;;int
;;;gdk_x11_device_get_id (GdkDevice *device);
;;;Returns the device ID as seen by XInput2.

;;;Parameters
;;;device

;;;a GdkDevice.

;;;[type GdkX11DeviceXI2]
;;;Returns
;;;the XInput2 device ID.

;;;gdk_x11_device_manager_lookup ()
;;;GdkDevice *
;;;gdk_x11_device_manager_lookup (GdkX11DeviceManagerXI2 *device_manager,
;;;                               int device_id);
;;;Returns the GdkDevice that wraps the given device ID.

;;;Parameters
;;;device_manager

;;;a GdkDeviceManager.

;;;[type GdkX11DeviceManagerXI2]
;;;device_id

;;;a device ID, as understood by the XInput2 protocol

;;;
;;;Returns
;;;The GdkDevice wrapping the device ID, or NULL if the given ID doesn’t currently represent a device.

;;;[transfer none][allow-none][type GdkX11DeviceXI2]

;;;gdk_x11_display_open ()
;;;GdkDisplay *
;;;gdk_x11_display_open (const char *display_name);
;;;Tries to open a new display to the X server given by display_name . If opening the display fails, NULL is returned.

;;;Parameters
;;;display_name

;;;name of the X display. See the XOpenDisplay() for details.

;;;[allow-none]
;;;Returns
;;;The new display or NULL on error.

;;;[nullable][transfer full]

;;;gdk_x11_display_set_program_class ()
;;;void
;;;gdk_x11_display_set_program_class (GdkDisplay *display,
;;;                                   const char *program_class);
;;;Sets the program class.

;;;The X11 backend uses the program class to set the class name part of the WM_CLASS property on toplevel windows; see the ICCCM.

;;;Parameters
;;;display

;;;a GdkDisplay

;;;
;;;program_class

;;;a string

;;;
;;;gdk_x11_display_get_user_time ()
;;;guint32
;;;gdk_x11_display_get_user_time (GdkDisplay *display);
;;;Returns the timestamp of the last user interaction on display . The timestamp is taken from events caused by user interaction such as key presses or pointer movements. See gdk_x11_surface_set_user_time().

;;;Parameters
;;;display

;;;a GdkDisplay.

;;;[type GdkX11Display]
;;;Returns
;;;the timestamp of the last user interaction

;;;gdk_x11_display_broadcast_startup_message ()
;;;void
;;;gdk_x11_display_broadcast_startup_message
;;;                               (GdkDisplay *display,
;;;                                const char *message_type,
;;;                                ...);
;;;Sends a startup notification message of type message_type to display .

;;;This is a convenience function for use by code that implements the freedesktop startup notification specification. Applications should not normally need to call it directly. See the Startup Notification Protocol specification for definitions of the message types and keys that can be used.

;;;Parameters
;;;display

;;;a GdkDisplay.

;;;[type GdkX11Display]
;;;message_type

;;;startup notification message type ("new", "change", or "remove")

;;;
;;;...

;;;a list of key/value pairs (as strings), terminated by a NULL key. (A NULL value for a key will cause that key to be skipped in the output.)

;;;
;;;gdk_x11_display_get_startup_notification_id ()
;;;const char *
;;;gdk_x11_display_get_startup_notification_id
;;;                               (GdkDisplay *display);
;;;Gets the startup notification ID for a display.

;;;Parameters
;;;display

;;;a GdkDisplay.

;;;[type GdkX11Display]
;;;Returns
;;;the startup notification ID for display

;;;gdk_x11_display_set_startup_notification_id ()
;;;void
;;;gdk_x11_display_set_startup_notification_id
;;;                               (GdkDisplay *display,
;;;                                const char *startup_id);
;;;Sets the startup notification ID for a display.

;;;This is usually taken from the value of the DESKTOP_STARTUP_ID environment variable, but in some cases (such as the application not being launched using exec()) it can come from other sources.

;;;If the ID contains the string "_TIME" then the portion following that string is taken to be the X11 timestamp of the event that triggered the application to be launched and the GDK current event time is set accordingly.

;;;The startup ID is also what is used to signal that the startup is complete (for example, when opening a window or when calling gdk_display_notify_startup_complete()).

;;;Parameters
;;;display

;;;a GdkDisplay.

;;;[type GdkX11Display]
;;;startup_id

;;;the startup notification ID (must be valid utf8)

;;;
;;;gdk_x11_display_get_xdisplay ()
;;;Display *
;;;gdk_x11_display_get_xdisplay (GdkDisplay *display);
;;;Returns the X display of a GdkDisplay.

;;;Parameters
;;;display

;;;a GdkDisplay.

;;;[type GdkX11Display]
;;;Returns
;;;an X display.

;;;[transfer none]

;;;gdk_x11_display_get_xscreen ()
;;;Screen *
;;;gdk_x11_display_get_xscreen (GdkDisplay *display);
;;;Returns the X Screen used by GdkDisplay.

;;;Parameters
;;;display

;;;a GdkDisplay.

;;;[type GdkX11Display]
;;;Returns
;;;an X Screen.

;;;[transfer none]

;;;gdk_x11_display_get_xrootwindow ()
;;;Window
;;;gdk_x11_display_get_xrootwindow (GdkDisplay *display);
;;;Returns the root X window used by GdkDisplay.

;;;Parameters
;;;display

;;;a GdkDisplay.

;;;[type GdkX11Display]
;;;Returns
;;;an X Window

;;;gdk_x11_display_get_xcursor ()
;;;Cursor
;;;gdk_x11_display_get_xcursor (GdkDisplay *display,
;;;                             GdkCursor *cursor);
;;;Returns the X cursor belonging to a GdkCursor, potentially creating the cursor.

;;;Be aware that the returned cursor may not be unique to cursor . It may for example be shared with its fallback cursor. On old X servers that don't support the XCursor extension, all cursors may even fall back to a few default cursors.

;;;Parameters
;;;display

;;;a GdkDisplay.

;;;[type GdkX11Display]
;;;cursor

;;;a GdkCursor.

;;;
;;;Returns
;;;an Xlib Cursor.

;;;gdk_x11_display_grab ()
;;;void
;;;gdk_x11_display_grab (GdkDisplay *display);
;;;Call XGrabServer() on display . To ungrab the display again, use gdk_x11_display_ungrab().

;;;gdk_x11_display_grab()/gdk_x11_display_ungrab() calls can be nested.

;;;Parameters
;;;display

;;;a GdkDisplay.

;;;[type GdkX11Display]
;;;gdk_x11_display_ungrab ()
;;;void
;;;gdk_x11_display_ungrab (GdkDisplay *display);
;;;Ungrab display after it has been grabbed with gdk_x11_display_grab().

;;;Parameters
;;;display

;;;a GdkDisplay.

;;;[type GdkX11Display]
;;;gdk_x11_display_get_default_group ()
;;;GdkSurface *
;;;gdk_x11_display_get_default_group (GdkDisplay *display);
;;;Returns the default group leader surface for all toplevel surfaces on display . This surface is implicitly created by GDK. See gdk_x11_surface_set_group().

;;;Parameters
;;;display

;;;a GdkDisplay.

;;;[type GdkX11Display]
;;;Returns
;;;The default group leader surface for display .

;;;[transfer none]

;;;gdk_x11_display_error_trap_push ()
;;;void
;;;gdk_x11_display_error_trap_push (GdkDisplay *display);
;;;Begins a range of X requests on display for which X error events will be ignored. Unignored errors (when no trap is pushed) will abort the application. Use gdk_x11_display_error_trap_pop() or gdk_x11_display_error_trap_pop_ignored()to lift a trap pushed with this function.

;;;Parameters
;;;display

;;;a GdkDisplay.

;;;[type GdkX11Display]
;;;gdk_x11_display_error_trap_pop ()
;;;int
;;;gdk_x11_display_error_trap_pop (GdkDisplay *display);
;;;Pops the error trap pushed by gdk_x11_display_error_trap_push(). Will XSync() if necessary and will always block until the error is known to have occurred or not occurred, so the error code can be returned.

;;;If you don’t need to use the return value, gdk_x11_display_error_trap_pop_ignored() would be more efficient.

;;;Parameters
;;;display

;;;the display.

;;;[type GdkX11Display]
;;;Returns
;;;X error code or 0 on success

;;;gdk_x11_display_error_trap_pop_ignored ()
;;;void
;;;gdk_x11_display_error_trap_pop_ignored
;;;                               (GdkDisplay *display);
;;;Pops the error trap pushed by gdk_x11_display_error_trap_push(). Does not block to see if an error occurred; merely records the range of requests to ignore errors for, and ignores those errors if they arrive asynchronously.

;;;Parameters
;;;display

;;;the display.

;;;[type GdkX11Display]
;;;gdk_x11_display_set_cursor_theme ()
;;;void
;;;gdk_x11_display_set_cursor_theme (GdkDisplay *display,
;;;                                  const char *theme,
;;;                                  const int size);
;;;Sets the cursor theme from which the images for cursor should be taken.

;;;If the windowing system supports it, existing cursors created with gdk_cursor_new_from_name() are updated to reflect the theme change. Custom cursors constructed with gdk_cursor_new_from_texture() will have to be handled by the application (GTK applications can learn about cursor theme changes by listening for change notification for the corresponding GtkSetting).

;;;Parameters
;;;display

;;;a GdkDisplay.

;;;[type GdkX11Display]
;;;theme

;;;the name of the cursor theme to use, or NULL to unset a previously set value.

;;;[nullable]
;;;size

;;;the cursor size to use, or 0 to keep the previous size

;;;
;;;gdk_x11_display_set_surface_scale ()
;;;void
;;;gdk_x11_display_set_surface_scale (GdkDisplay *display,
;;;                                   int scale);
;;;Forces a specific window scale for all windows on this display, instead of using the default or user configured scale. This is can be used to disable scaling support by setting scale to 1, or to programmatically set the window scale.

;;;Once the scale is set by this call it will not change in response to later user configuration changes.

;;;Parameters
;;;display

;;;the display.

;;;[type GdkX11Display]
;;;scale

;;;The new scale value

;;;
;;;gdk_x11_display_get_glx_version ()
;;;gboolean
;;;gdk_x11_display_get_glx_version (GdkDisplay *display,
;;;                                 int *major,
;;;                                 int *minor);
;;;Retrieves the version of the GLX implementation.

;;;Parameters
;;;display

;;;a GdkDisplay.

;;;[type GdkX11Display]
;;;major

;;;return location for the GLX major version.

;;;[out]
;;;minor

;;;return location for the GLX minor version.

;;;[out]
;;;Returns
;;;TRUE if GLX is available

;;;gdk_x11_display_get_primary_monitor ()
;;;GdkMonitor *
;;;gdk_x11_display_get_primary_monitor (GdkDisplay *display);
;;;Gets the primary monitor for the display.

;;;The primary monitor is considered the monitor where the “main desktop” lives. While normal application surfaces typically allow the window manager to place the surfaces, specialized desktop applications such as panels should place themselves on the primary monitor.

;;;If no monitor is the designated primary monitor, any monitor (usually the first) may be returned.

;;;Parameters
;;;display

;;;a GdkDisplay.

;;;[type GdkX11Display]
;;;Returns
;;;the primary monitor, or any monitor if no primary monitor is configured by the user.

;;;[transfer none]

;;;gdk_x11_display_get_screen ()
;;;GdkX11Screen *
;;;gdk_x11_display_get_screen (GdkDisplay *display);
;;;Retrieves the GdkX11Screen of the display .

;;;Parameters
;;;display

;;;a GdkX11Display.

;;;[type GdkX11Display]
;;;Returns
;;;the GdkX11Screen.

;;;[transfer none]

;;;gdk_x11_monitor_get_output ()
;;;XID
;;;gdk_x11_monitor_get_output (GdkMonitor *monitor);
;;;Returns the XID of the Output corresponding to monitor .

;;;Parameters
;;;monitor

;;;a GdkMonitor.

;;;[type GdkX11Monitor]
;;;Returns
;;;the XID of monitor

;;;gdk_x11_monitor_get_workarea ()
;;;void
;;;gdk_x11_monitor_get_workarea (GdkMonitor *monitor,
;;;                              GdkRectangle *workarea);
;;;Retrieves the size and position of the “work area” on a monitor within the display coordinate space. The returned geometry is in ”application pixels”, not in ”device pixels” (see gdk_monitor_get_scale_factor()).

;;;Parameters
;;;monitor

;;;a GdkMonitor.

;;;[type GdkX11Monitor]
;;;workarea

;;;a GdkRectangle to be filled with the monitor workarea.

;;;[out]
;;;gdk_x11_screen_get_screen_number ()
;;;int
;;;gdk_x11_screen_get_screen_number (GdkX11Screen *screen);
;;;Returns the index of a GdkX11Screen.

;;;Parameters
;;;screen

;;;a GdkX11Screen

;;;
;;;Returns
;;;the position of screen among the screens of its display

;;;gdk_x11_screen_get_xscreen ()
;;;Screen *
;;;gdk_x11_screen_get_xscreen (GdkX11Screen *screen);
;;;Returns the screen of a GdkX11Screen.

;;;Parameters
;;;screen

;;;a GdkX11Screen

;;;
;;;Returns
;;;an Xlib Screen*.

;;;[transfer none]

;;;gdk_x11_screen_get_window_manager_name ()
;;;const char *
;;;gdk_x11_screen_get_window_manager_name
;;;                               (GdkX11Screen *screen);
;;;Returns the name of the window manager for screen .

;;;Parameters
;;;screen

;;;a GdkX11Screen

;;;
;;;Returns
;;;the name of the window manager screen screen , or "unknown" if the window manager is unknown. The string is owned by GDK and should not be freed.

;;;gdk_x11_screen_get_monitor_output ()
;;;XID
;;;gdk_x11_screen_get_monitor_output (GdkX11Screen *screen,
;;;                                   int monitor_num);
;;;Gets the XID of the specified output/monitor. If the X server does not support version 1.2 of the RANDR extension, 0 is returned.

;;;Parameters
;;;screen

;;;a GdkX11Screen

;;;
;;;monitor_num

;;;number of the monitor, between 0 and gdk_screen_get_n_monitors (screen)

;;;
;;;Returns
;;;the XID of the monitor

;;;gdk_x11_screen_supports_net_wm_hint ()
;;;gboolean
;;;gdk_x11_screen_supports_net_wm_hint (GdkX11Screen *screen,
;;;                                     const char *property_name);
;;;This function is specific to the X11 backend of GDK, and indicates whether the window manager supports a certain hint from the Extended Window Manager Hints specification.

;;;When using this function, keep in mind that the window manager can change over time; so you shouldn’t use this function in a way that impacts persistent application state. A common bug is that your application can start up before the window manager does when the user logs in, and before the window manager starts gdk_x11_screen_supports_net_wm_hint() will return FALSE for every property. You can monitor the window_manager_changed signal on GdkX11Screen to detect a window manager change.

;;;Parameters
;;;screen

;;;the relevant GdkX11Screen.

;;;
;;;property_name

;;;name of the WM property

;;;
;;;Returns
;;;TRUE if the window manager supports property

;;;gdk_x11_screen_get_number_of_desktops ()
;;;guint32
;;;gdk_x11_screen_get_number_of_desktops (GdkX11Screen *screen);
;;;Returns the number of workspaces for screen when running under a window manager that supports multiple workspaces, as described in the Extended Window Manager Hints specification.

;;;Parameters
;;;screen

;;;a GdkX11Screen

;;;
;;;Returns
;;;the number of workspaces, or 0 if workspaces are not supported

;;;gdk_x11_screen_get_current_desktop ()
;;;guint32
;;;gdk_x11_screen_get_current_desktop (GdkX11Screen *screen);
;;;Returns the current workspace for screen when running under a window manager that supports multiple workspaces, as described in the Extended Window Manager Hints specification.

;;;Parameters
;;;screen

;;;a GdkX11Screen

;;;
;;;Returns
;;;the current workspace, or 0 if workspaces are not supported

;;;gdk_x11_surface_lookup_for_display ()
;;;GdkSurface *
;;;gdk_x11_surface_lookup_for_display (GdkDisplay *display,
;;;                                    Window window);
;;;Looks up the GdkSurface that wraps the given native window handle.

;;;Parameters
;;;display

;;;the GdkDisplay corresponding to the window handle.

;;;[type GdkX11Display]
;;;window

;;;an Xlib Window

;;;
;;;Returns
;;;the GdkSurface wrapper for the native window, or NULL if there is none.

;;;[transfer none][type GdkX11Surface]

;;;gdk_x11_surface_get_xid ()
;;;Window
;;;gdk_x11_surface_get_xid (GdkSurface *surface);
;;;Returns the X resource (surface) belonging to a GdkSurface.

;;;Parameters
;;;surface

;;;a native GdkSurface.

;;;[type GdkX11Surface]
;;;Returns
;;;the ID of drawable ’s X resource.

;;;gdk_x11_surface_set_theme_variant ()
;;;void
;;;gdk_x11_surface_set_theme_variant (GdkSurface *surface,
;;;                                   const char *variant);
;;;GTK applications can request a dark theme variant. In order to make other applications - namely window managers using GTK for themeing - aware of this choice, GTK uses this function to export the requested theme variant as _GTK_THEME_VARIANT property on toplevel surfaces.

;;;Note that this property is automatically updated by GTK, so this function should only be used by applications which do not use GTK to create toplevel surfaces.

;;;Parameters
;;;surface

;;;a GdkSurface.

;;;[type GdkX11Surface]
;;;variant

;;;the theme variant to export

;;;
;;;gdk_x11_surface_set_user_time ()
;;;void
;;;gdk_x11_surface_set_user_time (GdkSurface *surface,
;;;                               guint32 timestamp);
;;;The application can use this call to update the _NET_WM_USER_TIME property on a toplevel surface. This property stores an Xserver time which represents the time of the last user input event received for this surface. This property may be used by the window manager to alter the focus, stacking, and/or placement behavior of surfaces when they are mapped depending on whether the new surface was created by a user action or is a "pop-up" surface activated by a timer or some other event.

;;;Note that this property is automatically updated by GDK, so this function should only be used by applications which handle input events bypassing GDK.

;;;Parameters
;;;surface

;;;A toplevel GdkSurface.

;;;[type GdkX11Surface]
;;;timestamp

;;;An XServer timestamp to which the property should be set

;;;
;;;gdk_x11_surface_move_to_current_desktop ()
;;;void
;;;gdk_x11_surface_move_to_current_desktop
;;;                               (GdkSurface *surface);
;;;Moves the surface to the correct workspace when running under a window manager that supports multiple workspaces, as described in the Extended Window Manager Hints specification. Will not do anything if the surface is already on all workspaces.

;;;Parameters
;;;surface

;;;a GdkSurface.

;;;[type GdkX11Surface]
;;;gdk_x11_surface_move_to_desktop ()
;;;void
;;;gdk_x11_surface_move_to_desktop (GdkSurface *surface,
;;;                                 guint32 desktop);
;;;Moves the surface to the given workspace when running unde a window manager that supports multiple workspaces, as described in the Extended Window Manager Hints specification.

;;;Parameters
;;;surface

;;;a GdkSurface.

;;;[type GdkX11Surface]
;;;desktop

;;;the number of the workspace to move the surface to

;;;
;;;gdk_x11_surface_get_desktop ()
;;;guint32
;;;gdk_x11_surface_get_desktop (GdkSurface *surface);
;;;Gets the number of the workspace surface is on.

;;;Parameters
;;;surface

;;;a GdkSurface.

;;;[type GdkX11Surface]
;;;Returns
;;;the current workspace of surface

;;;gdk_x11_surface_set_utf8_property ()
;;;void
;;;gdk_x11_surface_set_utf8_property (GdkSurface *surface,
;;;                                   const char *name,
;;;                                   const char *value);
;;;This function modifies or removes an arbitrary X11 window property of type UTF8_STRING. If the given surface is not a toplevel surface, it is ignored.

;;;Parameters
;;;surface

;;;a GdkSurface.

;;;[type GdkX11Surface]
;;;name

;;;Property name, will be interned as an X atom

;;;
;;;value

;;;Property value, or NULL to delete.

;;;[allow-none]
;;;gdk_x11_surface_set_frame_sync_enabled ()
;;;void
;;;gdk_x11_surface_set_frame_sync_enabled
;;;                               (GdkSurface *surface,
;;;                                gboolean frame_sync_enabled);
;;;This function can be used to disable frame synchronization for a surface. Normally frame synchronziation will be enabled or disabled based on whether the system has a compositor that supports frame synchronization, but if the surface is not directly managed by the window manager, then frame synchronziation may need to be disabled. This is the case for a surface embedded via the XEMBED protocol.

;;;Parameters
;;;surface

;;;a native GdkSurface.

;;;[type GdkX11Surface]
;;;frame_sync_enabled

;;;whether frame-synchronization should be enabled

;;;
;;;gdk_x11_surface_set_group ()
;;;void
;;;gdk_x11_surface_set_group (GdkSurface *surface,
;;;                           GdkSurface *leader);
;;;Sets the group leader of surface to be leader . See the ICCCM for details.

;;;Parameters
;;;surface

;;;a native GdkSurface.

;;;[type GdkX11Surface]
;;;leader

;;;a GdkSurface

;;;
;;;gdk_x11_surface_get_group ()
;;;GdkSurface *
;;;gdk_x11_surface_get_group (GdkSurface *surface);
;;;Returns the group this surface belongs to.

;;;Parameters
;;;surface

;;;The GdkSurface.

;;;[type GdkX11Surface]
;;;Returns
;;;The group of this surface;.

;;;[transfer none]

;;;gdk_x11_surface_set_skip_pager_hint ()
;;;void
;;;gdk_x11_surface_set_skip_pager_hint (GdkSurface *surface,
;;;                                     gboolean skips_pager);
;;;Sets a hint on surface that pagers should not display it. See the EWMH for details.

;;;Parameters
;;;surface

;;;a GdkSurface.

;;;[type GdkX11Surface]
;;;skips_pager

;;;TRUE to skip pagers

;;;
;;;gdk_x11_surface_set_skip_taskbar_hint ()
;;;void
;;;gdk_x11_surface_set_skip_taskbar_hint (GdkSurface *surface,
;;;                                       gboolean skips_taskbar);
;;;Sets a hint on surface that taskbars should not display it. See the EWMH for details.

;;;Parameters
;;;surface

;;;a native GdkSurface.

;;;[type GdkX11Surface]
;;;skips_taskbar

;;;TRUE to skip taskbars

;;;
;;;gdk_x11_surface_set_urgency_hint ()
;;;void
;;;gdk_x11_surface_set_urgency_hint (GdkSurface *surface,
;;;                                  gboolean urgent);
;;;Sets a hint on surface that it needs user attention. See the ICCCM for details.

;;;Parameters
;;;surface

;;;a native GdkSurface.

;;;[type GdkX11Surface]
;;;urgent

;;;TRUE to indicate urgenct attention needed

;;;
;;;gdk_x11_get_xatom_by_name_for_display ()
;;;Atom
;;;gdk_x11_get_xatom_by_name_for_display (GdkDisplay *display,
;;;                                       const char *atom_name);
;;;Returns the X atom for a GdkDisplay corresponding to atom_name . This function caches the result, so if called repeatedly it is much faster than XInternAtom(), which is a round trip to the server each time.

;;;Parameters
;;;display

;;;a GdkDisplay.

;;;[type GdkX11Display]
;;;atom_name

;;;a string

;;;
;;;Returns
;;;a X atom for a GdkDisplay

;;;gdk_x11_get_xatom_name_for_display ()
;;;const char *
;;;gdk_x11_get_xatom_name_for_display (GdkDisplay *display,
;;;                                    Atom xatom);
;;;Returns the name of an X atom for its display. This function is meant mainly for debugging, so for convenience, unlike XAtomName() and the result doesn’t need to be freed.

;;;Parameters
;;;display

;;;the GdkDisplay where xatom is defined.

;;;[type GdkX11Display]
;;;xatom

;;;an X atom

;;;
;;;Returns
;;;name of the X atom; this string is owned by GDK, so it shouldn’t be modified or freed.

;;;gdk_x11_set_sm_client_id ()
;;;void
;;;gdk_x11_set_sm_client_id (const char *sm_client_id);
;;;Sets the SM_CLIENT_ID property on the application’s leader window so that the window manager can save the application’s state using the X11R6 ICCCM session management protocol.

;;;See the X Session Management Library documentation for more information on session management and the Inter-Client Communication Conventions Manual

;;;Parameters
;;;sm_client_id

;;;the client id assigned by the session manager when the connection was opened, or NULL to remove the property.

;;;[nullable]
;;;gdk_x11_display_text_property_to_text_list ()
;;;int
;;;gdk_x11_display_text_property_to_text_list
;;;                               (GdkDisplay *display,
;;;                                const char *encoding,
;;;                                int format,
;;;                                const guchar *text,
;;;                                int length,
;;;                                char ***list);
;;;Convert a text string from the encoding as it is stored in a property into an array of strings in the encoding of the current locale. (The elements of the array represent the nul-separated elements of the original text string.)

;;;Parameters
;;;display

;;;The GdkDisplay where the encoding is defined.

;;;[type GdkX11Display]
;;;encoding

;;;a string representing the encoding. The most common values for this are "STRING", or "COMPOUND_TEXT". This is value used as the type for the property

;;;
;;;format

;;;the format of the property

;;;
;;;text

;;;The text data

;;;
;;;length

;;;The number of items to transform

;;;
;;;list

;;;location to store an array of strings in the encoding of the current locale. This array should be freed using gdk_x11_free_text_list().

;;;
;;;Returns
;;;the number of strings stored in list, or 0, if the conversion failed

;;;gdk_x11_free_text_list ()
;;;void
;;;gdk_x11_free_text_list (char **list);
;;;Frees the array of strings created by gdk_x11_display_text_property_to_text_list().

;;;Parameters
;;;list

;;;the value stored in the list parameter by a call to gdk_x11_display_text_property_to_text_list().

;;;
;;;gdk_x11_display_string_to_compound_text ()
;;;int
;;;gdk_x11_display_string_to_compound_text
;;;                               (GdkDisplay *display,
;;;                                const char *str,
;;;                                const char **encoding,
;;;                                int *format,
;;;                                guchar **ctext,
;;;                                int *length);
;;;Convert a string from the encoding of the current locale into a form suitable for storing in a window property.

;;;Parameters
;;;display

;;;the GdkDisplay where the encoding is defined.

;;;[type GdkX11Display]
;;;str

;;;a nul-terminated string

;;;
;;;encoding

;;;location to store the encoding (to be used as the type for the property).

;;;[out][transfer none]
;;;format

;;;location to store the format of the property.

;;;[out]
;;;ctext

;;;location to store newly allocated data for the property.

;;;[out][array length=length]
;;;length

;;;the length of ctext , in bytes

;;;
;;;Returns
;;;0 upon success, non-zero upon failure

;;;gdk_x11_display_utf8_to_compound_text ()
;;;gboolean
;;;gdk_x11_display_utf8_to_compound_text (GdkDisplay *display,
;;;                                       const char *str,
;;;                                       const char **encoding,
;;;                                       int *format,
;;;                                       guchar **ctext,
;;;                                       int *length);
;;;Converts from UTF-8 to compound text.

;;;Parameters
;;;display

;;;a GdkDisplay.

;;;[type GdkX11Display]
;;;str

;;;a UTF-8 string

;;;
;;;encoding

;;;location to store resulting encoding.

;;;[out][transfer none]
;;;format

;;;location to store format of the result.

;;;[out]
;;;ctext

;;;location to store the data of the result.

;;;[out][array length=length]
;;;length

;;;location to store the length of the data stored in ctext

;;;
;;;Returns
;;;TRUE if the conversion succeeded, otherwise FALSE

;;;gdk_x11_free_compound_text ()
;;;void
;;;gdk_x11_free_compound_text (guchar *ctext);
;;;Frees the data returned from gdk_x11_display_string_to_compound_text().

;;;Parameters
;;;ctext

;;;The pointer stored in ctext from a call to gdk_x11_display_string_to_compound_text().


;;; --- End of file gdk.x11-interaction.lisp -----------------------------------
