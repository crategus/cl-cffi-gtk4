;;; ----------------------------------------------------------------------------
;;; gdk.toplevel.lisp
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
;;; Types and Values
;;;
;;;     GtkToplevel
;;;     GdkToplevelState
;;;     GdkFullScreenMode
;;;     GdkSurfaceEdge
;;;
;;; Functions
;;;
;;;     gdk_toplevel_present
;;;     gdk_toplevel_minimize
;;;     gdk_toplevel_lower
;;;     gdk_toplevel_focus
;;;     gdk_toplevel_get_state
;;;     gdk_toplevel_set_title
;;;     gdk_toplevel_set_startup_id
;;;     gdk_toplevel_set_transient_for
;;;     gdk_toplevel_set_modal
;;;     gdk_toplevel_set_icon_list
;;;     gdk_toplevel_show_window_menu
;;;     gdk_toplevel_set_decorated
;;;     gdk_toplevel_set_deletable
;;;     gdk_toplevel_supports_edge_constraints
;;;     gdk_toplevel_inhibit_system_shortcuts
;;;     gdk_toplevel_restore_system_shortcuts
;;;     gdk_toplevel_begin_resize
;;;     gdk_toplevel_begin_move
;;;
;;; Properties
;;;
;;;     decorated
;;;     deletable
;;;     fullscreen-mode
;;;     icon-list
;;;     modal
;;;     shortcuts-inhibited
;;;     startup-id
;;;     state
;;;     title
;;;     transient-for
;;;
;;; Signals
;;;
;;;     compute-size
;;;
;;; Object Hierarchy
;;;
;;;     GInterface
;;;     ╰── GdkToplevel
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;;Description
;;;A GdkToplevel is a freestanding toplevel surface.

;;;The GdkToplevel interface provides useful APIs for interacting with the windowing system, such as controlling maximization and size of the surface, setting icons and transient parents for dialogs.

;;;Functions
;;;gdk_toplevel_present ()
;;;void
;;;gdk_toplevel_present (GdkToplevel *toplevel,
;;;                      GdkToplevelLayout *layout);
;;;Present toplevel after having processed the GdkToplevelLayout rules. If the toplevel was previously not showing, it will be showed, otherwise it will change layout according to layout .

;;;GDK may emit the 'compute-size' signal to let the user of this toplevel compute the preferred size of the toplevel surface. See “compute-size” for details.

;;;Presenting is asynchronous and the specified layout parameters are not guaranteed to be respected.

;;;Parameters
;;;toplevel

;;;the GdkToplevel to show

;;;
;;;layout

;;;the GdkToplevelLayout object used to layout

;;;
;;;gdk_toplevel_minimize ()
;;;gboolean
;;;gdk_toplevel_minimize (GdkToplevel *toplevel);
;;;Asks to minimize the toplevel .

;;;The windowing system may choose to ignore the request.

;;;Parameters
;;;toplevel

;;;a GdkToplevel

;;;
;;;Returns
;;;TRUE if the surface was minimized

;;;gdk_toplevel_lower ()
;;;gboolean
;;;gdk_toplevel_lower (GdkToplevel *toplevel);
;;;Asks to lower the toplevel below other windows.

;;;The windowing system may choose to ignore the request.

;;;Parameters
;;;toplevel

;;;a GdkToplevel

;;;
;;;Returns
;;;TRUE if the surface was lowered

;;;gdk_toplevel_focus ()
;;;void
;;;gdk_toplevel_focus (GdkToplevel *toplevel,
;;;                    guint32 timestamp);
;;;Sets keyboard focus to surface .

;;;In most cases, gtk_window_present_with_time() should be used on a GtkWindow, rather than calling this function.

;;;Parameters
;;;toplevel

;;;a GdkToplevel

;;;
;;;timestamp

;;;timestamp of the event triggering the surface focus

;;;
;;;gdk_toplevel_get_state ()
;;;GdkToplevelState
;;;gdk_toplevel_get_state (GdkToplevel *toplevel);
;;;Gets the bitwise OR of the currently active surface state flags, from the GdkToplevelState enumeration.

;;;Parameters
;;;toplevel

;;;a GdkToplevel

;;;
;;;Returns
;;;surface state bitfield

;;;gdk_toplevel_set_title ()
;;;void
;;;gdk_toplevel_set_title (GdkToplevel *toplevel,
;;;                        const char *title);
;;;Sets the title of a toplevel surface, to be displayed in the titlebar, in lists of windows, etc.

;;;Parameters
;;;toplevel

;;;a GdkToplevel

;;;
;;;title

;;;title of surface

;;;
;;;gdk_toplevel_set_startup_id ()
;;;void
;;;gdk_toplevel_set_startup_id (GdkToplevel *toplevel,
;;;                             const char *startup_id);
;;;When using GTK, typically you should use gtk_window_set_startup_id() instead of this low-level function.

;;;Parameters
;;;toplevel

;;;a GdkToplevel

;;;
;;;startup_id

;;;a string with startup-notification identifier

;;;
;;;gdk_toplevel_set_transient_for ()
;;;void
;;;gdk_toplevel_set_transient_for (GdkToplevel *toplevel,
;;;                                GdkSurface *parent);
;;;Indicates to the window manager that surface is a transient dialog associated with the application surface parent . This allows the window manager to do things like center surface on parent and keep surface above parent .

;;;See gtk_window_set_transient_for() if you’re using GtkWindow or GtkDialog.

;;;Parameters
;;;toplevel

;;;a GdkToplevel

;;;
;;;parent

;;;another toplevel GdkSurface

;;;
;;;gdk_toplevel_set_modal ()
;;;void
;;;gdk_toplevel_set_modal (GdkToplevel *toplevel,
;;;                        gboolean modal);
;;;The application can use this hint to tell the window manager that a certain surface has modal behaviour. The window manager can use this information to handle modal surfaces in a special way.

;;;You should only use this on surfaces for which you have previously called gdk_toplevel_set_transient_for().

;;;Parameters
;;;toplevel

;;;A toplevel surface

;;;
;;;modal

;;;TRUE if the surface is modal, FALSE otherwise.

;;;
;;;gdk_toplevel_set_icon_list ()
;;;void
;;;gdk_toplevel_set_icon_list (GdkToplevel *toplevel,
;;;                            GList *surfaces);
;;;Sets a list of icons for the surface.

;;;One of these will be used to represent the surface in iconic form. The icon may be shown in window lists or task bars. Which icon size is shown depends on the window manager. The window manager can scale the icon but setting several size icons can give better image quality.

;;;Note that some platforms don't support surface icons.

;;;Parameters
;;;toplevel

;;;a GdkToplevel

;;;
;;;surfaces

;;;A list of textures to use as icon, of different sizes.

;;;[transfer none][element-type GdkTexture]
;;;gdk_toplevel_show_window_menu ()
;;;gboolean
;;;gdk_toplevel_show_window_menu (GdkToplevel *toplevel,
;;;                               GdkEvent *event);
;;;Asks the windowing system to show the window menu.

;;;The window menu is the menu shown when right-clicking the titlebar on traditional windows managed by the window manager. This is useful for windows using client-side decorations, activating it with a right-click on the window decorations.

;;;Parameters
;;;toplevel

;;;a GdkToplevel

;;;
;;;event

;;;a GdkEvent to show the menu for

;;;
;;;Returns
;;;TRUE if the window menu was shown and FALSE otherwise.

;;;gdk_toplevel_set_decorated ()
;;;void
;;;gdk_toplevel_set_decorated (GdkToplevel *toplevel,
;;;                            gboolean decorated);
;;;Setting decorated to FALSE hints the desktop environment that the surface has its own, client-side decorations and does not need to have window decorations added.

;;;Parameters
;;;toplevel

;;;a GdkToplevel

;;;
;;;decorated

;;;TRUE to request decorations

;;;
;;;gdk_toplevel_set_deletable ()
;;;void
;;;gdk_toplevel_set_deletable (GdkToplevel *toplevel,
;;;                            gboolean deletable);
;;;Setting deletable to TRUE hints the desktop environment that it should offer the user a way to close the surface.

;;;Parameters
;;;toplevel

;;;a GdkToplevel

;;;
;;;deletable

;;;TRUE to request a delete button

;;;
;;;gdk_toplevel_supports_edge_constraints ()
;;;gboolean
;;;gdk_toplevel_supports_edge_constraints
;;;                               (GdkToplevel *toplevel);
;;;Returns whether the desktop environment supports tiled window states.

;;;Parameters
;;;toplevel

;;;a GdkToplevel

;;;
;;;Returns
;;;TRUE if the desktop environment supports tiled window states

;;;gdk_toplevel_inhibit_system_shortcuts ()
;;;void
;;;gdk_toplevel_inhibit_system_shortcuts (GdkToplevel *toplevel,
;;;                                       GdkEvent *event);
;;;Requests that the toplevel inhibit the system shortcuts, asking the desktop environment/windowing system to let all keyboard events reach the surface, as long as it is focused, instead of triggering system actions.

;;;If granted, the rerouting remains active until the default shortcuts processing is restored with gdk_toplevel_restore_system_shortcuts(), or the request is revoked by the desktop environment, windowing system or the user.

;;;A typical use case for this API is remote desktop or virtual machine viewers which need to inhibit the default system keyboard shortcuts so that the remote session or virtual host gets those instead of the local environment.

;;;The windowing system or desktop environment may ask the user to grant or deny the request or even choose to ignore the request entirely.

;;;The caller can be notified whenever the request is granted or revoked by listening to the GdkToplevel::shortcuts-inhibited property.

;;;Parameters
;;;toplevel

;;;the GdkToplevel requesting system keyboard shortcuts

;;;
;;;event

;;;the GdkEvent that is triggering the inhibit request, or NULL if none is available.

;;;[nullable]
;;;gdk_toplevel_restore_system_shortcuts ()
;;;void
;;;gdk_toplevel_restore_system_shortcuts (GdkToplevel *toplevel);
;;;Restore default system keyboard shortcuts which were previously requested to be inhibited by gdk_toplevel_inhibit_system_shortcuts().

;;;Parameters
;;;toplevel

;;;a GdkToplevel

;;;
;;;gdk_toplevel_begin_resize ()
;;;void
;;;gdk_toplevel_begin_resize (GdkToplevel *toplevel,
;;;                           GdkSurfaceEdge edge,
;;;                           GdkDevice *device,
;;;                           int button,
;;;                           double x,
;;;                           double y,
;;;                           guint32 timestamp);
;;;Begins an interactive resize operation (for a toplevel surface). You might use this function to implement a “window resize grip.”

;;;Parameters
;;;toplevel

;;;a GdkToplevel

;;;
;;;edge

;;;the edge or corner from which the drag is started

;;;
;;;device

;;;the device used for the operation.

;;;[nullable]
;;;button

;;;the button being used to drag, or 0 for a keyboard-initiated drag

;;;
;;;x

;;;surface X coordinate of mouse click that began the drag

;;;
;;;y

;;;surface Y coordinate of mouse click that began the drag

;;;
;;;timestamp

;;;timestamp of mouse click that began the drag (use gdk_event_get_time())

;;;
;;;gdk_toplevel_begin_move ()
;;;void
;;;gdk_toplevel_begin_move (GdkToplevel *toplevel,
;;;                         GdkDevice *device,
;;;                         int button,
;;;                         double x,
;;;                         double y,
;;;                         guint32 timestamp);
;;;Begins an interactive move operation (for a toplevel surface). You might use this function to implement draggable titlebars.

;;;Parameters
;;;toplevel

;;;a GdkToplevel

;;;
;;;device

;;;the device used for the operation

;;;
;;;button

;;;the button being used to drag, or 0 for a keyboard-initiated drag

;;;
;;;x

;;;surface X coordinate of mouse click that began the drag

;;;
;;;y

;;;surface Y coordinate of mouse click that began the drag

;;;
;;;timestamp

;;;timestamp of mouse click that began the drag

;;;
;;;Types and Values
;;;GdkToplevel
;;;typedef struct _GdkToplevel GdkToplevel;
;;;An interface for top level surfaces.

;;;enum GdkToplevelState
;;;Specifies the state of a toplevel surface.

;;;On platforms that support information about individual edges, the GDK_TOPLEVEL_STATE_TILED state will be set whenever any of the individual tiled states is set. On platforms that lack that support, the tiled state will give an indication of tiledness without any of the per-edge states being set.

;;;Members
;;;GDK_TOPLEVEL_STATE_MINIMIZED

;;;the surface is minimized

;;;
;;;GDK_TOPLEVEL_STATE_MAXIMIZED

;;;the surface is maximized

;;;
;;;GDK_TOPLEVEL_STATE_STICKY

;;;the surface is sticky

;;;
;;;GDK_TOPLEVEL_STATE_FULLSCREEN

;;;the surface is maximized without decorations

;;;
;;;GDK_TOPLEVEL_STATE_ABOVE

;;;the surface is kept above other surfaces

;;;
;;;GDK_TOPLEVEL_STATE_BELOW

;;;the surface is kept below other surfaces

;;;
;;;GDK_TOPLEVEL_STATE_FOCUSED

;;;the surface is presented as focused (with active decorations)

;;;
;;;GDK_TOPLEVEL_STATE_TILED

;;;the surface is in a tiled state

;;;
;;;GDK_TOPLEVEL_STATE_TOP_TILED

;;;whether the top edge is tiled

;;;
;;;GDK_TOPLEVEL_STATE_TOP_RESIZABLE

;;;whether the top edge is resizable

;;;
;;;GDK_TOPLEVEL_STATE_RIGHT_TILED

;;;whether the right edge is tiled

;;;
;;;GDK_TOPLEVEL_STATE_RIGHT_RESIZABLE

;;;whether the right edge is resizable

;;;
;;;GDK_TOPLEVEL_STATE_BOTTOM_TILED

;;;whether the bottom edge is tiled

;;;
;;;GDK_TOPLEVEL_STATE_BOTTOM_RESIZABLE

;;;whether the bottom edge is resizable

;;;
;;;GDK_TOPLEVEL_STATE_LEFT_TILED

;;;whether the left edge is tiled

;;;
;;;GDK_TOPLEVEL_STATE_LEFT_RESIZABLE

;;;whether the left edge is resizable

;;;
;;;enum GdkFullscreenMode
;;;Indicates which monitor (in a multi-head setup) a surface should span over when in fullscreen mode.

;;;Members
;;;GDK_FULLSCREEN_ON_CURRENT_MONITOR

;;;Fullscreen on current monitor only.

;;;
;;;GDK_FULLSCREEN_ON_ALL_MONITORS

;;;Span across all monitors when fullscreen.

;;;
;;;enum GdkSurfaceEdge
;;;Determines a surface edge or corner.

;;;Members
;;;GDK_SURFACE_EDGE_NORTH_WEST

;;;the top left corner.

;;;
;;;GDK_SURFACE_EDGE_NORTH

;;;the top edge.

;;;
;;;GDK_SURFACE_EDGE_NORTH_EAST

;;;the top right corner.

;;;
;;;GDK_SURFACE_EDGE_WEST

;;;the left edge.

;;;
;;;GDK_SURFACE_EDGE_EAST

;;;the right edge.

;;;
;;;GDK_SURFACE_EDGE_SOUTH_WEST

;;;the lower left corner.

;;;
;;;GDK_SURFACE_EDGE_SOUTH

;;;the lower edge.

;;;
;;;GDK_SURFACE_EDGE_SOUTH_EAST

;;;the lower right corner.

;;;
;;;Property Details
;;;The “decorated” property
;;;  “decorated”                gboolean
;;;Decorated.

;;;Owner: GdkToplevel

;;;Flags: Read / Write

;;;Default value: FALSE

;;;The “deletable” property
;;;  “deletable”                gboolean
;;;Deletable.

;;;Owner: GdkToplevel

;;;Flags: Read / Write

;;;Default value: FALSE

;;;The “fullscreen-mode” property
;;;  “fullscreen-mode”          GdkFullscreenMode
;;;Fullscreen mode.

;;;Owner: GdkToplevel

;;;Flags: Read / Write

;;;Default value: GDK_FULLSCREEN_ON_CURRENT_MONITOR

;;;The “icon-list” property
;;;  “icon-list”                gpointer
;;;The list of icon textures.

;;;Owner: GdkToplevel

;;;Flags: Read / Write

;;;The “modal” property
;;;  “modal”                    gboolean
;;;Whether the surface is modal.

;;;Owner: GdkToplevel

;;;Flags: Read / Write

;;;Default value: FALSE

;;;The “shortcuts-inhibited” property
;;;  “shortcuts-inhibited”      gboolean
;;;Whether keyboard shortcuts are inhibited.

;;;Owner: GdkToplevel

;;;Flags: Read

;;;Default value: FALSE

;;;The “startup-id” property
;;;  “startup-id”               char *
;;;The startup ID of the surface.

;;;Owner: GdkToplevel

;;;Flags: Read / Write

;;;Default value: NULL

;;;The “state” property
;;;  “state”                    GdkToplevelState
;;;State.

;;;Owner: GdkToplevel

;;;Flags: Read

;;;The “title” property
;;;  “title”                    char *
;;;The title of the surface.

;;;Owner: GdkToplevel

;;;Flags: Read / Write

;;;Default value: NULL

;;;The “transient-for” property
;;;  “transient-for”            GdkSurface *
;;;The transient parent of the surface.

;;;Owner: GdkToplevel

;;;Flags: Read / Write

;;;Signal Details
;;;The “compute-size” signal
;;;void
;;;user_function (GdkToplevel     *toplevel,
;;;               GdkToplevelSize *size,
;;;               gpointer         user_data)
;;;Compute the desired size of the toplevel, given the information passed via the GdkToplevelSize object.

;;;It will normally be emitted during or after gdk_toplevel_present(), depending on the configuration received by the windowing system. It may also be emitted at any other point in time, in response to the windowing system spontaneously changing the configuration.

;;;It is the responsibility of the GdkToplevel user to handle this signal; failing to do so will result in an arbitrary size being used as a result.

;;;Parameters
;;;toplevel

;;;a GdkToplevel

;;;
;;;size

;;;a GdkToplevelSize.

;;;[type Gdk.ToplevelSize][out caller-allocates]
;;;user_data

;;;user data set when the signal handler was connected.

;;;
;;;Flags: Run Last

;;;See Also
;;;GdkSurface, GdkPopup

;;; --- End of file gdk.toplevel.lisp ------------------------------------------
