;;; ----------------------------------------------------------------------------
;;; gdk.monitor.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2022 Dieter Kaiser
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
;;; GdkMonitor
;;;
;;;     Object representing an output
;;;
;;; Types and Values
;;;
;;;     GdkMonitor
;;;     GdkSubpixelLayout
;;;
;;; Functions
;;;
;;;     gdk_monitor_get_display
;;;     gdk_monitor_get_geometry
;;;     gdk_monitor_get_width_mm
;;;     gdk_monitor_get_height_mm
;;;     gdk_monitor_get_manufacturer
;;;     gdk_monitor_get_model
;;;     gdk_monitor_get_connector
;;;     gdk_monitor_get_scale_factor
;;;     gdk_monitor_get_refresh_rate
;;;     gdk_monitor_get_subpixel_layout
;;;     gdk_monitor_is_valid
;;;
;;; Properties
;;;
;;;     connector
;;;     display
;;;     geometry
;;;     height-mm
;;;     manufacturer
;;;     model
;;;     refresh-rate
;;;     scale-factor
;;;     subpixel-layout
;;;     valid
;;;     width-mm
;;;
;;; Signals
;;;
;;;     invalidate
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkMonitor
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;;enum GdkSubpixelLayout
;;;This enumeration describes how the red, green and blue components of physical pixels on an output device are laid out.

;;;Members
;;;GDK_SUBPIXEL_LAYOUT_UNKNOWN

;;;The layout is not known

;;;
;;;GDK_SUBPIXEL_LAYOUT_NONE

;;;Not organized in this way

;;;
;;;GDK_SUBPIXEL_LAYOUT_HORIZONTAL_RGB

;;;The layout is horizontal, the order is RGB

;;;
;;;GDK_SUBPIXEL_LAYOUT_HORIZONTAL_BGR

;;;The layout is horizontal, the order is BGR

;;;
;;;GDK_SUBPIXEL_LAYOUT_VERTICAL_RGB

;;;The layout is vertical, the order is RGB

;;;
;;;GDK_SUBPIXEL_LAYOUT_VERTICAL_BGR

;;;The layout is vertical, the order is BGR



;;;GdkMonitor
;;;typedef struct _GdkMonitor GdkMonitor;
;;;The GdkMonitor struct contains only private fields and should not be accessed directly.



;;;
;;;Property Details
;;;The “connector” property
;;;  “connector”                char *
;;;The connector name.

;;;Owner: GdkMonitor

;;;Flags: Read

;;;Default value: NULL

;;;The “display” property
;;;  “display”                  GdkDisplay *
;;;The display of the monitor.

;;;Owner: GdkMonitor

;;;Flags: Read / Write / Construct Only

;;;The “geometry” property
;;;  “geometry”                 GdkRectangle *
;;;The geometry of the monitor.

;;;Owner: GdkMonitor

;;;Flags: Read

;;;The “height-mm” property
;;;  “height-mm”                int
;;;The height of the monitor, in millimeters.

;;;Owner: GdkMonitor

;;;Flags: Read

;;;Allowed values: >= 0

;;;Default value: 0

;;;The “manufacturer” property
;;;  “manufacturer”             char *
;;;The manufacturer name.

;;;Owner: GdkMonitor

;;;Flags: Read

;;;Default value: NULL

;;;The “model” property
;;;  “model”                    char *
;;;The model name.

;;;Owner: GdkMonitor

;;;Flags: Read

;;;Default value: NULL

;;;The “refresh-rate” property
;;;  “refresh-rate”             int
;;;The refresh rate, in millihertz.

;;;Owner: GdkMonitor

;;;Flags: Read

;;;Allowed values: >= 0

;;;Default value: 0

;;;The “scale-factor” property
;;;  “scale-factor”             int
;;;The scale factor.

;;;Owner: GdkMonitor

;;;Flags: Read

;;;Allowed values: >= 0

;;;Default value: 1

;;;The “subpixel-layout” property
;;;  “subpixel-layout”          GdkSubpixelLayout
;;;The subpixel layout.

;;;Owner: GdkMonitor

;;;Flags: Read

;;;Default value: GDK_SUBPIXEL_LAYOUT_UNKNOWN

;;;The “valid” property
;;;  “valid”                    gboolean
;;;Whether the monitor is still valid.

;;;Owner: GdkMonitor

;;;Flags: Read

;;;Default value: TRUE

;;;The “width-mm” property
;;;  “width-mm”                 int
;;;The width of the monitor, in millimeters.

;;;Owner: GdkMonitor

;;;Flags: Read

;;;Allowed values: >= 0

;;;Default value: 0

;;;Signal Details
;;;The “invalidate” signal
;;;void
;;;user_function (GdkMonitor *monitor,
;;;               gpointer    user_data)
;;;The ::invalidate signal gets emitted when the output represented by monitor gets disconnected.

;;;Parameters
;;;monitor

;;;the object on which this signal was emitted

;;;
;;;user_data

;;;user data set when the signal handler was connected.

;;;
;;;Flags: Run First






;;;Includes
;;;#include <gdk/gdk.h>
;;;Description
;;;GdkMonitor objects represent the individual outputs that are associated with a GdkDisplay. GdkDisplay keeps a GListModel to enumerate and monitor monitors with gdk_display_get_monitors(). You can use gdk_display_get_monitor_at_surface() to find a particular monitor.

;;;Functions
;;;gdk_monitor_get_display ()
;;;GdkDisplay *
;;;gdk_monitor_get_display (GdkMonitor *monitor);
;;;Gets the display that this monitor belongs to.

;;;Parameters
;;;monitor

;;;a GdkMonitor

;;;
;;;Returns
;;;the display.

;;;[transfer none]

;;;gdk_monitor_get_geometry ()
;;;void
;;;gdk_monitor_get_geometry (GdkMonitor *monitor,
;;;                          GdkRectangle *geometry);
;;;Retrieves the size and position of an individual monitor within the display coordinate space. The returned geometry is in ”application pixels”, not in ”device pixels” (see gdk_monitor_get_scale_factor()).

;;;Parameters
;;;monitor

;;;a GdkMonitor

;;;
;;;geometry

;;;a GdkRectangle to be filled with the monitor geometry.

;;;[out]
;;;gdk_monitor_get_width_mm ()
;;;int
;;;gdk_monitor_get_width_mm (GdkMonitor *monitor);
;;;Gets the width in millimeters of the monitor.

;;;Parameters
;;;monitor

;;;a GdkMonitor

;;;
;;;Returns
;;;the physical width of the monitor

;;;gdk_monitor_get_height_mm ()
;;;int
;;;gdk_monitor_get_height_mm (GdkMonitor *monitor);
;;;Gets the height in millimeters of the monitor.

;;;Parameters
;;;monitor

;;;a GdkMonitor

;;;
;;;Returns
;;;the physical height of the monitor

;;;gdk_monitor_get_manufacturer ()
;;;const char *
;;;gdk_monitor_get_manufacturer (GdkMonitor *monitor);
;;;Gets the name or PNP ID of the monitor's manufacturer, if available.

;;;Note that this value might also vary depending on actual display backend.

;;;PNP ID registry is located at https://uefi.org/pnp_id_list

;;;Parameters
;;;monitor

;;;a GdkMonitor

;;;
;;;Returns
;;;the name of the manufacturer, or NULL.

;;;[transfer none][nullable]

;;;gdk_monitor_get_model ()
;;;const char *
;;;gdk_monitor_get_model (GdkMonitor *monitor);
;;;Gets the string identifying the monitor model, if available.

;;;Parameters
;;;monitor

;;;a GdkMonitor

;;;
;;;Returns
;;;the monitor model, or NULL.

;;;[transfer none][nullable]

;;;gdk_monitor_get_connector ()
;;;const char *
;;;gdk_monitor_get_connector (GdkMonitor *monitor);
;;;Gets the name of the monitor's connector, if available.

;;;Parameters
;;;monitor

;;;a GdkMonitor

;;;
;;;Returns
;;;the name of the connector.

;;;[transfer none][nullable]

;;;gdk_monitor_get_scale_factor ()
;;;int
;;;gdk_monitor_get_scale_factor (GdkMonitor *monitor);
;;;Gets the internal scale factor that maps from monitor coordinates to the actual device pixels. On traditional systems this is 1, but on very high density outputs this can be a higher value (often 2).

;;;This can be used if you want to create pixel based data for a particular monitor, but most of the time you’re drawing to a surface where it is better to use gdk_surface_get_scale_factor() instead.

;;;Parameters
;;;monitor

;;;a GdkMonitor

;;;
;;;Returns
;;;the scale factor

;;;gdk_monitor_get_refresh_rate ()
;;;int
;;;gdk_monitor_get_refresh_rate (GdkMonitor *monitor);
;;;Gets the refresh rate of the monitor, if available.

;;;The value is in milli-Hertz, so a refresh rate of 60Hz is returned as 60000.

;;;Parameters
;;;monitor

;;;a GdkMonitor

;;;
;;;Returns
;;;the refresh rate in milli-Hertz, or 0

;;;gdk_monitor_get_subpixel_layout ()
;;;GdkSubpixelLayout
;;;gdk_monitor_get_subpixel_layout (GdkMonitor *monitor);
;;;Gets information about the layout of red, green and blue primaries for each pixel in this monitor, if available.

;;;Parameters
;;;monitor

;;;a GdkMonitor

;;;
;;;Returns
;;;the subpixel layout

;;;gdk_monitor_is_valid ()
;;;gboolean
;;;gdk_monitor_is_valid (GdkMonitor *monitor);
;;;Returns TRUE if the monitor object corresponds to a physical monitor. The monitor becomes invalid when the physical monitor is unplugged or removed.

;;;Parameters
;;;monitor

;;;a GdkMonitor

;;;
;;;Returns
;;;TRUE if the object corresponds to a physical monitor

