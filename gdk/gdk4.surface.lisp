;;; ----------------------------------------------------------------------------
;;; gdk.surface.lisp
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
;;; Type and Values
;;;
;;;     GdkSurface
;;;
;;;     GdkGravity
;;;     GdkModifierType
;;;
;;;     GDK_MODIFIER_MASK
;;;
;;; Functions
;;;
;;;     gdk_surface_new_toplevel
;;;     gdk_surface_new_popup
;;;     gdk_surface_destroy
;;;     gdk_surface_is_destroyed
;;;     gdk_surface_get_display
;;;     gdk_surface_hide
;;;     gdk_surface_get_mapped
;;;     gdk_surface_translate_coordinates
;;;     gdk_surface_beep
;;;     gdk_surface_get_scale_factor
;;;     gdk_surface_set_opaque_region
;;;     gdk_surface_create_gl_context
;;;     gdk_surface_create_vulkan_context
;;;     gdk_surface_create_cairo_context
;;;     gdk_surface_queue_render
;;;     gdk_surface_get_frame_clock
;;;     gdk_surface_request_layout
;;;     gdk_surface_set_cursor
;;;     gdk_surface_get_cursor
;;;     gdk_surface_set_input_region
;;;     gdk_surface_get_width
;;;     gdk_surface_get_height
;;;     gdk_surface_get_device_position
;;;     gdk_surface_get_device_cursor
;;;     gdk_surface_set_device_cursor
;;;
;;; Properties
;;;
;;;     cursor
;;;     display
;;;     frame-clock
;;;     height
;;;     mapped
;;;     scale-factor
;;;     width
;;;
;;; Signals
;;;
;;;     enter-monitor
;;;     event
;;;     layout
;;;     leave-monitor
;;;     render
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkSurface
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GDK_MODIFIER_MASK
;;;
;;; #define GDK_MODIFIER_MASK
;;;
;;; A mask covering all entries in GdkModifierType.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GdkGravity
;;;
;;; Defines the reference point of a surface and is used in GdkPopupLayout.
;;;
;;; GDK_GRAVITY_NORTH_WEST
;;;     the reference point is at the top left corner.
;;;
;;; GDK_GRAVITY_NORTH
;;;     the reference point is in the middle of the top edge.
;;;
;;; GDK_GRAVITY_NORTH_EAST
;;;     the reference point is at the top right corner.
;;;
;;; GDK_GRAVITY_WEST
;;;     the reference point is at the middle of the left edge.
;;;
;;; GDK_GRAVITY_CENTER
;;;     the reference point is at the center of the surface.
;;;
;;; GDK_GRAVITY_EAST
;;;     the reference point is at the middle of the right edge.
;;;
;;; GDK_GRAVITY_SOUTH_WEST
;;;     the reference point is at the lower left corner.
;;;
;;; GDK_GRAVITY_SOUTH
;;;     the reference point is at the middle of the lower edge.
;;;
;;; GDK_GRAVITY_SOUTH_EAST
;;;     the reference point is at the lower right corner.
;;;
;;; GDK_GRAVITY_STATIC
;;;     the reference point is at the top left corner of the surface itself,
;;;     ignoring window manager decorations.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; enum GdkModifierType
;;; ----------------------------------------------------------------------------

(define-g-flags "GdkModifierType" modifier-type
  (:export t
   :type-initializer "gdk_modifier_type_get_type")
  (:none         0)
  (:shift-mask   #.(ash 1 0))
  (:lock-mask    #.(ash 1 1))
  (:control-mask #.(ash 1 2))
  (:alt-mask     #.(ash 1 3))

  (:button1-mask #.(ash 1 8))
  (:button2-mask #.(ash 1 9))
  (:button3-mask #.(ash 1 10))
  (:button4-mask #.(ash 1 11))
  (:button5-mask #.(ash 1 12))

  (:super-mask   #.(ash 1 26))
  (:hyper-mask   #.(ash 1 27))
  (:meta-mask    #.(ash 1 28)))

#+liber-documentation
(setf (liber:alias-for-symbol 'modifier-type)
      "GFlags"
      (liber:symbol-documentation 'modifier-type)
 "@version{#2022-7-15}
  @begin{short}
    Flags to indicate the state of modifier keys and mouse buttons in events.
  @end{short}
  Typical modifier keys are @kbd{Shift}, @kbd{Control}, @kbd{Meta}, @kbd{Super},
  @kbd{Hyper}, @kbd{Alt}, @kbd{Compose}, @kbd{Apple}, @kbd{CapsLock} or
  @kbd{ShiftLock} keys.

  Note that GDK may add internal values to events which include  values
  outside this enumeration. Your code should
  preserve and ignore them. You can use the @variable{gdk:modifier-mask} value
  to remove all private values.
  @begin{pre}
(define-g-flags \"GdkModifierType\" modifier-type
  (:export t
   :type-initializer \"gdk_modifier_type_get_type\")
  (:none         0)
  (:shift-mask   #.(ash 1 0))
  (:lock-mask    #.(ash 1 1))
  (:control-mask #.(ash 1 2))
  (:alt-mask     #.(ash 1 3))
  (:button1-mask #.(ash 1 8))
  (:button2-mask #.(ash 1 9))
  (:button3-mask #.(ash 1 10))
  (:button4-mask #.(ash 1 11))
  (:button5-mask #.(ash 1 12))
  (:super-mask   #.(ash 1 26))
  (:hyper-mask   #.(ash 1 27))
  (:meta-mask    #.(ash 1 28)))
  @end{pre}
  @begin[code]{table}
    @entry[:none]{No modifier key.}
    @entry[:shift-mask]{The @kbd{Shift} key.}
    @entry[:lock-mask]{A Lock key, depending on the modifier mapping of the
      X server this may either be the @kbd{CapsLock} or @kbd{ShiftLock} key.}
    @entry[:control-mask]{The @kbd{Control} key.}
    @entry[:alt-mask]{The fourth modifier key. It depends on the modifier
      mapping of the X server which key is interpreted as this modifier, but
      normally it is the @kbd{Alt} key.}
    @entry[:button1-mask]{The first mouse button.}
    @entry[:button2-mask]{The second mouse button.}
    @entry[:button3-mask]{The third mouse button.}
    @entry[:button4-mask]{The fourth mouse button.}
    @entry[:button5-mask]{The fifth mouse button.}
    @entry[:super-mask]{The Super modifier.}
    @entry[:hyper-mask]{The Hyper modifier.}
    @entry[:meta-mask]{The Meta modifier.}
  @end{table}
  @see-class{gdk:event}")

;;; ----------------------------------------------------------------------------
;;; GdkSurface
;;;
;;; A GdkSurface is a rectangular region on the screen.
;;;
;;; It’s a low-level object, used to implement high-level objects such as
;;; Gtk.Window or Gtk.Dialog in GTK.
;;;
;;; The surfaces you see in practice are either GdkToplevel or GdkPopup, and
;;; those interfaces provide much of the required API to interact with these
;;; surfaces. Other, more specialized surface types exist, but you will rarely
;;; interact with them directly.
;;;
;;; Signal Details
;;;
;;;The “enter-monitor” signal
;;;void
;;;user_function (GdkSurface *surface,
;;;               GdkMonitor *monitor,
;;;               gpointer    user_data)
;;;Emitted when surface starts being present on the monitor.

;;;Parameters
;;;surface

;;;the GdkSurface

;;;
;;;monitor

;;;the monitor

;;;
;;;user_data

;;;user data set when the signal handler was connected.

;;;
;;;Flags: Run First

;;;The “event” signal
;;;gboolean
;;;user_function (GdkSurface *surface,
;;;               gpointer    event,
;;;               gpointer    user_data)
;;;Emitted when GDK receives an input event for surface .

;;;Parameters
;;;surface

;;;the GdkSurface

;;;
;;;event

;;;an input event.

;;;[type Gdk.Event]
;;;user_data

;;;user data set when the signal handler was connected.

;;;
;;;Returns
;;;TRUE to indicate that the event has been handled

;;;Flags: Run Last

;;;The “layout” signal
;;;void
;;;user_function (GdkSurface *surface,
;;;               int         width,
;;;               int         height,
;;;               gpointer    user_data)
;;;Emitted when the size of surface is changed, or when relayout should be performed.

;;;Surface size is reported in ”application pixels”, not ”device pixels” (see gdk_surface_get_scale_factor()).

;;;Parameters
;;;surface

;;;the GdkSurface

;;;
;;;width

;;;the current width

;;;
;;;height

;;;the current height

;;;
;;;user_data

;;;user data set when the signal handler was connected.

;;;
;;;Flags: Run First

;;;The “leave-monitor” signal
;;;void
;;;user_function (GdkSurface *surface,
;;;               GdkMonitor *monitor,
;;;               gpointer    user_data)
;;;Emitted when surface stops being present on the monitor.

;;;Parameters
;;;surface

;;;the GdkSurface

;;;
;;;monitor

;;;the monitor

;;;
;;;user_data

;;;user data set when the signal handler was connected.

;;;
;;;Flags: Run First

;;;The “render” signal
;;;gboolean
;;;user_function (GdkSurface  *surface,
;;;               CairoRegion *region,
;;;               gpointer     user_data)
;;;Emitted when part of the surface needs to be redrawn.

;;;Parameters
;;;surface

;;;the GdkSurface

;;;
;;;region

;;;the region that needs to be redrawn

;;;
;;;user_data

;;;user data set when the signal handler was connected.

;;;
;;;Returns
;;;TRUE to indicate that the signal has been handled

;;;Flags: Run Last

;;;See Also
;;;GdkToplevel, GdkPopup
;;; ----------------------------------------------------------------------------

(define-g-object-class "GdkSurface" surface
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_surface_get_type")
  ((cursor
    surface-cursor
    "cursor" "GdkCursor" t t)
   (display
    surface-display
    "display" "GdkDisplay" t nil)
   (frame-clock
    surface-frame-clock
    "frame-clock" "GdkFrameClock" t nil)
   (height
    surface-height
    "height" "gint" t nil)
   (mapped
    surface-mapped
    "mapped" "gboolean" t nil)
   (scale-factor
    surface-scale-factor
    "scale-factor" "gint" t nil)
   (width
    surface-width
    "width" "gint" t nil)))

;;; ----------------------------------------------------------------------------
;;; Property Details
;;; ----------------------------------------------------------------------------

;;;The “cursor” property
;;;  “cursor”                   GdkCursor *
;;;The mouse pointer for a GdkSurface. See gdk_surface_set_cursor() and gdk_surface_get_cursor() for details.

;;;Owner: GdkSurface

;;;Flags: Read / Write

;;;The “display” property
;;;  “display”                  GdkDisplay *
;;;The GdkDisplay connection of the surface. See gdk_surface_get_display() for details.

;;;Owner: GdkSurface

;;;Flags: Read / Write / Construct Only

;;;The “frame-clock” property
;;;  “frame-clock”              GdkFrameClock *
;;;Frame Clock.

;;;Owner: GdkSurface

;;;Flags: Read / Write / Construct Only

;;;The “height” property
;;;  “height”                   int
;;;Height.

;;;Owner: GdkSurface

;;;Flags: Read

;;;Allowed values: >= 0

;;;Default value: 0

;;;The “mapped” property
;;;  “mapped”                   gboolean
;;;Mapped.

;;;Owner: GdkSurface

;;;Flags: Read

;;;Default value: FALSE

;;;The “scale-factor” property
;;;  “scale-factor”             int
;;;Scale factor.

;;;Owner: GdkSurface

;;;Flags: Read

;;;Allowed values: >= 1

;;;Default value: 1

;;;The “width” property
;;;  “width”                    int
;;;Width.

;;;Owner: GdkSurface

;;;Flags: Read

;;;Allowed values: >= 0

;;;Default value: 0
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_surface_new_toplevel ()
;;;
;;; GdkSurface *
;;; gdk_surface_new_toplevel (GdkDisplay *display);
;;;
;;; Creates a new toplevel surface.
;;;
;;; display :
;;;     the display to create the surface on
;;;
;;; Returns :
;;;     the new GdkSurface.
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_surface_new_toplevel" surface-new-toplevel) (g:object surface)
  (display (g:object display)))

(export 'surface-new-toplevel)

;;; ----------------------------------------------------------------------------
;;;gdk_surface_new_popup ()
;;;GdkSurface *
;;;gdk_surface_new_popup (GdkSurface *parent,
;;;                       gboolean autohide);
;;;Create a new popup surface.

;;;The surface will be attached to parent and can be positioned relative to it using gdk_popup_present().

;;;[constructor]

;;;Parameters
;;;parent

;;;the parent surface to attach the surface to

;;;
;;;autohide

;;;whether to hide the surface on outside clicks

;;;
;;;Returns
;;;a new GdkSurface.

;;;[transfer full]

;;;gdk_surface_destroy ()
;;;void
;;;gdk_surface_destroy (GdkSurface *surface);
;;;Destroys the window system resources associated with surface and decrements surface 's reference count. The window system resources for all children of surface are also destroyed, but the children’s reference counts are not decremented.

;;;Note that a surface will not be destroyed automatically when its reference count reaches zero. You must call this function yourself before that happens.

;;;Parameters
;;;surface

;;;a GdkSurface

;;;
;;;gdk_surface_is_destroyed ()
;;;gboolean
;;;gdk_surface_is_destroyed (GdkSurface *surface);
;;;Check to see if a surface is destroyed..

;;;Parameters
;;;surface

;;;a GdkSurface

;;;
;;;Returns
;;;TRUE if the surface is destroyed

;;;gdk_surface_get_display ()
;;;GdkDisplay *
;;;gdk_surface_get_display (GdkSurface *surface);
;;;Gets the GdkDisplay associated with a GdkSurface.

;;;Parameters
;;;surface

;;;a GdkSurface

;;;
;;;Returns
;;;the GdkDisplay associated with surface .

;;;[transfer none]

;;;gdk_surface_hide ()
;;;void
;;;gdk_surface_hide (GdkSurface *surface);
;;;For toplevel surfaces, withdraws them, so they will no longer be known to the window manager; for all surfaces, unmaps them, so they won’t be displayed. Normally done automatically as part of gtk_widget_hide().

;;;Parameters
;;;surface

;;;a GdkSurface

;;;
;;;gdk_surface_get_mapped ()
;;;gboolean
;;;gdk_surface_get_mapped (GdkSurface *surface);
;;;Checks whether the surface has been mapped (with gdk_toplevel_present() or gdk_popup_present()).

;;;Parameters
;;;surface

;;;a GdkSurface

;;;
;;;Returns
;;;TRUE if the surface is mapped

;;;gdk_surface_translate_coordinates ()
;;;gboolean
;;;gdk_surface_translate_coordinates (GdkSurface *from,
;;;                                   GdkSurface *to,
;;;                                   double *x,
;;;                                   double *y);
;;;Translates the given coordinates from being relative to the from surface to being relative to the to surface.

;;;Note that this only works if to and from are popups or transient-for to the same toplevel (directly or indirectly).

;;;Parameters
;;;from

;;;the origin surface

;;;
;;;to

;;;the target surface

;;;
;;;x

;;;coordinates to translate

;;;
;;;y

;;;coordinates to translate

;;;
;;;Returns
;;;TRUE if the coordinates were successfully translated

;;;gdk_surface_beep ()
;;;void
;;;gdk_surface_beep (GdkSurface *surface);
;;;Emits a short beep associated to surface in the appropriate display, if supported. Otherwise, emits a short beep on the display just as gdk_display_beep().

;;;Parameters
;;;surface

;;;a toplevel GdkSurface

;;;
;;;gdk_surface_get_scale_factor ()
;;;int
;;;gdk_surface_get_scale_factor (GdkSurface *surface);
;;;Returns the internal scale factor that maps from surface coordinates to the actual device pixels. On traditional systems this is 1, but on very high density outputs this can be a higher value (often 2).

;;;A higher value means that drawing is automatically scaled up to a higher resolution, so any code doing drawing will automatically look nicer. However, if you are supplying pixel-based data the scale value can be used to determine whether to use a pixel resource with higher resolution data.

;;;The scale of a surface may change during runtime.

;;;Parameters
;;;surface

;;;surface to get scale factor for

;;;
;;;Returns
;;;the scale factor

;;;gdk_surface_set_opaque_region ()
;;;void
;;;gdk_surface_set_opaque_region (GdkSurface *surface,
;;;                               cairo_region_t *region);
;;;For optimisation purposes, compositing window managers may like to not draw obscured regions of surfaces, or turn off blending during for these regions. With RGB windows with no transparency, this is just the shape of the window, but with ARGB32 windows, the compositor does not know what regions of the window are transparent or not.

;;;This function only works for toplevel surfaces.

;;;GTK will update this property automatically if the surface background is opaque, as we know where the opaque regions are. If your surface background is not opaque, please update this property in your GtkWidgetClass.css_changed() handler.

;;;Parameters
;;;surface

;;;a top-level or non-native GdkSurface

;;;
;;;region

;;;a region, or NULL.

;;;[allow-none]
;;;gdk_surface_create_gl_context ()
;;;GdkGLContext *
;;;gdk_surface_create_gl_context (GdkSurface *surface,
;;;                               GError **error);
;;;Creates a new GdkGLContext matching the framebuffer format to the visual of the GdkSurface. The context is disconnected from any particular surface or surface.

;;;If the creation of the GdkGLContext failed, error will be set.

;;;Before using the returned GdkGLContext, you will need to call gdk_gl_context_make_current() or gdk_gl_context_realize().

;;;Parameters
;;;surface

;;;a GdkSurface

;;;
;;;error

;;;return location for an error

;;;
;;;Returns
;;;the newly created GdkGLContext, or NULL on error.

;;;[transfer full]

;;;gdk_surface_create_vulkan_context ()
;;;GdkVulkanContext *
;;;gdk_surface_create_vulkan_context (GdkSurface *surface,
;;;                                   GError **error);
;;;Creates a new GdkVulkanContext for rendering on surface .

;;;If the creation of the GdkVulkanContext failed, error will be set.

;;;Parameters
;;;surface

;;;a GdkSurface

;;;
;;;error

;;;return location for an error

;;;
;;;Returns
;;;the newly created GdkVulkanContext, or NULL on error.

;;;[transfer full]

;;;gdk_surface_create_cairo_context ()
;;;GdkCairoContext *
;;;gdk_surface_create_cairo_context (GdkSurface *surface);
;;;Creates a new GdkCairoContext for rendering on surface .

;;;Parameters
;;;surface

;;;a GdkSurface

;;;
;;;Returns
;;;the newly created GdkCairoContext.

;;;[transfer full]

;;;gdk_surface_queue_render ()
;;;void
;;;gdk_surface_queue_render (GdkSurface *surface);
;;;Forces a “render” signal emission for surface to be scheduled.

;;;This function is useful for implementations that track invalid regions on their own.

;;;Parameters
;;;surface

;;;a GdkSurface

;;;
;;;gdk_surface_get_frame_clock ()
;;;GdkFrameClock *
;;;gdk_surface_get_frame_clock (GdkSurface *surface);
;;;Gets the frame clock for the surface. The frame clock for a surface never changes unless the surface is reparented to a new toplevel surface.

;;;Parameters
;;;surface

;;;surface to get frame clock for

;;;
;;;Returns
;;;the frame clock.

;;;[transfer none]

;;;gdk_surface_request_layout ()
;;;void
;;;gdk_surface_request_layout (GdkSurface *surface);
;;;Request a GDK_FRAME_CLOCK_PHASE_LAYOUT from the surface's frame clock. See gdk_frame_clock_request_phase().

;;;Parameters
;;;surface

;;;a GdkSurface

;;;
;;;gdk_surface_set_cursor ()
;;;void
;;;gdk_surface_set_cursor (GdkSurface *surface,
;;;                        GdkCursor *cursor);
;;;Sets the default mouse pointer for a GdkSurface.

;;;Note that cursor must be for the same display as surface .

;;;Use gdk_cursor_new_from_name() or gdk_cursor_new_from_texture() to create the cursor. To make the cursor invisible, use GDK_BLANK_CURSOR. Passing NULL for the cursor argument to gdk_surface_set_cursor() means that surface will use the cursor of its parent surface. Most surfaces should use this default.

;;;Parameters
;;;surface

;;;a GdkSurface

;;;
;;;cursor

;;;a cursor.

;;;[allow-none]
;;;gdk_surface_get_cursor ()
;;;GdkCursor *
;;;gdk_surface_get_cursor (GdkSurface *surface);
;;;Retrieves a GdkCursor pointer for the cursor currently set on the specified GdkSurface, or NULL. If the return value is NULL then there is no custom cursor set on the specified surface, and it is using the cursor for its parent surface.

;;;Parameters
;;;surface

;;;a GdkSurface

;;;
;;;Returns
;;;a GdkCursor, or NULL. The returned object is owned by the GdkSurface and should not be unreferenced directly. Use gdk_surface_set_cursor() to unset the cursor of the surface.

;;;[nullable][transfer none]

;;;gdk_surface_set_input_region ()
;;;void
;;;gdk_surface_set_input_region (GdkSurface *surface,
;;;                              cairo_region_t *region);
;;;Apply the region to the surface for the purpose of event handling. Mouse events which happen while the pointer position corresponds to an unset bit in the mask will be passed on the surface below surface .

;;;An input region is typically used with RGBA surfaces. The alpha channel of the surface defines which pixels are invisible and allows for nicely antialiased borders, and the input region controls where the surface is “clickable”.

;;;Use gdk_display_supports_input_shapes() to find out if a particular backend supports input regions.

;;;Parameters
;;;surface

;;;a GdkSurface

;;;
;;;region

;;;region of surface to be reactive

;;;
;;;gdk_surface_get_width ()
;;;int
;;;gdk_surface_get_width (GdkSurface *surface);
;;;Returns the width of the given surface .

;;;Surface size is reported in ”application pixels”, not ”device pixels” (see gdk_surface_get_scale_factor()).

;;;Parameters
;;;surface

;;;a GdkSurface

;;;
;;;Returns
;;;The width of surface

;;;gdk_surface_get_height ()
;;;int
;;;gdk_surface_get_height (GdkSurface *surface);
;;;Returns the height of the given surface .

;;;Surface size is reported in ”application pixels”, not ”device pixels” (see gdk_surface_get_scale_factor()).

;;;Parameters
;;;surface

;;;a GdkSurface

;;;
;;;Returns
;;;The height of surface

;;;gdk_surface_get_device_position ()
;;;gboolean
;;;gdk_surface_get_device_position (GdkSurface *surface,
;;;                                 GdkDevice *device,
;;;                                 double *x,
;;;                                 double *y,
;;;                                 GdkModifierType *mask);
;;;Obtains the current device position in doubles and modifier state. The position is given in coordinates relative to the upper left corner of surface .

;;;Return: TRUE if the device is over the surface

;;;Parameters
;;;surface

;;;a GdkSurface.

;;;
;;;device

;;;pointer GdkDevice to query to.

;;;
;;;x

;;;return location for the X coordinate of device , or NULL.

;;;[out][allow-none]
;;;y

;;;return location for the Y coordinate of device , or NULL.

;;;[out][allow-none]
;;;mask

;;;return location for the modifier mask, or NULL.

;;;[out][allow-none]
;;;gdk_surface_get_device_cursor ()
;;;GdkCursor *
;;;gdk_surface_get_device_cursor (GdkSurface *surface,
;;;                               GdkDevice *device);
;;;Retrieves a GdkCursor pointer for the device currently set on the specified GdkSurface, or NULL. If the return value is NULL then there is no custom cursor set on the specified surface, and it is using the cursor for its parent surface.

;;;Parameters
;;;surface

;;;a GdkSurface.

;;;
;;;device

;;;a logical, pointer GdkDevice.

;;;
;;;Returns
;;;a GdkCursor, or NULL. The returned object is owned by the GdkSurface and should not be unreferenced directly. Use gdk_surface_set_cursor() to unset the cursor of the surface.

;;;[nullable][transfer none]

;;;gdk_surface_set_device_cursor ()
;;;void
;;;gdk_surface_set_device_cursor (GdkSurface *surface,
;;;                               GdkDevice *device,
;;;                               GdkCursor *cursor);
;;;Sets a specific GdkCursor for a given device when it gets inside surface . Use gdk_cursor_new_from_name() or gdk_cursor_new_from_texture() to create the cursor. To make the cursor invisible, use GDK_BLANK_CURSOR. Passing NULL for the cursor argument to gdk_surface_set_cursor() means that surface will use the cursor of its parent surface. Most surfaces should use this default.

;;;Parameters
;;;surface

;;;a GdkSurface

;;;
;;;device

;;;a logical, pointer GdkDevice

;;;
;;;cursor

;;;a GdkCursor


;;; --- End of file gdk.surface.lisp -------------------------------------------
