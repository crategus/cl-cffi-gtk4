;;; ----------------------------------------------------------------------------
;;; gdk4.surface.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2022 - 2024 Dieter Kaiser
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
;;; Type and Values
;;;
;;;     GdkSurface
;;;
;;; Accessors
;;;
;;;     gdk_surface_get_cursor
;;;     gdk_surface_set_cursor
;;;     gdk_surface_get_display
;;;     gdk_surface_get_frame_clock
;;;     gdk_surface_get_height
;;;     gdk_surface_get_mapped
;;;     gdk_surface_get_scale                              Since 4.12
;;;     gdk_surface_get_scale_factor
;;;     gdk_surface_get_width
;;;
;;; Functions
;;;
;;;     gdk_surface_new_toplevel
;;;     gdk_surface_new_popup
;;;
;;;     gdk_surface_destroy
;;;     gdk_surface_is_destroyed
;;;     gdk_surface_hide
;;;     gdk_surface_translate_coordinates
;;;     gdk_surface_beep
;;;     gdk_surface_set_opaque_region
;;;     gdk_surface_create_gl_context
;;;     gdk_surface_create_vulkan_context
;;;     gdk_surface_create_cairo_context
;;;     gdk_surface_create_similar_surface                 Deprecated 4.12
;;;     gdk_surface_queue_render
;;;     gdk_surface_request_layout
;;;     gdk_surface_set_input_region
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
;;;     scale                                              Since 4.12
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
;;; GdkSurface
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GdkSurface" surface
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
   #+gtk-4-12
   (scale
    surface-scale
    "scale" "gdouble" t nil)
   (scale-factor
    surface-scale-factor
    "scale-factor" "gint" t nil)
   (width
    surface-width
    "width" "gint" t nil)))

#+liber-documentation
(setf (documentation 'surface 'type)
 "@version{2024-1-8}
  @begin{short}
    A @class{gdk:surface} object is a rectangular region on the screen.
  @end{short}
  It is a low-level object, used to implement high-level objects such as the
  @class{gtk:window} or the @class{gtk:dialog} widgets in GTK.

  The surfaces you see in practice are either @class{gdk:toplevel} or
  @class{gdk:popup} objects, and those interfaces provide much of the required
  API to interact with these surfaces. Other, more specialized surface types
  exist, but you will rarely interact with them directly.
  @begin[Signal Details]{dictionary}
    @subheading{The \"enter-monitor\" signal}
      @begin{pre}
lambda (surface monitor)    :run-first
      @end{pre}
      Emitted when @arg{surface} starts being present on the monitor.
      @begin[code]{table}
       @entry[surface]{A @class{gdk:surface} object.}
       @entry[monitor]{A @class{gdk:monitor} object.}
      @end{table}
    @subheading{The \"event\" signal}
      @begin{pre}
lambda (surface event)    :run-last
      @end{pre}
      Emitted when GDK receives an input event for @arg{surface}.
      @begin[code]{table}
       @entry[surface]{A @class{gdk:surface} object.}
       @entry[event]{A @class{gdk:event} instance to an input event.}
       @entry[Returns]{@em{True} to indicate that the event has been handled.}
      @end{table}
    @subheading{The \"layout\" signal}
      @begin{pre}
lambda (surface width height)    :run-first
      @end{pre}
      Emitted when the size of @arg{surface} is changed, or when relayout
      should be performed. The surface size is reported in \"application
      pixels\", not \"device pixels\". See the @fun{gdk:surface-scale-factor}
      function.
      @begin[code]{table}
       @entry[surface]{A @class{gdk:surface} object.}
       @entry[width]{An integer with the current width.}
       @entry[height]{An integer with the current height.}
      @end{table}
    @subheading{The \"leave-monitor\" signal}
      @begin{pre}
lambda (surface monitor)    :run-first
      @end{pre}
      Emitted when @arg{surface} stops being present on the monitor.
      @begin[code]{table}
       @entry[surface]{A @class{gdk:surface} object.}
       @entry[monitor]{A @class{gdk:monitor} object.}
      @end{table}
    @subheading{The \"render\" signal}
      @begin{pre}
lambda (surface region)    :run-last
      @end{pre}
      Emitted when part of the surface needs to be redrawn.
      @begin[code]{table}
       @entry[surface]{A @class{gdk:surface} object.}
       @entry[region]{A @symbol{cairo:region-t} instance that needs to be
         redrawn.}
       @entry[Returns]{@em{True} to indicate that the signal has been handled.}
      @end{table}
  @end{dictionary}
  @see-constructor{gdk:surface-new-toplevel}
  @see-constructor{gdk:surface-new-popup}
  @see-slot{gdk:surface-cursor}
  @see-slot{gdk:surface-display}
  @see-slot{gdk:surface-frame-clock}
  @see-slot{gdk:surface-height}
  @see-slot{gdk:surface-mapped}
  @see-slot{gdk:surface-scale}
  @see-slot{gdk:surface-scale-factor}
  @see-slot{gdk:surface-width}
  @see-class{gdk:toplevel}
  @see-class{gdk:popup}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk:surface-cursor -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "cursor" 'surface) t)
 "The @code{cursor} property of type @class{gdk:cursor} (Read / Write) @br{}
  The mouse pointer for a surface.")

#+liber-documentation
(setf (liber:alias-for-function 'surface-cursor)
      "Accessor"
      (documentation 'surface-cursor 'function)
 "@version{2024-1-8}
  @syntax[]{(gdk:surface-cursor object) => cursor}
  @syntax[]{(setf (gdk:surface-cursor object) cursor)}
  @argument[object]{a @class{gdk:surface} object}
  @argument[cursor]{a @class{gdk:cursor} object}
  @begin{short}
    Accessor of the @slot[gdk:surface]{cursor} slot of the @class{gdk:surface}
    class.
  @end{short}
  The @fun{gdk:surface-cursor} function retrieves the cursor currently set on
  the specified surface, or @code{nil}. If the return value is @code{nil} then
  there is no custom cursor set on the specified surface, and it is using the
  cursor for its parent surface. The @setf{gdk:surface-cursor} function sets the
  default mouse pointer for a surface. Note that @arg{cursor} must be for the
  same display as @arg{surface}.

  Use the @fun{gdk:cursor-new-from-name} function or the
  @fun{gdk:cursor-new-from-texture} function to create the cursor. To make the
  cursor invisible, use a blank cursor. Passing @code{nil} for the @arg{cursor}
  argument means that the surface will use the cursor of its parent surface.
  Most surfaces should use this default.
  @see-class{gdk:surface}
  @see-class{gdk:cursor}
  @see-function{gdk:cursor-new-from-name}
  @see-function{gdk:cursor-new-from-texture}")

;;; --- gdk:surface-display ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "display" 'surface) t)
 "The @code{display} property of type @class{gdk:display}
  (Read / Write / Construct only) @br{}
  The display connection of the surface.")

#+liber-documentation
(setf (liber:alias-for-function 'surface-display)
      "Accessor"
      (documentation 'surface-display 'function)
 "@version{2024-1-8}
  @syntax[]{(gdk:surface-display object) => display}
  @syntax[]{(setf (gdk:surface-display object) display)}
  @argument[object]{a @class{gdk:surface} object}
  @argument[display]{a @class{gdk:display} object associated with @arg{surface}}
  @begin{short}
    Accessor of the @slot[gdk:surface]{display} slot of the @class{gdk:surface}
    class.
  @end{short}
  The @fun{gdk:surface-display} function gets the display associated with the
  surface.
  @see-class{gdk:surface}
  @see-class{gdk:display}")

;;; --- gdk:surface-frame-clock ------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "frame-clock" 'surface) t)
 "The @code{frame-clock} property of type @class{gdk:frame-clock}
  (Read / Write / Construct only) @br{}
  Frame clock.")

#+liber-documentation
(setf (liber:alias-for-function 'surface-frame-clock)
      "Accessor"
      (documentation 'surface-frame-clock 'function)
 "@version{2024-1-8}
  @syntax[]{(gdk:surface-frame-clock object) => clock}
  @syntax[]{(setf (gdk:surface-frame-clock object) clock)}
  @argument[object]{a @class{gdk:surface} object}
  @argument[clock]{a @class{gdk:frame-clock} object}
  @begin{short}
    Accessor of the @slot[gdk:surface]{frame-clock} slot of the
    @class{gdk:surface} class.
  @end{short}
  The @fun{gdk:surface-frame-clock} function gets the frame clock for the
  surface. The frame clock for a surface never changes unless the surface is
  reparented to a new toplevel surface.
  @see-class{gdk:surface}
  @see-class{gdk:frame-clock}")

;;; --- gdk:surface-height -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "height" 'surface) t)
 "The @code{height} property of type @code{:int} (Read) @br{}
  Height. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'surface-height)
      "Accessor"
      (documentation 'surface-height 'function)
 "@version{2024-1-8}
  @syntax[]{(gdk:surface-height object) => height}
  @syntax[]{(setf (gdk:surface-height object) height)}
  @argument[object]{a @class{gdk:surface} object}
  @argument[width]{an integer with the height of the surface}
  @begin{short}
    Accessor of the @slot[gdk:surface]{height} slot of the @class{gdk:surface}
    class.
  @end{short}
  The @fun{gdk:surface-height} function returns the height of the given
  @arg{surface}. Surface size is reported in \"application pixels\", not
  \"device pixels\". See the @fun{gdk:surface-scale-factor} function.
  @see-class{gdk:surface}
  @see-function{gdk:surface-scale-factor}")

;;; --- gdk:surface-mapped -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "mapped" 'surface) t)
 "The @code{mapped} property of type @code{:boolean} (Read) @br{}
  Mapped. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'surface-mapped)
      "Accessor"
      (documentation 'surface-mapped 'function)
 "@version{2024-1-8}
  @syntax[]{(gdk:surface-mapped object) => mapped}
  @syntax[]{(setf (gdk:surface-mapped object) mapped)}
  @argument[object]{a @class{gdk:surface} object}
  @argument[mapped]{a boolean whether the surface has been mapped}
  @begin{short}
    Accessor of the @slot[gdk:surface]{mapped} slot of the @class{gdk:surface}
    class.
  @end{short}
  The @fun{gdk:surface-mapped} function checks whether the surface has been
  mapped with the @fun{gdk:toplevel-present} or @fun{gdk:popup-present}
  functions.
  @see-class{gdk:surface}
  @see-function{gdk:toplevel-present}
  @see-function{gdk:popup-present}")

;;; --- gdk:surface-scale ------------------------------------------------------

#+(and gtk-4-12 liber-documentation)
(setf (documentation (liber:slot-documentation "scale" 'surface) t)
 "The @code{scale} property of type @code{:double} (Read) @br{}
  The scale of the surface. @br{}
  Default value: 1.0d0")

#+(and gtk-4-12 liber-documentation)
(setf (liber:alias-for-function 'surface-scale)
      "Accessor"
      (documentation 'surface-scale 'function)
 "@version{2024-1-8}
  @syntax[]{(gdk:surface-scale object) => scale}
  @syntax[]{(setf (gdk:surface-scale object) scale)}
  @argument[object]{a @class{gdk:surface} object}
  @argument[scale]{a double float with the scale}
  @begin{short}
    Accessor of the @slot[gdk:surface]{scale} slot of the @class{gdk:surface}
    class.
  @end{short}
  The @fun{gdk:surface-scale} function returns the internal scale that maps from
  surface coordinates to the actual device pixels. When the scale is bigger than
  1, the windowing system prefers to get buffers with a resolution that is
  bigger than the surface size, e.g. to show the surface on a high-resolution
  display, or in a magnifier.

  Compare with the @fun{gdk:surface-scale-factor} function, which returns the
  next larger integer.

  The scale may change during the lifetime of the surface.

  Since 4.12
  @see-class{gdk:surface}")

;;; --- gdk:surface-scale-factor -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "scale-factor" 'surface) t)
 "The @code{scale-factor} property of type @code{:int} (Read) @br{}
  The scale factor of the surface. @br{}
  Allowed values: >= 1 @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'surface-scale-factor)
      "Accessor"
      (documentation 'surface-scale-factor 'function)
 "@version{2024-1-8}
  @syntax[]{(gdk:surface-scale-factor object) => factor}
  @syntax[]{(setf (gdk:surface-scale-factor object) factor)}
  @argument[object]{a @class{gdk:surface} object}
  @argument[factor]{an integer with the scale factor}
  @begin{short}
    Accessor of the @slot[gdk:surface]{scale-factor} slot of the
    @class{gdk:surface} class.
  @end{short}
  The @fun{gdk:surface-scale-factor} function returns the internal scale factor
  that maps from surface coordinates to the actual device pixels. On traditional
  systems this is 1, but on very high density outputs this can be a higher value
  (often 2).

  A higher value means that drawing is automatically scaled up to a higher
  resolution, so any code doing drawing will automatically look nicer. However,
  if you are supplying pixel-based data the scale value can be used to determine
  whether to use a pixel resource with higher resolution data.

  The scale of a surface may change during runtime.
  @see-class{gdk:surface}")

;;; --- gdk:surface-width ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "width" 'surface) t)
 "The @code{width} property of type @code{:int} (Read) @br{}
  Width. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'surface-width)
      "Accessor"
      (documentation 'surface-width 'function)
 "@version{2024-1-8}
  @syntax[]{(gdk:surface-width object) => width}
  @syntax[]{(setf (gdk:surface-width object) width)}
  @argument[object]{a @class{gdk:surface} object}
  @argument[width]{an integer with the width of the surface}
  @begin{short}
    Accessor of the @slot[gdk:surface]{width} slot of the @class{gdk:surface}
    class.
  @end{short}
  The @fun{gdk:surface-width} function returns the width of the given
  @arg{surface}. Surface size is reported in \"application pixels\", not
  \"device pixels\". See the @fun{gdk:surface-scale-factor} function.
  @see-class{gdk:surface}
  @see-function{gdk:surface-scale-factor}")

;;; ----------------------------------------------------------------------------
;;; gdk_surface_new_toplevel ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_surface_new_toplevel" surface-new-toplevel)
    (g:object surface)
 #+liber-documentation
 "@version{2024-1-8}
  @argument[display]{a @class{gdk:display} object to create the surface on}
  @return{The new @class{gdk:surface} object.}
  @short{Creates a new toplevel surface.}
  @see-class{gdk:surface}
  @see-class{gdk:display}"
  (display (g:object display)))

(export 'surface-new-toplevel)

;;; ----------------------------------------------------------------------------
;;; gdk_surface_new_popup ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_surface_new_popup" surface-new-popup) (g:object surface)
 #+liber-documentation
 "@version{2024-1-8}
  @argument[parent]{a @class{gdk:surface} object with the parent surface to
    attach the surface to}
  @argument[autohide]{a boolean whether to hide the surface on outside clicks}
  @return{The new @class{gdk:surface} object.}
  @begin{short}
    Creates a new popup surface.
  @end{short}
  The surface will be attached to parent and can be positioned relative to it
  using the @fun{gdk:popup-present} function.
  @see-class{gdk:surface}
  @see-function{gdk:popup-present}"
  (parent (g:object surface))
  (autohide :boolean))

(export 'surface-new-popup)

;;; ----------------------------------------------------------------------------
;;; gdk_surface_destroy ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_surface_destroy" surface-destroy) :void
 #+liber-documentation
 "@version{2024-1-8}
  @argument[surface]{a @class{gdk:surface} object}
  @begin{short}
    Destroys the window system resources associated with @arg{surface} and
    decrements reference count of the surface.
  @end{short}
  The window system resources for all children of the surface are also
  destroyed, but the reference counts of the children are not decremented.

  Note that a surface will not be destroyed automatically when its reference
  count reaches zero. You must call this function yourself before that happens.
  @see-class{gdk:surface}"
  (surface (g:object surface)))

(export 'surface-destroy)

;;; ----------------------------------------------------------------------------
;;; gdk_surface_is_destroyed ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_surface_is_destroyed" surface-is-destroyed) :boolean
 #+liber-documentation
 "@version{2024-1-8}
  @argument[surface]{a @class{gdk:surface} object}
  @return{@em{True} if the surface is destroyed.}
  @short{Check to see if a surface is destroyed.}
  @see-class{gdk:surface}"
  (surface (g:object surface)))

(export 'surface-is-destroyed)

;;; ----------------------------------------------------------------------------
;;; gdk_surface_hide ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_surface_hide" surface-hide) :void
 #+liber-documentation
 "@version{#2023-4-8}
  @argument[surface]{a @class{gdk:surface} object}
  @begin{short}
    For toplevel surfaces, withdraws them, so they will no longer be known to
    the window manager.
  @end{short}
  For all surfaces, unmaps them, so they will not be displayed. Normally done
  automatically as part of the @fun{gtk:widget-hide} function.
  @see-class{gdk:surface}
  @see-function{gtk:widget-hide}"
  (surface (g:object surface)))

(export 'surface-hide)

;;; ----------------------------------------------------------------------------
;;; gdk_surface_translate_coordinates ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_surface_translate_coordinates"
               %surface-translate-coordinates) :boolean
  (from (g:object surface))
  (to (g:object surface))
  (x (:pointer :int))
  (y (:pointer :int)))

(defun surface-translate-coordinates (from to)
 #+liber-documentation
 "@version{#2023-4-8}
  @argument[from]{a @class{gdk:surface} object with the origin surface}
  @argument[to]{a @class{gdk:surface} object with the target surface}
  @begin{return}
    @arg{x} -- a double float with the x coordinate to translate @br{}
    @arg{y} -- a double float with the y coordinate to translate
  @end{return}
  @begin{short}
    Translates the given coordinates from being relative to the @arg{from}
    surface to being relative to the @arg{to} surface.
  @end{short}
  Note that this only works if @arg{to} and @arg{from} are popups or
  transient-for to the same toplevel (directly or indirectly).

  The function returns @code{nil}, if the coordinates were not successfully
  tanslated.
  @see-class{gdk:surface}"
  (cffi:with-foreign-objects ((x :int) (y :int))
    (when (%surface-translate-coordinates from to x y)
      (values x y))))

(export 'surface-translate-coordinates)

;;; ----------------------------------------------------------------------------
;;; gdk_surface_beep ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_surface_beep" surface-beep) :void
 #+liber-documentation
 "@version{#2023-4-8}
  @argument[surface]{a toplevel @class{gdk:surface} object}
  @begin{short}
    Emits a short beep associated to @arg{surface} in the appropriate display,
    if supported.
  @end{short}
  Otherwise, emits a short beep on the display just as the
  @fun{gdk:display-beep} function.
  @see-class{gdk:surface}
  @see-function{gdk:display-beep}"
  (surface (g:object surface)))

(export 'surface-beep)

;;; ----------------------------------------------------------------------------
;;; gdk_surface_set_opaque_region ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_surface_set_opaque_region" %surface-set-opaque-region) :void
  (surface (g:object surface))
  (region (:pointer (:struct cairo:region-t))))

(defun surface-set-opaque-region (surface region)
 #+liber-documentation
 "@version{#2023-4-8}
  @argument[surface]{a toplevel or non-native @class{gdk:surface} object}
  @argument[region]{a @symbol{cairo:region-t} instance, or @code{nil}}
  @begin{short}
    For optimisation purposes, compositing window managers may like to not draw
    obscured regions of surfaces, or turn off blending during for these regions.
  @end{short}
  With RGB windows with no transparency, this is just the shape of the window,
  but with ARGB32 windows, the compositor does not know what regions of the
  window are transparent or not. This function only works for toplevel surfaces.

  GTK will update this property automatically if the surface background is
  opaque, as we know where the opaque regions are. If your surface background
  is not opaque, please update this property in your
  @code{GtkWidgetClass.css_changed()} handler.

  @see-class{gdk:surface}
  @see-symbol{cairo:region-t}"
  (%surface-set-opaque-region surface
                              (if region region (cffi:null-pointer))))

(export 'surface-set-opaque-region)

;;; ----------------------------------------------------------------------------
;;; gdk_surface_create_gl_context ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_surface_create_gl_context" %surface-create-gl-context)
    (g:object gl-context)
  (surface (g:object surface))
  (err :pointer))

(defun surface-create-gl-context (surface)
 #+liber-documentation
 "@version{#2023-4-8}
  @argument[surface]{a @class{gdk:surface} object}
  @return{The newly created @class{gdk:gl-context} object.}
  @begin{short}
    Creates a new @class{gdk:gl-context} object matching the framebuffer format
    to the visual of the surface.
  @end{short}
  The context is disconnected from any particular surface.

  Before using the returned @class{gdk:gl-context} object, you will need to call
  the @fun{gdk:gl-context-make-current} or @fun{gdk:gl-context-realize}
  function.
  @see-class{gdk:surface}
  @see-class{gdk:gl-context}
  @see-function{gdk:gl-context-make-current}
  @see-function{gdk:gl-context-realize}"
  (glib:with-g-error (err)
    (%surface-create-gl-context surface err)))

(export 'surface-create-gl-context)

;;; ----------------------------------------------------------------------------
;;; gdk_surface_create_vulkan_context ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_surface_create_vulkan_context"
               %surface-create-vulkan-context) (g:object vulkan-context)
  (surface (g:object surface))
  (err :pointer))

(defun surface-create-vulkan-context (surface)
 #+liber-documentation
 "@version{#2023-4-8}
  @argument[surface]{a @class{gdk:surface} object}
  @return{The newly created @class{gdk:vulkan-context} object, of @code{nil}
    on error.}
  @begin{short}
    Creates a new @class{gdk:vulkan-context} object for rendering on
    @arg{surface}.
  @end{short}
  @see-class{gdk:surface}
  @see-class{gdk:vulkan-context}"
  (glib:with-g-error (err)
    (%surface-create-vulkan-context surface err)))

(export 'surface-create-vulkan-context)

;;; ----------------------------------------------------------------------------
;;; gdk_surface_create_cairo_context ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_surface_create_cairo_context" surface-create-cairo-context)
    (g:object cairo-context)
 #+liber-documentation
 "@version{#2023-7-30}
  @argument[surface]{a @class{gdk:surface} object}
  @return{The newly created @class{gdk:cairo-context} object.}
  @begin{short}
    Creates a new @class{gdk:cairo-context} object for rendering on
    @arg{surface}.
  @end{short}
  @see-class{gdk:surface}
  @see-class{gdk:cairo-context}"
  (surface (g:object surface)))

(export 'surface-create-cairo-context)

;;; ----------------------------------------------------------------------------
;;; gdk_surface_create_similar_surface ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_surface_create_similar_surface"
               %surface-create-similar-surface)
    (:pointer (:struct cairo:surface-t))
  (surface (g:object surface))
  (content cairo:content-t)
  (width :int)
  (height :int))

(defun surface-create-similar-surface (surface content width height)
 #+liber-documentation
 "@version{#2024-1-8}
  @argument[surface]{a @class{gdk:surface} object to make the new surface
    similar to}
  @argument[content]{a @symbol{cairo:content-t} value for the content of the
    new surface}
  @argument[width]{an integer with the width of the new surface}
  @argument[height]{an integer with the height of the new surface}
  @begin{return}
    A newly allocated @symbol{cairo:surface-t} instance. The caller owns the
    surface and should call the @fun{cairo:surface-destroy} function when done
    with it. This function always returns a valid pointer, but it will return
    a \"nil\" surface if the surface is in an error state.
  @end{return}
  @begin{short}
    Creates a new surface that is as compatible as possible with the given
    @arg{surface}.
  @end{short}
  For example the new surface will have the same fallback resolution and font
  options as @arg{surface}. Generally, the new surface will also use the same
  backend as @arg{surface}, unless that is not possible for some reason. The
  type of the returned surface may be examined with the
  @fun{cairo:surface-type} function.

  Initially the surface contents are all 0, transparent if contents have
  transparency, black otherwise.
  @begin[Warning]{dictionary}
    The @fun{gdk:surface-create-similar-surface} function is depreacted since
    4.12. Create a suitable Cairo image surface yourself.
  @end{dictionary}
  @see-class{gdk:surface}
  @see-symbol{cairo:surface-t}
  @see-symbol{cairo:content-t}
  @see-function{cairo:surface-destroy}
  @see-function{cairo:surface-type}"
  #+(and gtk-4-12 gtk-warn-deprecated)
  (when gtk-init:*gtk-warn-deprecated*
    (warn "GDK:SURFACE-CREATE-SIMILAR-SURFACE is deprecated since 4.12."))
  (%surface-create-similar-surface surface content width height))

(export 'surface-create-similar-surface)

;;; ----------------------------------------------------------------------------
;;; gdk_surface_queue_render ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_surface_queue_render" surface-queue-render) :void
 #+liber-documentation
 "@version{#2023-4-8}
  @argument[surface]{a @class{gdk:surface} object}
  @begin{short}
    Forces a @code{\"render\"} signal emission for surface to be scheduled.
  @end{short}
  This function is useful for implementations that track invalid regions on
  their own.
  @see-class{gdk:surface}"
  (surface (g:object surface)))

(export 'surface-queue-render)

;;; ----------------------------------------------------------------------------
;;; gdk_surface_request_layout ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_surface_request_layout" surface-request-layout) :void
 #+liber-documentation
 "@version{#2023-4-8}
  @argument[surface]{a @class{gdk:surface} object}
  @begin{short}
    Request a layout phase from the frame clock of the surface.
  @end{short}
  See the @fun{gdk:frame-clock-request-phase} function.
  @see-class{gdk:surface}
  @see-function{gdk:frame-clock-request-phase}"
  (surface (g:object surface)))

(export 'surface-request-layout)

;;; ----------------------------------------------------------------------------
;;; gdk_surface_set_input_region ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_surface_set_input_region" surface-set-input-region) :void
 #+liber-documentation
 "@version{#2023-4-8}
  @argument[surface]{a @class{gdk:surface} object}
  @argument[region]{a @symbol{cairo:region-t} instance with the region of
    @arg{surface} to be reactive}
  @begin{short}
    Apply the region to the surface for the purpose of event handling.
  @end{short}
  Mouse events which happen while the pointer position corresponds to an unset
  bit in the mask will be passed on the surface below @arg{surface}.

  An input region is typically used with RGBA surfaces. The alpha channel of the
  surface defines which pixels are invisible and allows for nicely antialiased
  borders, and the input region controls where the surface is \"clickable\".

  Use the @fun{gdk:display-supports-input-shapes} function to find out if a
  particular backend supports input regions.
  @see-class{gdk:surface}
  @see-symbol{cairo:region-t}
  @see-function{gdk:display-supports-input-shapes}"
  (surface (g:object surface))
  (region (:pointer (:struct cairo:region-t))))

(export 'surface-set-input-region)

;;; ----------------------------------------------------------------------------
;;; gdk_surface_get_device_position ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_surface_get_device_position" %surface-device-position)
    :boolean
  (surface (g:object surface))
  (device (g:object device))
  (x (:pointer :double))
  (y (:pointer :double))
  (mask (:pointer modifier-type)))

(defun surface-device-position (surface device)
 #+liber-documentation
 "@version{#2023-4-8}
  @argument[surface]{a @class{gdk:surface} object}
  @argument[device]{a @class{gdk:device} object}
  @begin{return}
    @arg{x} -- a double float with the x coordinate of @arg{device} @br{}
    @arg{y} -- a double float with the y coordinate of @arg{device} @br{}
    @arg{mask} -- a @symbol{gdk:modifier-type} value
  @end{return}
  @begin{short}
    Obtains the current device position in doubles and modifier state.
  @end{short}
  The position is given in coordinates relative to the upper left corner of
  @arg{surface}. Returns @em{false} if the device is not over the surface.
  @see-class{gdk:surface}
  @see-class{gdk:device}
  @see-symbol{gdk:modifier-type}"
  (cffi:with-foreign-objects ((x :double) (y :double) (mask 'modifier-type))
    (when (%surface-device-position surface device x y mask))))

(export 'surface-device-position)

;;; ----------------------------------------------------------------------------
;;; gdk_surface_get_device_cursor ()
;;; gdk_surface_set_device_cursor ()
;;; ----------------------------------------------------------------------------

(defun (setf surface-device-cursor) (cursor surface device)
  (cffi:foreign-funcall "gdk_surface_set_device_cursor"
                        (g:object surface) surface
                        (g:object device) device
                        (g:object cursor) cursor
                        :void)
  cursor)

(cffi:defcfun ("gdk_surface_get_device_cursor" surface-device-cursor)
    (g:object cursor)
 #+liber-documentation
 "@version{#2023-4-8}
  @syntax[]{(gdk:surface-device-cursor surface device) => cursor}
  @syntax[]{(setf (gdk:surface-device-cursor surface device) cursor)}
  @argument[surface]{a @class{gdk:surface} object}
  @argument[device]{a @class{gdk:device} object}
  @argument[cursor]{a @class{gdk:cursor} object}
  @begin{short}
    The @fun{gdk:surface-device-cursor} function retrieves a @class{gdk:cursor}
    pointer for the device currently set on the specified @arg{surface}, or
    @code{nil}.
  @end{short}
  If the return value is @code{nil} then there is no custom cursor set on the
  specified surface, and it is using the cursor for its parent surface.

  The @setf{gdk:surface-device-cursor} function sets a specific cursor for a
  given device when it gets inside @arg{surface}. Use the
  @fun{gdk:cursor-new-from-name} or @fun{gdk:cursor-new-from-texture} function
  to create the cursor. To make the cursor invisible, use a blank cursor.
  Passing @code{nil} for the @arg{cursor} argument means that @arg{surface} will
  use the cursor of its parent surface. Most surfaces should use this default.
  @see-class{gdk:surface}
  @see-class{gdk:device}
  @see-class{gdk:cursor}
  @see-function{gdk:cursor-new-from-name}
  @see-function{gdk:cursor-new-from-texture}"
  (surface (g:object surface))
  (device (g:object device)))

(export 'surface-device-cursor)

;;; --- End of file gdk4.surface.lisp ------------------------------------------
