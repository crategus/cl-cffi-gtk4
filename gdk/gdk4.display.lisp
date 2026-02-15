;;; ----------------------------------------------------------------------------
;;; gdk4.display.lisp
;;;
;;; The documentation in this file is taken from the GDK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GDK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2026 Dieter Kaiser
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
;;; GdkDisplay
;;;
;;;     Controls a set of monitors and their associated input devices
;;;
;;; Types and Values
;;;
;;;     GdkDisplay
;;;
;;; Accessors
;;;
;;;     gdk_display_get_dmabuf_formats                      Since 4.14
;;;
;;; Functions
;;;
;;;     gdk_display_open
;;;     gdk_display_get_default
;;;     gdk_display_get_name
;;;     gdk_display_device_is_grabbed
;;;     gdk_display_beep
;;;     gdk_display_sync
;;;     gdk_display_flush
;;;     gdk_display_close
;;;     gdk_display_is_closed
;;;     gdk_display_is_rgba
;;;     gdk_display_is_composited
;;;     gdk_display_supports_input_shapes
;;;     gdk_display_supports_shadow_width                   Since 4.14
;;;     gdk_display_get_app_launch_context
;;;     gdk_display_notify_startup_complete                 Deprecated 4.10
;;;     gdk_display_get_default_seat
;;;     gdk_display_list_seats
;;;     gdk_display_get_monitors
;;;     gdk_display_get_monitor_at_surface
;;;     gdk_display_get_clipboard
;;;     gdk_display_get_primary_clipboard
;;;     gdk_display_get_setting
;;;     gdk_display_get_startup_notification_id             Deprecated 4.10
;;;     gdk_display_put_event                               Deprecated 4.10
;;;     gdk_display_map_keyval
;;;     gdk_display_map_keycode
;;;     gdk_display_translate_key
;;;     gdk_display_prepare_gl                              Since 4.4
;;;     gdk_display_create_gl_context                       Since 4.6
;;;
;;; Properties
;;;
;;;     composited
;;;     dmabuf-formats                                      Since 4.14
;;;     input-shapes
;;;     rgba
;;;     shadow-width                                        Since 4.14
;;;
;;; Signals
;;;
;;;     closed
;;;     opened
;;;     seat-added
;;;     seat-removed
;;;     setting-changed
;;;
;;; Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkDisplay
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkDisplay
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkDisplay" display
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_display_get_type")
  ((composited
    display-composited
    "composited" "gboolean" t nil)
   #+gtk-4-14
   (dmabuf-formats
    display-dmabuf-formats
    "dmabuf-formats" "GdkDmabufFormats" t nil)
   (input-shapes
    display-input-shapes
    "input-shapes" "gboolean" t nil)
   (rgba
    display-rgba
    "rgba" "gboolean" t nil)
   #+gtk-4-14
   (shadow-width
    display-shadow-width
    "shadow-width" "gboolean" t nil)))

#+liber-documentation
(setf (documentation 'display 'type)
 "@version{2025-08-04}
  @begin{short}
    The @class{gdk:display} object is the GDK representation of a workstation.
  @end{short}
  The purpose is two fold:
  @begin{itemize}
    @item{To manage and provide information about input devices, for example,
      pointers and keyboards.}
    @item{To manage and provide information about output devices, for example,
      monitors and projectors.}
  @end{itemize}
  Most of the input device handling has been factored out into separate
  @class{gdk:seat} objects. Every display has one or more seats, which can be
  accessed with the @fun{gdk:display-default-seat} and
  @fun{gdk:display-list-seats} functions.

  Output devices are represented by @class{gdk:monitor} objects, which can be
  accessed with the @fun{gdk:display-monitor-at-surface} function and similar
  APIs.
  @begin[Signal Details]{dictionary}
    @begin[display::closed]{signal}
      @begin{pre}
lambda (display is-error)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[display]{The @class{gdk:display} object on which the signal is
          emitted.}
       @entry[is-error]{The boolean that is @em{true} if @arg{display} was
         closed due to an error.}
      @end{simple-table}
      Emitted when the connection to the windowing system for @arg{display} is
      closed.
    @end{signal}
    @begin[display::opened]{signal}
      @begin{pre}
lambda (display)   :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[display]{The @class{gdk:display} object on which the signal is
          emitted.}
      @end{simple-table}
      Emitted when the connection to the windowing system for @arg{display} is
      opened.
    @end{signal}
    @begin[display::seat-added]{signal}
      @begin{pre}
lambda (display seat)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[display]{The @class{gdk:display} object on which the signal is
          emitted.}
        @entry[seat]{The @class{gdk:seat} object that was just added.}
      @end{simple-table}
      Emitted whenever a new seat is made known to the windowing system.
    @end{signal}
    @begin[display::seat-removed]{signal}
      @begin{pre}
lambda (display seat)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[display]{The @class{gdk:display} object on which the signal is
          emitted.}
        @entry[seat]{The @class{gdk:seat} object that was just removed.}
      @end{simple-table}
      Emitted whenever a seat is removed by the windowing system.
    @end{signal}
    @begin[display::setting-changed]{signal}
      @begin{pre}
lambda (display setting)    :run-last
      @end{pre}
      @begin[code]{simple-table}
        @entry[display]{The @class{gdk:display} object on which the signal is
          emitted.}
        @entry[setting]{The string for the name of the setting that changed.}
      @end{simple-table}
    @end{signal}
  @end{dictionary}
  @see-slot{gdk:display-composited}
  @see-slot{gdk:display-dmabuf-formats}
  @see-slot{gdk:display-input-shapes}
  @see-slot{gdk:display-rgba}
  @see-slot{gdk:display-shadow-width}
  @see-class{gdk:seat}
  @see-class{gdk:monitor}
  @see-function{gdk:display-default-seat}
  @see-function{gdk:display-list-seats}
  @see-function{gdk:display-monitor-at-surface}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk:display-composited -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "composited" 'display) t)
 "The @code{composited} property of type @code{:boolean} (Read) @br{}
  @em{True} if the display properly composites the alpha channel.")

#+liber-documentation
(setf (liber:alias-for-function 'display-composited)
      "Accessor"
      (documentation 'display-composited 'function)
 "@version{2025-08-31}
  @syntax{(gdk:display-composited object) => composited}
  @argument[object]{a @class{gdk:display} object}
  @argument[composited]{@em{true} if the display properly composites the alpha
    channel}
  @begin{short}
    The accessor for the @slot[gdk:display]{composited} slot of the
    @class{gdk:display} class returns whether surfaces can reasonably be
    expected to have their alpha channel drawn correctly on the screen.
  @end{short}
  Check with the @fun{gdk:display-is-rgba} function for whether the display
  supports an alpha channel.

  On X11 this function returns whether a compositing manager is compositing on
  display. On modern displays, this value is always @em{true}.
  @see-class{gdk:display}
  @see-function{gdk:display-is-rgba}")

;;; --- gdk:display-dmabuf-formats ---------------------------------------------

#+(and gtk-4-14 liber-documentation)
(setf (documentation (liber:slot-documentation "dmabuf-formats" 'display) t)
 "The @code{dmabuf-formats} property of type @class{gdk:dmabuf-formats} (Read)
  @br{}
  The DmaBuf formats that are supported on the display. Since 4.14")

#+(and gtk-4-14 liber-documentation)
(setf (liber:alias-for-function 'display-dmabuf-formats)
      "Accessor"
      (documentation 'display-dmabuf-formats 'function)
 "@version{2025-08-31}
  @syntax{(gdk:display-dmabuf-formats object) => formats}
  @argument[object]{a @class{gdk:display} object}
  @argument[formats]{a @class{gdk:dmabuf-formats} instance}
  @begin{short}
    The accessor for the @slot[gdk:display]{dmabuf-formats} slot returns the
    dma-buf formats that are supported on the display.
  @end{short}
  GTK may use OpenGL or Vulkan to support some formats. Calling this function
  will then initialize them if they are not yet.

  The formats returned by this function can be used for negotiating buffer
  formats with producers such as @code{v4l}, @code{pipewire} or
  @code{GStreamer}. To learn more about dma-buf formats, see the
  @class{gdk:dmabuf-texture-builder} documentation.

  Since 4.14
  @see-class{gdk:display}
  @see-class{gdk:dmabuf-formats}
  @see-class{gdk:dmabuf-texture-builder}")

;;; --- gdk:display-input-shapes -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "input-shapes" 'display) t)
 "The @code{input-shapes} property of type @code{:boolean} (Read) @br{}
  @em{True} if the display supports input shapes.")

#+liber-documentation
(setf (liber:alias-for-function 'display-input-shapes)
      "Accessor"
      (documentation 'display-input-shapes 'function)
 "@version{2025-08-31}
  @syntax{(gdk:display-input-shapes object) => setting}
  @argument[object]{a @class{gdk:display} object}
  @argument[setting]{@em{true} if the display supports input shapes}
  @begin{short}
    The accessor for the @slot[gdk:display]{input-shapes} slot of the
    @class{gdk:display} class returns @em{true} if the display supports input
    shapes.
  @end{short}
  This means that the @fun{gdk:surface-set-input-region} function can be used
  to modify the input shape of surfaces on the display.

  On modern displays, this value is always @em{true}.
  @see-class{gdk:display}
  @see-function{gdk:surface-set-input-region}")

;;; --- gdk:display-rgba -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "rgba" 'display) t)
 "The @code{rgba} property of type @code{:boolean} (Read) @br{}
  @em{True} if the display supports an alpha channel.")

#+liber-documentation
(setf (liber:alias-for-function 'display-rgba)
      "Accessor"
      (documentation 'display-rgba 'function)
 "@version{2025-08-31}
  @syntax{(gdk:display-rgba object) => setting}
  @argument[object]{a @class{gdk:display} object}
  @argument[setting]{@em{true} if the display supports an alpha channel}
  @begin{short}
    The accessor for the @slot[gdk:display]{rgba} slot of the
    @class{gdk:display} class returns whether surfaces on the display are
    created with an alpha channel.
  @end{short}

  Even if a @em{true} is returned, it is possible that the alpha channel of the
  surface will not be honored when displaying the surface on the screen. In
  particular, for X an appropriate windowing manager and compositing manager
  must be running to provide appropriate display. Use the
  @fun{gdk:display-is-composited} function to check if that is the case.

  On modern displays, this value is always @em{true}.
  @see-class{gdk:display}
  @see-function{gdk:display-is-composited}")

;;; --- gdk:display-shadow-width -----------------------------------------------

#+(and gtk-4-14 liber-documentation)
(setf (documentation (liber:slot-documentation "shadow-width" 'display) t)
 "The @code{shadow-width} property of type @code{:boolean} (Read) @br{}
  @em{True} if the display supports extensible frames. Since 4.14")

#+(and gtk-4-14 liber-documentation)
(setf (liber:alias-for-function 'display-shadow-width)
      "Accessor"
      (documentation 'display-shadow-width 'function)
 "@version{2025-08-31}
  @syntax{(gdk:display-shadow-width object) => setting}
  @argument[object]{a @class{gdk:display} object}
  @argument[setting]{@em{true} if surfaces can draw shadows or @em{false} if
    the display does not support this functionality}
  @begin{short}
    The accessor for the @slot[gdk:display]{shadow-width} slot returns whether
    it is possible for a surface to draw outside of the window area.
  @end{short}
  If @em{true} is returned the application decides if it wants to draw shadows.
  If @em{false} is returned, the compositor decides if it wants to draw shadows.

  Since 4.14
  @see-class{gdk:display}")

;;; ----------------------------------------------------------------------------
;;; gdk_display_open
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_open" display-open) (g:object display)
 #+liber-documentation
 "@version{2025-05-01}
  @argument[name]{a string for the name of the display to open}
  @begin{return}
    The @class{gdk:display} object, or @code{nil} if the display could not be
    opened.
  @end{return}
  @begin{short}
    Opens a display named by @arg{name}.
  @end{short}
  If opening the display fails, @code{nil} is returned.
  @see-class{gdk:display}"
  (name :string))

(export 'display-open)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_default
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_get_default" display-default) (g:object display)
 #+liber-documentation
 "@version{2025-05-01}
  @begin{return}
    The @class{gdk:display} object, or @code{nil} if there is no default
    display.
  @end{return}
  @begin{short}
    Gets the default display.
  @end{short}
  This is a convenience function for the call:
  @begin{pre}
(gdk:display-manager-default-display (gdk:display-manager-get))
  @end{pre}
  @begin[Examples]{dictionary}
    @begin{pre}
(gdk:display-default)
=> #<GDK:DISPLAY {1007501013@}>
(eq (gdk:display-default)
    (gdk:display-manager-default-display (gdk:display-manager-get)))
=> T
    @end{pre}
  @end{dictionary}
  @see-class{gdk:display}
  @see-function{gdk:display-manager-get}
  @see-function{gdk:display-manager-default-display}")

(export 'display-default)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_name
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_get_name" display-name) :string
 #+liber-documentation
 "@version{2025-05-01}
  @argument[display]{a @class{gdk:display} object}
  @return{The string representing the display name.}
  @short{Gets the name of the display.}
  @see-class{gdk:display}"
  (display (g:object display)))

(export 'display-name)

;;; ----------------------------------------------------------------------------
;;; gdk_display_device_is_grabbed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_device_is_grabbed" display-device-is-grabbed)
    :boolean
 #+liber-documentation
 "@version{2025-05-01}
  @argument[display]{a @class{gdk:display} object}
  @argument[device]{a @class{gdk:device} object}
  @begin{return}
    The boolean that is @em{true} if there is a grab in effect for @arg{device}.
  @end{return}
  @begin{short}
    Returns @em{true} if there is an ongoing grab on the device for the
    display.
  @end{short}
  @see-class{gdk:display}
  @see-class{gdk:device}"
  (display (g:object display))
  (device (g:object device)))

(export 'display-device-is-grabbed)

;;; ----------------------------------------------------------------------------
;;; gdk_display_beep
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_beep" display-beep) :void
 #+liber-documentation
 "@version{2025-05-01}
  @argument[display]{a @class{gdk:display} object}
  @short{Emits a short beep on the display.}
  @see-class{gdk:display}"
  (display (g:object display)))

(export 'display-beep)

;;; ----------------------------------------------------------------------------
;;; gdk_display_sync
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_sync" display-sync) :void
 #+liber-documentation
 "@version{2025-05-01}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    Flushes any requests queued for the windowing system and waits until all
    requests have been handled.
  @end{short}
  This is often used for making sure that the display is synchronized with the
  current state of the program. Calling the @fun{gdk:display-sync} function
  before the @code{gdk_x11_display_error_trap_pop()} function makes sure that
  any errors generated from earlier requests are handled before the error trap
  is removed.

  This is most useful for X11. On windowing systems where requests are handled
  synchronously, this function will do nothing.
  @see-class{gdk:display}"
  (display (g:object display)))

(export 'display-sync)

;;; ----------------------------------------------------------------------------
;;; gdk_display_flush
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_flush" display-flush) :void
 #+liber-documentation
 "@version{2025-05-01}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    Flushes any requests queued for the windowing system.
  @end{short}
  This happens automatically when the main loop blocks waiting for new events,
  but if your application is drawing without returning control to the main
  loop, you may need to call this function explicitely. A common case where
  this function needs to be called is when an application is executing drawing
  commands from a thread other than the thread where the main loop is running.

  This is most useful for X11. On windowing systems where requests are handled
  synchronously, this function will do nothing.
  @see-class{gdk:display}"
  (display (g:object display)))

(export 'display-flush)

;;; ----------------------------------------------------------------------------
;;; gdk_display_close
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_close" display-close) :void
 #+liber-documentation
 "@version{#2025-05-01}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    Closes the connection to the windowing system for the given display.
  @end{short}
  This cleans up associated resources.
  @see-class{gdk:display}"
  (display (g:object display)))

(export 'display-close)

;;; ----------------------------------------------------------------------------
;;; gdk_display_is_closed
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_is_closed" display-is-closed) :boolean
 #+liber-documentation
 "@version{2025-05-01}
  @argument[display]{a @class{gdk:display} object}
  @return{@em{True} if @arg{display} is closed.}
  @short{Finds out if the display has been closed.}
  @see-class{gdk:display}"
  (display (g:object display)))

(export 'display-is-closed)

;;; ----------------------------------------------------------------------------
;;; gdk_display_is_rgba
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_is_rgba" display-is-rgba) :boolean
 #+liber-documentation
 "@version{2025-05-01}
  @argument[display]{a @class{gdk:display} object}
  @return{@em{True} if the display supports an alpha channel.}
  @begin{short}
    Returns whether surfaces on this display are created with an alpha channel.
  @end{short}

  Even if a @em{true} value is returned, it is possible that the alpha channel
  of the surface will not be honored when displaying the surface on the screen.
  In particular, for X an appropriate windowing manager and compositing manager
  must be running to provide appropriate display. Use the
  @fun{gdk:display-is-composited} function to check if that is the case. On
  modern displays, this value is always @em{true}.
  @see-class{gdk:display}
  @see-function{gdk:display-is-composited}"
  (display (g:object display)))

(export 'display-is-rgba)

;;; ----------------------------------------------------------------------------
;;; gdk_display_is_composited
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_is_composited" display-is-composited) :boolean
 #+liber-documentation
 "@version{2025-05-01}
  @argument[display]{a @class{gdk:display} object}
  @return{Whether surfaces with RGBA visuals supports an alpha channel.}
  @begin{short}
    Returns whether surfaces can reasonably be expected to have their alpha
    channel drawn correctly on the screen.
  @end{short}

  Check the value of the @fun{gdk:display-is-rgba} function for whether the
  display supports an alpha channel. On X11 this function returns whether a
  compositing manager is compositing on display. On modern displays, this value
  is always @em{true}.
  @see-class{gdk:display}"
  (display (g:object display)))

(export 'display-is-composited)

;;; ----------------------------------------------------------------------------
;;; gdk_display_supports_input_shapes
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_supports_input_shapes"
               display-supports-input-shapes) :boolean
 #+liber-documentation
 "@version{2025-05-01}
  @argument[display]{a @class{gdk:display} object}
  @return{@em{True} if surfaces with modified input shape are supported.}
  @begin{short}
    Returns @em{true} if the display supports input shapes.
  @end{short}
  This means that the @fun{gdk:surface-set-input-region} function can be used to
  modify the input shape of surfaces on the display. On modern displays, this
  value is always @em{true}.
  @see-class{gdk:display}
  @see-function{gdk:surface-set-input-region}"
  (display (g:object display)))

(export 'display-supports-input-shapes)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_app_launch_context
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_get_app_launch_context" display-app-launch-context)
    (g:object app-launch-context :return)
 #+liber-documentation
 "@version{2025-05-01}
  @argument[display]{a @class{gdk:display} object}
  @begin{return}
    The new @class{gdk:app-launch-context} object for @arg{display}.
  @end{return}
  @begin{short}
    Returns a @class{gdk:app-launch-context} object suitable for launching
    applications on the given display.
  @end{short}
  @see-class{gdk:display}
  @see-class{gdk:app-launch-context}"
  (display (g:object display)))

(export 'display-app-launch-context)

;;; ----------------------------------------------------------------------------
;;; gdk_display_notify_startup_complete                     Deprecated 4.10
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_notify_startup_complete"
               %display-notify-startup-complete) :void
  (display (g:object display))
  (startup :string))

(defun display-notify-startup-complete (display startup)
 #+liber-documentation
 "@version{#2025-05-01}
  @argument[display]{a @class{gdk:display} object}
  @argument[startup]{a string for a startup notification identifier, for which
    notification process should be completed}
  @begin{short}
    Indicates to the GUI environment that the application has finished loading,
    using a given identifier.
  @end{short}
  GTK will call this function automatically for @class{gtk:window} widgets
  with a custom startup notification identifier unless the
  @fun{gtk:window-set-auto-startup-notification} function is called to disable
  that feature.
  @begin[Warning]{dictionary}
    The @fun{gdk:display-notify-startup-complete} function is deprecated since
    4.10. Using the @fun{gdk:toplevel-startup-id} function is sufficient.
  @end{dictionary}
  @see-class{gdk:display}
  @see-class{gtk:window}
  @see-function{gdk:toplevel-startup-id}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*warn-deprecated*
    (warn "GDK:DISPLAY-NOTIFY-STARTUP-COMPLETE is deprecated since 4.10."))
  (%display-notify-startup-complete display startup))

(export 'display-notify-startup-complete)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_default_seat
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_get_default_seat"
               display-default-seat) (g:object seat)
 #+liber-documentation
 "@version{2025-05-01}
  @argument[display]{a @class{gdk:display} object}
  @return{The default @class{gdk:seat} object.}
  @begin{short}
    Returns the default seat object for this display.
  @end{short}
  Note that a display may not have a seat. In this case, this function will
  return @code{nil}.
  @see-class{gdk:display}
  @see-class{gdk:seat}"
  (display (g:object display)))

(export 'display-default-seat)

;;; ----------------------------------------------------------------------------
;;; gdk_display_list_seats
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_list_seats" display-list-seats)
    (g:list-t (g:object seat))
 #+liber-documentation
 "@version{2025-05-01}
  @argument[display]{a @class{gdk:display} object}
  @return{The list of @class{gdk:seat} objects known to @arg{display}.}
  @begin{short}
    Returns the list of seats known to the display.
  @end{short}
  @see-class{gdk:display}
  @see-class{gdk:seat}"
  (display (g:object display)))

(export 'display-list-seats)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_monitors
;;; ----------------------------------------------------------------------------

;; TODO: Consider to export the CREATE-GOBJECT-FROM-POINTER function

(cffi:defcfun ("gdk_display_get_monitors" %display-monitors)
    (g:object g:list-model)
  (display (g:object display)))

(defun display-monitors (display)
 #+liber-documentation
 "@version{2025-05-01}
  @argument[display]{a @class{gdk:display} object}
  @return{The list of @class{gdk:monitor} objects.}
  @begin{short}
    Gets the list of monitors associated with the display.
  @end{short}
  Subsequent calls to this function will always return the same list for the
  same display.
  @begin[Lisp implementation]{dictionary}
    The C code returns a @class{g:list-model} instance with pointers to the
    monitors. For the Lisp implementation, the list is converted to a Lisp
    list with @class{gdk:monitor} objects.
  @end{dictionary}
  @see-class{gdk:display}
  @see-class{gdk:monitor}
  @see-class{g:list-model}"
  (let ((monitors (%display-monitors display)))
    (iter (for i from 0 below (g:list-model-n-items monitors))
          (collect (g:list-model-item monitors i)))))

(export 'display-monitors)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_monitor_at_surface
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_get_monitor_at_surface" display-monitor-at-surface)
    (g:object monitor)
 #+liber-documentation
 "@version{#2025-08-04}
  @argument[display]{a @class{gdk:display} object}
  @argument[surface]{a @class{gdk:surface} object}
  @begin{return}
    The @class{gdk:monitor} object for the largest overlap with @arg{surface}.
  @end{return}
  @begin{short}
    Gets the monitor in which the largest area of the surface resides.
  @end{short}
  Returns a monitor close to surface if it is outside of all monitors.
  @see-class{gdk:display}
  @see-class{gdk:surface}
  @see-class{gdk:monitor}"
  (display (g:object display))
  (surface (g:object surface)))

(export 'display-monitor-at-surface)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_clipboard
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_get_clipboard" display-clipboard)
    (g:object clipboard)
 #+liber-documentation
 "@version{2025-05-01}
  @argument[display]{a @class{gdk:display} object}
  @return{The @class{gdk:clipboard} object.}
  @short{Gets the clipboard used for copy/paste operations.}
  @see-class{gdk:display}
  @see-class{gdk:clipboard}"
  (display (g:object display)))

(export 'display-clipboard)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_primary_clipboard
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_get_primary_clipboard" display-primary-clipboard)
    (g:object clipboard)
 #+liber-documentation
 "@version{2025-05-01}
  @argument[display]{a @class{gdk:display} object}
  @return{The @class{gdk:clipboard} object.}
  @begin{short}
    Gets the clipboard used for the primary selection.
  @end{short}
  On backends where the primary clipboard is not supported natively, GDK
  emulates this clipboard locally.
  @see-class{gdk:display}
  @see-class{gdk:clipboard}"
  (display (g:object display)))

(export 'display-primary-clipboard)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_setting
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_get_setting" %display-setting) :boolean
  (display (g:object display))
  (name :string)
  (gvalue (:pointer (:struct g:value))))

(defun display-setting (display name gtype)
 #+liber-documentation
 "@version{2025-05-01}
  @argument[display]{a @class{gdk:display} object}
  @argument[name]{a string for the name of the setting}
  @argument[gtype]{a @class{g:type-t} type ID for the type of the setting}
  @return{The value of the setting or @code{nil} if the setting does not exist.}
  @begin{short}
    Retrieves a desktop-wide setting such as double-click time for the display.
  @end{short}
  @begin[Examples]{dictionary}
    @begin{pre}
(let ((display (gdk:display-default)))
  (gdk:display-setting display \"gtk-double-click-time\" \"gint\"))
=> 400
    @end{pre}
  @end{dictionary}
  @see-class{gdk:display}
  @see-symbol{g:type-t}"
  (gobject:with-value (gvalue gtype)
    (when (%display-setting display name gvalue)
      (g:value-get gvalue))))

(export 'display-setting)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_startup_notification_id                 Deprecated 4.10
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_get_startup_notification_id"
               %display-startup-notification-id) :string
  (display (g:object display)))

(defun display-startup-notification-id (display)
 #+liber-documentation
 "@version{2025-08-04}
  @argument[display]{a @class{gdk:display} object}
  @return{The string for the startup notification ID for @arg{display}.}
  @begin{short}
    Gets the startup notification ID for a Wayland display, or @code{nil} if no
    ID has been defined.
  @end{short}
  @begin[Warning]{dictionary}
    The @fun{gdk:display-startup-notification-id} function is deprecated since
    4.10. Do not use it in newly written code.
  @end{dictionary}
  @see-class{gdk:display}"
  #+(and gtk-4-10 gtk-warn-deprecated)
  (when gtk-init:*warn-deprecated*
    (warn "GDK:DISPLAY-STARTUP-NOTIFICATION-ID is deprecated since 4.10."))
  (%display-startup-notification-id display))

(export 'display-startup-notification-id)

;;; ----------------------------------------------------------------------------
;;; gdk_display_put_event                                   Deprecated 4.10
;;; ----------------------------------------------------------------------------

;; not implemented

;;; ----------------------------------------------------------------------------
;;; gdk_display_map_keyval
;;; ----------------------------------------------------------------------------

(cffi:defcstruct %keymap-key
  (keycode :uint)
  (group :int)
  (level :int))

(cffi:defcfun ("gdk_display_map_keyval" %display-map-keyval) :boolean
  (display (g:object display))
  (keyval :uint)
  (keys :pointer)
  (n-keys (:pointer :int)))

(defun display-map-keyval (display keyval)
 #+liber-documentation
 "@version{2025-08-04}
  @argument[display]{a @class{gdk:display} object}
  @argument[keyval]{an unsigned integer for the keyval}
  @begin{return}
    The list of integer for the grouped keycode/group/level combinations,
    or @code{nil}.
  @end{return}
  @begin{short}
    Obtains a list of keycode/group/level combinations that will generate
    @arg{keyval}.
  @end{short}
  Groups and levels are two kinds of keyboard mode. In general, the level
  determines whether the top or bottom symbol on a key is used, and the group
  determines whether the left or right symbol is used.

  On US keyboards, the shift key changes the keyboard level, and there are no
  groups. A group switch key might convert a keyboard between Hebrew to English
  modes, for example.
  @begin[Examples]{dictionary}
    @begin{pre}
(gdk:display-map-keyval (gdk:display-default) (char-code #\a))
=> ((65 0 0) (65 0 3) (65 0 15) (65 0 2))
    @end{pre}
  @end{dictionary}
  @see-class{gdk:display}"
  (cffi:with-foreign-objects ((keys-ptr :pointer) (n-keys-ptr :int))
    (when (%display-map-keyval display keyval keys-ptr n-keys-ptr)
      (let ((keys (cffi:mem-ref keys-ptr :pointer))
            (n-keys (cffi:mem-ref n-keys-ptr :int)))
        (iter (for i from 0 below n-keys)
              (for key = (cffi:mem-aptr keys '(:struct %keymap-key) i))
              (collect (cffi:with-foreign-slots ((keycode group level)
                                                 key
                                                 (:struct %keymap-key))
                         (list keycode group level)))
              (finally (g:free keys)))))))

(export 'display-map-keyval)

;;; ----------------------------------------------------------------------------
;;; gdk_display_map_keycode
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_map_keycode" %display-map-keycode) :boolean
  (display (g:object display))
  (keycode :uint)
  (keys :pointer)
  (keyvals (:pointer :uint))
  (n-entries (:pointer :int)))

(defun display-map-keycode (display keycode)
 #+liber-documentation
 "@version{2025-05-01}
  @argument[display]{a @class{gdk:display} object}
  @argument[keycode]{an unsigned integer for a keycode}
  @begin{return}
    The list of keyvals with the corresponding key of the form
    (keyval keycode group level).
  @end{return}
  @begin{short}
    Returns the keyvals bound to @arg{keycode}.
  @end{short}
  When a keycode is pressed by the user, the keyval from this list of entries
  is selected by considering the effective keyboard group and level.
  @see-class{gdk:display}"
  (cffi:with-foreign-objects ((keys-ptr :pointer)
                              (keyvals-ptr :pointer)
                              (n-entries-ptr :int))
    (when (%display-map-keycode display
                                keycode
                                keys-ptr
                                keyvals-ptr
                                n-entries-ptr)
      (let ((keys (cffi:mem-ref keys-ptr :pointer))
            (keyvals (cffi:mem-ref keyvals-ptr :pointer))
            (n-entries (cffi:mem-ref n-entries-ptr :int)))
        (iter (for i from 0 below n-entries)
              (for keyval = (cffi:mem-aref keyvals :uint i))
              (for key = (cffi:mem-aptr keys '(:struct %keymap-key) i))
              (collect (cffi:with-foreign-slots ((keycode group level)
                                                 key
                                                 (:struct %keymap-key))
                         (list keyval keycode group level)))
              (finally (g:free keys)
                       (g:free keyvals)))))))

(export 'display-map-keycode)

;;; ----------------------------------------------------------------------------
;;; gdk_display_translate_key
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_display_translate_key" %display-translate-key) :boolean
  (display (g:object display))
  (keycode :uint)
  (state modifier-type)
  (group :int)
  (keyval (:pointer :uint))
  (effective (:pointer :int))
  (level (:pointer :int))
  (consumed (:pointer modifier-type)))

(defun display-translate-key (display keycode state group)
 #+liber-documentation
 "@version{2025-07-31}
  @syntax{(gdk:display-translate-key display keycode state group) =>
    keyval, effective, level, consumed}
  @argument[display]{a @class{gdk:display} object}
  @argument[keycode]{an unsigned integer for the keycode}
  @argument[state]{a @sym{gdk:modifier-type} state}
  @argument[group]{an integer for the active keyboard group}
  @argument[keyval]{an unsigned integer for the keyval}
  @argument[effective]{an integer for the effective group}
  @argument[level]{an integer for the level}
  @argument[consumend]{a @sym{gdk:modifier-type} value for the modifiers that
    were used to determine the group or level}
  @begin{short}
    Translates a keycode into a keyval, effective group, and level.
  @end{short}
  Modifiers that affected the translation and are thus unavailable for
  application use are returned in @arg{consumed}.

  The @arg{effective} value is the group that was actually used for the
  translation. Some keys such as @kbd{Enter} are not affected by the active
  keyboard group. The @arg{level} value is derived from @arg{state}.

  The @arg{consumed} value gives modifiers that should be masked out from state
  when comparing this key press to a keyboard shortcut. For instance, on a US
  keyboard, the @kbd{plus} symbol is shifted, so when comparing a key press to
  a @kbd{<Control>plus} accelerator @kbd{<Shift>} should be masked out.
  @see-class{gdk:display}
  @see-symbol{gdk:modifier-type}"
  (cffi:with-foreign-objects ((keyval :uint)
                              (effective :int)
                              (level :int)
                              (consumed 'modifier-type))
    (when (%display-translate-key display
                                  keycode
                                  state
                                  group
                                  keyval
                                  effective
                                  level
                                  consumed)
      (values (cffi:mem-ref keyval :uint)
              (cffi:mem-ref effective :int)
              (cffi:mem-ref level :int)
              (cffi:mem-ref consumed 'modifier-type)))))

(export 'display-translate-key)

;;; ----------------------------------------------------------------------------
;;; gdk_display_prepare_gl
;;; ----------------------------------------------------------------------------

#+gtk-4-4
(cffi:defcfun ("gdk_display_prepare_gl" %display-prepare-gl) :boolean
  (display (g:object display))
  (err :pointer))

#+gtk-4-4
(defun display-prepare-gl (display)
 #+liber-documentation
 "@version{#2025-05-01}
  @argument[display]{a @class{gdk:display} object}
  @return{@em{True} if the display supports OpenGL.}
  @begin{short}
    Checks that OpenGL is available for @arg{display} and ensures that it is
    properly initialized.
  @end{short}
  When this fails, an error will be set describing the error and this function
  returns @em{false}.

  Note that even if this function succeeds, creating a @class{gdk:gl-context}
  object may still fail. This function is idempotent. Calling it multiple times
  will just return the same value or error.

  You never need to call this function, GDK will call it automatically as
  needed. But you can use it as a check when setting up code that might make
  use of OpenGL.

  Since 4.4
  @see-class{gdk:display}
  @see-class{gdk:gl-context}"
  (glib:with-error (err)
    (%display-prepare-gl display err)))

#+gtk-4-4
(export 'display-prepare-gl)

;;; ----------------------------------------------------------------------------
;;; gdk_display_create_gl_context
;;; ----------------------------------------------------------------------------

#+gtk-4-6
(cffi:defcfun ("gdk_display_create_gl_context" %display-create-gl-context)
    (g:object gl-context)
  (display (g:object display))
  (err :pointer))

#+gtk-4-6
(defun display-create-gl-context (display)
 #+liber-documentation
 "@version{#2025-05-01}
  @argument[display]{a @class{gdk:display} object}
  @return{The newly created @class{gdk:gl-context} object.}
  @begin{short}
    Creates a new OpenGL context for the display.
  @end{short}
  The context is disconnected from any particular surface and cannot be used to
  draw to any surface. It can only be used to draw to non-surface framebuffers
  like textures.

  If the creation of the OpenGL context failed, an error will be set. Before
  using the returned OpenGL context, you will need to call the
  @fun{gdk:gl-context-make-current} or @fun{gdk:gl-context-realize} functions.

  Since 4.6
  @see-class{gdk:display}
  @see-class{gdk:gl-context}
  @see-function{gdk:gl-context-make-current}
  @see-function{gdk:gl-context-realize}"
  (glib:with-error (err)
    (%display-create-gl-context display err)))

#+gtk-4-6
(export 'display-create-gl-context)

;;; --- End of file gdk4.display.lisp ------------------------------------------
