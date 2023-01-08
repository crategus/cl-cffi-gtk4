;;; ----------------------------------------------------------------------------
;;; gdk.display.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.0 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
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
;;; GdkDisplay
;;;
;;;     Controls a set of monitors and their associated input devices
;;;
;;; Types and Values
;;;
;;;     GdkDisplay
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
;;;     gdk_display_get_app_launch_context
;;;     gdk_display_notify_startup_complete
;;;     gdk_display_get_default_seat
;;;     gdk_display_list_seats
;;;     gdk_display_get_monitors
;;;     gdk_display_get_monitor_at_surface
;;;     gdk_display_get_clipboard
;;;     gdk_display_get_primary_clipboard
;;;     gdk_display_get_setting
;;;     gdk_display_get_startup_notification_id
;;;     gdk_display_put_event
;;;     gdk_display_map_keyval
;;;     gdk_display_map_keycode
;;;     gdk_display_translate_key
;;;     gdk_display_prepare_gl                             Since 4.4
;;;     gdk_display_create_gl_context                      Since 4.6
;;;
;;; Properties
;;;
;;;     composited
;;;     input-shapes
;;;     rgba
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

(define-g-object-class "GdkDisplay" display
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_display_get_type")
  ((composited
    display-composited
    "composited" "gboolean" t nil)
   (input-shapes
    display-input-shapes
    "input-shapes" "gboolean" t nil)
   (rgba
    display-rgba
    "rgba" "gboolean" t nil)))

#+liber-documentation
(setf (documentation 'display 'type)
 "@version{#2022-1-8}
  @begin{short}
    The @sym{gdk:display} object is the GDK representation of a workstation.
  @end{short}
  The purpose is two fold:
  @begin{itemize}
    @item{To manage and provide information about input devices, e.g. pointers
      and keyboards.}
    @item{To manage and provide information about output devices, e.g. monitors
      and projectors.}
  @end{itemize}
  Most of the input device handling has been factored out into separate
  @class{gdk:seat} objects. Every display has one or more seats, which can be
  accessed with the @fun{gdk:display-default-seat} and
  @fun{gdk:display-list-seats} functions.

  Output devices are represented by @class{gdk:monitor} objects, which can be
  accessed with the @fun{gdk:display-monitor-at-surface} function and similar
  APIs.
  @begin[Signal Details]{dictionary}
    @subheading{The \"closed\" signal}
      @begin{pre}
lambda (display is-error)    :run-last
      @end{pre}
      Emitted when the connection to the windowing system for @arg{display} is
      closed.
      @begin[code]{table}
        @entry[display]{The @sym{gdk:display} object on which the signal is
          emitted.}
       @entry[is-error]{A boolean that is @em{true} if @arg{display} was closed
         due to an error.}
      @end{table}
    @subheading{The \"opened\" signal}
      @begin{pre}
lambda (display)   :run-last
      @end{pre}
      Emitted when the connection to the windowing system for @arg{display} is
      opened.
      @begin[code]{table}
        @entry[display]{The @sym{gdk:display} object on which the signal is
          emitted.}
      @end{table}
    @subheading{The \"seat-added\" signal}
      @begin{pre}
lambda (display seat)    :run-last
      @end{pre}
      Emitted whenever a new seat is made known to the windowing system.
      @begin[code]{table}
        @entry[display]{The @sym{gdk:display} object on which the signal is
          emitted.}
        @entry[seat]{The @class{gdk:seat} object that was just added.}
      @end{table}
    @subheading{The \"seat-removed\" signal}
      @begin{pre}
lambda (display seat)    :run-last
      @end{pre}
      Emitted whenever a seat is removed by the windowing system.
      @begin[code]{table}
        @entry[display]{The @sym{gdk:display} object on which the signal is
          emitted.}
        @entry[seat]{The @class{gdk:seat} object that was just removed.}
      @end{table}
  @end{dictionary}
  @subheading{The \"setting-changed\" signal}
    @begin{pre}
lambda (display setting)    :run-last
    @end{pre}
    @begin[code]{table}
      @entry[display]{The @sym{gdk:display} object on which the signal is
        emitted.}
      @entry[setting]{A string with the name of the setting that changed.}
    @end{table}
  @see-slot{gdk:display-composited}
  @see-slot{gdk:display-input-shapes}
  @see-slot{gdk:display-rgba}
  @see-class{gdk:seat}
  @see-class{gdk:monitor}
  @see-function{gdk:display-default-seat}
  @see-function{gdk:display-list-seats}
  @see-function{gdk:display-monitor-at-surface}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- display-composited -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "composited" 'display) t)
 "The @code{composited} property of type @code{:boolean} (Read) @br{}
  @em{True} if the display properly composites the alpha channel.")

#+liber-documentation
(setf (liber:alias-for-function 'display-composited)
      "Accessor"
      (documentation 'display-composited 'function)
 "@version{#2022-1-8}
  @syntax[]{(gdk:display-composited object) => composited}
  @argument[object]{a @class{gdk:display} object}
  @argument[composited]{@em{true} if the display properly composites the alpha
    channel}
  @begin{short}
    Accessor of the @slot[gdk:display]{composited} slot of the
    @class{gdk:display} class.
  @end{short}

  The @sym{gdk:display-composited} function returns whether surfaces can
  reasonably be expected to have their alpha channel drawn correctly on the
  screen. Check with the @fun{gdk:display-is-rgba} function for whether the
  display supports an alpha channel.

  On X11 this function returns whether a compositing manager is compositing on
  display. On modern displays, this value is always @em{true}.
  @see-class{gdk:display}
  @see-function{gdk:display-is-rgba}")

;;; --- display-input-shapes -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "input-shapes" 'display) t)
 "The @code{input-shapes} property of type @code{:boolean} (Read) @br{}
  @em{True} if the display supports input shapes.")

#+liber-documentation
(setf (liber:alias-for-function 'display-input-shapes)
      "Accessor"
      (documentation 'display-input-shapes 'function)
 "@version{#2022-1-8}
  @syntax[]{(gdk:display-input-shapes object) => setting}
  @argument[object]{a @class{gdk:display} object}
  @argument[setting]{@em{true} if the display supports input shapes}
  @begin{short}
    Accessor of the @slot[gdk:display]{input-shapes} slot of the
    @class{gdk:display} class.
  @end{short}

  The @sym{gdk:display-input-shapes} function returns @em{true} if the display
  supports input shapes. This means that the @fun{gdk:surface-set-input-region}
  function can be used to modify the input shape of surfaces on the display.

  On modern displays, this value is always @em{true}.
  @see-class{gdk:display}
  @see-function{gdk:surface-set-input-region}")

;;; --- display-rgba -------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "rgba" 'display) t)
 "The @code{rgba} property of type @code{:boolean} (Read) @br{}
  @em{True} if the display supports an alpha channel.")

#+liber-documentation
(setf (liber:alias-for-function 'display-rgba)
      "Accessor"
      (documentation 'display-rgba 'function)
 "@version{#2022-1-8}
  @syntax[]{(gdk:display-rgba object) => setting}
  @argument[object]{a @class{gdk:display} object}
  @argument[setting]{@em{true} if the display supports an alpha channel}
  @begin{short}
    Accessor of the @slot[gdk:display]{rgba} slot of the @class{gdk:display}
    class.
  @end{short}

  The @sym{gdk:display-rgba} function returns whether surfaces on the display
  are created with an alpha channel.

  Even if a @em{true} is returned, it is possible that the alpha channel of the
  surface will not be honored when displaying the surface on the screen. In
  particular, for X an appropriate windowing manager and compositing manager
  must be running to provide appropriate display. Use the
  @fun{gdk:display-is-composited} function to check if that is the case.

  On modern displays, this value is always @em{true}.
  @see-class{gdk:display}
  @see-function{gdk:display-is-composited}")

;;; ----------------------------------------------------------------------------
;;; gdk_display_open
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_open" display-open) (g:object display)
 #+liber-documentation
 "@version{#2022-1-8}
  @argument[name]{a string with the name of the display to open}
  @begin{return}
    A @class{gdk:display} object, or @code{nil} if the display could not be
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
;;; gdk_display_get_default -> display-default
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_default" display-default) (g:object display)
 #+liber-documentation
 "@version{#2022-1-8}
  @begin{return}
    A @class{gdk:display} object, or @code{nil} if there is no default display.
  @end{return}
  @begin{short}
    Gets the default display.
  @end{short}
  This is a convenience function for the call
  @begin{pre}
(gdk:display-manager-default-display (gdk:display-manager-get))
  @end{pre}
  @see-class{gdk:display}
  @see-function{gdk:display-manager-get}
  @see-function{gdk:display-manager-default-display}")

(export 'display-default)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_name -> display-name
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_name" display-name) :string
 #+liber-documentation
 "@version{#2022-1-8}
  @argument[display]{a @class{gdk:display} object}
  @return{A string representing the display name.}
  @short{Gets the name of the display.}
  @see-class{gdk:display}"
  (display (g:object display)))

(export 'display-name)

;;; ----------------------------------------------------------------------------
;;; gdk_display_device_is_grabbed
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_device_is_grabbed" display-device-is-grabbed)
    :boolean
 #+liber-documentation
 "@version{#2022-1-8}
  @argument[display]{a @class{gdk:display} object}
  @argument[device]{a @class{gdk:device} object}
  @return{A boolean that is @em{true} if there is a grab in effect for
    @arg{device}.}
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

(defcfun ("gdk_display_beep" display-beep) :void
 #+liber-documentation
 "@version{#2022-1-8}
  @argument[display]{a @class{gdk:display} object}
  @short{Emits a short beep on the display.}
  @see-class{gdk:display}"
  (display (g:object display)))

(export 'display-beep)

;;; ----------------------------------------------------------------------------
;;; gdk_display_sync
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_sync" display-sync) :void
 #+liber-documentation
 "@version{#2022-1-8}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    Flushes any requests queued for the windowing system and waits until all
    requests have been handled.
  @end{short}
  This is often used for making sure that the display is synchronized with the
  current state of the program. Calling the @sym{gdk:display-sync} function
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

(defcfun ("gdk_display_flush" display-flush) :void
 #+liber-documentation
 "@version{#2022-1-8}
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

(defcfun ("gdk_display_close" display-close) :void
 #+liber-documentation
 "@version{#2022-1-8}
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

(defcfun ("gdk_display_is_closed" display-is-closed) :boolean
 #+liber-documentation
 "@version{#2022-1-8}
  @argument[display]{a @class{gdk:display} object}
  @return{@em{true} if @arg{display} is closed.}
  @short{Finds out if the display has been closed.}
  @see-class{gdk:display}"
  (display (g:object display)))

(export 'display-is-closed)

;;; ----------------------------------------------------------------------------
;;; gdk_display_is_rgba
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_is_rgba" display-is-rgba) :boolean
 #+liber-documentation
 "@version{#2022-1-8}
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

(defcfun ("gdk_display_is_composited" display-is-composited) :boolean
 #+liber-documentation
 "@version{#2022-1-8}
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

(defcfun ("gdk_display_supports_input_shapes" display-supports-input-shapes)
    :boolean
 #+liber-documentation
 "@version{#2022-1-8}
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
;;; gdk_display_get_app_launch_context -> display-app-launch-context
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_app_launch_context"
           display-app-launch-context) (g:object app-launch-context)
 #+liber-documentation
 "@version{#2022-1-8}
  @argument[display]{a @class{gdk:display} object}
  @begin{return}
    A new @class{gdk:app-launch-context} object for @arg{display}.
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
;;; gdk_display_notify_startup_complete
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_notify_startup_complete"
           display-notify-startup-complete) :void
 #+liber-documentation
 "@version{#2022-1-8}
  @argument[display]{a @class{gdk:display} object}
  @argument[startup]{a string with a startup notification identifier, for which
    notification process should be completed}
  @begin{short}
    Indicates to the GUI environment that the application has finished loading,
    using a given identifier.
  @end{short}
  GTK will call this function automatically for @class{gtk:window} widgets
  with a custom startup notification identifier unless the
  @fun{gtk:window-set-auto-startup-notification} function is called to disable
  that feature.
  @see-class{gdk:display}
  @see-class{gtk:window}
  @see-function{gtk:window-set-auto-startup-notification}"
  (display (g:object display))
  (startup :string))

(export 'display-notify-startup-complete)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_default_seat -> display-default-seat
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_default_seat"
           display-default-seat) (g:object seat)
 #+liber-documentation
 "@version{#2022-1-8}
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
;;; gdk_display_list_seats ()
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_list_seats" display-list-seats)
    (g:list-t (g:object seat))
 #+liber-documentation
 "@version{#2022-1-8}
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
;;; gdk_display_get_monitors -> display-monitors
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_monitors" display-monitors)
    (g:object g:list-model)
 #+liber-documentation
 "@version{#2022-1-8}
  @argument[display]{a @class{gdk:display} object}
  @return{A @class{g:list-model} list of @class{gdk:monitor} objects.}
  @begin{short}
    Gets the list of monitors associated with the display.
  @end{short}
  Subsequent calls to this function will always return the same list for the
  same display.

  You can listen to the \"GListModel::items-changed\" signal on this list to
  monitor changes to the monitor of the display.
  @see-class{gdk:display}
  @see-class{gdk:monitor}
  @see-class{g:list-model}"
  (display (g:object display)))

(export 'display-monitors)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_monitor_at_surface -> display-monitor-at-surface
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_monitor_at_surface" display-monitor-at-surface)
    (g:object monitor)
 #+liber-documentation
 "@version{#2022-1-8}
  @argument[display]{a @class{gdk:display} object}
  @argument[surface]{a @class{gdk:surface} object}
  @return{A @class{gdk:monitor} object with the largest overlap with
    @arg{surface}.}
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
;;; gdk_display_get_clipboard -> display-clipboard
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_clipboard" display-clipboard)
    (g:object clipboard)
 #+liber-documentation
 "@version{#2022-1-8}
  @argument[display]{a @class{gdk:display} object}
  @return{A @class{gdk:clipboard} object.}
  @short{Gets the clipboard used for copy/paste operations.}
  @see-class{gdk:display}
  @see-class{gdk:clipboard}"
  (display (g:object display)))

(export 'display-clipboard)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_primary_clipboard -> display-primary-clipboard
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_primary_clipboard" display-primary-clipboard)
    (g:object clipboard)
 #+liber-documentation
 "@version{#2022-1-8}
  @argument[display]{a @class{gdk:display} object}
  @return{A @class{gdk:clipboard} object.}
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
;;; gdk_display_get_setting -> display-setting
;;; ----------------------------------------------------------------------------

;; TODO: Do an implementation which returns a gvalue

(defcfun ("gdk_display_get_setting" display-setting) :boolean
 #+liber-documentation
 "@version{#2022-1-21}
  @argument[name]{a string with the name of the setting}
  @argument[gvalue]{a @symbol{g:value} location to store the value of the
    setting}
  @return{@em{True} if the setting existed and a value was stored in
    @arg{gvalue}, otherwise @em{false}.}
  @begin{short}
    Retrieves a desktop-wide setting such as double-click time for the display.
  @end{short}
  @see-class{gdk:display}
  @see-symbol{g:value}"
  (name :string)
  (gvalue (:pointer (:struct g:value))))

(export 'display-setting)

;;; ----------------------------------------------------------------------------
;;; gdk_display_get_startup_notification_id
;;; -> display-startup-notification-id
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_get_startup_notification_id"
           display-startup-notification-id) :string
 #+liber-documentation
 "@version{#2022-1-8}
  @argument[display]{a @class{gdk:display} object}
  @return{A string with the startup notification ID for @arg{display}.}
  @begin{short}
    Gets the startup notification ID for a Wayland display, or @code{nil} if no
    ID has been defined.
  @end{short}
  @see-class{gdk:display}"
  (display (g:object display)))

(export 'display-startup-notification-id)

;;; ----------------------------------------------------------------------------
;;; gdk_display_put_event
;;; ----------------------------------------------------------------------------

(defcfun ("gdk_display_put_event" display-put-event) :void
 #+liber-documentation
 "@version{#2022-1-8}
  @argument[display]{a @class{gdk:display} object}
  @argument[event]{a @class{gdk:event} event}
  @begin{short}
    Appends the given event onto the front of the event queue for the display.
  @end{short}
  This function is only useful in very special situations and should not be
  used by applications.
  @see-class{gdk:display}
  @see-class{gdk:event}"
  (display (g:object display))
  (event (g:object event)))

(export 'display-put-event)

;;; ----------------------------------------------------------------------------
;;; gdk_display_map_keyval
;;;
;;; gboolean
;;; gdk_display_map_keyval (GdkDisplay* display,
;;;                         guint keyval,
;;;                         GdkKeymapKey** keys,
;;;                         int* n_keys)
;;;
;;; Obtains a list of keycode/group/level combinations that will generate
;;; keyval.
;;;
;;; Groups and levels are two kinds of keyboard mode; in general, the level
;;; determines whether the top or bottom symbol on a key is used, and the group
;;; determines whether the left or right symbol is used.
;;;
;;; On US keyboards, the shift key changes the keyboard level, and there are no
;;; groups. A group switch key might convert a keyboard between Hebrew to
;;; English modes, for example.
;;;
;;; GdkEventKey contains a %group field that indicates the active keyboard
;;; group. The level is computed from the modifier mask.
;;;
;;; The returned array should be freed with g_free().
;;;
;;; keyval :
;;;     A keyval, such as %GDK_KEY_a, %GDK_KEY_Up, %GDK_KEY_Return, etc.
;;;
;;; keys :
;;;     Return location for an array of GdkKeymapKey
;;;     The argument will be set by the function. The length of the array is
;;;     specified in the n_keys argument. The instance takes ownership of the
;;;     data, and is responsible for freeing it.
;;;
;;; n_keys :
;;;     Return location for number of elements in returned array.
;;;     The argument will be set by the function.
;;;
;;; Returns :
;;;     TRUE if keys were found and returned.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_display_map_keycode
;;;
;;; gboolean
;;; gdk_display_map_keycode (GdkDisplay* display,
;;;                          guint keycode,
;;;                          GdkKeymapKey** keys,
;;;                          guint** keyvals,
;;;                          int* n_entries)
;;;
;;; Returns the keyvals bound to keycode.
;;;
;;; The Nth GdkKeymapKey in keys is bound to the Nth keyval in keyvals.
;;;
;;; When a keycode is pressed by the user, the keyval from this list of entries
;;; is selected by considering the effective keyboard group and level.
;;;
;;; Free the returned arrays with g_free().
;;;
;;; keycode :
;;;     A keycode.
;;;
;;; keys :
;;;     Return location for array of GdkKeymapKey
;;;     The argument will be set by the function. The argument can be NULL.
;;;     The length of the array is specified in the n_entries argument. The
;;;     instance takes ownership of the data, and is responsible for freeing it.
;;;
;;; keyvals :
;;;     Return location for array of keyvals.
;;;     The argument will be set by the function. The argument can be NULL.
;;;     The length of the array is specified in the n_entries argument. The
;;;     instance takes ownership of the data, and is responsible for freeing it.
;;;
;;; n_entries :
;;;     Length of keys and keyvals.
;;;     The argument will be set by the function.
;;;
;;; Returns :
;;;     TRUE if there were any entries.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_display_translate_key
;;;
;;; gboolean
;;; gdk_display_translate_key (GdkDisplay* display,
;;;                            guint keycode,
;;;                            GdkModifierType state,
;;;                            int group,
;;;                            guint* keyval,
;;;                            int* effective_group,
;;;                            int* level,
;;;                            GdkModifierType* consumed)
;;;
;;; Translates the contents of a GdkEventKey into a keyval, effective group, and
;;; level.
;;;
;;; Modifiers that affected the translation and are thus unavailable for
;;; application use are returned in consumed_modifiers.
;;;
;;; The effective_group is the group that was actually used for the translation;
;;; some keys such as Enter are not affected by the active keyboard group. The
;;; level is derived from state.
;;;
;;; consumed_modifiers gives modifiers that should be masked out from state when
;;; comparing this key press to a keyboard shortcut. For instance, on a US
;;; keyboard, the plus symbol is shifted, so when comparing a key press to a
;;; <Control>plus accelerator <Shift> should be masked out.
;;;
;;; This function should rarely be needed, since GdkEventKey already contains
;;; the translated keyval. It is exported for the benefit of virtualized test
;;; environments.
;;;
;;; keycode :
;;;     A keycode.
;;;
;;; state :
;;;     A modifier state.
;;;
;;; group :
;;;     Active keyboard group.
;;;
;;; keyval :
;;;     Return location for keyval.
;;;     The argument will be set by the function. The argument can be NULL.
;;;
;;; effective_group :
;;;     Return location for effective group.
;;;     The argument will be set by the function. The argument can be NULL.
;;;
;;; level :
;;;     Return location for level.
;;;     The argument will be set by the function. The argument can be NULL.
;;;
;;; consumed :
;;;     Return location for modifiers that were used to determine the group or
;;;     level.
;;;     The argument will be set by the function. The argument can be NULL. The
;;;     instance takes ownership of the data, and is responsible for freeing it.
;;;
;;; Returns :
;;;     TRUE if there was a keyval bound to keycode/state/group.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_display_prepare_gl
;;;
;;; gboolean
;;; gdk_display_prepare_gl (GdkDisplay* self
;;;                         GError** error)
;;;
;;; Checks that OpenGL is available for self and ensures that it is properly
;;; initialized. When this fails, an error will be set describing the error and
;;; this function returns FALSE.
;;;
;;; Note that even if this function succeeds, creating a GdkGLContext may still
;;; fail.
;;;
;;; This function is idempotent. Calling it multiple times will just return the
;;; same value or error.
;;;
;;; You never need to call this function, GDK will call it automatically as
;;; needed. But you can use it as a check when setting up code that might make
;;; use of OpenGL.
;;;
;;; Since: 4.4
;;;
;;; Returns :
;;;     TRUE if the display supports OpenGL.
;;; ----------------------------------------------------------------------------

;;; ----------------------------------------------------------------------------
;;; gdk_display_create_gl_context
;;; ----------------------------------------------------------------------------

#+gtk-4-6
(defcfun ("gdk_display_create_gl_context" %display-create-gl-context)
    (g:object gl-context)
  (display (g:object display))
  (err :pointer))

#+gtk-4-6
(defun display-create-gl-context (display)
 #+liber-documentation
 "@version{#2022-1-8}
  @argument[display]{a @class{gdk:display} object}
  @return{A newly created @class{gdk:gl-context} object.}
  @begin{short}
    Creates a new @class{gdk:gl-context} object for the display.
  @end{short}

  The context is disconnected from any particular surface and cannot be used to
  draw to any surface. It can only be used to draw to non-surface framebuffers
  like textures.

  If the creation of the @class{gdk:gl-context} object failed, an error will be
  set. Before using the returned @class{gdk:gl-context} object, you will need
  to call the @fun{gdk:gl-context-make-current} or @fun{gdk:gl-context-realize}
  functions.
  @see-class{gdk:display}
  @see-class{gdk:gl-context}
  @see-function{gdk:gl-context-make-current}
  @see-function{gdk:gl-context-realize}"
  (with-g-error (err)
    (%display-create-gl-context display err)))

#+gtk-4-6
(export 'display-create-gl-context)

;;; --- End of file gdk.display.lisp -------------------------------------------
