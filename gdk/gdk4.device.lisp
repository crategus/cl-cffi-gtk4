;;; ----------------------------------------------------------------------------
;;; gdk4.device.lisp
;;;
;;; The documentation of this file is taken from the GDK 4 Reference Manual
;;; Version 4.12 and modified to document the Lisp binding to the GDK library.
;;; See <http://www.gtk.org>. The API documentation of the Lisp binding is
;;; available from <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2011 - 2024 Dieter Kaiser
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
;;; GdkDevice
;;;
;;;     Object representing an input device
;;;
;;; Types and Values
;;;
;;;     GdkDevice
;;;     GdkInputSource
;;;     GdkAxisUse
;;;     GdkAxisFlags
;;;
;;; Accessors
;;;
;;;     gdk_device_get_caps_lock_state
;;;     gdk_device_get_direction
;;;     gdk_device_get_display
;;;     gdk_device_has_bidi_layouts
;;;     gdk_device_get_has_cursor
;;;     gdk_device_get_modifier_state
;;;     gdk_device_get_name
;;;     gdk_device_get_num_lock_state
;;;     gdk_device_get_num_touches
;;;     gdk_device_get_product_id
;;;     gdk_device_get_scroll_lock_state
;;;     gdk_device_get_seat
;;;     gdk_device_get_source
;;;     gdk_device_get_device_tool
;;;     gdk_device_get_vendor_id
;;;
;;; Functions
;;;
;;;     gdk_device_get_surface_at_position
;;;     gdk_device_get_timestamp                           Since 4.2
;;;
;;; Properties
;;;
;;;     caps-lock-state
;;;     direction
;;;     display
;;;     has-bidi-layouts
;;;     has-cursor
;;;     modifier-state
;;;     n-axes
;;;     name
;;;     num-lock-state
;;;     num-touches
;;;     product-id
;;;     scroll-lock-state
;;;     seat
;;;     source
;;;     tool
;;;     vendor-id
;;;
;;; Signals
;;;
;;;     changed
;;;     tool-changed
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkDevice
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; enum GdkInputSource
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GdkInputSource" input-source
  (:export t
   :type-initializer "gdk_input_source_get_type")
  :mouse
  :pen
  :keyboard
  :touchscreen
  :touchpad
  :trackpoint
  :tablet-pad)

#+liber-documentation
(setf (liber:alias-for-symbol 'input-source)
      "GEnum"
      (liber:symbol-documentation 'input-source)
 "@version{2023-4-15}
  @begin{short}
    An enumeration describing the type of an input device in general terms.
  @end{short}
  @begin{pre}
(gobject:define-g-enum \"GdkInputSource\" input-source
  (:export t
   :type-initializer \"gdk_input_source_get_type\")
  :mouse
  :pen
  :keyboard
  :touchscreen
  :touchpad
  :trackpoint
  :tablet-pad)
  @end{pre}
  @begin[code]{table}
    @entry[:mouse]{The device is a mouse. This will be reported for the core
      pointer, even if it is something else, such as a trackball.}
    @entry[:pen]{The device is a stylus of a graphics tablet or similar device.}
    @entry[:keyboard]{The device is a keyboard.}
    @entry[:touchscreen]{The device is a direct-input touch device, such as a
      touchscreen or tablet.}
    @entry[:touchpad]{The device is an indirect touch device, such as a
      touchpad.}
    @entry[:trackpoint]{The device is a trackpoint.}
    @entry[:tablet-pad]{The device is a \"pad\", a collection of buttons, rings
      and strips found in drawing tablets.}
  @end{table}
  @see-class{gdk:device}")

;;; ----------------------------------------------------------------------------
;;; enum GdkAxisUse
;;; ----------------------------------------------------------------------------

(gobject:define-g-enum "GdkAxisUse" axis-use
  (:export t
   :type-initializer "gdk_axis_use_get_type")
  :ignore
  :x
  :y
  :delta-x
  :delty-y
  :pressure
  :xtilt
  :ytilt
  :wheel
  :distance
  :rotation
  :slider
  :last)

#+liber-documentation
(setf (liber:alias-for-symbol 'axis-use)
      "GEnum"
      (liber:symbol-documentation 'axis-use)
 "@version{2023-4-15}
  @begin{short}
    An enumeration describing the way in which a device axis (valuator) maps
    onto the predefined valuator types that GTK understands.
  @end{short}
  Note that the X and Y axes are not really needed. Pointer devices report their
  location via the x/y members of events regardless. Whether X and Y are present
  as axes depends on the GDK backend.
  @begin{pre}
(gobject:define-g-enum \"GdkAxisUse\" gdk-axis-use
  (:export t
   :type-initializer \"gdk_axis_use_get_type\")
  :ignore
  :x
  :y
  :delta-x
  :delty-y
  :pressure
  :xtilt
  :ytilt
  :wheel
  :distance
  :rotation
  :slider
  :last)
  @end{pre}
  @begin[code]{table}
    @entry[:ignore]{The axis is ignored.}
    @entry[:x]{The axis is used as the x axis.}
    @entry[:y]{The axis is used as the y axis.}
    @entry[:delta-x]{The axis is used as the scroll x delta.}
    @entry[:delta-y]{The axis is used as the scroll y delta.}
    @entry[:pressure]{The axis is used for pressure information.}
    @entry[:xtilt]{The axis is used for x tilt information.}
    @entry[:ytilt]{The axis is used for y tilt information.}
    @entry[:wheel]{The axis is used for wheel information.}
    @entry[:distance]{The axis is used for pen/tablet distance information.}
    @entry[:rotation]{The axis is used for pen rotation information.}
    @entry[:slider]{The axis is used for pen slider information.}
    @entry[:last]{A constant equal to the numerically highest axis value.}
  @end{table}
  @see-class{gdk:device}")

;;; ----------------------------------------------------------------------------
;;; enum GdkAxisFlags
;;; ----------------------------------------------------------------------------

(gobject:define-g-flags "GdkAxisFlags" axis-flags
  (:export t
   :type-initializer "gdk_axis_flags_get_type")
  (:x        #.(ash 1 1))
  (:y        #.(ash 1 2))
  (:delta-x  #.(ash 1 3))
  (:delta-y  #.(ash 1 4))
  (:pressure #.(ash 1 5))
  (:xtilt    #.(ash 1 6))
  (:ytilt    #.(ash 1 7))
  (:wheel    #.(ash 1 8))
  (:distance #.(ash 1 9))
  (:rotation #.(ash 1 10))
  (:slider   #.(ash 1 11)))

#+liber-documentation
(setf (liber:alias-for-symbol 'axis-flags)
      "GFlags"
      (liber:symbol-documentation 'axis-flags)
 "@version{2023-4-15}
  @begin{short}
    Flags describing the current capabilities of a device/tool.
  @end{short}
  @begin{pre}
(gobject:define-g-flags \"GdkAxisFlags\" axis-flags
  (:export t
   :type-initializer \"gdk_axis_flags_get_type\")
  (:x        #.(ash 1 1))
  (:y        #.(ash 1 2))
  (:delta-x  #.(ash 1 3))
  (:delta-y  #.(ash 1 4))
  (:pressure #.(ash 1 5))
  (:xtilt    #.(ash 1 6))
  (:ytilt    #.(ash 1 7))
  (:wheel    #.(ash 1 8))
  (:distance #.(ash 1 9))
  (:rotation #.(ash 1 10))
  (:slider   #.(ash 1 11)))
  @end{pre}
  @begin[code]{table}
    @entry[:x]{x axis is present.}
    @entry[:y]{y axis is present.}
    @entry[:delta-x]{Scroll X delta axis is present.}
    @entry[:delta-y]{Scroll Y delta axis is present}
    @entry[:pressure]{Pressure axis is present.}
    @entry[:xtilt]{x tilt axis is present.}
    @entry[:ytilt]{y tilt axis is present.}
    @entry[:wheel]{Wheel axis is present.}
    @entry[:distance]{Distance axis is present.}
    @entry[:rotation]{z-axis rotation is present.}
    @entry[:slider]{Slider axis is present.}
  @end{table}
  @see-class{gdk:device}")

;;; ----------------------------------------------------------------------------
;;; GdkDevice
;;; ----------------------------------------------------------------------------

(gobject:define-g-object-class "GdkDevice" device
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_device_get_type")
  ((caps-lock-state
    device-caps-lock-state
    "caps-lock-state" "gboolean" t nil)
   (direction
    device-direction
    "direction" "PangoDirection" t nil)
   (display
    device-display
    "display" "GdkDisplay" t t)
   (has-bidi-layouts
    device-has-bidi-layouts
    "has-bidi-layouts" "gboolean" t nil)
   (has-cursor
    device-has-cursor
    "has-cursor" "gboolean" t t)
   (modifier-state
    device-modifier-state
    "modifier-state" "GdkModifierType" t nil)
   (n-axes
    device-n-axes
    "n-axes" "gint" t nil)
   (name
    device-name
    "name" "gchararray" t t)
   (num-lock-state
    device-num-lock-state
    "num-lock-state" "gboolean" t nil)
   (num-touches
    device-num-touches
    "num-touches" "guint" t t)
   (product-id
    device-product-id
    "product-id" "gchararray" t t)
   (scroll-lock-state
    device-scroll-lock-state
    "scroll-lock-state" "gboolean" t nil)
   (seat
    device-seat
    "seat" "GdkSeat" t t)
   (source
    device-source
    "source" "GdkInputSource" t nil)
   (tool
    device-tool
    "tool" "GdkDeviceTool" t nil)
   (vendor-id
    device-vendor-id
    "vendor-id" "gchararray" t t)))

#+liber-documentation
(setf (documentation 'device 'type)
 "@version{2024-1-7}
  @begin{short}
    The @class{gdk:device} object represents a single input device, such as a
    keyboard, a mouse, a touchpad, etc.
  @end{short}
  See the @class{gdk:seat} documentation for more information about the various
  kinds of devices, and their relationships.
  @begin[Signal Details]{dictionary}
    @subheading{The \"changed\" signal}
      @begin{pre}
lambda (device)    :run-last
      @end{pre}
      The signal is emitted either when the @class{gdk:device} object has
      changed the number of either axes or keys. For example In X this will
      normally happen when the physical device routing events through the
      logical device changes, for example, user switches from the USB mouse to
      a tablet, in that case the logical device will change to reflect the axes
      and keys on the new physical device.
      @begin[code]{table}
        @entry[device]{The @class{gdk:device} object that changed.}
      @end{table}
    @subheading{The \"tool-changed\" signal}
      @begin{pre}
lambda (device tool)    :run-last
      @end{pre}
      The signal is emitted on pen/eraser devices whenever tools enter or leave
      proximity.
      @begin[code]{table}
        @entry[device]{The @class{gdk:device} object that changed.}
        @entry[tool]{The new @class{gdk:device-tool} current device tool.}
      @end{table}
  @end{dictionary}
  @see-slot{gdk:device-caps-lock-state}
  @see-slot{gdk:device-direction}
  @see-slot{gdk:device-display}
  @see-slot{gdk:device-has-bidi-layouts}
  @see-slot{gdk:device-has-cursor}
  @see-slot{gdk:device-modifier-state}
  @see-slot{gdk:device-n-axes}
  @see-slot{gdk:device-name}
  @see-slot{gdk:device-num-lock-state}
  @see-slot{gdk:device-num-touches}
  @see-slot{gdk:device-product-id}
  @see-slot{gdk:device-scroll-lock-state}
  @see-slot{gdk:device-seat}
  @see-slot{gdk:device-source}
  @see-slot{gdk:device-tool}
  @see-slot{gdk:device-vendor-id}
  @see-class{gdk:seat}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk:device-caps-lock-state ---------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "caps-lock-state" 'device) t)
 "The @code{caps-lock-state} property of type @code{:boolean} (Read) @br{}
  Whether the keyboard caps lock is on. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'device-caps-lock-state)
      "Accessor"
      (documentation 'device-caps-lock-state 'function)
 "@version{2024-1-7}
  @syntax[]{(gdk:device-caps-lock-state object) => setting}
  @argument[object]{a @class{gdk:device} object}
  @argument[setting]{a boolean whether the keyboard caps lock is on}
  @begin{short}
    Accessor of the @slot[gdk:device]{caps-lock-state} slot of the
    @class{gdk:device} class.
  @end{short}
  The @fun{gdk:device-caps-lock-state} function retrieves whether the Caps Lock
  modifier of the keyboard is locked, if @arg{object} is a keyboard device.
  @see-class{gdk:device}")

;;; --- gdk:device-direction ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "direction" 'device) t)
 "The @code{direction} property of type @symbol{pango:direction} (Read) @br{}
  The direction of the current layout of the keyboard. @br{}
  Default value: @code{:neutral}")

#+liber-documentation
(setf (liber:alias-for-function 'device-direction)
      "Accessor"
      (documentation 'device-direction 'function)
 "@version{2024-1-7}
  @syntax[]{(gdk:device-direction object) => direction}
  @argument[object]{a @class{gdk:device} object}
  @argument[direction]{a @symbol{pango:direction} value}
  @begin{short}
    Accessor of the @slot[gdk:device]{direction} slot of the @class{gdk:device}
    class.
  @end{short}
  The @fun{gdk:device-direction} function returns the direction of the effective
  layout of the keyboard, if @arg{object} is a keyboard device. The direction of
  a layout is the direction of the majority of its symbols.
  @see-class{gdk:device}")

;;; --- gdk:device-display -----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "display" 'device) t)
 "The @code{display} property of type @class{gdk:display}
  (Read / Write / Construct Only) @br{}
  The display the device pertains to.")

#+liber-documentation
(setf (liber:alias-for-function 'device-display)
      "Accessor"
      (documentation 'device-display 'function)
 "@version{2024-1-7}
  @syntax[]{(gdk:device-display object) => display}
  @argument[object]{a @class{gdk:device} object}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    Accessor of the @slot[gdk:device]{display} slot of the @class{gdk:device}
    class.
  @end{short}
  The @fun{gdk:device-display} function returns the display to which the device
  pertains.
  @see-class{gdk:device}
  @see-class{gdk:display}")

;;; --- gdk:device-has-bidi-layouts --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-bidi-layouts" 'device) t)
 "The @code{has-bidi-layouts} property of type @code{:boolean} (Read) @br{}
  Whether the keyboard has bidi layouts. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'device-has-bidi-layouts)
      "Accessor"
      (documentation 'device-has-bidi-layouts 'function)
 "@version{2024-1-7}
  @syntax[]{(gdk:device-has-bidi-layouts object) => setting}
  @argument[object]{a @class{gdk:device} object}
  @argument[setting]{@em{true} if there are layouts with both directions,
    @em{false} otherwise}
  @begin{short}
    Accessor of the @slot[gdk:device]{has-bidi-layouts} slot of the
    @class{gdk:device} class.
  @end{short}
  The @fun{gdk:device-has-bidi-layouts} function determines if keyboard layouts
  for both right-to-left and left-to-right languages are in use on the keyboard,
  if @arg{object} is a keyboard device.
  @see-class{gdk:device}")

;;; --- gdk:device-has-cursor --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "has-cursor" 'device) t)
 "The @code{cursor} property of type @code{:boolean}
  (Read / Write / Contstruct Only) @br{}
  Whether the device is represented by a cursor on the screen. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'device-has-cursor)
      "Accessor"
      (documentation 'device-has-cursor 'function)
 "@version{2024-1-7}
  @syntax[]{(gdk:device-has-cursor object) => setting}
  @argument[object]{a @class{gdk:device} object}
  @argument[setting]{@em{true} if the pointer follows device motion}
  @begin{short}
    Accessor of the @slot[gdk:device]{has-cursor} slot of the @class{gdk:device}
    class.
  @end{short}
  The @fun{gdk:device-has-cursor} function determines whether the pointer
  follows device motion. This is not meaningful for keyboard devices, which
  do not have a pointer.
  @see-class{gdk:device}")

;;; --- gdk:device-modifier-state ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "modifier-state" 'device) t)
 "The @code{modifier-state} property of type @symbol{gdk:modifier-type} (Read)
  @br{}
  The modifier state of the keyboard.")

#+liber-documentation
(setf (liber:alias-for-function 'device-modifier-state)
      "Accessor"
      (documentation 'device-modifier-state 'function)
 "@version{2024-1-7}
  @syntax[]{(gdk:device-modifier-state object) => state}
  @argument[object]{a @class{gdk:device} object}
  @argument[state]{a @symbol{gdk:modifier-state} value with the current modifier
    state}
  @begin{short}
    Accessor of the @slot[gdk:device]{modifier-state} slot of the
    @class{gdk:device} class.
  @end{short}
  The @fun{gdk:device-modifier-state} function retrieves the current modifier
  state of the keyboard, if @arg{object} is a keyboard device.
  @see-class{gdk:device}
  @see-symbol{gdk:modifier-type}")

;;; --- gdk:device-n-axes ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "n-axes" 'device) t)
 "The @code{n-axes} property of type @code{:uint} (Read) @br{}
  Number of axes in the device. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'device-n-axes)
      "Accessor"
      (documentation 'device-n-axes 'function)
 "@version{2024-1-7}
  @syntax[]{(gdk:device-n-axes object) => n-axes}
  @argument[object]{a @class{gdk:device} object}
  @argument[n-axes]{an unsigned integer with the number of axes in the device}
  @begin{short}
    Accessor of the @slot[gdk:device]{n-axes} slot of the @class{gdk:device}
    class.
  @end{short}
  The @fun{gdk:device-n-axes} function gets the number of axes in the device.
  @see-class{gdk:device}")

;;; --- gdk:device-name --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "name" 'device) t)
 "The @code{name} property of type @code{:string}
  (Read / Write / Construct Only) @br{}
  The device name. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'device-name)
      "Accessor"
      (documentation 'device-name 'function)
 "@version{2024-1-7}
  @syntax[]{(gdk:device-name object) => name}
  @argument[object]{a @class{gdk:device} object}
  @argument[name]{a string with the name of the device}
  @begin{short}
    Accessor of the @slot[gdk:device]{name} slot of the @class{gdk:device}
    class.
  @end{short}
  The @fun{gdk:device-name} function determines the name of the device, suitable
  for showing in a user interface.
  @see-class{gdk:device}")

;;; --- gdk:device-num-lock-state ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "num-lock-state" 'device) t)
 "The @code{num-lock-state} property of type @code{:boolean} (Read) @br{}
  Whether the keyboard num lock is on. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'device-num-lock-state)
      "Accessor"
      (documentation 'device-num-lock-state 'function)
 "@version{2024-1-7}
  @syntax[]{(gdk:device-num-lock-state object) => setting}
  @argument[object]{a @class{gdk:device} object}
  @argument[setting]{@em{true} if Num Lock is on for the device}
  @begin{short}
    Accessor of the @slot[gdk:device]{num-lock-state} slot of the
    @class{gdk:device} class.
  @end{short}
  The @fun{gdk:device-num-lock-state} function retrieves whether the Num Lock
  modifier of the keyboard is locked, if @arg{object} is a keyboard device.
  @see-class{gdk:device}")

;;; --- gdk:device-num-touches -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "num-touches" 'device) t)
 "The @code{num-touches} property of type @code{:uint}
  (Read / Write / Construct Only) @br{}
  The maximal number of concurrent touches on a touch device. Will be 0 if the
  device is not a touch device or if the number of touches is unknown. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'device-num-touches)
      "Accessor"
      (documentation 'device-num-touches 'function)
 "@version{2024-1-7}
  @syntax[]{(gdk:device-num-touches object) => num-touches}
  @argument[object]{a @class{gdk:device} object}
  @argument[num-touches]{an unsigned integer with the number of touch points}
  @begin{short}
    Accessor of the @slot[gdk:device]{num-touches} slot of the
    @class{gdk:device} class.
  @end{short}
  The @fun{gdk:device-num-touches} function retrieves the number of touch points
  associated to the device.
  @see-class{gdk:device}")

;;; --- gdk:device-product-id --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "product-id" 'device) t)
 "The @code{product-id} property of type @code{:string}
  (Read / Write / Construct Only) @br{}
  Product ID of the device. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'device-product-id)
      "Accessor"
      (documentation 'device-product-id 'function)
 "@version{2024-1-7}
  @syntax[]{(gdk:device-product-id object) => product-id}
  @argument[object]{a @class{gdk:device} object}
  @argument[product-id]{a string with the product ID, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gdk:device]{product-id} slot of the @class{gdk:device}
    class.
  @end{short}
  The @fun{gdk:device-product-id} function returns the product ID of the device,
  or @code{nil} if the information could not be obtained. The ID is retrieved
  from the device, and is thus constant for it. See the
  @fun{gdk:device-vendor-id} function for more information.
  @see-class{gdk:device}
  @see-function{gdk:device-vendor-id}")

;;; --- gdk:device-scroll-lock-state -------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "scroll-lock-state" 'device) t)
 "The @code{scroll-lock-state} property of type @code{:boolean} (Read) @br{}
  Whether the keyboard scroll lock is on. @br{}
  Default value: @em{false}")

#+liber-documentation
(setf (liber:alias-for-function 'device-scroll-lock-state)
      "Accessor"
      (documentation 'device-scroll-lock-state 'function)
 "@version{2024-1-7}
  @syntax[]{(gdk:device-scroll-lock-id object) => setting}
  @argument[object]{a @class{gdk:device} object}
  @argument[scroll-lock-id]{@em{true} if Scroll Lock is on for the device}
  @begin{short}
    Accessor of the @slot[gdk:device]{scroll-lock-state} slot of the
    @class{gdk:device} class.
  @end{short}
  The @fun{gdk:device-scroll-lock-state} function retrieves whether the Scroll
  Lock modifier of the keyboard is locked, if @arg{object} is a keyboard device.
  @see-class{gdk:device}")

;;; --- gdk:device-seat --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "seat" 'device) t)
 "The @code{seat} property of type @class{gdk:seat} (Read / Write) @br{}
  The seat of the device.")

#+liber-documentation
(setf (liber:alias-for-function 'device-seat)
      "Accessor"
      (documentation 'device-seat 'function)
 "@version{2024-1-7}
  @syntax[]{(gdk:device-seat object) => seat}
  @syntax[]{(setf (gdk:device-seat object) seat)}
  @argument[object]{a @class{gdk:device} object}
  @argument[seat]{a @class{gdk:seat} object}
  @begin{short}
    Accessor of the @slot[gdk:device]{seat} slot of the @class{gdk:device}
    class.
  @end{short}
  The @fun{gdk:device-seat} function returns the seat the device belongs to.
  The @setf{gdk:device-seat} function sets the seat.
  @see-class{gdk:device}
  @see-class{gdk:seat}")

;;; --- gdk:device-source ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "source" 'device) t)
 "The @code{source} property of type @symbol{gdk:input-source}
  (Read / Write / Construct Only) @br{}
  Source type of the device. @br{}
  Default value: @code{:mouse}")

#+liber-documentation
(setf (liber:alias-for-function 'device-source)
      "Accessor"
      (documentation 'device-source 'function)
 "@version{2024-1-7}
  @syntax[]{(gdk:device-source object) => source}
  @argument[object]{a @class{gdk:device} object}
  @argument[source]{a @symbol{gdk:input-source} value}
  @begin{short}
    Accessor of the @slot[gdk:device]{source} slot of the @class{gdk:device}
    class.
  @end{short}
  The @fun{gdk:device-source} function determines the type of the device.
  @see-class{gdk:device}
  @see-symbol{gdk:input-source}")

;;; --- gdk:device-tool --------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tool" 'device) t)
 "The @code{tool} property of type @class{gdk:device-tool} (Read) @br{}
  The tool that is currently used with the device.")

#+liber-documentation
(setf (liber:alias-for-function 'device-tool)
      "Accessor"
      (documentation 'device-tool 'function)
 "@version{2024-1-7}
  @syntax[]{(gdk:device-tool object) => tool}
  @argument[object]{a @class{gdk:device} object}
  @argument[tool]{a @symbol{gdk:device-tool} object}
  @begin{short}
    Accessor of the @slot[gdk:device]{tool} slot of the @class{gdk:device}
    class.
  @end{short}
  The @fun{gdk:device-tool} function retrieves the device tool associated to
  the device.
  @see-class{gdk:device}
  @see-class{gdk:device-tool}")

;;; --- gdk:device-vendor-id ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "vendor-id" 'device) t)
 "The @code{vendor-id} property of type @code{:string}
  (Read / Write / Construct Only) @br{}
  Vendor ID of this device. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'device-vendor-id)
      "Accessor"
      (documentation 'device-vendor-id 'function)
 "@version{2024-1-7}
  @syntax[]{(gdk:device-vendor-id object) => vendor-id}
  @argument[object]{a @class{gdk:device} object}
  @argument[vendor-id]{a string with the vendor ID, or @code{nil}}
  @begin{short}
    Accessor of the @slot[gdk:device]{vendor-id} slot of the @class{gdk:device}
    class.
  @end{short}
  The @fun{gdk:device-vendor-id} function returns the vendor ID of this device,
  or @code{nil} if the information could not be obtained. This ID is retrieved
  from the device, and is thus constant for it.
  @see-class{gdk:device}
  @see-class{gdk:device-product-id}")

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_device_tool ()
;;; ----------------------------------------------------------------------------

;; Implemented as the gdk:device-tool function.

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_surface_at_position ()
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_device_get_surface_at_position" %device-surface-at-position)
    (g:object surface)
  (device (g:object device))
  (xwin (:pointer :double))
  (ywin (:pointer :double)))

(defun device-surface-at-position (device)
 #+liber-documentation
 "@version{2024-1-7}
  @argument[device]{a @class{gdk:device} object}
  @begin{return}
    @arg{surface} -a @symbol{gdk:surface} object under the device position @br{}
    @arg{xwin} - a double float with the x coordinate of the device location
      @br{}
    @arg{ywin} - a double float with the y coordinate of the device location
  @end{return}
  @begin{short}
    Obtains the surface underneath @arg{device}, returning the location of the
    device in @arg{xwin} and @arg{ywin} in double precision.
  @end{short}
  Returns @code{nil} if the surface tree under device is not known to GDK, for
  example, belongs to another application.
  @see-class{gdk:device}
  @see-class{gdk:surface}"
  (cffi:with-foreign-objects ((xwin :double) (ywin :double))
    (let (surface)
      (when (setf surface (%device-surface-at-position device xwin ywin))
        (values surface
                (cffi:mem-ref xwin :double)
                (cffi:mem-ref ywin :double))))))

(export 'device-surface-at-position)

;;; ----------------------------------------------------------------------------
;;; gdk_device_get_timestamp ()
;;; ----------------------------------------------------------------------------

#+gtk-4-2
(cffi:defcfun ("gdk_device_get_timestamp" device-timestamp) :uint32
 #+liber-documentation
 "@version{2024-1-7}
  @argument[device]{a @class{gdk:device} object}
  @return{The unsigned integer with the timestamp of the last activity for this
    device.}
  @begin{short}
    Returns the timestamp of the last activity for this device.
  @end{short}
  In practice, this means the timestamp of the last event that was received
  from the OS for this device. GTK may occasionally produce events for a
  device that are not received from the OS, and will not update the timestamp.

  Since 4.2
  @see-class{gdk:device}"
  (device (g:object device)))

#+gtk-4-2
(export 'device-timestamp)

;;; --- End of file gdk4.device.lisp -------------------------------------------
