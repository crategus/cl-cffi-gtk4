;;; ----------------------------------------------------------------------------
;;; gdk4.monitor.lisp
;;;
;;; The documentation in this file is taken from the GDK 4 Reference Manual
;;; version 4.18 and modified to document the Lisp binding to the GDK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2019 - 2025 Dieter Kaiser
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
;;;     gdk_monitor_get_description                         Since 4.10
;;;     gdk_monitor_get_display
;;;     gdk_monitor_get_geometry
;;;     gdk_monitor_get_width_mm
;;;     gdk_monitor_get_height_mm
;;;     gdk_monitor_get_manufacturer
;;;     gdk_monitor_get_model
;;;     gdk_monitor_get_connector
;;;     gdk_monitor_get_scale                               Since 4.14
;;;     gdk_monitor_get_scale_factor
;;;     gdk_monitor_get_refresh_rate
;;;     gdk_monitor_get_subpixel_layout
;;;     gdk_monitor_is_valid
;;;
;;; Properties
;;;
;;;     connector
;;;     description                                         Since 4.10
;;;     display
;;;     geometry
;;;     height-mm
;;;     manufacturer
;;;     model
;;;     refresh-rate
;;;     scale                                               Since 4.14
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

;;; ----------------------------------------------------------------------------
;;; GdkSubpixelLayout
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GdkSubpixelLayout" subpixel-layout
  (:export t
   :type-initializer "gdk_subpixel_layout_get_type")
  (:unknown 0)
  (:none 1)
  (:horizontal-rgb 2)
  (:horizontal-bgr 3)
  (:vertical-rgb 3)
  (:vertical-brg 4))

#+liber-documentation
(setf (liber:alias-for-symbol 'subpixel-layout)
      "GEnum"
      (liber:symbol-documentation 'subpixel-layout)
 "@version{2025-06-29}
  @begin{declaration}
(gobject:define-genum \"GdkSubpixelLayout\" subpixel-layout
  (:export t
   :type-initializer \"gdk_subpixel_layout_get_type\")
  (:unknown 0)
  (:none 1)
  (:horizontal-rgb 2)
  (:horizontal-bgr 3)
  (:vertical-rgb 3)
  (:vertical-brg 4))
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:unknown]{The layout is not known.}
      @entry[:none]{Not organized in this way.}
      @entry[:horizontal-rgb]{The layout is horizontal, the order is RGB.}
      @entry[:horizontal-bgr]{The layout is horizontal, the order is BGR.}
      @entry[:vertical-rgb]{The layout is vertical, the order is RGB.}
      @entry[:vertical-bgr]{The layout is vertical, the order is BGR.}
    @end{simple-table}
  @end{values}
  @begin{short}
    This enumeration describes how the red, green and blue components of
    physical pixels on an output device are laid out.
  @end{short}
  @see-class{gdk:monitor}")

;;; ----------------------------------------------------------------------------
;;; GdkMonitor
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkMonitor" monitor
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_monitor_get_type")
  ((connector
    monitor-connector
    "connector" "gchararray" t nil)
   #+gtk-4-10
   (description
    monitor-description
    "description" "gchararray" t nil)
   (display
    monitor-display
    "display" "GdkDisplay" t t)
   (geometry
    monitor-geometry
    "geometry" "GdkRectangle" t nil)
   (height-mm
    monitor-height-mm
    "height-mm" "gint" t nil)
   (manufacturer
    monitor-manufacturer
    "manufacturer" "gcharrarray" t nil)
   (model
    monitor-model
    "model" "gchararray" t nil)
   (refresh-rate
    monitor-refresh-rate
    "refresh-rate" "gint" t nil)
   #+gtk-4-14
   (scale
    monitor-scale
    "scale" "gdouble" t nil)
   (scale-factor
    monitor-scale-factor
    "scale-factor" "gint" t nil)
   (subpixel-layout
    monitor-subpixel-layout
    "subpixel-layout" "GdkSubpixelLayout" t nil)
   (valid
    monitor-valid
    "valid" "gboolean" t nil)
   (width-mm
    monitor-width-mm
    "width-mm" "gint" t nil)))

#+liber-documentation
(setf (liber:alias-for-class 'monitor)
      "Class"
      (documentation 'monitor 'type)
 "@version{2025-08-02}
  @begin{short}
    The @class{gdk:monitor} objects represent the individual outputs that are
    associated with a @class{gdk:display} object.
  @end{short}
  The @class{gdk:display} object keeps a @class{g:list-model} instance to
  enumerate and monitor monitors with the @fun{gdk:display-monitors} function.
  You can use the @fun{gdk:display-monitor-at-surface} function to find a
  particular monitor.
  @begin[Signal Details]{dictionary}
    @begin[monitor::invalidate]{signal}
      @begin{pre}
lambda (monitor)    :run-first
      @end{pre}
      @begin[code]{simple-table}
        @entry[monitor]{The @class{gdk:monitor} object on which this signal was
          emitted.}
      @end{simple-table}
      The signal gets emitted when the output represented by @arg{monitor} gets
      disconnected.
    @end{signal}
  @end{dictionary}
  @see-slot{gdk:monitor-connector}
  @see-slot{gdk:monitor-description}
  @see-slot{gdk:monitor-display}
  @see-slot{gdk:monitor-geometry}
  @see-slot{gdk:monitor-height-mm}
  @see-slot{gdk:monitor-manufacturer}
  @see-slot{gdk:monitor-model}
  @see-slot{gdk:monitor-refresh-rate}
  @see-slot{gdk:monitor-scale}
  @see-slot{gdk:monitor-scale-factor}
  @see-slot{gdk:monitor-subpixel-layout}
  @see-slot{gdk:monitor-valid}
  @see-slot{gdk:monitor-width-mm}
  @see-class{gdk:display}")

;;; ----------------------------------------------------------------------------
;;; Property and Accesor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk:monitor-connector --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "connector" 'monitor) t)
 "The @code{connector} property of type @code{:string} (Read) @br{}
  The connector name. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'monitor-connector)
      "Accessor"
      (documentation 'monitor-connector 'function)
 "@version{2025-08-02}
  @syntax{(gdk:monitor-connector object) => connector}
  @argument[object]{a @class{gdk:monitor} object}
  @argument[connector]{a string for the name of the connector}
  @begin{short}
    The accessor for the @slot[gdk:monitor]{connector} slot of the
    @class{gdk:monitor} class gets the name of the monitor's connector, if
    available.
  @end{short}
  @see-class{gdk:monitor}")

;;; --- gdk:monitor-description ------------------------------------------------

#+(and gtk-4-10 liber-documentation)
(setf (documentation (liber:slot-documentation "description" 'monitor) t)
 "The @code{description} property of type @code{:string} (Read) @br{}
  The short description of the monitor, meant for display to the user.
  Since 4.10 @br{}
  Default value: @code{nil}")

#+(and gtk-4-10 liber-documentation)
(setf (liber:alias-for-function 'monitor-description)
      "Accessor"
      (documentation 'monitor-description 'function)
 "@version{2025-08-02}
  @syntax{(gdk:monitor-description object) => description}
  @argument[object]{a @class{gdk:monitor} object}
  @argument[description]{a string for the monitor description}
  @begin{short}
    The accessor for the @slot[gdk:monitor]{description} slot of the
    @class{gdk:monitor} class gets a string describing the monitor, if
    available.
  @end{short}
  This can be used to identify a monitor in the UI.

  Since 4.10
  @see-class{gdk:monitor}")

;;; --- gdk:monitor-display ----------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "display" 'monitor) t)
 "The @code{display} property of type @class{gdk:display}
  (Read / Write / Construct only) @br{}
  The display of the monitor.")

#+liber-documentation
(setf (liber:alias-for-function 'monitor-display)
      "Accessor"
      (documentation 'monitor-display 'function)
 "@version{2025-08-02}
  @syntax{(gdk:monitor-display object) => display}
  @syntax{(setf (gdk:monitor-display object) display)}
  @argument[object]{a @class{gdk:monitor} object}
  @argument[display]{a @class{gdk:display} object}
  @begin{short}
    The accessor for the @slot[gdk:monitor]{display} slot of the
    @class{gdk:monitor} class gets the display that this monitor belongs to.
  @end{short}
  @see-class{gdk:monitor}
  @see-class{gdk:display}
  @see-function{gdk:monitor}")

;;; --- gdk:monitor-geometry ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "geometry" 'monitor) t)
 "The @code{geometry} property of type @class{gdk:rectangle} (Read) @br{}
  The geometry of the monitor.")

#+liber-documentation
(setf (liber:alias-for-function 'monitor-geometry)
      "Accessor"
      (documentation 'monitor-geometry 'function)
 "@version{2025-08-02}
  @syntax{(gdk:monitor-geometry object) => geometry}
  @argument[object]{a @class{gdk:monitor} object}
  @argument[geometry]{a @class{gdk:rectangle} instance for the monitor geometry}
  @begin{short}
    The accessor for the @slot[gdk:monitor]{geometry} slot of the
    @class{gdk:monitor} class retrieves the size and position of an individual
    monitor within the display coordinate space.
  @end{short}
  The returned geometry is in \"application pixels\", not in \"device pixels\".
  See the @fun{gdk:monitor-scale-factor} function.
  @see-class{gdk:monitor}
  @see-class{gdk:rectangle}
  @see-function{gdk:monitor-scale-factor}")

;;; --- gdk:monitor-height-mm --------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "height-mm" 'monitor) t)
 "The @code{height-mm} property of type @code{:int} (Read) @br{}
  The height of the monitor, in millimeters. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'monitor-height-mm)
      "Accessor"
      (documentation 'monitor-height-mm 'function)
 "@version{2025-08-02}
  @syntax{(gdk:monitor-height-mm object) => height}
  @argument[object]{a @class{gdk:monitor} object}
  @argument[height]{an integer for physical height of the monitor}
  @begin{short}
    The accessor for the @slot[gdk:monitor]{height-mm} slot of the
    @class{gdk:monitor} class gets the height in millimeters of the monitor.
  @end{short}
  @see-class{gdk:monitor}")

;;; --- gdk:monitor-manufacturer -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "manufacturer" 'monitor) t)
 "The @code{manufacturer} property of type @code{:string} (Read) @br{}
  The manufucturer name. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'monitor-manufacturer)
      "Accessor"
      (documentation 'monitor-manufacturer 'function)
 "@version{2025-08-02}
  @syntax{(gdk:monitor-manufacturer object) => manufacturer}
  @argument[object]{a @class{gdk:monitor} object}
  @argument[manufacturer]{a string for the name of the manufacturer, or
    @code{nil}}
  @begin{short}
    The accessor for the @slot[gdk:monitor]{manufacturer} slot of the
    @class{gdk:monitor} class gets the name or PNP ID of the monitor's
    manufacturer, if available.
  @end{short}
  Note that this value might also vary depending on actual display backend. PNP
  ID registry is located at @file{https://uefi.org/pnp_id_list}.
  @see-class{gdk:monitor}")

;;; --- gdk:monitor-model ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "model" 'monitor) t)
 "The @code{model} property of type @code{:string} (Read) @br{}
  The model name. @br{}
  Default value: @code{nil}")

#+liber-documentation
(setf (liber:alias-for-function 'monitor-model)
      "Accessor"
      (documentation 'monitor-model 'function)
 "@version{2025-08-02}
  @syntax{(gdk:monitor-model object) => model}
  @argument[object]{a @class{gdk:monitor} object}
  @argument[model]{a string for the monitor model, or @code{nil}}
  @begin{short}
    The accessor for the @slot[gdk:monitor]{model} slot of the
    @class{gdk:monitor} class gets the string identifying the monitor model,
    if available.
  @end{short}
  @see-class{gdk:monitor}")

;;; --- gdk:monitor-refresh-rate -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "refresh-rate" 'monitor) t)
 "The @code{refresh-rate} property of type @code{:int} (Read) @br{}
  The refresh rate, in millihertz. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'monitor-refresh-rate)
      "Accessor"
      (documentation 'monitor-refresh-rate 'function)
 "@version{2025-08-02}
  @syntax{(gdk:monitor-refresh-rate object) => rate}
  @argument[object]{a @class{gdk:monitor} object}
  @argument[rate]{an integer for the refresh rate in milli-Hertz, or 0}
  @begin{short}
    The accessor for the @slot[gdk:monitor]{refresh-rate} slot of the
    @class{gdk:monitor} class gets the refresh rate of the monitor, if
    available.
  @end{short}
  The value is in milli-Hertz, so a refresh rate of 60Hz is returned as 60000.
  @see-class{gdk:monitor}")

;;; --- gdk:monitor-scale ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "scale" 'monitor) t)
 "The @code{scale} property of type @code{:int} (Read) @br{}
  The scale of the monitor. Since 4.14 @br{}
  Default value: 1.0")

#+liber-documentation
(setf (liber:alias-for-function 'monitor-scale)
      "Accessor"
      (documentation 'monitor-scale 'function)
 "@version{2025-08-02}
  @syntax{(gdk:monitor-scale object) => scale}
  @argument[object]{a @class{gdk:monitor} object}
  @argument[scale]{a double float for the scale}
  @begin{short}
    Gets the internal scale factor that maps from monitor coordinates to device
    pixels.
  @end{short}
  This can be used if you want to create pixel based data for a particular
  monitor, but most of the time you are drawing to a surface where it is better
  to use the @fun{gdk:surface-scale} function instead.

  Since 4.14
  @see-class{gdk:monitor}
  @see-function{gdk:surface-scale}")

;;; --- gdk:monitor-scale-factor -----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "scale-factor" 'monitor) t)
 "The @code{scale-factor} property of type @code{:int} (Read) @br{}
  The scale factor. @br{}
  Allowed values: >= 0 @br{}
  Default value: 1")

#+liber-documentation
(setf (liber:alias-for-function 'monitor-scale-factor)
      "Accessor"
      (documentation 'monitor-scale-factor 'function)
 "@version{2025-08-02}
  @syntax{(gdk:monitor-scale-factor object) => scale}
  @argument[object]{a @class{gdk:monitor} object}
  @argument[scale]{an integer for the scale factor}
  @begin{short}
    The accessor for the @slot[gdk:monitor]{scale-factor} slot of the
    @class{gdk:monitor} class gets the internal scale factor that maps from
    monitor coordinates to the actual device pixels.
  @end{short}
  On traditional systems this is 1, but on very high density outputs this can
  be a higher value (often 2).

  This can be used if you want to create pixel based data for a particular
  monitor, but most of the time you are drawing to a surface where it is better
  to use the @fun{gdk:surface-scale-factor} function instead.
  @see-class{gdk:monitor}
  @see-function{gdk:surface-scale-factor}")

;;; --- gdk:monitor-subpixel-layout --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "subpixel-layout" 'monitor) t)
 "The @code{subpixel-layout} property of type @sym{gdk:subpixel-layout} (Read)
  @br{}
  The subpixel layout. @br{}
  Default value: @val[gdk:subpixel-layout]{:unknown}")

#+liber-documentation
(setf (liber:alias-for-function 'monitor-subpixel-layout)
      "Accessor"
      (documentation 'monitor-subpixel-layout 'function)
 "@version{2025-08-02}
  @syntax{(gdk:monitor-subpixel-layout object) => layout}
  @argument[object]{a @class{gdk:monitor} object}
  @argument[layout]{a @sym{gdk:subpixel-layout} value}
  @begin{short}
    The accessor for the @slot[gdk:monitor]{subpixel-layout} slot of the
    @class{gdk:monitor} class gets information about the layout of red, green
    and blue primaries for each pixel in this monitor, if available.
  @end{short}
  @see-class{gdk:monitor}
  @see-symbol{gdk:subpixel-layout}")

;;; --- gdk:monitor-valid ------------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "valid" 'monitor) t)
 "The @code{valid} property of type @code{:boolean} (Read) @br{}
  Whether the monitor is still valid. @br{}
  Default value: @em{true}")

#+liber-documentation
(setf (liber:alias-for-function 'monitor-valid)
      "Accessor"
      (documentation 'monitor-valid 'function)
 "@version{2025-08-02}
  @syntax{(gdk:monitor-valid object) => valid}
  @argument[object]{a @class{gdk:monitor} object}
  @argument[valid]{an boolean whether the monitor is still valid}
  @begin{short}
    The accessor for the @slot[gdk:monitor]{valid} slot of the
    @class{gdk:monitor} class gets whether the monitor is still valid.
  @end{short}
  @see-class{gdk:monitor}")

;;; --- gdk:monitor-width-mm ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "width-mm" 'monitor) t)
 "The @code{width-mm} property of type @code{:int} (Read) @br{}
  The width of the monitor, in millimeters. @br{}
  Allowed values: >= 0 @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'monitor-width-mm)
      "Accessor"
      (documentation 'monitor-width-mm 'function)
 "@version{2025-08-02}
  @syntax{(gdk:monitor-width-mm object) => width}
  @argument[object]{a @class{gdk:monitor} object}
  @argument[width]{an integer for physical width of the monitor}
  @begin{short}
    The accessor for the @slot[gdk:monitor]{width-mm} slot of the
    @class{gdk:monitor} class gets the width in millimeters of the monitor.
  @end{short}
  @see-class{gdk:monitor}")

;;; ----------------------------------------------------------------------------
;;; gdk_monitor_is_valid
;;; ----------------------------------------------------------------------------

(cffi:defcfun ("gdk_monitor_is_valid" monitor-is-valid) :boolean
 #+liber-documentation
 "@version{2024-01-07}
  @argument[monitor]{a @class{gdk:monitor} object}
  @begin{short}
    Returns @em{true} if the monitor object corresponds to a physical monitor.
  @end{short}
  The monitor becomes invalid when the physical monitor is unplugged or removed.
  @see-class{gdk:monitor}
  @see-function{gdk:monitor-valid}"
  (monitor (g:object monitor)))

(export 'monitor-is-valid)

;;; --- End of file gdk4.monitor.lisp ------------------------------------------
