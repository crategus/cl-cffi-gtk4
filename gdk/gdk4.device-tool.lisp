;;; ----------------------------------------------------------------------------
;;; gdk4.device-tool.lisp
;;;
;;; The documentation in this file is taken from the GDK 4 Reference Manual
;;; version 4.20 and modified to document the Lisp binding to the GDK library,
;;; see <http://www.gtk.org>. The API documentation for the Lisp binding is
;;; available at <http://www.crategus.com/books/cl-cffi-gtk4/>.
;;;
;;; Copyright (C) 2024 - 2026 Dieter Kaiser
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
;;; Types and Values
;;;
;;;     GdkDeviceTool
;;;     GdkDeviceToolType
;;;
;;; Accessors
;;;
;;;     gdk_device_tool_get_axes
;;;     gdk_device_tool_get_hardware_id
;;;     gdk_device_tool_get_serial
;;;     gdk_device_tool_get_tool_type
;;;
;;; Properties
;;;
;;;     axes
;;;     hardware-id
;;;     serial
;;;     tool-type
;;;
;;; Object Hierarchy
;;;
;;;     GObject
;;;     ╰── GdkDeviceTool
;;; ----------------------------------------------------------------------------

(in-package :gdk)

;;; ----------------------------------------------------------------------------
;;; GdkDeviceToolType
;;; ----------------------------------------------------------------------------

(gobject:define-genum "GdkDeviceToolType" device-tool-type
  (:export t
   :type-initializer "gdk_device_tool_type_get_type")
  :unknown
  :pen
  :eraser
  :brush
  :pencil
  :airbrush
  :mouse
  :lens)

#+liber-documentation
(setf (liber:alias-for-symbol 'device-tool-type)
      "GEnum"
      (liber:symbol-documentation 'device-tool-type)
 "@version{2025-07-30}
  @begin{declaration}
(gobject:define-genum \"GdkDeviceToolType\" device-tool-type
  (:export t
   :type-initializer \"gdk_device_tool_type_get_type\")
  :unknown
  :pen
  :eraser
  :brush
  :pencil
  :airbrush
  :mouse
  :lens)
  @end{declaration}
  @begin{values}
    @begin[code]{simple-table}
      @entry[:unkown]{Tool is of an unknown type.}
      @entry[:pen]{Tool is a standard tablet stylus.}
      @entry[:eraser]{Tool is standard tablet eraser.}
      @entry[:brush]{Tool is a brush stylus.}
      @entry[:pencil]{Tool is a pencil stylus.}
      @entry[:airbrush]{Tool is an airbrush stylus.}
      @entry[:mouse]{Tool is a mouse.}
      @entry[:lens]{Tool is a lens cursor.}
    @end{simple-table}
  @end{values}
  @begin{short}
    Indicates the specific type of tool being used being a tablet.
  @end{short}
  Such as an airbrush, pencil, etc.
  @see-class{gdk:device}
  @see-class{gdk:device-tool}")

;;; ----------------------------------------------------------------------------
;;; GdkDeviceTool
;;; ----------------------------------------------------------------------------

(gobject:define-gobject "GdkDeviceTool" device-tool
  (:superclass g:object
   :export t
   :interfaces nil
   :type-initializer "gdk_device_tool_get_type")
  ((axes
    device-tool-axes
    "axes" "GdkAxisFlags" t nil)
   (hardware-id
    device-tool-hardware-id
    "hardware-id" "guint64" t nil)
   (serial
    device-tool-serial
    "serial" "guint64" t nil)
   (tool-type
    device-tool-tool-type
    "tool-type" "GdkDeviceToolType" t nil)))

#+liber-documentation
(setf (documentation 'device-tool 'type)
 "@version{2025-07-31}
  @short{A physical tool associated to a @class{gdk:device} object.}
  @see-slot{gdk:device-tool-axes}
  @see-slot{gdk:device-tool-hardware-id}
  @see-slot{gdk:device-tool-serial}
  @see-slot{gdk:device-tool-tool-type}
  @see-class{gdk:device}")

;;; ----------------------------------------------------------------------------
;;; Property and Accessor Details
;;; ----------------------------------------------------------------------------

;;; --- gdk:device-tool-axes ---------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "axes" 'device-tool) t)
 "The @code{axes} property of type @sym{gdk:axis-flags}
  (Read / Write / Construct Only) @br{}
  The device tool axes.")

#+liber-documentation
(setf (liber:alias-for-function 'device-tool-axes)
      "Accessor"
      (documentation 'device-tool-axes 'function)
 "@version{2025-07-31}
  @syntax{(gdk:device-tool-axes object) => axes}
  @argument[object]{a @class{gdk:device-tool} object}
  @argument[axes]{a @sym{gdk:axis-flags} value}
  @begin{short}
    The accessor for the @slot[gdk:device-tool]{axes} slot of the
    @class{gdk:device-tool} class gets the axes of the device tool.
  @end{short}
  @see-class{gdk:device-tool}
  @see-symbol{gdk:axis-flags}")

;;; --- gdk:device-tool-hardware-id --------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "hardware-id" 'device-tool) t)
 "The @code{hardware-id} property of type @code{:uint64}
  (Read / Write / Construct Only) @br{}
  The hardware ID. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'device-tool-hardware-id)
      "Accessor"
      (documentation 'device-tool-hardware-id 'function)
 "@version{2025-07-31}
  @syntax{(gdk:device-tool-hardware-id object) => hardware-id}
  @argument[object]{a @class{gdk:device-tool} object}
  @argument[hardware-id]{an unsigned integer for the hardware identificator of
    the device tool}
  @begin{short}
    The accessor for the @slot[gdk:device-tool]{hardware-id} slot of the
    @class{gdk:device-tool} class gets the hardware ID of the device tool, or 0
    if it is not known.
  @end{short}
  When non-zero, the identificator is unique for the given tool model, meaning
  that two identical tools will share the same hardware ID, but will have
  different serial numbers. See the @fun{gdk:device-tool-serial} function.

  This is a more concrete and device specific method to identify a
  device tool than the @fun{gdk:device-tool-tool-type} function, as a tablet may
  support multiple devices with the same @sym{gdk:device-tool-type} value, but
  having different hardware identificators.
  @see-class{gdk:device-tool}
  @see-symbol{gdk:device-tool-type}
  @see-function{gdk:device-tool-serial}
  @see-function{gdk:device-tool-tool-type}")

;;; --- gdk:device-tool-serial -------------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "serial" 'device-tool) t)
 "The @code{serial} property of type @code{:uint64}
  (Read / Write / Construct Only) @br{}
  The serial number. @br{}
  Default value: 0")

#+liber-documentation
(setf (liber:alias-for-function 'device-tool-serial)
      "Accessor"
      (documentation 'device-tool-serial 'function)
 "@version{2025-08-04}
  @syntax{(gdk:device-tool-serial object) => serial}
  @argument[object]{a @class{gdk:device-tool} object}
  @argument[serial]{an unsigned integer for the serial ID for the device tool}
  @begin{short}
    The accessor for the @slot[gdk:device-tool]{serial} slot of the
    @class{gdk:device-tool} class gets the serial of the device tool.
  @end{short}
  The value can be used to identify a physical tool, for example, a tablet pen,
  across program executions.
  @see-class{gdk:device-tool}")

;;; --- gdk:device-tool-tool-type ----------------------------------------------

#+liber-documentation
(setf (documentation (liber:slot-documentation "tool-type" 'device-tool) t)
 "The @code{tool-type} property of type @sym{gdk:device-tool-type}
  (Read / Write / Construct Only) @br{}
  The tool type. @br{}
  Default value: @val[gdk:device-tool-type]{:unknown}")

#+liber-documentation
(setf (liber:alias-for-function 'device-tool-tool-type)
      "Accessor"
      (documentation 'device-tool-tool-type 'function)
 "@version{2025-07-31}
  @syntax{(gdk:device-tool-tool-type object) => tool-type}
  @argument[object]{a @class{gdk:device-tool} object}
  @argument[tool-type]{a @sym{gdk:device-tool-type} value}
  @begin{short}
    The accessor for the @slot[gdk:device-tool]{tool-type} slot of the
    @class{gdk:device-tool} class gets the @sym{gdk:device-tool-type} value of
    the device tool.
  @end{short}
  This can be used to figure out what sort of pen is being used, such as an
  airbrush or a pencil.
  @see-class{gdk:device-tool}
  @see-symbol{gdk:device-tool-type}")

;;; --- End of file gdk4.device-tool.lisp --------------------------------------
